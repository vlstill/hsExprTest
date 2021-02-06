# (c) 2021 Vladimír Štill <code@vstill.eu>

from typing import Optional, Union, AsyncGenerator
from dataclasses import dataclass
from config import Config, Course
from testenv import RunResult
from contextlib import asynccontextmanager

import asyncpg  # type: ignore
import logging
import hashlib


def sha256(data: Union[str, bytes]) -> bytes:
    if isinstance(data, str):
        data = data.encode("utf-8")
    return hashlib.sha256(data).digest()


@dataclass
class CacheRes:
    req_id: int
    rev_id: int
    result: Optional[RunResult] = None


class DB:
    def __init__(self, config: Config) -> None:
        self.config = config
        self.log = logging.getLogger("cache")

    @asynccontextmanager
    async def connect(self) -> AsyncGenerator[asyncpg.Connection, None]:
        assert self.config.postgres_cache
        conn = await asyncpg.connect(
            user=self.config.postgres_user,
            database="exprtest",
            host=self.config.postgres_host)
        await conn.execute("set search_path to exprtest")
        try:
            yield conn
        finally:
            await conn.close()

    @staticmethod
    async def create_schema_if_not_exists(name: str, conn: asyncpg.Connection)\
            -> None:
        """Creates a schema if it does not exist, but in a way that does not
        violate permissions if the schema already exists
        (unlike CREATE SCHEMA IF NOT EXISTS command)."""
        val = await conn.fetchval("""
            select count(*) from information_schema.schemata
              where schema_name = $1
            """, name)
        if val == 0:
            await conn.execute(f"create schema {name}")

    async def init(self) -> None:
        if not self.config.postgres_cache:
            return
        self.log.debug("db init start")
        async with self.connect() as conn:
            async with conn.transaction():
                await DB.create_schema_if_not_exists("exprtest", conn)
                await conn.execute("set search_path to exprtest")
                utc_now_defined = await conn.fetchval("""
                    select exists(
                        select * from pg_proc where proname = 'utc_now_st'
                    )
                    """)
                if not utc_now_defined:
                    await conn.execute("""
                        create function utc_now_st()
                          returns timestamp without time zone
                          language sql security definer
                          as $$
                              select statement_timestamp() at time zone 'utc'
                          $$
                          """)
                await conn.execute("""
                    create table if not exists exprtest.content (
                      sha bytea not null primary key,
                      data bytea not null,
                      constraint content_sha_check check ((sha = sha256(data)))
                    );

                    create table if not exists exprtest.course_revision (
                      id serial primary key,
                      course bytea not null,
                      stamp bytea not null,
                      constraint course_revision_unique
                        unique (course, stamp)
                    );

                    create table if not exists exprtest.eval_data (
                      id serial primary key,
                      course bytea not null,
                      question bytea not null,
                      option bytea not null,
                      hint boolean not null,
                      student_sha bytea references content ( sha ) not null,
                      unique ( course, question, option, hint, student_sha )
                    );

                    create table if not exists exprtest.eval_cache (
                      id serial primary key,
                      revision_id int references course_revision ( id )
                          not null,
                      data_id int references eval_data ( id ) not null,
                      result boolean not null,
                      out_sha bytea references content ( sha ) not null,
                      err_sha bytea references content ( sha ) not null,
                      unique ( revision_id, data_id, result, out_sha, err_sha )
                    );

                    create table if not exists exprtest.eval_request (
                      id serial primary key,
                      author int null,
                      data_id int references eval_data ( id ) not null,
                      cache_id int references eval_cache ( id )
                          null default null,
                      stamp timestamp without time zone
                          default utc_now_st() not null
                    );

                    create index if not exists eval_cache_lookup_idx
                        on eval_cache ( revision_id, data_id );

                    create or replace view eval_log as
                        select course,
                               author,
                               question,
                               option,
                               hint,
                               result,
                               o.data as output,
                               e.data as errors,
                               revision_id,
                               stamp,
                               eval_request.id as id,
                               a.data as answer
                        from eval_request
                        join eval_data on ( data_id = eval_data.id)
                        left join eval_cache on ( cache_id = eval_cache.id )
                        join content as e on ( err_sha = e.sha )
                        join content as o on ( out_sha = o.sha )
                        join content as a on ( student_sha = a.sha );

                    create or replace view usage as
                        with
                          cache as (
                            select count(*) as cache,
                                   course
                              from eval_cache
                              join eval_data on ( data_id = eval_data.id )
                              group by ( course )
                          ),
                          req as (
                            select count(*) as req,
                                   course
                              from eval_request
                              join eval_data on ( data_id = eval_data.id )
                              group by ( course )
                          ),
                          stat as (
                              select *
                                from cache
                                natural join req
                          )
                        select course,
                               req,
                               cache,
                               100 - cache :: float * 100 / req as ratio
                          from stat
                        union all
                        select 'all' as course,
                               sum( req ) as req,
                               sum( cache ) as cache,
                               100 - 100 * sum( cache )::float / sum( req ) as ratio
                          from stat;
                    """)
                self.log.debug("db initialized")

    async def get(self, course: Course, question: Optional[str],
                  option: Optional[str], author: Optional[int], answer: str,
                  hint: bool) \
            -> Optional[CacheRes]:
        if not self.config.postgres_cache:
            return None
        if option is None:
            option = ""
        if question is None:
            question = ""

        async with self.connect() as conn:
            async with conn.transaction():
                student_sha = await self._get_content(conn, answer)

                course_name_bytes = course.name.encode('utf-8')
                data_args = (course_name_bytes, question.encode('utf-8'),
                             option.encode('utf-8'), hint, student_sha)
                data_id = await conn.fetchval("""
                    insert into eval_data
                        ( course, question, option, hint, student_sha )
                        values ( $1, $2, $3, $4, $5 )
                        on conflict do nothing
                        returning ( id )
                    """, *data_args)
                if data_id is None:
                    data_id = await conn.fetchval("""
                        select id from eval_data
                            where course = $1 and question = $2 and option = $3
                              and hint = $4 and student_sha = $5
                        """, *data_args)

                assert data_id is not None

                req_id = await conn.fetchval("""
                    insert into eval_request ( author, data_id )
                        values ( $1, $2 )
                        returning ( id )
                    """, author, data_id)

                course_stamp = course.full_stamp().encode('utf-8')
                rev_id = await conn.fetchval("""
                    insert into course_revision ( course, stamp )
                        values ( $1, $2 )
                        on conflict do nothing
                        returning ( id )
                    """, course_name_bytes, course_stamp)
                if rev_id is None:
                    rev_id = await conn.fetchval("""
                        select id from course_revision
                          where course = $1
                            and stamp = $2
                        """, course_name_bytes, course_stamp)

                # please note that there can be duplicate entries, however,
                # these should be equivalent if the evaluator is correct
                result = await conn.fetchrow("""
                    select id, result, o.data as out, e.data as err
                        from eval_cache
                        join content as o on ( out_sha = o.sha )
                        join content as e on ( err_sha = e.sha )
                        where revision_id = $1 and data_id = $2
                        limit 1
                    """, rev_id, data_id)  # TODO timeout
                if result is None:
                    self.log.debug(f"cache miss for {req_id}")
                    return CacheRes(req_id, rev_id)
                await self._report_hit(conn, req_id, result[0])
                return CacheRes(req_id, rev_id,
                                RunResult(result[1], result[2].decode('utf-8'),
                                          result[3].decode('utf-8'), []))

    async def _report_hit(self, conn: asyncpg.Connection, req_id: int,
                          cache_id: int) -> None:
        self.log.debug(f"cache hit for {req_id} {cache_id}")
        await conn.execute("""
            update eval_request
                set cache_id = $1
                where id = $2
            """, cache_id, req_id)

    async def _get_content(self, conn: asyncpg.Connection, data: str) -> bytes:
        sha = sha256(data)
        await conn.execute("""
            insert into content ( sha, data )
                values ( $1, $2 )
                on conflict do nothing
            """, sha, data.encode('utf-8'))
        return sha

    async def set(self, cache_handle: Optional[CacheRes],
                  result: RunResult) -> None:
        if not self.config.postgres_cache or cache_handle is None:
            return
        req_id = cache_handle.req_id
        rev_id = cache_handle.rev_id
        
        async with self.connect() as conn:
            async with conn.transaction():
                self.log.debug(f"set cache {rev_id}")
                data_id = await conn.fetchval(
                    "select data_id from eval_request where id = $1", req_id)
                assert data_id is not None

                out_sha = await self._get_content(conn, result.stdout)
                err_sha = await self._get_content(conn, result.stderr)

                args = (rev_id, data_id, result.result, out_sha, err_sha)
                cache_id = await conn.fetchval("""
                    insert into eval_cache
                        ( revision_id, data_id, result, out_sha, err_sha )
                        values ( $1, $2, $3, $4, $5 )
                        on conflict do nothing
                        returning ( id )
                    """, *args)
                if cache_id is None:
                    cache_id = await conn.fetchval("""
                        select id from eval_cache
                            where revision_id = $1 and data_id = $2
                              and result = $3 and out_sha = $4 and err_sha = $5
                        """, *args)
                await self._report_hit(conn, req_id, cache_id)


# vim: colorcolumn=80 expandtab sw=4 ts=4
