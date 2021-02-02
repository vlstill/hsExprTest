# (c) 2019–2021 Vladimír Štill <code@vstill.eu>

import aiohttp_jinja2  # type: ignore
import config
import dateutil.parser
import datetime

from cache import Cache
from dataclasses import dataclass
from aiohttp import web
from typing import Optional, Dict, Any


def parse_date(raw: str, defhour: int = 0, defminute: int = 0,
               defsecond: int = 0, defmicrosecond: int = 0) \
        -> datetime.datetime:
    dt = dateutil.parser.parse(raw)
    if " " not in raw:
        dt = dt.replace(hour=defhour, minute=defminute, second=defsecond,
                        microsecond=defmicrosecond)
    return dt


@dataclass
class Admin:
    req: web.Request
    conf: config.Config
    user: str
    course: Optional[str]
    page: Optional[str]

    async def dispatch(self, page: Optional[str]) -> web.Response:
        if self.course is None:
            return await self.summary()

        course = self.conf.courses[self.course]
        if self.user not in course.authorized:
            return self._forbidden()

        if self.page is None:
            return await self.log_summary(course)

        args_ = self.page.split(';')
        page = args_[0]
        args = {s[0]: s[1] for s in (s.split('=', 1) for s in args_[1:])}

        if page == "log":
            return await self.log(course, args)

        if page == "log_detail":
            return await self.log_detail(course, int(args["id"]))

        return await self.e404()

    async def e404(self) -> web.Response:
        return web.Response(status=404, text="404 not found")

    async def summary(self) -> web.Response:
        async with Cache(self.conf).connect() as conn:
            stats_ = await conn.fetch("select * from usage order by req desc")
            stats = [[row[0].decode("utf-8"), row[1], row[2], round(row[3], 1)]
                     for row in stats_]
            since = await conn.fetchval("""
                select stamp from eval_log order by stamp asc limit 1
                """)
            return self._render("admin/summary.html.j2",
                                stats=stats,
                                since=since.replace(microsecond=0))

    async def log_summary(self, course: config.Course) -> web.Response:
        assert self.course is not None
        async with Cache(self.conf).connect() as conn:
            dates = await conn.fetch("""
                select stamp :: date, count(*) from eval_log
                    where course = $1
                    group by ( stamp :: date )
                    order by stamp desc
                """, course.name.encode('utf-8'))
            return self._render("admin/log_summary.html.j2",
                                dates=dates)

    async def log(self, course: config.Course, args: Dict[str, str]) \
            -> web.Response:
        from_ = parse_date(args["from"])
        to = parse_date(args["to"], 23, 59, 59, 999999)
        async with Cache(self.conf).connect() as conn:
            rows = await conn.fetch("""
                select stamp as timestamp,
                       author,
                       convert_from(question, 'UTF8') as question,
                       convert_from(option, 'UTF8') as option,
                       hint,
                       result,
                       id
                    from eval_log
                    where course = $1
                      and stamp >= $2
                      and stamp <= $3
                    order by stamp asc
                """, course.name.encode('utf-8'), from_, to)
            return self._render("admin/log.html.j2", rows=rows)

    async def log_detail(self, course: config.Course, eval_id: int) \
            -> web.Response:
        async with Cache(self.conf).connect() as conn:
            row = await conn.fetchrow("""
                select author,
                       convert_from(question, 'UTF8') as question,
                       convert_from(option, 'UTF8') as option,
                       hint,
                       result,
                       convert_from(output, 'UTF8') as output,
                       convert_from(errors, 'UTF8') as errors,
                       stamp,
                       convert_from(answer, 'UTF8') as answer,
                       id
                  from eval_log
                  where course = $1 and id = $2
                """, course.name.encode('utf-8'), eval_id)
            return self._render("admin/log_detail.html.j2", entry=row)

    def _render(self, template: str, **extra: Any) -> web.Response:
        out = {"config": self.conf.to_dict(),
               "user": self.user,
               "course": self.course}
        for k, v in extra.items():
            out[k] = v
        return aiohttp_jinja2.render_template(  # type: ignore
                  template, self.req, out)

    def _forbidden(self) -> web.Response:
        return web.Response(status=403, text="403 Forbidden")


async def get(req: web.Request, conf: config.Config, user: str,
              course: Optional[str], page: Optional[str]) -> web.Response:
    admin = Admin(req, conf, user, course, page)
    return await admin.dispatch(page)

# vim: colorcolumn=80 expandtab sw=4 ts=4
