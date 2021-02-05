# (c) 2019–2021 Vladimír Štill <code@vstill.eu>

import aiohttp_jinja2  # type: ignore
import config
import dateutil.parser
import datetime
import functor

from cache import Cache
from dataclasses import dataclass
from aiohttp import web
from typing import Optional, Dict, Any


def parse_date(date: Optional[str], time: Optional[str],
               defhour: int = 0, defminute: int = 0,
               defsecond: int = 0, defmicrosecond: int = 0) \
        -> Optional[datetime.datetime]:
    try:
        dt = dateutil.parser.parse(f"{date} {time or ''}")
        if not time:
            dt = dt.replace(hour=defhour, minute=defminute)
        return dt.replace(second=defsecond, microsecond=defmicrosecond)
    except ValueError:
        return None


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

        course = self.conf.courses.get(self.course)
        if course is None:
            return await self._e404(f"course {self.course}")
        if self.user not in course.authorized:
            return self._forbidden()

        if self.page is None:
            return await self.log_summary(course)

        if self.page == "log":
            return await self.log(course)

        if self.page == "log_detail":
            id_ = self.req.query.get("id")
            if id_ is not None and id_.isdigit():
                return await self.log_detail(course, int(id_))
            return await self._e404("id not specified or invalid")

        return await self._e404(f"page {page}")

    async def _e404(self, extra: str = "") -> web.Response:
        if extra:
            extra = f" ({extra})"
        return web.Response(status=404, text=f"404 not found{extra}")

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
                                dates=dates,
                                today=datetime.datetime.now().date())

    async def log(self, course: config.Course) -> web.Response:
        from_ = parse_date(self.req.query.get("from"),
                           self.req.query.get("from_time")) \
            or datetime.datetime(1, 1, 1)
        to = parse_date(self.req.query.get("to"),
                        self.req.query.get("to_time"),
                        23, 59, 59, 999999) \
            or datetime.datetime(9999, 12, 31)
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
            return self._render("admin/log.html.j2",
                                **{"rows": rows, "from": from_, "to": to})

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
