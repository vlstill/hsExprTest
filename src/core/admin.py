# (c) 2019–2021 Vladimír Štill <code@vstill.eu>

import typing
import aiohttp_jinja2  # type: ignore
import config

from cache import Cache
from dataclasses import dataclass
from aiohttp import web
from typing import Optional, Dict, Any


@dataclass
class Admin:
    req: web.Request
    conf: config.Config
    user: str
    course: Optional[str]

    async def dispatch(self, page: Optional[str]) -> web.Response:
        if self.course is None:
            return await self.summary()

        return await self.e404()

    async def e404(self) -> web.Response:
        return web.Response(status=404, text="404 not found")

    async def summary(self) -> web.Response:
        async with Cache(self.conf).connect() as conn:
            stats_ = await conn.fetch("select * from usage")
            stats = [[row[0].decode("utf-8"), row[1], row[2], round(row[3], 1)]
                     for row in stats_]
            since = await conn.fetchval("""
                select stamp from eval_log order by stamp asc limit 1
                """)
            return self._render("admin/summary.html.j2",
                                {"stats": stats,
                                 "since": since.replace(microsecond=0)})

    def _render(self, template: str, extra: Dict[str, Any]) -> web.Response:
        out = {"config": self.conf.to_dict(),
               "user": self.user,
               "course": self.course}
        for k, v in extra.items():
            out[k] = v
        return aiohttp_jinja2.render_template(template, self.req, out)


async def get(req: web.Request, conf: config.Config, user: str,
              course: Optional[str], page: Optional[str]) -> web.Response:
    admin = Admin(req, conf, user, course)
    return await admin.dispatch(page)

# vim: colorcolumn=80 expandtab sw=4 ts=4
