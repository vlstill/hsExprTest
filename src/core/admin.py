from typing import Optional, Dict, Any
import typing
from aiohttp import web
import aiohttp_mako  # type: ignore
import io
import config

def render_template(name : str, req : web.Request, context : Dict[str, Any],
                    content_type : str = "text/html") -> web.Response:
    resp = typing.cast(web.Response,
                       aiohttp_mako.render_template(name, req, context))
    resp.content_type = content_type
    return resp


async def dispatch_index(req : web.Request, conf : config.Course, user : str) \
        -> web.Response:
    context = {"config": conf.dump(expand=True),
               "course": conf.name,
               "links": ["simulate submission", "logs"]}
    return render_template("admin/index.html", req, context)


async def dispatch_404(req : web.Request, conf : config.Course, user : str) \
        -> web.Response:
    return web.Response(status=404, text="404 not found")


dispatch = {None: dispatch_index,
            "index.html": dispatch_index}

async def get(req : web.Request, conf : config.Course, user : str,
              page : Optional[str]) -> web.Response:
    return await (dispatch.get(page, dispatch_404))(req, conf, user)

# vim: colorcolumn=80 expandtab sw=4 ts=4
