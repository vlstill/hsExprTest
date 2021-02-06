# (c) 2019–2021 Vladimír Štill <code@vstill.eu>

from __future__ import annotations

from typing import Any, Optional, Iterator, Tuple, Union, Dict, List, \
    TypeVar, Callable, Awaitable, overload
from aiohttp import web
from glob import glob
import asyncio
import enum
import signal
import multidict
import time
import sys
import os.path
import testenv
import traceback
import socket
import subprocess
import textwrap
import aiohttp_jinja2  # type: ignore
import jinja2
import yaml
import re
import html
import logging

from db import DB
import config
import cgroup
import functor
import admin


ta = TypeVar("ta")
QSET_RE = re.compile(r"/el/(?P<faculty>[^/]*)/(?P<semester>[^/]*)/(?P<course>[^/]*)/odp/tb/(?P<file>.*[.]qdefx)")  # noqa: E501


class PostOrGet:
    """
    A wrapper for accessing either POST or GET (query) arguments in an uniform
    way. Please do not use the constructor, use coroutine 'create'.
    Prefers POST.
    """

    def __init__(self) -> None:
        """Creates an empty PostOrGet, do not use directly, use 'create'"""
        self.post: Optional[multidict.MultiDictProxy[
                            Union[str, bytes, web.FileField]]] = None
        self.query: Optional[multidict.MultiDictProxy[str]] = None

    @staticmethod
    async def create(request: web.Request) -> PostOrGet:
        self = PostOrGet()
        self.post = await request.post()
        self.query = request.query
        return self

    @overload
    def get(self, key: str) -> Optional[str]: ...
    @overload
    def get(self, key: str, default: str) -> str: ...

    def get(self, key: str, default: Optional[str] = None) -> Optional[str]:
        """
        Gets value for given key, prefering post parameters and falling back to
        query if needed.
        """
        assert self.post is not None
        assert self.query is not None
        try:
            v = self.post[key]
            assert isinstance(v, str)
            return v
        except KeyError:
            return self.query.get(key, default)

    def items(self) -> Iterator[Tuple[str, Any]]:
        """Iterates over all post and query parameters"""
        assert self.post is not None
        assert self.query is not None
        yield from self.post.items()
        yield from self.query.items()


class InvalidInput(Exception):
    pass


class MissingField(InvalidInput):
    def __init__(self, name: str, key: str) -> None:
        InvalidInput.__init__(self, f"Evaluator error: "
                              f"Missing mandatory parameter `{key}' ({name})")


class InterfaceMode (enum.Flag):
    Null = 0
    IS = enum.auto()
    Priviledged = enum.auto()


class EvalTask:
    @staticmethod
    def parse_qid(qid: str) -> Tuple[Optional[str], Optional[str]]:
        if qid is None:
            return (None, None)
        s = qid.split("?", 1)
        if len(s) == 1:
            return (s[0], None)
        return (s[0] or None, s[1])

    def __init__(self, data: PostOrGet, mode: InterfaceMode) -> None:
        def ifis(a: ta, b: Optional[ta] = None) -> ta:
            if InterfaceMode.IS in mode or b is None:
                return a
            return b

        def getMandatory(a: str, b: Optional[str] = None,
                         info: Optional[str] = None) -> str:
            key = ifis(a, b)
            v = data.get(key)
            if v is None:
                raise MissingField(info or b or a, key)
            return v

        self.course_id = getMandatory("kod", "course_id", "course ID").lower()
        self.question_id, self.option = EvalTask.parse_qid(getMandatory("id"))
        self.answer = getMandatory("odp", "answer")
        self.student_id = functor.mapO(functor.readInt, data.get("uco"))
        self.view_only = ifis(data.get("zobrazeni") == "p", False)
        self.qset = None

        if InterfaceMode.IS in mode:
            self.qset = os.path.normpath(data.get("sada", "NO_QSET_GIVEN"))
            qset_match = QSET_RE.fullmatch(self.qset)
            if qset_match:
                self.course_id = qset_match.group('course').lower()
            elif InterfaceMode.Priviledged in mode:
                raise InvalidInput(
                    f"Questionare `{self.qset}' is not authorized")

        if self.question_id is None and self.option is None:
            raise MissingField("question ID", "id")


async def handle_evaluation(conf: config.Config, slots: cgroup.SlotManager,
                            db: DB, data: PostOrGet,
                            mode: InterfaceMode) \
        -> Tuple[bool, str, List[testenv.PointEntry]]:
    try:
        task = EvalTask(data, mode)

        course = conf.courses.get(task.course_id)
        if course is None:
            raise InvalidInput(f"Course {task.course_id} not defined")
        if InterfaceMode.Priviledged not in mode and not course.hint:
            raise InvalidInput(
                "This course does not allow unathorized (hint) access")

        question: Optional[str] = None

        cache_res = await db.get(course=course,
                                 question=task.question_id,
                                 option=task.option,
                                 answer=task.answer,
                                 author=task.student_id,
                                 hint=InterfaceMode.Priviledged not in mode)
        if cache_res is not None and cache_res.result is not None:
            run_res = cache_res.result
        else:
            if task.question_id is not None:
                if os.path.isabs(task.question_id) \
                        or task.question_id[0:1] == '.':
                    raise InvalidInput(
                        f"Invalid question ID {task.question_id}")
                qglobs = glob(os.path.join(
                              course.qdir, f"{task.question_id}.q*"))
                question_candidates = list(filter(os.path.isfile, qglobs))

                if len(question_candidates) == 0:
                    raise InvalidInput("No questions found for ID "
                                       f"{task.question_id}", qglobs)
                if len(question_candidates) > 1:
                    raise InvalidInput("Too many questions found for ID "
                                       f"{task.question_id} "
                                       f"({question_candidates})")
                question = question_candidates[0]
            async with testenv.TestEnvironment(question, task.answer,
                                               course, slots) as env:
                run_res = await env.run(
                            task.option,
                            hint=InterfaceMode.Priviledged not in mode)
                await db.set(cache_res, run_res)

        log = 80 * "="
        log += textwrap.dedent(f"""
              date: {time.asctime()}
              course_id: {task.course_id}
              question_id: {task.question_id}
              option: {task.option}
              qdir: {course.qdir}
              question_id: {task.question_id}
              question: {question}
              student_id: {task.student_id}
              qset: {task.qset}
              view_only: {task.view_only}
              interface_mode: {mode}
              cached: {cache_res is not None and cache_res.result is not None}
              answer: |
              """)
        log += textwrap.indent(task.answer, "    ")
        log += "\nlog: |\n"
        log += textwrap.indent(run_res.stderr, "    ")
        log += "\n"
        log += yaml.safe_dump({"points":
                              [p.__dict__ for p in run_res.points]})
        log += f"\nresult: {run_res.result}\nreply: |\n"
        log += textwrap.indent(run_res.stdout, "    ")
        print(log, file=sys.stderr, flush=True)

        output = run_res.stdout
        if (InterfaceMode.IS in mode and course.escape_is) \
                or data.get("escape", "true").lower() == "true":
            output = "<pre class=\"exprtest-result-escaped\">\n" \
                     f"{html.escape(output, quote=True)}</pre>"

        return (run_res.result, output, run_res.points)

    except InvalidInput as ex:
        print(f"ERROR: {ex}", file=sys.stderr)
        return (False, str(ex), [])
    except Exception as ex:
        traceback.print_exc()
        return (False, f"Error while evaluating: {ex}", [])


def get_eval_handler(eval_sem: asyncio.BoundedSemaphore, conf: config.Config,
                     slots: cgroup.SlotManager, db: DB,
                     mode: InterfaceMode) \
                     -> Callable[[web.Request], Awaitable[web.Response]]:
    headers: Dict[str, str] = {}
    if InterfaceMode.Priviledged not in mode and conf.hint_origin is not None:
        headers["Access-Control-Allow-Methods"] = "POST"
        headers["Access-Control-Allow-Origin"] = conf.hint_origin

    async def handle_eval(request: web.Request) -> web.Response:
        async with eval_sem:
            start = time.perf_counter()
            data = await PostOrGet.create(request)
            (result, comment, points) = await handle_evaluation(conf, slots,
                                                                db,
                                                                data, mode)
            end = time.perf_counter()
            print(f"Handled in {end - start}", file=sys.stderr, flush=True)

            if InterfaceMode.IS not in mode:
                dpoints = [p.__dict__ for p in points]
                return web.json_response({"result": result,
                                          "comment": comment,
                                          "points": dpoints},
                                         headers=headers)
            else:
                tpoints = '\n'.join([f"{p.comment}: {p.points}/{p.out_of}"
                                     for p in points])
                if points:
                    comment = f"{tpoints}\n\n{comment}"

                oknok = "ok" if result else "nok"
                return web.Response(text=f"{oknok}~~{comment}\n",
                                    headers=headers)

    return handle_eval


def get_handle_admin(conf: config.Config) \
        -> Callable[[web.Request], Awaitable[web.Response]]:
    async def handle_admin(req: web.Request) -> web.Response:
        # assuming we run behind a proxy which sets this
        auth_user = req.match_info.get("user")
        if auth_user is None:
            return web.Response(status=401, text="No user info\n")

        course_name = req.match_info.get("course_id")
        page = req.match_info.get("page")
        return await admin.get(req, conf, auth_user, course_name, page)

    return handle_admin


async def update_qdir(course: config.Course) -> Optional[str]:
    try:
        git = await asyncio.create_subprocess_exec("git", "pull", "--ff-only",
                                                   stdout=subprocess.PIPE,
                                                   stderr=subprocess.STDOUT,
                                                   cwd=course.qdir)
    except FileNotFoundError as ex:
        return f"Error while updating: {ex}"

    out, _ = await asyncio.shield(git.communicate())

    pre = course.stamp
    await course.async_update_stamp()
    logging.getLogger("update-qdir") \
           .debug(f"Update result {course.name}: {out.decode('utf-8').strip()}"
                  f"\nstamp {pre} → {course.stamp}")

    if git.returncode != 0:
        return out.decode('utf-8')
    return None


def get_handle_update(conf: config.Config) \
        -> Callable[[web.Request], Awaitable[web.Response]]:
    async def handle_update(req: web.Request) -> web.Response:
        course = req.match_info.get("course_id")
        conf.logger.debug(f"update req {course}")
        if course not in conf.courses:
            return web.Response(status=404, text=f"Not found {course}\n")
        res = await update_qdir(conf.courses[course])
        if res is None:
            return web.Response(text=f"Updated {course}")
        else:
            return web.Response(status=418,
                                text=f"Update for {course} error: {res}")
    return handle_update


def main() -> None:
    conf = config.parse(sys.argv)
    slots = cgroup.SlotManager(conf.limit)
    if not slots.available() and conf.limit.any_set():
        print("W: limits requested but cgroups are not available",
              file=sys.stderr, flush=True)
    start_web(conf, slots)


def start_web(conf: config.Config, slots: cgroup.SlotManager) -> None:
    async def shutdown() -> None:
        print("letting runner do cleanup")
        await runner.cleanup()

    def sigusr1_handler() -> None:
        print("Received SIGUSR1, shutting down...")
        loop.create_task(shutdown())

    async def stop_loop(app: web.Application) -> None:
        print("shutdown")
        loop.stop()

    db = DB(conf)

    async def start_runner(runner: web.AppRunner, conf: config.Config) -> None:
        await db.init()
        await runner.setup()
        site: Optional[Union[web.TCPSite, web.UnixSite, web.SockSite]] = None
        if conf.port is not None:
            print(f"Starting HTTP server on localhost:{conf.port}")
            site = web.TCPSite(runner, 'localhost', conf.port)
        elif conf.socket is not None:
            print(f"Starting UNIX socket server on {conf.socket}")
            site = web.UnixSite(runner, conf.socket)
        elif conf.socket_fd is not None:
            print(f"Starting UNIX socket server on FD {conf.socket_fd}")
            sock = socket.socket(fileno=conf.socket_fd)
            site = web.SockSite(runner, sock)
        assert site is not None, "Invalid config, no listening address"
        await site.start()

    app = web.Application()
    templates_dir = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "templates")
    aiohttp_jinja2.setup(app, loader=jinja2.FileSystemLoader(templates_dir))

    eval_sem = asyncio.BoundedSemaphore(conf.max_workers)

    handle_is = get_eval_handler(eval_sem, conf, slots, db,
                                 InterfaceMode.IS | InterfaceMode.Priviledged)
    app.router.add_get("/is", handle_is)
    app.router.add_post("/is", handle_is)

    handle_hint = get_eval_handler(eval_sem, conf, slots, db,
                                   InterfaceMode.Null)
    app.router.add_get("/hint", handle_hint)
    app.router.add_post("/hint", handle_hint)

    handle_internal = get_eval_handler(eval_sem, conf, slots, db,
                                       InterfaceMode.Priviledged)
    app.router.add_get("/internal", handle_internal)
    app.router.add_post("/internal", handle_internal)

    app.router.add_get("/update-qdir/{course_id}",  # noqa: FS003
                       get_handle_update(conf))
    app.router.add_post("/update-qdir/{course_id}",  # noqa: FS003
                        get_handle_update(conf))

    handle_admin = get_handle_admin(conf)
    app.router.add_get("/admin/{user}/", handle_admin)  # noqa: FS003
    app.router.add_get("/admin/{user}/{course_id}/", handle_admin)  # noqa: FS003, E501
    app.router.add_post("/admin/{user}/{course_id}/", handle_admin)  # noqa: FS003, E501
    app.router.add_get("/admin/{user}/{course_id}/{page}", handle_admin)  # noqa: FS003, E501
    app.router.add_post("/admin/{user}{course_id}/{page}", handle_admin)  # noqa: FS003, E501

    runner = web.AppRunner(app, handle_signals=True)
    app.on_cleanup.append(stop_loop)

    loop = asyncio.get_event_loop()
    loop.add_signal_handler(signal.SIGUSR1, sigusr1_handler)
    try:
        loop.run_until_complete(start_runner(runner, conf))
    except Exception:
        print("ERROR starting server", file=sys.stderr)
        traceback.print_exc()
        sys.exit(1)

    print("started, loaded following configuration:")
    conf.dump(sys.stdout)
    try:
        loop.run_forever()
    finally:
        loop.close()


if __name__ == "__main__":
    main()

# vim: colorcolumn=80 expandtab sw=4 ts=4
