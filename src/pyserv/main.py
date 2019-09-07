#!/usr/bin/env python3

from typing import Any, Optional, Iterator, Tuple, Union, Dict
from aiohttp import web
import asyncio
import signal
import multidict
import time
import config
import sys
from glob import glob
import os.path
import testenv
import traceback
import functor
import socket
import textwrap


class PostOrGet:
    """
    A wrapper for accessing either POST or GET (query) arguments in an uniform
    way. Please do not use the constructor, use coroutine 'create'.
    Prefers POST.
    """

    def __init__(self):
        """Creates an empty PostOrGet, do not use directly, use 'create'"""
        self.post : Optional[multidict.MultiDictProxy] = None
        self.query : Optional[multidict.MultiDictProxy] = None

    @staticmethod
    async def create(request : web.Request):
        self = PostOrGet()
        self.post = await request.post()
        self.query = request.query
        return self

    def get(self, key : str, default : Any = None) -> Any:
        """
        Gets value for given key, prefering post parameters and falling back to
        query if needed.
        """
        assert self.post is not None
        assert self.query is not None
        try:
            return self.post[key]
        except KeyError:
            return self.query.get(key, default)

    def items(self) -> Iterator[Tuple[str, Any]]:
        """Iterates over all post and query parameters"""
        assert self.post is not None
        assert self.query is not None
        yield from self.post.items()
        yield from self.query.items()


async def handle_evaluation(conf : config.Config, data : PostOrGet,
                            hint : bool) -> Tuple[str, str]:
    def error(msg : str, extra="") -> Tuple[str, str]:
        print(f"ERROR: {msg}\n    {extra}", file=sys.stderr)
        return ("nok", msg)

    def missing(name : str, key : str) -> Tuple[str, str]:
        return error("Evaluator error: "
                     f"Missing mandatory parameter `{key}' ({name})")

    def parse_qid(qid : Optional[str]) -> Tuple[Optional[str], Optional[str]]:
        if qid is None:
            return (None, None)
        s = qid.split("?", 1)
        if len(s) == 1:
            return (s[0], None)
        return (s[0], s[1])

    try:
        course_id = functor.fmapO(str.lower, data.get("kod"))
        question_id, option = parse_qid(data.get("id"))
        answer = data.get("odp")
        if course_id is None:
            return missing("course ID", "kod")
        if question_id is None:
            return missing("question ID", "id")
        if answer is None:
            return missing("answer", "odp")
        student_id = functor.fmapO(functor.rint, data.get("uco"))
        qset = data.get("sada")
        view_only = data.get("zobrazeni") == "p"

        course = conf.courses.get(course_id)
        if course is None:
            return error(f"Course {course_id} not defined")
        if hint and not course.hint:
            return error(
                "This course does not allow unathorized (hint) access")
        qglobs = glob(os.path.join(course.qdir, f"{question_id}.q*"))
        question_candidates = list(filter(os.path.isfile, qglobs))

        if len(question_candidates) == 0:
            return error(f"No questions found for ID {question_id}", qglobs)
        if len(question_candidates) > 1:
            return error(f"Too many questions found for ID {question_id} "
                         f"({question_candidates})")
        question = question_candidates[0]

        async with testenv.TestEnvironment(question, answer, course) as env:
            res, comment = await env.run(option, hint=hint)

            log = 80 * "="
            log += textwrap.dedent(f"""
                          date: {time.asctime()}
                          course_id: {course_id}
                          question_id: {question_id}
                          option: {option}
                          qdir: {course.qdir}
                          question_candidates: {question_candidates}
                          student_id: {student_id}
                          qset: {qset}
                          view_only: {view_only}
                          hint: {hint}
                          answer:
                          """)
            log += textwrap.indent(answer, "    ")
            log += f"\nresult: {res}\nreply:\n"
            log += textwrap.indent(comment, "    ")
            print(log, file=sys.stderr)

            return (res, comment)

    except Exception as ex:
        traceback.print_exc()
        return error(f"Error while evaluating: {ex}")


def get_eval_handler(eval_sem : asyncio.BoundedSemaphore, conf : config.Config,
                     hint : bool):
    headers : Dict[str, str] = {}
    if hint and conf.hint_origin is not None:
        headers["Access-Control-Allow-Methods"] = "POST"
        headers["Access-Control-Allow-Origin"] = conf.hint_origin
    async def handle_eval(request : web.Request) -> web.Response:
        async with eval_sem:
            start = time.perf_counter()
            data = await PostOrGet.create(request)
            print("start handling IS")
            (points, response) = await handle_evaluation(conf, data, hint=hint)
            end = time.perf_counter()
            return web.Response(text=f"{points}~~{response}\n"
                                     f"Handled in {end - start:0.1f} s\n",
                                headers=headers)

    return handle_eval


async def handle_admin(req : web.Request) -> web.Response:
    auth_user = req.headers.get("X-Auth-User") # assuming we run behind a proxy which sets this
    if auth_user is None:
        return web.Response(status=401, text="no user info\n")
    print(f"HTTP auth user {auth_user}")
    return web.Response(text=f"Authenticated as {auth_user}\n")


def main() -> None:
    conf = config.parse(sys.argv)
    start_web(conf)


def start_web(conf : config.Config) -> None:
    async def shutdown():
        for i in range(conf.max_workers):
            print(f"Shutdown: Blocking slot {i}...")
            await eval_sem.acquire()
        print("Shutdown: done blocking, ready to shutdown")
        await runner.cleanup()

    def sigusr1_handler() -> None:
        print("Received SIGUSR1, shutting down...")
        loop.create_task(shutdown())

    def sigusr2_handler() -> None:
        sigusr2_cnt += 1
        print(f"SIGUSR2 {sigusr2_cnt}")

    async def stop_loop(app) -> None:
        loop.stop()

    async def start_runner(runner, conf : config.Config):
        await runner.setup()
        site : Optional[Union[web.TCPSite, web.UnixSite, web.SockSite]] = None
        if conf.port is not None:
            print(f"Starting HTTP on localhost:{conf.port}")
            site = web.TCPSite(runner, 'localhost', conf.port)
        elif conf.socket is not None:
            print(f"Starting UNIX on {conf.socket}")
            site = web.UnixSite(runner, conf.socket)
        elif conf.socket_fd is not None:
            print(f"Starting UNIX of FD {conf.socket_fd}")
            sock = socket.socket(fileno=conf.socket_fd)
            site = web.SockSite(runner, sock)
        assert site is not None, "Invalid config, not listening address"
        return await site.start()

    sigusr2_cnt = 0

    app = web.Application()

    eval_sem = asyncio.BoundedSemaphore(conf.max_workers)

    handle_is = get_eval_handler(eval_sem, conf, hint=False)
    app.router.add_get("/is", handle_is)
    app.router.add_post("/is", handle_is)

    handle_hint = get_eval_handler(eval_sem, conf, hint=True)
    app.router.add_get("/hint", handle_hint)
    app.router.add_post("/hint", handle_hint)

    app.router.add_get("/admin", handle_admin)
    app.router.add_post("/admin", handle_admin)

    runner = web.AppRunner(app)
    app.on_cleanup.append(stop_loop)

    loop = asyncio.get_event_loop()
    loop.add_signal_handler(signal.SIGUSR1, sigusr1_handler)
    loop.add_signal_handler(signal.SIGUSR2, sigusr2_handler)
    try:
        loop.run_until_complete(start_runner(runner, conf))
    except Exception as ex:
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
