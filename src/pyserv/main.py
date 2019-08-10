#!/usr/bin/env python3

from typing import Any, Optional, Iterator, Tuple
from aiohttp import web
import asyncio
import signal
import multidict
import time


routes = web.RouteTableDef()


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


@routes.get('/')
@routes.post('/')
async def hanlde_root(request : web.Request) -> web.Response:
    data = await PostOrGet.create(request)
    print(list(data.items()))
    # name = request.match_info.get('name', "X")
    name = data.get('name', "X")
    return web.Response(text=f"Hello, {name}")


@routes.get('/demo')
@routes.post('/demo')
async def handle_demo(request : web.Request) -> web.Response:
    start = time.asctime()
    data = await PostOrGet.create(request)
    print("start handling demo")
    sleep = int(data.get("sleep", 10))
    await asyncio.sleep(sleep)
    end = time.asctime()
    print(f"ended waiting for {sleep} s, {start} -> {end}")
    return web.Response(text=f"{start} -> {end}")


def main(routes : web.RouteTableDef) -> None:
    start_web(routes)


def start_web(routes : web.RouteTableDef) -> None:
    def sigusr1_handler() -> None:
        print("Received SIGUSR1, shutting down")
        loop.create_task(runner.cleanup())

    def sigusr2_handler() -> None:
        sigusr2_cnt += 1
        print(f"SIGUSR2 {sigusr2_cnt}")

    async def stop_loop(app) -> None:
        loop.stop()

    async def start_web(runner):
        await runner.setup()
        site = web.TCPSite(runner, 'localhost', 8080)
        await site.start()

    sigusr2_cnt = 0

    app = web.Application()
    app.add_routes(routes)
    runner = web.AppRunner(app)
    app.on_cleanup.append(stop_loop)

    loop = asyncio.get_event_loop()
    loop.add_signal_handler(signal.SIGUSR1, sigusr1_handler)
    loop.add_signal_handler(signal.SIGUSR2, sigusr2_handler)
    loop.create_task(start_web(runner))

    print("starting")
    try:
        loop.run_forever()
    finally:
        loop.close()


if __name__ == "__main__":
    main(routes)
