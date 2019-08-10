#!/usr/bin/env python3

from aiohttp import web
import asyncio
import signal


routes = web.RouteTableDef()


@routes.get('/')
@routes.post('/')
async def hanlde_root(request : web.Request) -> web.Response:
    data = await request.post()
    print(list(data.items()))
    # name = request.match_info.get('name', "X")
    name = data.get('name', "X")
    return web.Response(text=f"Hello, {name}")


def main(routes : web.RouteTableDef) -> None:

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
