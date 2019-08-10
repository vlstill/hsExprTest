#!/usr/bin/env python3

from aiohttp import web
import asyncio
import signal

routes = web.RouteTableDef()


@routes.get('/')
@routes.post('/')
async def handle(request : web.Request) -> web.Response:
    data = await request.post()
    print(list(data.items()))
    # name = request.match_info.get('name', "X")
    name = data.get('name', "X")
    return web.Response(text=f"Hello, {name}")


async def stop_loop(app) -> None:
    global loop
    loop.stop()


app = web.Application()
app.add_routes(routes)
runner = web.AppRunner(app)
loop = asyncio.get_event_loop()
app.on_cleanup.append(stop_loop)


async def start_web() -> None:
    global runner
    await runner.setup()
    site = web.TCPSite(runner, 'localhost', 8080)
    await site.start()


sigusr2_cnt = 0


def sigusr2_handler() -> None:
    global sigusr2_cnt
    sigusr2_cnt += 1
    print(f"SIGUSR2 {sigusr2_cnt}")


def sigusr1_handler() -> None:
    global loop
    global runner
    print("Received SIGUSR1, shutting down")
    loop.create_task(runner.cleanup())


print("starting")
loop.add_signal_handler(signal.SIGUSR1, sigusr1_handler)
loop.add_signal_handler(signal.SIGUSR2, sigusr2_handler)
loop.create_task(start_web())
try:
    loop.run_forever()
finally:
    loop.close()
