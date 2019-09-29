import isapi.files
import asyncio
import signal
import sys


loop = asyncio.get_event_loop()


async def poll():
    print("poll… ", end="", flush=True)
    await asyncio.sleep(5)
    print("done", flush=True)


async def poller():
    while True:
        start = loop.time()
        poll_task = loop.create_task(poll())
        try:
            await asyncio.shield(poll_task)
        except asyncio.CancelledError:
            await poll_task
            raise
        await asyncio.sleep(max(0, 10.0 - (loop.time() - start)))


poller_task = loop.create_task(poller())


def stop():
    poller_task.cancel()
    print("cancellation pending… ", end="", flush=True)


loop.add_signal_handler(signal.SIGTERM, stop)

try:
    loop.run_until_complete(poller_task)
except asyncio.CancelledError:
    print("Cancelled, exiting…", file=sys.stderr)
