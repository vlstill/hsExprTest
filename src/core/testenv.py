import tempfile
import config
import os.path
import aiofiles  # type: ignore
import posix1e   # type: ignore
import pwd
import os
import sys
import asyncio
import subprocess
import contextlib
import json
from typing import Tuple, List, Optional


class PointEntry:
    def __init__(self, points : int, out_of : int, comment : str, **kvargs):
        self.points = points
        self.out_of = out_of
        self.comment = comment


class RunResult:
    def __init__(self, res : bool, stdout : str, stderr : str,
                 points : List[PointEntry]):
        self.result = res
        self.stdout = stdout
        self.stderr = stderr
        self.points = points


class TestEnvironment(object):
    def __init__(self, question : str, answer : str, course : config.Course):
        self.question = question
        self.answer = answer
        self.course = course
        self.tmpdirHandle = tempfile.TemporaryDirectory(prefix="exprtest.")

    async def __aenter__(self):
        self.tmpdir = self.tmpdirHandle.__enter__()

        if self.course.isolation:
            user = f"rc-{self.course.name}"
            uid = pwd.getpwnam(user).pw_uid
            acl = posix1e.ACL(file=self.tmpdir)

            e = acl.append()
            # add entry for test user to ensure it can access test files
            e.tag_type = posix1e.ACL_USER
            e.qualifier = uid
            e.permset.clear()
            e.permset.read = True
            e.permset.write = True
            e.permset.execute = True
            acl.calc_mask()
            acl.applyto(self.tmpdir)

            # add another default entry for checker to ensure we can delete
            # everythibng
            ec = acl.append()
            ec.tag_type = posix1e.ACL_USER
            ec.qualifier = os.geteuid()
            ec.permset.clear()
            ec.permset.read = True
            ec.permset.write = True
            ec.permset.execute = True
            acl.calc_mask()
            acl.applyto(self.tmpdir, posix1e.ACL_TYPE_DEFAULT)

        ext = os.path.splitext(self.question)[1]
        self.qfile = os.path.join(self.tmpdir, f"question{ext}")
        self.afile = os.path.join(self.tmpdir, f"answer{ext}")
        async with aiofiles.open(self.question) as src:
            async with aiofiles.open(self.qfile, "w") as tgt:
                contents = await src.read()
                await tgt.write(contents)
        async with aiofiles.open(self.afile, "w") as ans:
            await ans.write(self.answer)

        return self

    @staticmethod
    async def get_points_pipe(estack : contextlib.ExitStack)\
                              -> Tuple[asyncio.StreamReader, int]:
        rfd, wfd = os.pipe()
        ro = open(rfd, 'rb', buffering=0)
        loop = asyncio.get_running_loop()
        reader = asyncio.StreamReader(loop=loop)
        protocol = asyncio.StreamReaderProtocol(reader, loop=loop)
        transport, _ = await loop.connect_read_pipe(lambda: protocol, ro)
        estack.callback(lambda t: t.close(), transport)
        return reader, wfd

    async def run(self, *options, hint : bool)\
                  -> RunResult:
        args = []
        if self.course.isolation:
            args.extend(["sudo", "-n", "-u", f"rc-{self.course.name}"])
        args.extend(self.course.checker.split(' '))
        args.extend([self.qfile, self.afile, f"-I{self.course.qdir}"])
        args.extend([f"-o{opt}" for opt in options if opt is not None])
        if hint:
            args.append("--hint")
        pass_fds : List[int] = []
        points_read : Optional[asyncio.StreamReader] = None
        points_wfd : Optional[int] = None
        points : List[PointEntry] = []
        with contextlib.ExitStack() as estack:
            if self.course.extended:
                points_read, points_wfd = await self.get_points_pipe(estack)
                args.append(f"-p{points_wfd}")
                pass_fds = [points_wfd]

            print("+ " + " ".join(args))
            proc = await asyncio.create_subprocess_exec(
                                  *args,
                                  stdin=subprocess.DEVNULL,
                                  stdout=subprocess.PIPE,
                                  stderr=subprocess.PIPE,
                                  cwd=self.tmpdir,
                                  start_new_session=True,
                                  pass_fds=pass_fds)
            if self.course.extended:
                assert points_read is not None
                assert points_wfd is not None
                os.close(points_wfd)
                (raw_stdout, raw_stderr), raw_points = await asyncio.gather(
                                        proc.communicate(), points_read.read())
                point_lines = raw_points.decode("utf8").splitlines()
                points = [PointEntry(**json.loads(x))
                                 for x in point_lines]
            else:
                raw_stdout, raw_stderr = await proc.communicate()
            stdout = raw_stdout.decode("utf8")
            stderr = raw_stderr.decode("utf8")
        return RunResult(proc.returncode == 0, stdout, stderr, points)

    async def __aexit__(self, type, value, traceback):
        self.tmpdirHandle.__exit__(type, value, traceback)

# vim: colorcolumn=80 expandtab sw=4 ts=4
