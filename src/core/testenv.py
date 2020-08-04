import tempfile
import config
import copy
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
from pathlib import Path, PurePath
from typing import Tuple, List, Optional


import cgroup
from evalconf import EvalConf


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
    def __init__(self, question : Optional[str], answer : str,
                 course : config.Course, slots : cgroup.SlotManager):
        self.question = question
        self.answer = answer
        self.course = course
        self.slotmgr = slots
        self.tmpdirHandle = tempfile.TemporaryDirectory(prefix="exprtest.")

    @staticmethod
    def set_rwx(e) -> None:
        e.permset.clear()
        e.permset.read = True
        e.permset.write = True
        e.permset.execute = True

    def setup_isolation(self) -> None:
        if self.course.isolation:
            user = f"rc-{self.course.name}"
            uid = pwd.getpwnam(user).pw_uid
            acl = posix1e.ACL(file=self.tmpdir)

            e = acl.append()
            # add entry for test user to ensure it can access test files
            e.tag_type = posix1e.ACL_USER
            e.qualifier = uid
            self.set_rwx(e)
            acl.applyto(self.tmpdir)

            # add another default entry for checker to ensure we can delete
            # everythibng
            ec = acl.append()
            ec.tag_type = posix1e.ACL_USER
            ec.qualifier = os.geteuid()
            self.set_rwx(e)
            acl.applyto(self.tmpdir, posix1e.ACL_TYPE_DEFAULT)

            acl.calc_mask()

    def setup_config(self, path : str) -> None:
        if not self.course.extended:
            self.conffile : Optional[str] = None
            return

        self.conffile = path
        conf = EvalConf()
        conf.add(self.course.evalconf)
        conf["qdir"] = self.course.qdir
        conf["question_path"] = self.question

        qdir_p = PurePath(self.course.qdir)
        if self.question is not None:
            question_p = PurePath(self.question)
        else:
            question_p = qdir_p
        question_rel = question_p.relative_to(qdir_p).parts

        # note we are skipping the last element (file name)
        for i in range(len(question_rel)):
            conf_p = Path(qdir_p.joinpath(*question_rel[:i], "eval.conf"))
            print(f"looking for {conf_p}")
            if conf_p.exists():
                conf.load(str(conf_p))

        conf.dump(self.conffile)

    async def __aenter__(self):
        self.tmpdir = self.tmpdirHandle.__enter__()

        self.setup_isolation()

        ext = ""
        self.qfile : Optional[str] = None
        if self.question is not None:
            ext = os.path.splitext(self.question)[1]
            self.qfile = os.path.join(self.tmpdir, f"question{ext}")
        self.afile = os.path.join(self.tmpdir, f"answer{ext}")

        if self.question is not None:
            async with aiofiles.open(self.question) as src:
                async with aiofiles.open(self.qfile, "w") as tgt:
                    contents = await src.read()
                    await tgt.write(contents)
        async with aiofiles.open(self.afile, "w") as ans:
            await ans.write(self.answer)

        self.setup_config(os.path.join(self.tmpdir, "eval.conf"))

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

    async def run(self, *options, hint : bool) -> RunResult:
        with self.slotmgr.get() as slot:
            args = []
            if self.course.isolation:
                args.extend(["sudo", "-n", "-u", f"rc-{self.course.name}"])
            args.extend(self.course.checker.split(' '))
            if self.qfile is not None:
                args.append(self.qfile)
            args.extend([self.afile, f"-I{self.course.qdir}"])
            args.extend([f"-o{opt}" for opt in options if opt is not None])
            if hint:
                args.append("--hint")
            pass_fds : List[int] = []
            points_read : Optional[asyncio.StreamReader] = None
            points_wfd : Optional[int] = None
            points : List[PointEntry] = []

            env = copy.deepcopy(os.environ)
            for var in self.course.path_append:
                if var not in env:
                    env[var] = self.course.qdir
                else:
                    env[var] = f"{env[var]}:{self.course.qdir}"

            with contextlib.ExitStack() as estack:
                if self.course.extended:
                    points_read, points_wfd = await self.get_points_pipe(estack)
                    args.append(f"-p{points_wfd}")
                    pass_fds = [points_wfd]
                if self.conffile:
                    args.append(f"-c{self.conffile}")

                print("+ " + " ".join(args), file=sys.stderr, flush=True)
                preexec = None
                if self.slotmgr.available() and self.slotmgr.cg is not None:
                    preexec = lambda: self.slotmgr.cg.register_me(slot)
                proc = await asyncio.create_subprocess_exec(
                                      *args,
                                      stdin=subprocess.DEVNULL,
                                      stdout=subprocess.PIPE,
                                      stderr=subprocess.PIPE,
                                      cwd=self.tmpdir,
                                      start_new_session=True,
                                      pass_fds=pass_fds,
                                      preexec_fn=preexec,
                                      env=env)
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

                # documentation says the return code for signal termination
                # should be negative, it seems that it also might be > 127
                rc = proc.returncode
                assert rc is not None
                if rc > 127:
                    rc = -(rc - 128)
                if rc < 0:
                    stdout += f"\n\nKILLED WITH SIGNAL {-rc}"
            return RunResult(proc.returncode == 0, stdout, stderr, points)

    async def __aexit__(self, type, value, traceback):
        self.tmpdirHandle.__exit__(type, value, traceback)

# vim: colorcolumn=80 expandtab sw=4 ts=4
