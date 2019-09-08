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

    async def run(self, *options, hint : bool):
        args = []
        if self.course.isolation:
            args.extend(["sudo", "-n", "-u", f"rc-{self.course.name}"])
        args.extend(self.course.checker.split(' '))
        args.extend([self.qfile, self.afile, f"-I{self.course.qdir}"])
        args.extend([f"-o{opt}" for opt in options if opt is not None])
        if hint:
            args.append("--hint")
        print("+ " + " ".join(args))
        proc = await asyncio.create_subprocess_exec(
                              *args,
                              stdin=subprocess.DEVNULL,
                              stdout=subprocess.PIPE,
                              stderr=sys.stderr,
                              cwd=self.tmpdir,
                              start_new_session=True,
                              pass_fds=[])
        stdout = (await proc.communicate())[0].decode("utf8")
        if proc.returncode == 0:
            return ("ok", stdout)
        else:
            return ("nok", stdout)

    async def __aexit__(self, type, value, traceback):
        self.tmpdirHandle.__exit__(type, value, traceback)

# vim: colorcolumn=80 expandtab sw=4 ts=4
