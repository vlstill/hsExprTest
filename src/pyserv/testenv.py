import tempfile
import config
import os.path
import aiofiles # type: ignorea
import posix1e
import pwd
import os

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
            e.permset.read, e.permset.write, e.permset.execute = True, True, True
            acl.calc_mask()
            acl.applyto(self.tmpdir)

            # add another default entry for checker to ensure we can delete everythibng
            ec = acl.append()
            ec.tag_type = posix1e.ACL_USER
            ec.qualifier = os.geteuid()
            ec.permset.clear()
            ec.permset.read, ec.permset.write, ec.permset.execute = True, True, True
            acl.calc_mask()
            acl.applyto(self.tmpdir, posix1e.ACL_TYPE_DEFAULT)

            
        ext = os.path.splitext(self.question)[1]
        async with aiofiles.open(self.question) as src:
            async with aiofiles.open(
                    os.path.join(self.tmpdir, f"question{ext}"), "w") as tgt:
                contents = await src.read()
                await tgt.write(contents)
        async with aiofiles.open(
                os.path.join(self.tmpdir, f"answer{ext}"), "w") as ans:
            await ans.write(self.answer)

        return self            
        
    async def __aexit__(self, type, value, traceback):
        self.tmpdirHandle.__exit__(type, value, traceback)

# vim: colorcolumn=80 expandtab sw=4 ts=4
