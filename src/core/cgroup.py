from typing import Optional, List, Set
import os
import os.path
import contextlib
import sys
import signal

from limit import Limit


class CGException(Exception):
    pass


class CGControl:
    CPU_MAX_PERIOD = 10000  # 10ms

    def __init__(self):
        cgpath : Optional[str] = None
        basepath : Optional[str] = None
        try:
            with open("/proc/self/cgroup") as cginfo:
                for l in cginfo:
                    n0, _, path = l.split(":")
                    n = int(n0)
                    if n != 0:
                        raise CGException("cgroups v2 not fully available")
                    cgpath = path.strip()[1:]
        except OSError:
            raise CGException("Could not read cgroup info from /proc")
        if cgpath is None:
            raise CGException("Could not detect process' cgroup, maybe no "
                              "cgroup support enabled?")

        try:
            with open("/etc/mtab") as mtab:
                for l in mtab:
                    typ, path, _ = l.split(" ", 2)
                    if typ == "cgroup2" and basepath is None:
                        basepath = path
        except OSError:
            raise CGException("Could not read /etc/mtab to detect cgroup root")
        if basepath is None:
            raise CGException("Could not found cgroup hierarchy, maybe it is "
                              "not mounted")
        self.cg = os.path.join(basepath, cgpath)

    def delegate(self, moveTo : str) -> None:
        """
        Moves the current processes to {moveTo} and enables subtree_controll
        """
        self.subdivide(moveTo)
        self.register(self.procs(), moveTo)
        self.enable_available_subtrees()

    def procs(self, subpath : str = ".") -> Set[int]:
        with open(os.path.join(self.cg, subpath, "cgroup.procs")) as prcs:
            return {int(p) for p in prcs}

    def controllers(self, subpath : str = ".") -> Set[str]:
        with open(os.path.join(self.cg, subpath, "cgroup.controllers")) \
                  as ctrlsF:
            return set(ctrlsF.read().strip().split(' '))

    def register(self, pids : Set[int], subpath : str = ".") \
            -> None:
        with open(os.path.join(self.cg, subpath, "cgroup.procs"), "w") \
                  as tgtPrcs:
            for pid in pids:
                tgtPrcs.write(str(pid))

    def register_me(self, subpath : str = ".") -> None:
        self.register({os.getpid()}, subpath)

    def _write(self, path : str, key : str, value : str) -> None:
        with open(os.path.join(self.cg, path, key), "w") as dst:
            dst.write(value)

    def enable_subtrees(self, controllers : Set[str],
                        subpath : str = ".") -> None:
        self._write(subpath, "cgroup.subtree_control",
                    f"+{' +'.join(controllers)}")

    def enable_available_subtrees(self, subpath : str = ".") -> None:
        self.enable_subtrees(self.controllers(subpath), subpath)

    def subdivide(self, subpath : str) -> str:
        try:
            os.makedirs(os.path.join(self.cg, subpath), exist_ok=True)
        except Exception as ex:
            raise CGException(f"Error creating sub-hierarchy: {ex}")
        return subpath

    def set_limit(self, limit : Limit, subpath : str = ".") -> None:
        def wr(key, val, name):
            try:
                self._write(subpath, key, val)
            except OSError as ex:
                print(f"W: cgroup error: could not set {name} limit, "
                      f"ignoring it; error {ex}", file=sys.stderr, flush=True)

        for key, val, name in [("memory.max", limit.memory, "memory"),
                               ("memory.swap.max", limit.swap, "swap")]:
            wr(key, str(val) if val is not None else "max", name)
        wr("cpu.max",
           f"{round(limit.cpu * self.CPU_MAX_PERIOD)} {self.CPU_MAX_PERIOD}"
               if limit.cpu is not None else "max",
           "CPU")


class SlotManager:
    def __init__(self, limit : Limit):
        self.cg : Optional[CGControl] = None
        self._available = False
        try:
            self.cg = CGControl()
            self.cg.delegate("master")
            self.slaves = "slaves"
            self.cg.subdivide(self.slaves)
            self.cg.enable_available_subtrees(self.slaves)
            self._free_slots : List[str] = []
            self._slot_cnt = 0
            self.limit = limit
            self._available = True
        except CGException as ex:
            print(f"W: cgroup error: {ex}", file=sys.stderr, flush=True)

    def available(self) -> bool:
        return self._available

    def _mkslot(self) -> str:
        assert self.cg is not None

        if self._free_slots:
            return self._free_slots.pop()
        slot_id = self._slot_cnt
        self._slot_cnt += 1
        slot = self.cg.subdivide(f"{self.slaves}/slot{slot_id}")
        self.cg.set_limit(self.limit, slot)
        return slot

    def _terminate(self, slot : str) -> None:
        assert self.cg is not None
        for p in self.cg.procs(slot):
            print(f"W: killing stale {p}", file=sys.stderr, flush=True)
            os.kill(p, signal.SIGKILL)
        self._free_slots.append(slot)

    @contextlib.contextmanager
    def get(self):
        if not self._available:
            yield None
        else:
            slot = self._mkslot()
            try:
                yield slot
            finally:
                self._terminate(slot)


# vim: colorcolumn=80 expandtab sw=4 ts=4
