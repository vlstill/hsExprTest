# (c) 2019–2021 Vladimír Štill <code@vstill.eu>

from __future__ import annotations

import argparse
import asyncio
import getpass
import sys
import subprocess
import yaml
import os.path
import logging

from typing import List, Optional, Dict, Any, Union
from systemd.journal import JournalHandler  # type: ignore

from limit import Limit


class ConfigException(Exception):
    pass


_GIT_STAMP_ARGS = ["git", "log", "--pretty=format:%H", "-n1"]


def _sync_git_stamp(path: str) -> str:
    git = subprocess.run(_GIT_STAMP_ARGS,
                         cwd=path, stdout=subprocess.PIPE)
    return git.stdout.strip().decode('utf-8')


async def _async_git_stmap(path: str) -> str:
    git = await asyncio.create_subprocess_exec(
                *_GIT_STAMP_ARGS, cwd=path,
                stdout=asyncio.subprocess.PIPE)
    out = (await git.communicate())[0]
    return out.strip().decode('utf-8')


_EXPRTEST_GIT_STAMP = _sync_git_stamp(os.path.dirname(
                                      os.path.abspath(__file__)))


class Course:
    DictT = Dict[str, Union[str, bool, List[str], Dict[str, Any]]]

    def __init__(self, raw: Dict[str, Any], qdir_root: str) -> None:
        if not isinstance(raw, dict):
            raise ConfigException("Course must be an object")
        try:
            self.name = str(raw["name"])
            self.checker = str(raw["checker"])
            self._qdir = raw.get("qdir", self.name)
            self.qdir = os.path.abspath(os.path.join(qdir_root, self._qdir))
            self.isolation = bool(raw.get("isolation", False))
            self.hint = bool(raw.get("hint", False))
            self.authorized: List[str] = raw.get("authorized", [])
            self.path_append: List[str] = raw.get("path_append", [])
            self.extended = bool(raw.get("extended", False))
            self.escape_is = bool(raw.get("escape_is", False))
            self.evalconf = raw.get("config", {})
            self.stamp = _sync_git_stamp(self.qdir)
        except KeyError as ex:
            raise ConfigException(
                f"Course must set at least 'name' and 'checker': missing {ex}")

    def to_dict(self, expand: bool = False) -> Course.DictT:
        res: Course.DictT = \
            {"name": self.name,
             "checker": self.checker,
             "isolation": self.isolation,
             "hint": self.hint,
             "authorized": self.authorized,
             "path_append": self.path_append,
             "extended": self.extended,
             "escape_is": self.escape_is,
             "config": self.evalconf}
        if expand:
            res["qdir"] = self.qdir
        else:
            res["qdir"] = self._qdir
        return res

    def dump(self, stream: Any = None, expand: bool = False) -> Any:
        return yaml.safe_dump(self.to_dict(expand=expand), stream,
                              default_flow_style=False)

    async def async_update_stamp(self) -> None:
        self.stamp = await _async_git_stmap(self.qdir)

    def full_stamp(self) -> str:
        return f"{_EXPRTEST_GIT_STAMP}+{self.stamp}"


class Config:
    def __init__(self, argv: List[str]) -> None:
        self.argv = argv
        self.config_file = "exprtest.yaml"
        self.socket_fd: Optional[int] = None
        self.socket: Optional[str] = None
        self.port: Optional[int] = None
        self.qdir_root: Optional[str] = None
        self.courses: Dict[str, Course] = {}
        self.max_workers = 4
        self.hint_origin: Optional[str] = None
        self.limit = Limit()
        self.verbose: bool = False
        self.journal: bool = False
        self.postgres: bool = True
        self.postgres_host: str = "/var/run/postgresql"
        self.postgres_user: str = getpass.getuser()
        self.exprtest_stamp = _EXPRTEST_GIT_STAMP

        self._load_from_argv()
        self._load_from_file()
        self._setup_logging()

    def _load_from_argv(self) -> None:
        parser = argparse.ArgumentParser(
                  description="ExprTest evaluation service")
        parser.add_argument(
                  '--socket-fd', metavar='FD', dest='socket_fd', type=int,
                  help="socket file descriptor to be used for UNIX socket "
                       "server")
        parser.add_argument(
                  '--socket', metavar='FILE', dest='socket', type=str,
                  help="named socket to be used for UNIX socket server")
        parser.add_argument(
                  '--port', metavar='TPC_PORT', dest='port', type=int,
                  help="TCP port to be used for HTTP server on localhost")
        parser.add_argument(
                  '--config', metavar='FILE',
                  help="YAML config file with description of evaluation "
                       "environment")
        parser.add_argument(
                  '--verbose', action='store_const', const=True, default=False,
                  help="Verbose logging")
        parser.add_argument(
                  '--journal', action='store_const', const=True, default=False,
                  help="log to systemd journal")

        args = parser.parse_args(self.argv[1:])
        self.socket_fd = args.socket_fd
        self.socket = args.socket
        self.port = args.port
        self.verbose = args.verbose
        self.journal = args.journal
        if args.config is not None:
            self.config_file = args.config

    @staticmethod
    def _parse_proc(val: Union[None, str, int, float]) -> Optional[float]:
        if val is None:
            return None
        if isinstance(val, float):
            return val
        if isinstance(val, int):
            return float(val)
        if val[-1:] == '%':
            return int(val[:-1]) / 100
        return float(val)

    MEM_MULTIPLIERS = {"k": 1024,
                       "M": 1024 * 1024,
                       "G": 1024 * 1024 * 1024,
                       "T": 1024 * 1024 * 1024 * 1024}

    @staticmethod
    def _parse_mem(val: Union[None, str, int]) -> Optional[int]:
        if val is None:
            return None
        if isinstance(val, int):
            return val
        mult = Config.MEM_MULTIPLIERS.get(val[-1:])
        if mult is None:
            return int(val)
        return int(val[:-1]) * mult

    def _load_from_file(self) -> None:
        try:
            with open(self.config_file, 'r') as fh:
                conf = yaml.safe_load(fh)
        except FileNotFoundError as ex:
            raise ConfigException(
                    f"Config file {self.config_file} not found: {ex}")
        except yaml.YAMLError as ex:
            raise ConfigException(
                    f"Failed to load config from {self.config_file}: {ex}")

        if not isinstance(conf, dict):
            raise ConfigException("Config must be a YAML object")

        self.qdir_root = conf.get("qdir_root")
        self.max_workers = conf.get("max_workers", self.max_workers)
        self.hint_origin = conf.get("hint_origin")

        limit_raw = conf.get("limit", {})
        self.limit = Limit(memory=self._parse_mem(limit_raw.get("memory")),
                           swap=self._parse_mem(limit_raw.get("swap")),
                           cpu=self._parse_proc(limit_raw.get("cpu")))

        if self.qdir_root is None:
            raise ConfigException("Field 'qdir_root' must be set")
        courses0 = conf.get("courses", [])
        if not (isinstance(courses0, list) or isinstance(courses0, dict)):
            raise ConfigException(
                    "Courses must be an array or map of course objects")
        if isinstance(courses0, dict):
            courses = []
            for n, c in courses0.items():
                if "name" not in c:
                    c["name"] = n
                courses.append(c)
        else:
            courses = courses0
        for c in courses:
            cc = Course(c, self.qdir_root)
            self.courses[cc.name.lower()] = cc

        out = len([x for x in [self.socket, self.socket_fd, self.port]
                   if x is not None])
        if out == 0:
            self.port = 8080
        if out > 1:
            raise ConfigException("At most one of '--socket', '--socket-fd' "
                                  "or '--port' must be used")
        if len(self.courses) == 0:
            raise ConfigException("At least one course must be set")

        self.postgres = conf.get("postgres", False)
        self.postgres_host = conf.get("postgres_host", self.postgres_host)
        self.postgres_user = conf.get("postgres_user", self.postgres_user)

    def _setup_logging(self) -> None:
        root = logging.getLogger()
        if self.verbose:
            root.setLevel(logging.DEBUG)
        else:
            root.setLevel(logging.INFO)

        if self.journal:
            handler = JournalHandler()
            formatter = logging.Formatter('%(message)s')
        else:
            handler = logging.StreamHandler(sys.stderr)
            formatter = logging.Formatter('%(levelname)s: %(message)s')

        handler.setFormatter(formatter)
        root.addHandler(handler)
        self.logger = logging.getLogger("core")

    def dump(self, stream: Any = None) -> Any:
        return yaml.safe_dump(self.to_dict(), stream, default_flow_style=False)

    def to_dict(self) -> Dict[str, Any]:
        return {"socket_fd": self.socket_fd,
                "socket": self.socket,
                "port": self.port,
                "qdir_root": self.qdir_root,
                "max_workers": self.max_workers,
                "hint_origin": self.hint_origin,
                "limit": {k: v for k, v in [("memory", self.limit.memory),
                                            ("swap", self.limit.swap),
                                            ("cpu", self.limit.cpu)]
                          if v is not None},
                "courses": list(map(Course.to_dict, self.courses.values())),
                "postgres_user": self.postgres_user,
                "postgres_host": self.postgres_host,
                "postgres": self.postgres,
                "exprtest_stamp": self.exprtest_stamp}


def parse(argv: List[str]) -> Config:
    return Config(argv)

# vim: colorcolumn=80 expandtab sw=4 ts=4
