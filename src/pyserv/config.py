import argparse
import yaml
import sys
from typing import List, Optional, Dict, Any, Union


class ConfigException(Exception):
    pass


class Course:
    def __init__(self, raw : Dict[str, Any]):
        if not isinstance(raw, dict):
            raise ConfigException("Course must be an object")
        try:
            self.name = str(raw["name"])
            self.checker = str(raw["checker"])
            self.qdir = str(raw["qdir"])
            self.isolation = bool(raw.get("isolation", False))
            self.hint = bool(raw.get("hint", False))
            self.extended = bool(raw.get("extended", False))
        except KeyError as ex:
            raise ConfigException(f"Course must set at least 'name', 'checker', and 'qdir': missing {ex}")

    def to_dict(self) -> Dict[str, Union[str, bool]]:
        return {"name": self.name,
                "checker": self.checker,
                "qdir": self.qdir,
                "isolation": self.isolation,
                "hint": self.hint,
                "extended": self.extended}


class Config:
    def __init__(self, argv : List[str]):
        self.argv = argv
        self.config_file = "exprtest.yaml"
        self.socket_fd : Optional[int] = None
        self.socket : Optional[str] = None
        self.qdir_root : Optional[str] = None
        self.courses : List[Course] = []
        self.max_workers = 4
        self._load_from_argv()
        self._load_from_file()

    def _load_from_argv(self) -> None:
        parser = argparse.ArgumentParser(
                  description="ExprTest evaluation service")
        parser.add_argument(
                  '--socket-fd', metavar='FD', dest='socket_fd', type=int,
                  help="socket file descriptor to be used for UNIX socket server")
        parser.add_argument(
                  '--config', metavar='FILE',
                  help="YAML config file with description of evaluation environment")
        args = parser.parse_args(self.argv[1:])
        self.socket_fd = args.socket_fd
        if args.config is not None:
            self.config_file = args.config

    def _load_from_file(self) -> None:
        try:
            with open(self.config_file, 'r') as fh:
                conf = yaml.safe_load(fh)
        except FileNotFoundError as ex:
            raise ConfigException(f"config file {self.config_file} not found: {ex}")
        except yaml.YAMLError as ex:
            raise ConfigException(f"Failed to load config from {self.config_file}: {ex}")
            return

        if not isinstance(conf, dict):
            raise ConfigException("Config must be a YAML object")

        self.socket = conf.get("socket")
        self.qdir_root = conf.get("qdir_root", ".")
        self.max_workers = conf.get("max_workers", self.max_workers)

        courses = conf.get("courses", [])
        if not isinstance(courses, list):
            raise ConfigException("courses must be an array of course objects")
        for c in courses:
            self.courses.append(Course(c))

        if (self.socket is None and self.socket_fd is None):
            raise ConfigException("One of 'socket' or '--socket-fd' must be used")
        if self.qdir_root is None:
            raise ConfigException("'qdir_root' must be set")
        if len(self.courses) == 0:
            raise ConfigException("At least one course must be set")

    def dump(self, stream : Any = None) -> Any:
        return yaml.safe_dump(self.to_dict(), stream, default_flow_style=False)

    def to_dict(self) -> Dict[str, Any]:
        return {"socket_fd": self.socket_fd,
                "socket": self.socket,
                "qdir_root": self.qdir_root,
                "max_workers": self.max_workers,
                "courses": list(map(Course.to_dict, self.courses))}


def parse(argv : List[str]) -> Config:
    return Config(argv)

# vim: colorcolumn=80 expandtab sw=4 ts=4
