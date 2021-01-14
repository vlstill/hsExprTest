# (c) 2020–2021 Vladimír Štill <code@vstill.eu>

from __future__ import annotations
import yaml
import copy
import re
from pathlib import Path
from os import PathLike
from typing import Dict, Any, TypeVar, Union

T = TypeVar("T")


class EvalConf:

    def __init__(self) -> None:
        self.config: Dict[str, Any] = {}

    @staticmethod
    def _merge_in(left: Dict[str, Any], right: Dict[str, Any]) -> None:
        def checktype(r: Any, t: type) -> None:
            if not isinstance(r, t):
                raise ValueError(f"config type mismatch in {k}, expected {t}, "
                                 f"got {type(r)}")

        for k, v in right.items():
            if k not in left:
                left[k] = v
            elif isinstance(left[k], dict):
                checktype(v, dict)
                EvalConf._merge_in(left[k], v)
            elif isinstance(left[k], list):
                checktype(v, list)
                left[k] += v
            else:
                left[k] = v

    def add(self, raw: Dict[str, Any]) -> EvalConf:
        EvalConf._merge_in(self.config, copy.deepcopy(raw))
        return self

    def load(self, path: str) -> EvalConf:
        with open(path, "r") as fh:
            # do not go through add - avoid copy
            EvalConf._merge_in(self.config, yaml.safe_load(fh))
        return self

    def get(self) -> Dict[str, Any]:
        return copy.deepcopy(self.config)

    def dump_to(self, stream: Any) -> None:
        yaml.safe_dump(self.config, stream, default_flow_style=False)

    def dump(self, path: str) -> None:
        with open(path, "w") as fh:
            self.dump_to(fh)

    def from_source_file(self, path: Union[str, PathLike[Any]]) -> EvalConf:
        path = Path(path)
        if path.suffix == ".hs":
            self._from_source_file(path, "-- @")
        elif path.suffix in [".c", ".cc", "cpp"]:
            self._from_source_file(path, "//@")
        elif path.suffix in [".py", ".pl", ".sh"]:
            self._from_source_file(path, "#@")
        return self

    def _from_source_file(self, path: Path, prefix: str) -> None:
        pat = re.compile(f"{prefix}\\s+([^:]*):\\s*(.*)")
        collected = ""
        with open(path, "r") as h:
            for line in h:
                m = pat.match(line)
                if m is not None:
                    collected += f"{m[1]}: {m[2]}\n"
        raw = yaml.safe_load(collected)
        if raw is not None:
            self.add(raw)

    def __setitem__(self, key: str, value: T) -> T:
        self.config[key] = value
        return value

# vim: colorcolumn=80 expandtab sw=4 ts=4
