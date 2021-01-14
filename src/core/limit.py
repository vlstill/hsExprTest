# (c) 2020–2021 Vladimír Štill <code@vstill.eu>

from typing import Optional


class Limit:
    def __init__(self,
                 memory: Optional[int] = None,
                 swap: Optional[int] = None,
                 cpu: Optional[float] = None) -> None:
        self.memory = memory
        self.swap = swap
        if self.swap is None and self.memory is not None:
            self.swap = 0
        self.cpu = cpu

    def any_set(self) -> bool:
        return any([self.memory, self.swap, self.cpu])

# vim: colorcolumn=80 expandtab sw=4 ts=4
