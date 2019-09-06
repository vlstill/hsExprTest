from typing import Optional, Callable, TypeVar


ta = TypeVar("ta")
tb = TypeVar("tb")


def fmapO(fun : Callable[[ta], tb], val : Optional[ta]) -> Optional[tb]:
    if val is None:
        return None
    return fun(val)

def rint(val : str) -> Optional[int]:
    try:
        return int(val)
    except:
        return None

# vim: colorcolumn=80 expandtab sw=4 ts=4
