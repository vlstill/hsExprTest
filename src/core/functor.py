# (c) 2019–2021 Vladimír Štill <code@vstill.eu>

from typing import Optional, Callable, TypeVar


ta = TypeVar("ta")
tb = TypeVar("tb")


def mapO(fun: Callable[[ta], tb], val: Optional[ta]) -> Optional[tb]:
    """
    Fmap on Optional[T] i.e., if a value is not None, apply a function to it
    and return result, otherwise return None.
    """
    if val is None:
        return None
    return fun(val)


def readInt(val: str) -> Optional[int]:
    """
    Get an int from a string, or None it the string does not reprsent a number.
    """
    try:
        return int(val)
    except ValueError:
        return None


# vim: colorcolumn=80 expandtab sw=4 ts=4
