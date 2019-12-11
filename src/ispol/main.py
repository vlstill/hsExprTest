#!/usr/bin/env python3
import enum
import json
import os.path
import re
import signal
import smtplib
import sys
import textwrap
import time
import traceback
from email.mime.text import MIMEText
from email.utils import COMMASPACE, formatdate  # type: ignore
from typing import Callable, List, Set, TypeVar, Union, cast

import isapi.files
import isapi.notebooks

import requests

from typing_extensions import Final

import yaml


ta = TypeVar("ta")


def usage(script_name : str) -> None:
    fprint(textwrap.dedent(f"""\
        Usage:
            {script_name} CONFIG.yml [IS_PATHS]

        CONFIG.yml: a configuration file for the poller, it sets courses and
            homeworsk which should be checked.


        IS_PATHS: a list of paths in the IS (starting with /el/…) to be
            processed.

        If IS_PATHS are given, the script runs in single forced evaluation
        mode, i.e., it evaluates these paths only, disregarding the number of
        attempts allowed. The paths are evaluated according to the config.
        Otherwise the poller will monitor paths given in the CONFIG.
        """))


@enum.unique
class MailType (enum.Enum):
    Student = 1
    Teacher = 2


@enum.unique
class MailMode (enum.Flag):
    Never = 0
    OnSuccess = 0x1
    OnFailure = 0x2
    OnError = 0x4
    Always = OnSuccess | OnFailure | OnError

    @staticmethod
    def parse(value : Union[str, List[str]]):
        """
        Parse a MailMode out of a string or a list of strigns which represent
        it. The values are matched in case-insensitive mode and spaces or
        underscores can be used in the string representation to make it more
        readable: e.g. "On_Success", "on_success" or "on success" are all valid
        representations of the MailMode.OnSuccess value.
        """
        if isinstance(value, str):
            value = [value]
        out = MailMode.Never
        mapping = {k.lower(): v
                   for k, v in MailMode.__members__.items()}
        for v in value:
            ev = mapping.get(v.lower().replace(' ', '').replace('_', ''))
            if ev is None:
                raise ValueError(f"Invalid value for MailMode: {v}")
            out |= ev
        return out


# a set of paths to be processed, if nonempty, only these paths will be
# processed and the script will exit afterward
OVERRIDES : Final[Set[str]] = set(sys.argv[2:])

RE_STARNUM : Final = re.compile(r'[*]([.]?[0-9])')
RE_WHITENL : Final = re.compile(r' *\n')


def escape_points(txt : str) -> str:
    return RE_STARNUM.sub("*\N{ZERO WIDTH SPACE}\\1", txt)


def fprint(what, **kvargs):
    print(what, flush=True, **kvargs)


class LiteralUnicode(str):
    pass


def literal_unicode_representer(dumper, data):
    return dumper.represent_scalar(u'tag:yaml.org,2002:str', data, style='|')


yaml.add_representer(LiteralUnicode, literal_unicode_representer)


def string_yaml(data : dict) -> str:
    return cast(str, yaml.dump(data, default_flow_style=False, width=999,
                               indent=2, allow_unicode=True))


def send_mail(course : str, mail_type : MailType, conf : dict, result : dict,
              author : int, notebooks : isapi.notebooks.Connection,
              failure : bool = False) -> None:
    mode = MailMode.parse(conf.get(f"mail_{mail_type.name.lower()}", "never"))
    if mail_type is MailType.Teacher:
        mode |= MailMode.OnError

    send = False
    success = all(e.get("points", 0) >= e.get("out_of", 0)
                  for e in result["attempts"][0].get("points", {}))
    send |= failure and MailMode.OnError in mode
    send |= success and MailMode.OnSuccess in mode
    send |= not success and MailMode.OnFailure in mode
    if not send:
        return

    text = textwrap.dedent(f'Výsledky opravení {conf["notebook"]["name"]} pro '
                           f'{author}:\n\n' + string_yaml(result))
    if failure:
        if mail_type is MailType.Teacher:
            text = "Došlo k chybě při zápisu do poznámkového bloku, prosím " \
                   "zadejte následující výsledek (bez průvodního textu) " \
                   "studentvi do bloku ručně.\n\n" + text
        else:
            text = "Došlo k chybě při zápisu do poznámkového bloku, " \
                   "vyučující byl notifikován a měl by zřídit nápravu" \
                   "\n\n" + text
    msg = MIMEText(text)
    msg["Subject"] = f'[{course.upper()}][{mail_type.name.lower()}] ' \
                     f'{conf["notebook"]["name"]}'
    if "from" in conf:
        msg["From"] = conf['from']
    msg['Date'] = formatdate(localtime=True)

    send_to = []
    if mail_type is MailType.Student:
        send_to.append(f"{author}@mail.muni.cz")
    if mail_type is MailType.Teacher:
        send_to = conf.get('teachers', [])
        if conf.get('teachers_from_group', False):
            send_to = [f"{p.uco}@mail.muni.cz"
                       for p in notebooks.seminars().get_teachers(author)]
    msg['To'] = COMMASPACE.join(send_to)

    with smtplib.SMTP("localhost") as smtp:
        bcc = conf.get('bcc', [])
        if isinstance(bcc, str):
            bcc = [bcc]
        smtp.sendmail(conf['from'], send_to + bcc, msg.as_string())


def fileapi_try_attempts(fn : Callable[[], ta], attempts) -> ta:
    for _ in range(attempts - 1):
        try:
            return fn()
        except isapi.files.FileAPIException as ex:
            fprint(f"FILE API retry ({ex})")
    return fn()


def process_file(course : str, notebooks : isapi.notebooks.Connection,
                 files : isapi.files.Connection,
                 filemeta : isapi.files.FileMeta, conf : dict,
                 upstream : str, forced : bool = False, reeval : bool = False) -> None:
    fprint(f"Processing {filemeta.ispath}…")
    qid = conf["id"]
    attempts = conf.get("attempts")
    note = conf["notebook"]["short"]
    note_name = conf["notebook"]["name"]
    notebook = notebooks.get_or_create(
                      shortcut=note, name=note_name,
                      visible=conf["notebook"].get("visible", False),
                      statistics=conf["notebook"].get("statistics", False))
    is_entry = notebook.get(filemeta.author, isapi.notebooks.Entry(""))
    entry = yaml.safe_load(is_entry.text) or {}
    timestamp = time.strftime("%Y-%m-%d %H:%M")
    dirty = False

    base_entry = {"time": timestamp, "filename": filemeta.shortname}
    data = fileapi_try_attempts(lambda: files.get_file(filemeta).data, 5)
    total_points = entry.get("total_points")
    if total_points is not None and total_points[0:1] == '*':
        total_points = float(total_points[1:])
    if "attempts" not in entry:
        entry["attempts"] = [base_entry]
    elif reeval:
        if "error" in entry["attempts"][0]:
            fprint(f"W: could not reeval after error: {filemeta.author}")
            return
        if entry["attempts"][0].get("filename", "") != os.path.basename(filemeta.ispath):
            fprint(f"W: skipping file {filemeta.ispath} as it is not last")
            return
        entry["attempts"][0].update(base_entry)
    else:
        entry["attempts"].insert(0, base_entry)

    if not forced and attempts is not None \
            and len(entry.get("attempts", [])) > attempts:
        entry["attempts"][0]["error"] = "Too many attempts"
    else:
        req = requests.post(upstream, {"kod": course, "id": qid, "odp": data,
                                       "uco": filemeta.author,
                                       "mode": "json"})
        assert req.status_code == 200
        response = json.loads(req.text)
        if "comment" in response:
            c = RE_WHITENL.sub('\n', response["comment"].rstrip()
                                                        .replace('\t', "  "))
            response["comment"] = c
        if reeval:
            for k, v in response.items():
                if k not in entry["attempts"][0] or entry["attempts"][0].get(k) != v:
                    dirty = True
                    fprint(f"reeval {filemeta.author}/{k}:\n" +\
                           textwrap.indent(f"{entry['attempts'][0].get(k)}\n==>\n{v}\n\n", "    "))
        entry["attempts"][0].update(response)

        all_points = [sum(p["points"] for p in a["points"]) for a in entry["attempts"]]
        if conf.get("aggregate", "last") == "max":
            total_points = max(all_points)
        else:
            total_points = all_points[0]
        entry["total_points"] = f"*{total_points}"

    for i in range(len(entry["attempts"])):
        if "comment" in entry["attempts"][i]:
            entry["attempts"][i]["comment"] = \
                              LiteralUnicode(entry["attempts"][i]["comment"])

    is_entry.text = string_yaml(entry)
    failure = False
    if not reeval or dirty:
        try:
            notebooks.store(note, filemeta.author, is_entry)
        except isapi.notebooks.NotebookException as ex:
            fprint(f"Error with isapi.notebooks (sending mail): {ex}")
            failure = True
        for mail_type in [MailType.Student, MailType.Teacher]:
            try:
                send_mail(course=course, conf=conf, result=entry,
                          author=filemeta.author, notebooks=notebooks,
                          failure=failure, mail_type=mail_type)
            except Exception:
                fprint(f"ERROR: Mail sending failed for {filemeta.author}:\n"
                       + is_entry.text)
                traceback.print_exc()
        fprint(f"done, {filemeta.author}: {total_points} points")
    else:
        fprint(f"done, {filemeta.author} not updated")


def poll():
    files = isapi.files.Connection()
    with open(sys.argv[1]) as conf_file:
        config = yaml.safe_load(conf_file)
    for course, dirs in config["courses"].items():
        notebooks = isapi.notebooks.Connection(course=course)
        for d in dirs:
            paths = d.get("paths", [])
            if "path" in d:
                paths.append(d["path"])
            if not d.get("enabled", True):
                continue
            for path in paths:
                try:
                    entries = files.list_directory(path).entries
                except isapi.files.FileAPIException as ex:
                    fprint(f"ERROR while lising {path}: {ex}")
                    continue

                for f in entries:
                    forced = f.ispath in OVERRIDES
                    reeval = "reeval" in OVERRIDES and f.read
                    if not reeval and not forced and len(OVERRIDES):
                        continue
                    if not forced and not reeval and f.read:
                        continue
                    process_file(course, notebooks, files, f, d,
                                 config["upstream"], forced=forced,
                                 reeval=reeval)


def main():
    if len(sys.argv) < 2:
        usage(sys.argv[0])
        sys.exit(1)
    with open(sys.argv[1]) as conf_file:
        config = yaml.safe_load(conf_file)
    fprint(config)
    interval = float(config["interval"])

    stop_signal = False

    def poller():
        while True:
            start = time.perf_counter()
            poll()
            sleep_for = int((max(0, interval - (time.perf_counter() - start))))
            for _ in range(sleep_for):
                if stop_signal:
                    return
                time.sleep(1)
            if stop_signal:
                return

    def stop(sig, stack):
        nonlocal stop_signal
        stop_signal = True
        fprint(f"cancellation pending (SIG={sig})… ")

    signal.signal(signal.SIGTERM, stop)

    if len(sys.argv) > 2:
        stop_signal = True
        if "reeval" in OVERRIDES:
            fprint("Reevaluating")
        else:
            fprint("Will only handle forced files")

    poller()
    fprint("exiting…")


if __name__ == "__main__":
    main()

# vim: colorcolumn=80 expandtab sw=4 ts=4
