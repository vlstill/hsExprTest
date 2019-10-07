import isapi.files
import isapi.notebooks
import signal
import sys
import yaml
import json
import time
import requests
import re
import smtplib
from email.mime.text import MIMEText
from email.utils import COMMASPACE, formatdate  # type: ignore
import enum
import textwrap
from typing import Union, List, cast
import traceback


@enum.unique
class MailType (enum.Enum):
    Student = 1
    Teacher = 2


@enum.unique
class MailMode (enum.Flag):
    Never = 0
    OnSuccess = 0x1
    OnFailure = 0x2
    OnError   = 0x4
    Always = OnSuccess | OnFailure | OnError

    @staticmethod
    def parse(value : Union[str, List[str]]):
        if isinstance(value, str):
            value = [value]
        out = MailMode.Never
        mapping = dict([(k.lower(), v)
                        for k, v in MailMode.__members__.items()])
        for v in value:
            ev = mapping.get(v.lower().replace(' ', '').replace('_', ''))
            if ev is None:
                raise ValueError(f"Invalid value for MailMode: {v}")
            out |= ev
        return out


overrides = set(sys.argv[2:])
RE_STARNUM = re.compile(r'[*]([.]?[0-9])')


def escape_points(txt : str) -> str:
    return RE_STARNUM.sub(r'* \1', txt)


def fprint(what, **kvargs):
    print(what, flush=True, **kvargs)


class LiteralUnicode(str): pass

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
    success = all([e.get("points", 0) >= e.get("out_of", 0)
                  for e in result["attempts"][0]["points"]])
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
                   "vyučující byl notifikovanán a měl by zřídit nápravu" \
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


def process_file(course : str, notebooks : isapi.notebooks.Connection,
                 files : isapi.files.Connection,
                 filemeta : isapi.files.FileMeta, conf : dict,
                 upstream : str, forced : bool = False) -> None:
    fprint(f"Processing {filemeta.ispath}…")
    qid = conf["id"]
    attempts = conf.get("attempts")
    note = conf["notebook"]["short"]
    note_name = conf["notebook"]["name"]
    notebook = notebooks.get_or_create(shortcut=note, name=note_name,
                          visible=conf["notebook"].get("visible", False),
                          statistics=conf["notebook"].get("statistics", False))
    is_entry = notebook.get(filemeta.author, isapi.notebooks.Entry(""))
    entry = yaml.safe_load(is_entry.text) or {}
    timestamp = time.strftime("%Y-%m-%d %H:%M")

    base_entry = {"time": timestamp, "filename": filemeta.shortname}
    data = files.get_file(filemeta).data.decode("utf8")
    total_points = entry.get("total_points")
    if "attempts" not in entry:
        entry["attempts"] = [base_entry]
    else:
        entry["attempts"].insert(0, base_entry)
    if not forced and attempts is not None and len(entry.get("attempts", [])) > attempts:
        entry["attempts"][0]["error"] = "Too many attempts"
    else:
        req = requests.post(upstream, {"kod": course, "id": qid, "odp": data,
                                       "uco": filemeta.author,
                                       "mode": "json"})
        assert req.status_code == 200
        response = json.loads(req.text)
        if "comment" in response:
            response["comment"] = LiteralUnicode(response["comment"].rstrip())
        new_total_points = sum(p["points"] for p in response["points"])
        if conf.get("aggregate", "last") == "avg":
            total_points = max(filter(lambda x: x is not None,
                                      [total_points, new_total_points]))
        else:
            total_points = new_total_points
        entry["total_points"] = f"*{total_points}"
        entry["attempts"][0].update(response)

    is_entry.text = string_yaml(entry)
    failure = False
    try:
        notebooks.store(note, filemeta.author, is_entry)
    except isapi.notebooks.NotebookException as ex:
        failure = True
    for mail_type in [MailType.Student, MailType.Teacher]:
        try:
            send_mail(course=course, conf=conf, result=entry,
                      author=filemeta.author, notebooks=notebooks, failure=failure,
                      mail_type=mail_type)
        except:
            fprint(f"ERROR: Mail sending failed for {filemeta.author}:\n"
                   + is_entry.text)
            traceback.print_exc()
    fprint(f"done, {total_points} points")


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
            for path in paths:
                try:
                    entries = files.list_directory(path).entries
                except isapi.files.FileAPIException as ex:
                    fprint(f"ERROR while lising {path}: {ex}")
                    continue

                for f in entries:
                    forced = f.ispath in overrides
                    if not forced and len(overrides):
                        continue
                    if not forced and f.read:
                        fprint(f"Skipping read {f.ispath}")
                        continue
                    process_file(course, notebooks, files, f, d,
                                 config["upstream"], forced)


def main():
    if len(sys.argv) < 2:
        fprint(f"Usage {sys.argv[0]} config.yml")
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
        fprint("Will only handle forced files")
    poller()
    fprint("exiting…")


if __name__ == "__main__":
    main()

# vim: colorcolumn=80 expandtab sw=4 ts=4
