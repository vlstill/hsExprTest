import isapi.files
import isapi.notebooks
import signal
import sys
import yaml
import time
import requests


def fprint(what, **kvargs):
    print(what, flush=True, **kvargs)


def process_file(course : str, notebooks : isapi.notebooks.Connection,
                 files : isapi.files.Connection,
                 filemeta : isapi.files.FileMeta, conf : dict,
                 upstream : str) -> None:
    fprint(f"Processing {filemeta.ispath}…")
    qid = conf["id"]
    attempts = conf["attempts"]
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
    points = None
    if "attempts" not in entry:
        entry["attempts"] = [base_entry]
    else:
        entry["attempts"].insert(0, base_entry)
    if attempts is not None and len(entry.get("attempts", [])) > attempts:
        entry["attempts"][0]["error"] = "Too many attempts"
    else:
        req = requests.post(upstream, {"kod": course, "id": qid, "odp": data,
                                       "uco": filemeta.author,
                                       "zobrazeni": "u", "sada": "ext"})
        assert req.status_code == 200
        raw_points, response = req.text.split("~~", 1)
        response = response.rstrip()
        if raw_points.endswith("ok"):
            points = 1 if raw_points == "ok" else 0
        else:
            points = int(raw_points)
        entry["total_points"] = f"*{points}"
        entry["attempts"][0].update({"points": points, "comment": response})

    is_entry.text = yaml.dump(entry)
    notebooks.store(note, filemeta.author, is_entry)
    fprint(f"done, {points} points")


def poll():
    files = isapi.files.Connection()
    with open(sys.argv[1]) as conf_file:
        config = yaml.safe_load(conf_file)
    for course, dirs in config["courses"].items():
        notebooks = isapi.notebooks.Connection(course=course)
        for d in dirs:
            path = d["path"]
            try:
                for f in files.list_directory(path).entries:
                    if f.read:
                        fprint(f"Skipping read {f.ispath}")
                        continue
                    process_file(course, notebooks, files, f, d,
                                 config["upstream"])
            except isapi.files.FileAPIException as ex:
                fprint(f"ERROR while lising {path}: {ex}")


def main():
    if len(sys.argv) != 2:
        fprint(f"Usage {sys.argv[0]} config.yml")
        sys.exit(1)
    with open(sys.argv[1]) as conf_file:
        config = yaml.safe_load(conf_file)
    fprint(config)
    interval = float(config["interval"])

    stop_signal = False

    def poller():
        while not stop_signal:
            start = time.perf_counter()
            poll()
            sleep_for = int((max(1, interval - (time.perf_counter() - start))))
            for _ in range(sleep_for):
                if stop_signal:
                    break
                time.sleep(1)

    def stop(sig, stack):
        nonlocal stop_signal
        stop_signal = True
        fprint(f"cancellation pending (SIG={sig})… ")

    signal.signal(signal.SIGTERM, stop)

    poller()
    fprint("Cancelled, exiting…")


if __name__ == "__main__":
    main()

# vim: colorcolumn=80 expandtab sw=4 ts=4
