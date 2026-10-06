#!/usr/bin/env python
"""
wind_pull.py -- Study A, Week 1 (Blueprint B.3 "Alternative: WindPy")

OPTIONAL. Pulls the series listed in config/wind_request.csv through WindPy, Wind's Python
interface, and saves each one as an .xlsx file in the layout requested in the sheet
(dates in column A, values in column B, header rows with name and Wind code), so the checks
and the register step (register_wind.py, Week 2) are the same as for a manual export.

WindPy only works on a Windows computer with the Wind terminal installed and logged in.
It is installed by the Wind terminal's Python plugin repair tool, not by pip.

Wind codes are NEVER guessed here: a row is pulled only if its wind_code, wind_field (for wsd)
and wind_function (wsd or edb) columns have been filled in from the Wind terminal
(Code Generator / 代码生成器). Rows without a code are skipped and listed.

The script never overwrites a file and never writes the source ledger (register_wind.py does).

Usage (from the project root, .venv active, Wind terminal running and logged in):
  python code/python/wind_pull.py --dry-run
  python code/python/wind_pull.py
  python code/python/wind_pull.py --ids W01

On a helper's computer without this repository (only python, pandas, openpyxl and WindPy):
  python wind_pull.py --request wind_request.csv --out-dir . --log-dir .
  -> send the .xlsx files and the .log file back.
"""
from __future__ import annotations

import argparse
import csv
import datetime as dt
import platform
import re
import sys
import traceback
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
DEFAULT_REQUEST = ROOT / "config" / "wind_request.csv"
DEFAULT_OUT = ROOT / "data" / "raw" / "proprietary"      # Blueprint B.3
DEFAULT_LOGS = ROOT / "outputs" / "logs"
EARLIEST = "1990-01-01"   # used when the sheet says 'earliest available'; Wind returns what exists
REQUIRED_COLUMNS = ["request_id", "series_description", "start_date", "end_date",
                    "file_name_wanted", "wind_code", "wind_field", "wind_function", "wind_options"]


class RunLog:
    def __init__(self, script_name: str, log_dir: Path):
        log_dir.mkdir(parents=True, exist_ok=True)
        self.started = dt.datetime.now()
        self.path = log_dir / f"{script_name}_{self.started:%Y%m%d-%H%M%S}.log"
        self._fh = open(self.path, "w", encoding="utf-8", newline="\n")
        self.warnings = 0
        self.errors = 0

    def _write(self, level: str, msg: str) -> None:
        line = f"{dt.datetime.now():%Y-%m-%d %H:%M:%S} {level:<5} {msg}"
        print(line)
        self._fh.write(line + "\n")
        self._fh.flush()

    def info(self, msg): self._write("INFO", msg)

    def warn(self, msg):
        self.warnings += 1
        self._write("WARN", msg)

    def error(self, msg):
        self.errors += 1
        self._write("ERROR", msg)

    def finish(self) -> str:
        status = "FAIL" if self.errors else ("WARN" if self.warnings else "OK")
        self._write("INFO", f"STATUS: {status} (warnings={self.warnings}, errors={self.errors})")
        self._fh.close()
        return status


def read_request(path: Path) -> list[dict]:
    with open(path, encoding="utf-8-sig", newline="") as f:
        rows = list(csv.DictReader(f))
    if not rows:
        raise ValueError(f"{path} has no rows")
    missing = [c for c in REQUIRED_COLUMNS if c not in rows[0]]
    if missing:
        raise ValueError(f"{path} is missing columns {missing}")
    return rows


def as_date(s: str) -> str:
    s = (s or "").strip()
    if re.fullmatch(r"\d{4}-\d{2}-\d{2}", s):
        return s
    return EARLIEST


def ready_rows(rows: list[dict], ids: set[str] | None, log: RunLog) -> list[dict]:
    """Rows with a Wind code and function filled in (and a field for wsd)."""
    out = []
    for r in rows:
        rid = r["request_id"].strip()
        if ids and rid not in ids:
            continue
        func = r["wind_function"].strip().lower()
        code, fld = r["wind_code"].strip(), r["wind_field"].strip()
        if not code or func not in ("wsd", "edb") or (func == "wsd" and not fld):
            log.info(f"{rid}: skipped (wind_code / wind_function / wind_field not filled in)")
            continue
        out.append(r)
    return out


def pull_one(w, r: dict, log: RunLog):
    """Call WindPy once. Returns (times, values, name) or raises."""
    func = r["wind_function"].strip().lower()
    code, fld = r["wind_code"].strip(), r["wind_field"].strip()
    opts = r["wind_options"].strip()
    begin, end = as_date(r["start_date"]), as_date(r["end_date"])
    if func == "wsd":
        data = w.wsd(code, fld, begin, end, opts)
    else:
        data = w.edb(code, begin, end, opts)
    if data.ErrorCode != 0:
        raise RuntimeError(f"WindPy {func} returned ErrorCode {data.ErrorCode} for {code}")
    if not data.Data or len(data.Data) != 1:
        raise RuntimeError(f"Expected one data column for {code}, got {len(data.Data or [])}")
    times, values = list(data.Times), list(data.Data[0])
    if len(times) != len(values):
        raise RuntimeError(f"{code}: {len(times)} dates but {len(values)} values")
    name = r["series_description"].strip()
    if func == "wsd":                       # ask Wind for its own series name (optional)
        try:
            nm = w.wss(code, "sec_name")
            if nm.ErrorCode == 0 and nm.Data and nm.Data[0] and nm.Data[0][0]:
                name = str(nm.Data[0][0])
        except Exception:                   # noqa: BLE001 - the name is optional
            pass
    return times, values, name, f"w.{func}('{code}', {repr(fld) + ', ' if func == 'wsd' else ''}"\
                                 f"'{begin}', '{end}', '{opts}')"


def save_xlsx(path: Path, r: dict, times, values, name: str, call: str) -> None:
    """Header rows (key, value), a blank row, then 'date','value' and the data.
    Values are written exactly as WindPy returned them; nothing is filled or dropped."""
    from openpyxl import Workbook
    wb = Workbook()
    ws = wb.active
    ws.title = r["request_id"].strip()[:31]
    header = [("series_name", name), ("wind_code", r["wind_code"].strip()),
              ("wind_field", r["wind_field"].strip()), ("wind_function", r["wind_function"].strip()),
              ("wind_options", r["wind_options"].strip()), ("request_id", r["request_id"].strip()),
              ("request_description", r["series_description"].strip()),
              ("source", "Wind, pulled with WindPy"), ("windpy_call", call),
              ("pulled_at", dt.datetime.now().strftime("%Y-%m-%d %H:%M:%S"))]
    for k, v in header:
        ws.append([k, v])
    ws.append([])
    ws.append(["date", "value"])
    for t, v in zip(times, values):
        d = t.date() if isinstance(t, dt.datetime) else t
        ws.append([d.isoformat() if hasattr(d, "isoformat") else str(d), v])
    wb.save(str(path))


def describe(times, values) -> str:
    """Counts and dates only (no values) for the log."""
    n = len(values)
    n_missing = sum(1 for v in values if v is None or (isinstance(v, float) and v != v))
    first = times[0] if times else None
    last = times[-1] if times else None
    return f"{n} dates, {n_missing} missing values, first {first}, last {last}"


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description="Pull the Wind request sheet with WindPy")
    ap.add_argument("--request", default=str(DEFAULT_REQUEST))
    ap.add_argument("--out-dir", default=str(DEFAULT_OUT))
    ap.add_argument("--log-dir", default=str(DEFAULT_LOGS))
    ap.add_argument("--ids", help="comma-separated request_ids, e.g. W01")
    ap.add_argument("--dry-run", action="store_true", help="list what would be pulled; no Wind calls")
    args = ap.parse_args(argv)
    try:
        sys.stdout.reconfigure(errors="replace")
    except AttributeError:
        pass
    log = RunLog("wind_pull", Path(args.log_dir))
    w = None
    try:
        log.info(f"Python {platform.python_version()}; request sheet {args.request}")
        rows = read_request(Path(args.request))
        ids = {s.strip() for s in args.ids.split(",")} if args.ids else None
        todo = ready_rows(rows, ids, log)
        log.info(f"{len(todo)} of {len(rows)} request rows have Wind codes filled in")
        out_dir = Path(args.out_dir)
        if args.dry_run or not todo:
            for r in todo:
                log.info(f"would pull {r['request_id']}: {r['wind_function']} {r['wind_code']} "
                         f"{r['wind_field']} {as_date(r['start_date'])}..{as_date(r['end_date'])}")
        else:
            from WindPy import w as wind                   # only exists with the Wind terminal
            w = wind
            w.start(waitTime=60)
            if not w.isconnected():
                raise RuntimeError("WindPy is not connected: open and log in to the Wind terminal")
            log.info("WindPy connected")
            out_dir.mkdir(parents=True, exist_ok=True)
            stamp = dt.date.today().strftime("%Y%m%d")
            for r in todo:
                rid = r["request_id"].strip()
                stem = Path(r["file_name_wanted"].strip() or rid).stem
                target = out_dir / f"{stem}_windpy_{stamp}.xlsx"
                if target.exists():
                    log.warn(f"{rid}: {target.name} already exists; not overwritten")
                    continue
                try:
                    times, values, name, call = pull_one(w, r, log)
                except Exception as exc:                   # noqa: BLE001
                    log.error(f"{rid}: {exc}")
                    continue
                if not values:
                    log.error(f"{rid}: no data returned")
                    continue
                save_xlsx(target, r, times, values, name, call)
                log.info(f"{rid}: saved {target.name} ({describe(times, values)})")
    except ModuleNotFoundError as exc:
        log.error(f"{exc}. WindPy is installed by the Wind terminal's Python plugin repair "
                  "tool, into the Python that runs this script.")
    except Exception as exc:                               # noqa: BLE001
        log.error(f"{type(exc).__name__}: {exc}")
        for ln in traceback.format_exc().splitlines():
            log.error("  " + ln)
    finally:
        if w is not None:
            try:
                w.stop()
            except Exception:                              # noqa: BLE001
                pass
    status = log.finish()
    print(f"Run log: {log.path}")
    return 1 if status == "FAIL" else 0


if __name__ == "__main__":
    sys.exit(main())
