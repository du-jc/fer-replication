"""
code/python/register_wind.py -- Week 2, Study A (Blueprint A.2 step A2 and B.3; Replication Plan s.2.2)

Checks and registers the Wind export(s) saved untouched in data/raw/proprietary/, then
builds the two monthly versions of the 12-month USD/CNY NDF compared in 03_select_bop_ndf.R.

A Wind Excel export usually looks like this (the reader does not assume fixed rows):

    header rows   first cell = a label (指标名称 / 频率 / 单位 / 指标ID ...), one column per series
    data rows     first cell = a date, then one value per series
    footer        a source line such as 数据来源：Wind

What the script does, for every sheet of every .xlsx file:
  1. Reads the layout: header labels (name, frequency, unit, Wind ID, source), the first
     and last data rows, the date order, and the footer text. Prints it (use --inspect to
     stop here).
  2. Checks each series: Wind ID present; unit present; frequency daily; dates readable,
     unique and ordered; gaps; at least one observation in every month Jan 2000 - Dec 2011;
     values numeric; source line present.
  3. Writes one ledger row per series (acquisition_method = manual_proprietary), with
     reading instructions for R in `notes`. A rerun on the same file adds nothing.
  4. Writes data/interim/ndf_daily.csv (ascending, blanks dropped) and
     data/interim/ndf_monthly.csv with, for each month (decision W2-06):
        ndf_mavg : mean of the available daily values in the month
        ndf_mend : value on the last available trading day of the month
        n_days, last_date
     data/interim/ is git-ignored, so Wind-derived values never reach GitHub.

The raw file is opened read-only and never saved. No data value is typed in this file.
Wind codes are read from the export, never supplied by the code (Blueprint B.3).

Usage (project root, .venv active):
  python code/python/register_wind.py --inspect      layout only, writes nothing but the log
  python code/python/register_wind.py                check, register, build monthly NDF
  python code/python/register_wind.py --file data/raw/proprietary/<name>.xlsx --series <WindID>
  python code/python/register_wind.py --unit yuan --frequency daily
                                                     record unit/frequency the export leaves blank
"""
from __future__ import annotations

import argparse
import csv
import datetime as dt
import hashlib
import platform
import re
import statistics
import sys
import traceback
import unicodedata
from dataclasses import dataclass, field
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
RAW_DIR = ROOT / "data" / "raw" / "proprietary"
DAILY_CSV = ROOT / "data" / "interim" / "ndf_daily.csv"
MONTHLY_CSV = ROOT / "data" / "interim" / "ndf_monthly.csv"
LEDGER_CSV = ROOT / "data" / "metadata" / "source_ledger.csv"
TRANSFORM_CSV = ROOT / "data" / "metadata" / "transformation_log.csv"
REQUEST_CSV = ROOT / "config" / "wind_request.csv"
GITIGNORE = ROOT / ".gitignore"
LOG_DIR = ROOT / "outputs" / "logs"
SCRIPT_REL = "code/python/register_wind.py"

SAMPLE_FIRST_MONTH = "2000-01"          # Replication Plan s.1
SAMPLE_LAST_MONTH = "2011-12"
GAP_WARN_DAYS = 7                       # calendar-day gap between observations that is reported

# Header labels are matched by keyword (after NFKC normalisation and lower-casing).
# These are the words Wind uses in its export headers, not data.
HEADER_KEYS = {
    "name": ["指标名称", "名称", "indicator", "name", "series"],
    "freq": ["频率", "frequency", "freq"],
    "unit": ["单位", "unit"],
    "id": ["指标id", "windid", "wind代码", "代码", "code", "id"],
    "source": ["来源", "source"],
}
DATE_HEADER_WORDS = ["日期", "date", "时间", "time"]
DAILY_WORDS = ["日", "daily", "day"]
SOURCE_WORDS = ["wind", "万得"]

LEDGER_FIELDS = ["series_name", "provider", "original_file_name", "download_date",
                 "source_url_or_access_note", "series_key_or_code", "frequency", "unit",
                 "currency", "seasonal_adjustment_status", "coverage_start", "coverage_end",
                 "acquisition_method", "layer", "license_or_access_restriction",
                 "transformation_script", "notes"]
TRANSFORM_FIELDS = ["timestamp_utc", "script", "step", "input_file", "input_sha256",
                    "output_file", "description", "n_affected", "decision_ref"]
LICENSE = ("Proprietary Wind data exported by a licensed Wind user; not redistributable; "
           "kept in git-ignored data/raw/proprietary/")


def configure_paths(root: Path) -> None:
    global ROOT, RAW_DIR, DAILY_CSV, MONTHLY_CSV, LEDGER_CSV, TRANSFORM_CSV, REQUEST_CSV
    global GITIGNORE, LOG_DIR
    ROOT = Path(root)
    RAW_DIR = ROOT / "data" / "raw" / "proprietary"
    DAILY_CSV = ROOT / "data" / "interim" / "ndf_daily.csv"
    MONTHLY_CSV = ROOT / "data" / "interim" / "ndf_monthly.csv"
    LEDGER_CSV = ROOT / "data" / "metadata" / "source_ledger.csv"
    TRANSFORM_CSV = ROOT / "data" / "metadata" / "transformation_log.csv"
    REQUEST_CSV = ROOT / "config" / "wind_request.csv"
    GITIGNORE = ROOT / ".gitignore"
    LOG_DIR = ROOT / "outputs" / "logs"


# --------------------------------------------------------------------------------------
# Run log (same format as the Week 1 scripts)
# --------------------------------------------------------------------------------------
class RunLog:
    def __init__(self, script_name: str, log_dir: Path | None = None):
        log_dir = log_dir or LOG_DIR
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

    def info(self, msg: str) -> None:
        self._write("INFO", msg)

    def warn(self, msg: str) -> None:
        self.warnings += 1
        self._write("WARN", msg)

    def error(self, msg: str) -> None:
        self.errors += 1
        self._write("ERROR", msg)

    def finish(self) -> str:
        status = "FAIL" if self.errors else ("WARN" if self.warnings else "OK")
        self._write("INFO", f"STATUS: {status} (warnings={self.warnings}, errors={self.errors})")
        self._fh.close()
        return status


# --------------------------------------------------------------------------------------
# Helpers
# --------------------------------------------------------------------------------------
def norm(s) -> str:
    if s is None:
        return ""
    return re.sub(r"\s+", "", unicodedata.normalize("NFKC", str(s))).lower()


def text(s) -> str:
    return "" if s is None else unicodedata.normalize("NFKC", str(s)).strip()


def sha256_file(p: Path) -> str:
    return hashlib.sha256(p.read_bytes()).hexdigest()


def rel(p: Path) -> str:
    try:
        return p.relative_to(ROOT).as_posix()
    except ValueError:
        return str(p)


def read_csv_rows(path: Path) -> tuple[list[str], list[dict]]:
    if not path.exists():
        return [], []
    with open(path, encoding="utf-8-sig", newline="") as f:
        rd = csv.DictReader(f)
        return list(rd.fieldnames or []), list(rd)


def append_csv_row(path: Path, fields: list[str], row: dict) -> None:
    header, _ = read_csv_rows(path)
    if header:
        missing = [k for k in row if k not in header]
        if missing:
            raise ValueError(f"{rel(path)} has no column(s) {missing}; header is {header}")
    else:
        header = fields
        path.parent.mkdir(parents=True, exist_ok=True)
        with open(path, "w", encoding="utf-8", newline="") as f:
            csv.writer(f, lineterminator="\n").writerow(header)
    with open(path, "a", encoding="utf-8", newline="") as f:
        csv.DictWriter(f, fieldnames=header, lineterminator="\n").writerow(
            {k: row.get(k, "") for k in header})


def log_transformation(step: str, input_file: Path, sha: str, output: Path,
                       description: str, n: int, decision: str) -> None:
    key = (SCRIPT_REL, step, sha, rel(output), str(n))
    _, rows = read_csv_rows(TRANSFORM_CSV)
    if any((r.get("script"), r.get("step"), r.get("input_sha256"), r.get("output_file"),
            r.get("n_affected")) == key for r in rows):
        return
    append_csv_row(TRANSFORM_CSV, TRANSFORM_FIELDS, {
        "timestamp_utc": dt.datetime.now(dt.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
        "script": SCRIPT_REL, "step": step, "input_file": rel(input_file),
        "input_sha256": sha, "output_file": rel(output), "description": description,
        "n_affected": n, "decision_ref": decision})


_DATE_RE = re.compile(r"^(\d{4})[-/.年](\d{1,2})[-/.月](\d{1,2})日?(?:\s+\d{1,2}:\d{2}(?::\d{2})?)?$")


def as_date(v) -> dt.date | None:
    """A cell that is a date: an Excel date cell or text like 2000-01-04 / 2000/1/4 / 2000年1月4日."""
    if isinstance(v, dt.datetime):
        return v.date()
    if isinstance(v, dt.date):
        return v
    if isinstance(v, str):
        m = _DATE_RE.match(unicodedata.normalize("NFKC", v).strip())
        if m:
            try:
                return dt.date(int(m.group(1)), int(m.group(2)), int(m.group(3)))
            except ValueError:
                return None
    return None


def as_number(v) -> float | None:
    if v is None or isinstance(v, bool):
        return None
    if isinstance(v, (int, float)):
        return float(v)
    s = text(v).replace(",", "")
    try:
        return float(s)
    except ValueError:
        return None


def header_kind(label: str) -> str:
    n = norm(label).rstrip(":：")
    if not n:
        return ""
    for kind, words in HEADER_KEYS.items():
        # 'id' and 'code' must match as whole labels or suffixes, not inside other words
        for w in words:
            if (kind == "id" and (n == w or n.endswith(w))) or (kind != "id" and w in n):
                return kind
    if any(w == n for w in DATE_HEADER_WORDS):
        return "date_header"
    return ""


# --------------------------------------------------------------------------------------
# Layout
# --------------------------------------------------------------------------------------
@dataclass
class Series:
    file: Path
    sheet: str
    col: int                         # 0-based column in the sheet
    header: dict = field(default_factory=dict)   # kind -> text
    supplied: set = field(default_factory=set)    # header kinds supplied on the command line
    obs: list = field(default_factory=list)      # [(date, value or None)] in file order


@dataclass
class Layout:
    file: Path
    sheet: str
    header_rows: list = field(default_factory=list)   # 1-based (Excel) row numbers
    header_labels: list = field(default_factory=list)  # (row, label text, kind)
    first_data_row: int = 0
    last_data_row: int = 0
    n_data_rows: int = 0
    order: str = ""
    footer: list = field(default_factory=list)
    stray_rows: list = field(default_factory=list)    # non-date rows inside the data block
    series: list = field(default_factory=list)


def read_layout(path: Path, sheet_name: str, rows: list[list]) -> Layout:
    lay = Layout(file=path, sheet=sheet_name)
    data_idx = [i for i, r in enumerate(rows) if r and as_date(r[0]) is not None]
    if not data_idx:
        return lay
    first, last = data_idx[0], data_idx[-1]
    lay.first_data_row, lay.last_data_row, lay.n_data_rows = first + 1, last + 1, len(data_idx)
    width = max(len(r) for r in rows) if rows else 0
    # header block: non-empty rows above the first date row
    hdr_rows = [i for i in range(first) if any(c is not None and text(c) for c in rows[i])]
    lay.header_rows = [i + 1 for i in hdr_rows]
    for i in hdr_rows:
        lab = text(rows[i][0]) if rows[i] else ""
        lay.header_labels.append((i + 1, lab, header_kind(lab)))
    # rows after the data block, and non-date rows inside it
    for i in range(last + 1, len(rows)):
        t = " ".join(text(c) for c in rows[i] if c is not None and text(c))
        if t:
            lay.footer.append(t)
    for i in range(first, last + 1):
        if rows[i] and as_date(rows[i][0]) is None and any(c is not None and text(c) for c in rows[i]):
            lay.stray_rows.append(i + 1)
    # series columns: any column > 0 with a header entry or a value in the data block
    for j in range(1, width):
        has = any(j < len(rows[i]) and rows[i][j] is not None and text(rows[i][j])
                  for i in hdr_rows + data_idx)
        if not has:
            continue
        s = Series(file=path, sheet=sheet_name, col=j)
        for i in hdr_rows:
            kind = header_kind(text(rows[i][0])) if rows[i] else ""
            val = text(rows[i][j]) if j < len(rows[i]) else ""
            if kind in HEADER_KEYS and val:
                s.header[kind] = val
            elif kind == "date_header" and val and "name" not in s.header:
                s.header["name"] = val
        for i in data_idx:
            v = rows[i][j] if j < len(rows[i]) else None
            s.obs.append((as_date(rows[i][0]), as_number(v), v))
        lay.series.append(s)
    dates = [as_date(rows[i][0]) for i in data_idx]
    if all(a < b for a, b in zip(dates, dates[1:])):
        lay.order = "ascending"
    elif all(a > b for a, b in zip(dates, dates[1:])):
        lay.order = "descending"
    else:
        lay.order = "unordered_or_duplicates"
    return lay


def load_layouts(path: Path) -> list[Layout]:
    import openpyxl
    wb = openpyxl.load_workbook(path, read_only=True, data_only=True)
    out = []
    for ws in wb.worksheets:
        # Wind exports can store a wrong sheet size (e.g. "A1") in the file; read-only mode
        # trusts it and would return one row. Reset it so every stored row is read.
        ws.reset_dimensions()
        rows = [list(r) for r in ws.iter_rows(values_only=True)]
        out.append(read_layout(path, ws.title, rows))
    wb.close()
    return out


def describe_layout(lay: Layout, log: RunLog) -> None:
    log.info(f"Layout of {rel(lay.file)} / sheet '{lay.sheet}':")
    if not lay.n_data_rows:
        log.info("  no rows whose first cell is a date (sheet skipped)")
        return
    for r, lab, kind in lay.header_labels:
        log.info(f"  header row {r}: first cell '{lab}' -> {kind or 'unrecognised'}")
    log.info(f"  data rows {lay.first_data_row}..{lay.last_data_row} ({lay.n_data_rows} dated rows), "
             f"date order {lay.order}")
    log.info(f"  footer lines: {lay.footer if lay.footer else 'none'}")
    for s in lay.series:
        log.info(f"  series in column {chr(65 + s.col) if s.col < 26 else s.col + 1}: "
                 + "; ".join(f"{k}={v}" for k, v in s.header.items()))


# --------------------------------------------------------------------------------------
# Checks
# --------------------------------------------------------------------------------------
def month_key(d: dt.date) -> str:
    return f"{d.year}-{d.month:02d}"


def months_between(a: str, b: str) -> list[str]:
    y, m = int(a[:4]), int(a[5:])
    out = []
    while f"{y}-{m:02d}" <= b:
        out.append(f"{y}-{m:02d}")
        m += 1
        if m == 13:
            y, m = y + 1, 1
    return out


def check_series(lay: Layout, s: Series, log: RunLog) -> bool:
    """Returns False if an ERROR-level problem was found."""
    ok = True
    tag = s.header.get("id") or f"col{s.col + 1}"

    def fail(msg):
        nonlocal ok
        ok = False
        log.error(f"CHECK FAIL  [{tag}] {msg}")

    def passed(msg):
        log.info(f"CHECK PASS  [{tag}] {msg}")

    if s.header.get("id"):
        passed(f"Wind ID present: {s.header['id']}")
    else:
        fail("no Wind ID/code in the header rows (Blueprint B.3: code present)")
    if s.header.get("unit"):
        passed(f"unit: {s.header['unit']}"
               + (" (supplied with --unit; not in the Wind export)" if "unit" in s.supplied else ""))
    else:
        log.warn(f"[{tag}] no unit in the header rows")
    f = norm(s.header.get("freq", ""))
    if f and any(w in f for w in DAILY_WORDS):
        passed(f"frequency: {s.header['freq']}"
               + (" (supplied with --frequency; not in the Wind export)" if "freq" in s.supplied else ""))
    else:
        log.warn(f"[{tag}] frequency '{s.header.get('freq', '')}' does not say daily")
    if s.header.get("name"):
        passed(f"name: {s.header['name']}")
    if lay.order not in ("ascending", "descending"):
        fail(f"dates are {lay.order}")
    else:
        passed(f"dates strictly {lay.order} (no duplicates)")
    if lay.stray_rows:
        log.warn(f"[{tag}] non-date rows inside the data block at Excel rows {lay.stray_rows[:5]}")
    dates = [d for d, _, _ in s.obs]
    wk = sum(1 for d in dates if d.weekday() >= 5)
    if wk:
        log.warn(f"[{tag}] {wk} observation(s) dated on a weekend")
    vals = [(d, v, raw) for d, v, raw in s.obs]
    blank = [d for d, v, raw in vals if v is None and (raw is None or text(raw) == "")]
    nonnum = [d for d, v, raw in vals if v is None and raw is not None and text(raw) != ""]
    if nonnum:
        fail(f"{len(nonnum)} non-numeric value cell(s), first on {nonnum[0]}")
    if blank:
        log.warn(f"[{tag}] {len(blank)} blank value cell(s) (dropped when building monthly data)")
    good = sorted((d, v) for d, v, _ in vals if v is not None)
    nonpos = [d for d, v in good if v <= 0]
    if nonpos:
        log.warn(f"[{tag}] {len(nonpos)} value(s) <= 0, first on {nonpos[0]}")
    if good:
        passed(f"{len(good)} numeric observations, {good[0][0]} .. {good[-1][0]}")
        gaps = [(a[0], b[0], (b[0] - a[0]).days) for a, b in zip(good, good[1:])
                if (b[0] - a[0]).days > GAP_WARN_DAYS]
        if gaps:
            log.warn(f"[{tag}] {len(gaps)} gap(s) longer than {GAP_WARN_DAYS} days; longest "
                     + ", ".join(f"{a}->{b} ({n}d)" for a, b, n in sorted(gaps, key=lambda g: -g[2])[:5]))
        else:
            passed(f"no gap longer than {GAP_WARN_DAYS} calendar days")
        have = {month_key(d) for d, _ in good}
        need = months_between(SAMPLE_FIRST_MONTH, SAMPLE_LAST_MONTH)
        miss = [m for m in need if m not in have]
        if miss:
            fail(f"{len(miss)} sample month(s) with no observation: {miss[:6]}")
        else:
            passed(f"every month {SAMPLE_FIRST_MONTH}..{SAMPLE_LAST_MONTH} has observations")
    else:
        fail("no numeric observations")
    src = " ".join(lay.footer) + " " + s.header.get("source", "")
    if any(w in norm(src) for w in SOURCE_WORDS):
        passed("source line mentions Wind")
    else:
        log.warn(f"[{tag}] no source line mentioning Wind found (footer: {lay.footer[:2]})")
    return ok


def gitignore_ok(log: RunLog) -> None:
    if not GITIGNORE.exists():
        log.error(".gitignore not found: data/raw/proprietary/ must be git-ignored")
        return
    lines = {ln.strip().rstrip("/") for ln in GITIGNORE.read_text(encoding="utf-8").splitlines()}
    if "data/raw/proprietary" in lines:
        log.info("CHECK PASS  data/raw/proprietary/ is in .gitignore")
    else:
        log.error("data/raw/proprietary/ is not in .gitignore (Blueprint 0.3)")
    if "data/interim" not in lines:
        log.warn("data/interim/ is not in .gitignore; the monthly NDF file written there "
                 "contains Wind-derived values")


# --------------------------------------------------------------------------------------
# Ledger and monthly NDF
# --------------------------------------------------------------------------------------
def request_id() -> str:
    _, rows = read_csv_rows(REQUEST_CSV)
    return rows[0].get("request_id", "") if rows else ""


def write_ledger(lay: Layout, s: Series, sha: str, log: RunLog) -> None:
    code = s.header.get("id", "")
    _, rows = read_csv_rows(LEDGER_CSV)
    if any(r.get("series_key_or_code") == code and f"sha256={sha}" in r.get("notes", "")
           for r in rows):
        log.info(f"Ledger already has {code} for this file (same sha256); not added again")
        return
    good = sorted(d for d, v, _ in s.obs if v is not None)
    col = chr(65 + s.col) if s.col < 26 else str(s.col + 1)
    mtime = dt.datetime.fromtimestamp(lay.file.stat().st_mtime).date().isoformat()
    notes = "; ".join([
        f"file={rel(lay.file)}", f"sheet={lay.sheet}",
        f"header_rows_excel={','.join(map(str, lay.header_rows))}",
        f"first_data_row_excel={lay.first_data_row}", f"last_data_row_excel={lay.last_data_row}",
        "date_col=A", f"value_col={col}", f"date_order={lay.order}",
        f"footer={' | '.join(lay.footer)[:120]}", f"registered_on={dt.date.today().isoformat()}",
        "download_date_is=file modification date", f"wind_request_id={request_id()}",
        *[f"{k}_source=stated by the user, not in the Wind export" for k in sorted(s.supplied)],
        f"interim_daily={rel(DAILY_CSV)}", f"interim_monthly={rel(MONTHLY_CSV)}", f"sha256={sha}"])
    append_csv_row(LEDGER_CSV, LEDGER_FIELDS, {
        "series_name": s.header.get("name", f"Wind series in column {col}"),
        "provider": "Wind Information Co. (Wind Financial Terminal)",
        "original_file_name": lay.file.name,
        "download_date": mtime,
        "source_url_or_access_note": "Wind terminal Excel export by a Wind user; received as "
                                     ".xlsx and saved untouched",
        "series_key_or_code": code,
        "frequency": s.header.get("freq", ""),
        "unit": s.header.get("unit", ""),
        "currency": "see unit",
        "seasonal_adjustment_status": "not applicable (market price)",
        "coverage_start": good[0].isoformat() if good else "",
        "coverage_end": good[-1].isoformat() if good else "",
        "acquisition_method": "manual_proprietary",
        "layer": "rep",
        "license_or_access_restriction": LICENSE,
        "transformation_script": SCRIPT_REL,
        "notes": notes})
    log.info(f"Ledger row written: {code}")


def monthly_ndf(obs: list[tuple[dt.date, float]]) -> list[dict]:
    """obs ascending, no blanks. Month average and month-end (last available day)."""
    by_month: dict[str, list] = {}
    for d, v in obs:
        by_month.setdefault(month_key(d), []).append((d, v))
    out = []
    for m in sorted(by_month):
        xs = by_month[m]
        out.append({"month": m, "ndf_mavg": statistics.fmean(v for _, v in xs),
                    "ndf_mend": xs[-1][1], "n_days": len(xs), "last_date": xs[-1][0].isoformat()})
    return out


def write_ndf(s: Series, lay: Layout, sha: str, log: RunLog) -> None:
    good = sorted((d, v) for d, v, _ in s.obs if v is not None)
    DAILY_CSV.parent.mkdir(parents=True, exist_ok=True)
    with open(DAILY_CSV, "w", encoding="utf-8", newline="") as f:
        w = csv.writer(f, lineterminator="\n")
        w.writerow(["date", "ndf", "wind_id"])
        for d, v in good:
            w.writerow([d.isoformat(), repr(v), s.header.get("id", "")])
    rows = monthly_ndf(good)
    with open(MONTHLY_CSV, "w", encoding="utf-8", newline="") as f:
        w = csv.writer(f, lineterminator="\n")
        w.writerow(["month", "ndf_mavg", "ndf_mend", "n_days", "last_date", "wind_id"])
        for r in rows:
            w.writerow([r["month"], repr(r["ndf_mavg"]), repr(r["ndf_mend"]), r["n_days"],
                        r["last_date"], s.header.get("id", "")])
    n_drop = sum(1 for _, v, _ in s.obs if v is None)
    log.info(f"Wrote {rel(DAILY_CSV)} ({len(good)} days) and {rel(MONTHLY_CSV)} ({len(rows)} months)")
    if lay.order == "descending":
        log_transformation("ndf_sort_ascending", lay.file, sha, DAILY_CSV,
                           "Daily rows re-ordered from descending to ascending dates", len(good), "W2-06")
    log_transformation("ndf_drop_blank", lay.file, sha, DAILY_CSV,
                       "Blank daily value cells dropped (no filling)", n_drop, "W2-06")
    log_transformation("ndf_month_average", lay.file, sha, MONTHLY_CSV,
                       "Mean of available daily values in each month", len(rows), "W2-06")
    log_transformation("ndf_month_end", lay.file, sha, MONTHLY_CSV,
                       "Value on the last available trading day of each month", len(rows), "W2-06")
    short = [r["month"] for r in rows if SAMPLE_FIRST_MONTH <= r["month"] <= SAMPLE_LAST_MONTH
             and r["n_days"] < 10]
    if short:
        log.warn(f"{len(short)} sample month(s) with fewer than 10 daily observations: {short[:6]}")


# --------------------------------------------------------------------------------------
# Main
# --------------------------------------------------------------------------------------
def find_files(arg: str | None) -> list[Path]:
    if arg:
        p = Path(arg)
        p = p if p.is_absolute() else ROOT / p
        if not p.exists():
            raise FileNotFoundError(f"{p} not found")
        if RAW_DIR.resolve() not in p.resolve().parents:
            raise ValueError(f"{rel(p)} is not inside data/raw/proprietary/")
        return [p]
    files = sorted(p for p in RAW_DIR.rglob("*") if p.suffix.lower() in (".xlsx", ".xlsm")
                   and not p.name.startswith("~$"))
    if not files:
        raise FileNotFoundError("no .xlsx file in data/raw/proprietary/")
    return files


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description="Check and register Wind exports (Study A, Week 2)")
    ap.add_argument("--file", help="one file in data/raw/proprietary/ (default: all .xlsx there)")
    ap.add_argument("--series", help="Wind ID of the 12-month NDF if the files hold several series")
    ap.add_argument("--inspect", action="store_true", help="print the layout only; write nothing")
    ap.add_argument("--unit", help="unit to record when the export leaves it blank")
    ap.add_argument("--frequency", help="frequency to record when the export leaves it blank")
    args = ap.parse_args(argv)
    log = RunLog("register_wind")
    try:
        log.info(f"Python {platform.python_version()} on {platform.system()} {platform.release()}")
        gitignore_ok(log)
        found: list[tuple[Layout, Series, str]] = []
        for path in find_files(args.file):
            sha = sha256_file(path)
            log.info(f"File {rel(path)} sha256={sha}")
            for lay in load_layouts(path):
                describe_layout(lay, log)
                for s in lay.series:
                    found.append((lay, s, sha))
            if sha256_file(path) != sha:
                log.error(f"{rel(path)} changed while being read")
        if not found:
            raise ValueError("no dated series found in any sheet")
        for _, s, _ in found:                     # fill only what the export leaves blank
            for kind, val in (("unit", args.unit), ("freq", args.frequency)):
                if not val:
                    continue
                if s.header.get(kind):
                    if s.header[kind] != val:
                        log.warn(f"[{s.header.get('id', '')}] export states {kind} "
                                 f"'{s.header[kind]}'; --{'frequency' if kind == 'freq' else kind} "
                                 f"'{val}' ignored")
                else:
                    s.header[kind] = val
                    s.supplied.add(kind)
                    log.info(f"[{s.header.get('id', '')}] {kind} '{val}' supplied on the command line")
        if args.inspect:
            log.info("--inspect: layout only; nothing checked, registered or written")
        else:
            all_ok = True
            for lay, s, sha in found:
                all_ok = check_series(lay, s, log) and all_ok
            if not all_ok:
                raise ValueError("checks failed; nothing registered")
            for lay, s, sha in found:
                write_ledger(lay, s, sha, log)
            chosen = found
            if args.series:
                chosen = [t for t in found if t[1].header.get("id") == args.series]
            if len(chosen) != 1:
                raise ValueError(f"{len(chosen)} series match; the monthly NDF needs exactly one. "
                                 "Rerun with --series <WindID> (IDs are listed above)")
            lay, s, sha = chosen[0]
            log.info(f"Monthly NDF built from {s.header.get('id')} ({s.header.get('name', '')})")
            write_ndf(s, lay, sha, log)
    except Exception as exc:
        log.error(f"{type(exc).__name__}: {exc}")
        for ln in traceback.format_exc().splitlines()[-6:]:
            log.info(f"  {ln}")
    status = log.finish()
    print(f"Run log: {log.path}")
    return 1 if status == "FAIL" else 0


if __name__ == "__main__":
    sys.exit(main())
