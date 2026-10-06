"""
code/python/collect_bop.py -- Week 2, Study A (Blueprint A.2 step A2; Replication Plan s.2.2)

Collects China's quarterly balance of payments (BOP) from SAFE:

  1. Opens SAFE's BOP list page and finds the release titled
     中国国际收支平衡表时间序列(BPM6).
  2. Opens the release page and downloads the Excel file linked there.
  3. Saves it untouched in data/raw/safe/ (a new file only if the content changed).
  4. Reads sheet 4 (by position, Blueprint A.2) and the row
       1. 经常账户            (current account balance)
     with its quarterly column headers. Only the current account is used (decision W2-12;
     the Plan's second row, 2. 资本和金融账户, is not collected).
  5. Writes one row to data/metadata/source_ledger.csv.
  6. Writes data/interim/bop_quarterly.csv and the three monthly candidates
     (1 series x 3 quarterly-to-monthly conversions, Replication Plan s.2.2; W2-12) to
     data/interim/bop_monthly_candidates.csv, and logs each conversion in
     data/metadata/transformation_log.csv.

Conversions (decision W2-04), for quarter q of year Y with published value Q(Y,q):
  hold : each month of quarter q gets Q(Y,q)
  div3 : each month of quarter q gets Q(Y,q) / 3
  ytd  : each month of quarter q gets Q(Y,1) + ... + Q(Y,q)   (resets every January,
         the same construction as the appendix GDP)

Rules (Blueprint Part 0): no data value is typed in this file; the raw file is never
edited or overwritten; every run writes outputs/logs/collect_bop_<timestamp>.log.

Usage (project root, .venv active):
  python code/python/collect_bop.py                  download and process
  python code/python/collect_bop.py --file data/raw/safe/<name>.xlsx
                                                     process a file already saved there
                                                     (e.g. downloaded by hand in a browser)
"""
from __future__ import annotations

import argparse
import csv
import datetime as dt
import hashlib
import platform
import re
import sys
import time
import traceback
import unicodedata
from html.parser import HTMLParser
from pathlib import Path
from urllib.parse import urljoin, urlparse

ROOT = Path(__file__).resolve().parents[2]
RAW_DIR = ROOT / "data" / "raw" / "safe"
QUARTERLY_CSV = ROOT / "data" / "interim" / "bop_quarterly.csv"
MONTHLY_CSV = ROOT / "data" / "interim" / "bop_monthly_candidates.csv"
LEDGER_CSV = ROOT / "data" / "metadata" / "source_ledger.csv"
TRANSFORM_CSV = ROOT / "data" / "metadata" / "transformation_log.csv"
LOG_DIR = ROOT / "outputs" / "logs"
SCRIPT_REL = "code/python/collect_bop.py"

# ---- Specification (Blueprint A.2; Replication Plan s.2.2). Names and positions, not data.
LIST_URL = "https://www.safe.gov.cn/safe/zggjszphb/index.html"   # SAFE: 中国国际收支平衡表 list
RELEASE_TITLE = "中国国际收支平衡表时间序列(BPM6)"
SHEET_POSITION = 4                                                # 1-based, Blueprint A.2
SERIES = [  # (column name used downstream, row label in sheet 4, English gloss)
    ("bop_ca", "1. 经常账户", "current account balance"),   # W2-12: current account only
]
UNIT_EXPECTED = "亿美元"      # BOP unit in the original study: 100 million USD (Table 4.2 note)
SAMPLE_FIRST = (2000, 1)      # Replication Plan s.1: Jan 2000 - Dec 2011
SAMPLE_LAST = (2011, 4)
CONVERSIONS = ["hold", "div3", "ytd"]
HTTP_HEADERS = {"User-Agent": "Mozilla/5.0 (academic replication; fer-replication)"}
SAFE_WAIT_SECONDS = 1.0       # Blueprint B.1: one second between SAFE requests
RETRIES = 3

LEDGER_FIELDS = ["series_name", "provider", "original_file_name", "download_date",
                 "source_url_or_access_note", "series_key_or_code", "frequency", "unit",
                 "currency", "seasonal_adjustment_status", "coverage_start", "coverage_end",
                 "acquisition_method", "layer", "license_or_access_restriction",
                 "transformation_script", "notes"]
TRANSFORM_FIELDS = ["timestamp_utc", "script", "step", "input_file", "input_sha256",
                    "output_file", "description", "n_affected", "decision_ref"]


def configure_paths(root: Path) -> None:
    """Point every path at another project root (used by the tests)."""
    global ROOT, RAW_DIR, QUARTERLY_CSV, MONTHLY_CSV, LEDGER_CSV, TRANSFORM_CSV, LOG_DIR
    ROOT = Path(root)
    RAW_DIR = ROOT / "data" / "raw" / "safe"
    QUARTERLY_CSV = ROOT / "data" / "interim" / "bop_quarterly.csv"
    MONTHLY_CSV = ROOT / "data" / "interim" / "bop_monthly_candidates.csv"
    LEDGER_CSV = ROOT / "data" / "metadata" / "source_ledger.csv"
    TRANSFORM_CSV = ROOT / "data" / "metadata" / "transformation_log.csv"
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
# Small helpers
# --------------------------------------------------------------------------------------
def norm(s) -> str:
    """Compare labels safely: full-width -> half-width (NFKC), all whitespace removed."""
    if s is None:
        return ""
    return re.sub(r"\s+", "", unicodedata.normalize("NFKC", str(s)))


def sha256_bytes(b: bytes) -> str:
    return hashlib.sha256(b).hexdigest()


def sha256_file(p: Path) -> str:
    return sha256_bytes(p.read_bytes())


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


def append_csv_row(path: Path, fields: list[str], row: dict, log: RunLog) -> None:
    """Append one row, keeping the column order of an existing file (header-only files
    were created in Week 1 by init_config_and_metadata.ps1)."""
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
                       description: str, n: int, decision: str, log: RunLog) -> None:
    """One row per step; a rerun on the same input does not add a duplicate."""
    key = (SCRIPT_REL, step, sha, rel(output), str(n))
    _, rows = read_csv_rows(TRANSFORM_CSV)
    for r in rows:
        if (r.get("script"), r.get("step"), r.get("input_sha256"), r.get("output_file"),
                r.get("n_affected")) == key:
            return
    append_csv_row(TRANSFORM_CSV, TRANSFORM_FIELDS, {
        "timestamp_utc": dt.datetime.now(dt.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
        "script": SCRIPT_REL, "step": step, "input_file": rel(input_file),
        "input_sha256": sha, "output_file": rel(output), "description": description,
        "n_affected": n, "decision_ref": decision}, log)


# --------------------------------------------------------------------------------------
# Web: list page -> release page -> Excel link
# --------------------------------------------------------------------------------------
class LinkParser(HTMLParser):
    """Collects <a> tags (href, title, text) and the page's visible text."""

    def __init__(self):
        super().__init__(convert_charrefs=True)
        self.links: list[dict] = []
        self.text_parts: list[str] = []
        self._a: dict | None = None

    def handle_starttag(self, tag, attrs):
        if tag.lower() == "a":
            a = dict(attrs)
            self._a = {"href": a.get("href") or "", "title": a.get("title") or "", "text": ""}

    def handle_endtag(self, tag):
        if tag.lower() == "a" and self._a is not None:
            self.links.append(self._a)
            self._a = None

    def handle_data(self, data):
        self.text_parts.append(data)
        if self._a is not None:
            self._a["text"] += data


def parse_html(html: str) -> LinkParser:
    p = LinkParser()
    p.feed(html)
    return p


def http_get(session, url: str, log: RunLog) -> bytes:
    """GET with retries; waits SAFE_WAIT_SECONDS before every request."""
    last = None
    for attempt in range(1, RETRIES + 1):
        time.sleep(SAFE_WAIT_SECONDS)
        try:
            r = session.get(url, headers=HTTP_HEADERS, timeout=60)
            r.raise_for_status()
            log.info(f"GET {url} -> HTTP {r.status_code}, {len(r.content)} bytes")
            return r.content
        except Exception as exc:  # network errors and HTTP errors are retried
            last = exc
            log.warn(f"GET {url} attempt {attempt}/{RETRIES} failed: {exc}")
            time.sleep(2 * attempt)
    raise RuntimeError(f"could not download {url}: {last}")


def find_release_link(list_html: str, base_url: str) -> tuple[str, list[str]]:
    """Return (absolute URL of the release page, titles of all BPM6-like links seen)."""
    links = parse_html(list_html).links
    want = norm(RELEASE_TITLE)
    seen = [norm(a["title"] or a["text"]) for a in links if "BPM6" in norm(a["title"] + a["text"])]
    for a in links:
        if norm(a["title"]) == want or norm(a["text"]) == want:
            return urljoin(base_url, a["href"]), seen
    return "", seen


def find_excel_link(release_html: str, base_url: str) -> tuple[str, str, str]:
    """Return (absolute file URL, link title, release date text) from the release page."""
    p = parse_html(release_html)
    cands = [a for a in p.links
             if re.search(r"\.xlsx?$", urlparse(a["href"]).path, re.IGNORECASE)]
    cands.sort(key=lambda a: 0 if "BPM6" in norm(a["title"] + a["text"]) else 1)
    text = norm(" ".join(p.text_parts))
    m = re.search(r"发布日期[:：]?(\d{4}-\d{2}-\d{2})", text)
    date = m.group(1) if m else ""
    if not cands:
        return "", "", date
    a = cands[0]
    return urljoin(base_url, a["href"]), (a["title"] or a["text"]).strip(), date


# --------------------------------------------------------------------------------------
# Raw file: save untouched, new vintage only if content changed
# --------------------------------------------------------------------------------------
def save_raw(content: bytes, file_url: str, release_date: str, log: RunLog) -> tuple[Path, bool]:
    RAW_DIR.mkdir(parents=True, exist_ok=True)
    sha = sha256_bytes(content)
    for f in sorted(RAW_DIR.glob("*")):
        if f.is_file() and sha256_file(f) == sha:
            log.info(f"Same content already saved as {rel(f)}; no new vintage")
            return f, False
    ext = Path(urlparse(file_url).path).suffix.lower() or ".xlsx"
    tag = (release_date or "undated").replace("-", "")
    name = f"SAFE_BOP_BPM6_timeseries_release{tag}_dl{dt.date.today():%Y%m%d}_{sha[:8]}{ext}"
    dest = RAW_DIR / name
    with open(dest, "xb") as f:          # 'x' refuses to overwrite an existing file
        f.write(content)
    log.info(f"Saved new vintage {rel(dest)} (sha256 {sha[:16]}...)")
    return dest, True


# --------------------------------------------------------------------------------------
# Sheet parsing
# --------------------------------------------------------------------------------------
_CN_Q = {"一": 1, "二": 2, "三": 3, "四": 4, "1": 1, "2": 2, "3": 3, "4": 4}


def parse_period(text) -> tuple | None:
    """('Q', year, q) for a quarter header, ('A', year) for an annual one, None otherwise.
    Accepts forms such as 2000Q1, 2000 Q1, Q1 2000, 2000年一季度, 2000年第1季度, 2000年1-3月."""
    if text is None:
        return None
    if isinstance(text, (dt.date, dt.datetime)):
        return ("Q", text.year, (text.month - 1) // 3 + 1)
    t = norm(text)
    m = re.search(r"(19|20)\d{2}", t)
    if not m:
        return None
    year = int(m.group(0))
    rest = t[:m.start()] + "|" + t[m.end():]
    for pat in (r"[Qq]([1-4])", r"([1-4])[Qq]", r"第?([一二三四1-4])季"):
        mq = re.search(pat, rest)
        if mq:
            return ("Q", year, _CN_Q[mq.group(1)])
    mm = re.search(r"(\d{1,2})-(\d{1,2})月", rest)
    if mm and int(mm.group(2)) - int(mm.group(1)) == 2 and int(mm.group(2)) % 3 == 0:
        return ("Q", year, int(mm.group(2)) // 3)
    if re.fullmatch(r"\|(年|年度|全年)?", rest):
        return ("A", year)
    return None


def to_float(v) -> float | None:
    if v is None:
        return None
    if isinstance(v, (int, float)) and not isinstance(v, bool):
        return float(v)
    s = norm(v).replace(",", "").replace("\u2212", "-")
    if s in ("", "-", "--", "…", "...", "NA", "n.a."):
        return None
    try:
        return float(s)
    except ValueError:
        return None


def read_sheet(path: Path, log: RunLog) -> tuple[str, list[list], list[str]]:
    import openpyxl
    if path.suffix.lower() != ".xlsx":
        raise ValueError(f"{path.name}: expected an .xlsx file (got {path.suffix}); "
                         "the raw file is never converted -- report this")
    wb = openpyxl.load_workbook(path, read_only=True, data_only=True)
    names = list(wb.sheetnames)
    for i, n in enumerate(names, start=1):
        log.info(f"  sheet {i}: {n}")
    if len(names) < SHEET_POSITION:
        raise ValueError(f"workbook has {len(names)} sheets; sheet {SHEET_POSITION} requested")
    ws = wb.worksheets[SHEET_POSITION - 1]
    rows = [list(r) for r in ws.iter_rows(values_only=True)]
    wb.close()
    return names[SHEET_POSITION - 1], rows, names


def find_unit(rows: list[list]) -> str:
    for r in rows[:30]:
        for c in r:
            if c is not None and "单位" in norm(c):
                return str(c).strip()
    return ""


def locate_label(rows: list[list], label: str) -> list[tuple[int, int]]:
    want = norm(label)
    return [(i, j) for i, r in enumerate(rows) for j, c in enumerate(r) if norm(c) == want]


def period_columns(rows: list[list], above: int, log: RunLog) -> tuple[dict, list[int]]:
    """Find header rows above row `above` that carry years, forward-fill merged cells
    horizontally, combine the header rows per column and parse each column's period.
    Returns ({col: ('Q', y, q) or ('A', y)}, header_row_indices)."""
    year_rows = [i for i in range(above)
                 if sum(1 for c in rows[i] if c is not None and re.search(r"(19|20)\d{2}", norm(c))) >= 3]
    if not year_rows:
        return {}, []
    # the header block runs from the first row with years down to the first BOP row;
    # it keeps every row with at least 3 filled cells (e.g. a quarter row under a year row)
    hdr = [i for i in range(year_rows[0], above)
           if sum(1 for c in rows[i] if c is not None and norm(c) != "") >= 3]
    width = max(len(rows[i]) for i in hdr)
    padded = [list(rows[i]) + [None] * (width - len(rows[i])) for i in hdr]
    # a column is active if any header row has text in it; only active columns are
    # forward-filled (merged year cells), so trailing empty columns never become periods
    active = [any(r[j] is not None and norm(r[j]) != "" for r in padded) for j in range(width)]
    filled = []
    for r in padded:
        last = None
        out = []
        for j, c in enumerate(r):
            if c is not None and norm(c) != "":
                last = c
                out.append(c)
            else:
                out.append(last if active[j] else None)
        filled.append(out)
    cols = {}
    for j in range(width):
        parts = [str(f[j]) if not isinstance(f[j], (dt.date, dt.datetime)) else f[j]
                 for f in filled if f[j] is not None]
        per = None
        for p in parts:                          # a date-typed header is parsed on its own
            if isinstance(p, (dt.date, dt.datetime)):
                per = parse_period(p)
        if per is None and parts:
            per = parse_period(" ".join(str(p) for p in parts))
        if per:
            cols[j] = per
    return cols, hdr


def extract_series(rows: list[list], log: RunLog) -> tuple[dict, dict]:
    """Returns ({name: {(y, q): value}}, info). Raises on structural problems."""
    info: dict = {"row_index": {}, "label_col": {}}
    positions = {}
    for name, label, _ in SERIES:
        hits = locate_label(rows, label)
        if len(hits) != 1:
            starts = sorted({str(c).strip() for r in rows for c in r[:3]
                             if c is not None and re.match(r"^\s*\d", str(c))})[:15]
            raise ValueError(f"row label '{label}' found {len(hits)} times in sheet "
                             f"{SHEET_POSITION} (need exactly 1). Labels starting with a digit: {starts}")
        positions[name] = hits[0]
        info["row_index"][name] = hits[0][0] + 1          # 1-based, as in Excel
        info["label_col"][name] = hits[0][1] + 1
    top = min(p[0] for p in positions.values())
    cols, hdr = period_columns(rows, top, log)
    info["header_rows"] = [i + 1 for i in hdr]
    q_cols = {j: p for j, p in cols.items() if p[0] == "Q"}
    a_cols = [j for j, p in cols.items() if p[0] == "A"]
    info["n_quarter_cols"], info["n_annual_cols_skipped"] = len(q_cols), len(a_cols)
    if not q_cols:
        sample = [[str(c) for c in rows[i][:8]] for i in range(min(top, 6))]
        raise ValueError("no quarterly column headers found above the BOP rows. "
                         f"First header cells (text only): {sample}")
    out = {}
    for name, (i, _) in positions.items():
        series: dict = {}
        for j, (_, y, q) in sorted(q_cols.items()):
            if (y, q) in series:
                raise ValueError(f"{name}: quarter {y}Q{q} appears in two columns")
            v = to_float(rows[i][j]) if j < len(rows[i]) else None
            series[(y, q)] = v
        out[name] = series
    return out, info


def check_coverage(series: dict, log: RunLog) -> bool:
    ok = True
    y0, q0 = SAMPLE_FIRST
    y1, q1 = SAMPLE_LAST
    need = [(y, q) for y in range(y0, y1 + 1) for q in range(1, 5)
            if (y, q) >= (y0, q0) and (y, q) <= (y1, q1)]
    for name, s in series.items():
        miss = [f"{y}Q{q}" for (y, q) in need if s.get((y, q)) is None]
        qs = sorted(k for k, v in s.items() if v is not None)
        if miss:
            ok = False
            log.error(f"CHECK FAIL  {name}: {len(miss)} quarter(s) of the sample missing: {miss[:6]}")
        else:
            log.info(f"CHECK PASS  {name}: all {len(need)} sample quarters present "
                     f"(file covers {qs[0][0]}Q{qs[0][1]}..{qs[-1][0]}Q{qs[-1][1]})")
        gaps = [f"{a[0]}Q{a[1]}" for a, b in zip(qs, qs[1:])
                if (b[0] * 4 + b[1]) - (a[0] * 4 + a[1]) != 1]
        if gaps:
            log.warn(f"{name}: non-consecutive quarters after {gaps[:5]}")
    return ok


# --------------------------------------------------------------------------------------
# Quarterly -> monthly conversions (decision W2-04)
# --------------------------------------------------------------------------------------
def monthly_candidates(series: dict) -> list[dict]:
    """Six candidate series for Jan of SAMPLE_FIRST year .. Dec of SAMPLE_LAST year."""
    rows = []
    for y in range(SAMPLE_FIRST[0], SAMPLE_LAST[0] + 1):
        for m in range(1, 13):
            q = (m - 1) // 3 + 1
            rec = {"month": f"{y}-{m:02d}", "quarter": f"{y}Q{q}"}
            for name, s in series.items():
                short = name.replace("bop_", "")
                v = s.get((y, q))
                ytd = None
                if all(s.get((y, k)) is not None for k in range(1, q + 1)):
                    ytd = sum(s[(y, k)] for k in range(1, q + 1))
                rec[f"{short}_hold"] = v
                rec[f"{short}_div3"] = None if v is None else v / 3.0
                rec[f"{short}_ytd"] = ytd
            rows.append(rec)
    return rows


def fmt(v) -> str:
    return "" if v is None else repr(float(v))


def write_outputs(series: dict, monthly: list[dict]) -> None:
    QUARTERLY_CSV.parent.mkdir(parents=True, exist_ok=True)
    quarters = sorted(set().union(*[set(s) for s in series.values()]))
    names = [n for n, _, _ in SERIES]
    with open(QUARTERLY_CSV, "w", encoding="utf-8", newline="") as f:
        w = csv.writer(f, lineterminator="\n")
        w.writerow(["quarter", "year", "q"] + names)
        for (y, q) in quarters:
            w.writerow([f"{y}Q{q}", y, q] + [fmt(series[n].get((y, q))) for n in names])
    cols = ["month", "quarter"] + [f"{n.replace('bop_', '')}_{c}" for n in names for c in CONVERSIONS]
    with open(MONTHLY_CSV, "w", encoding="utf-8", newline="") as f:
        w = csv.writer(f, lineterminator="\n")
        w.writerow(cols)
        for r in monthly:
            w.writerow([r["month"], r["quarter"]] + [fmt(r[c]) for c in cols[2:]])


# --------------------------------------------------------------------------------------
# Ledger
# --------------------------------------------------------------------------------------
def write_ledger(raw: Path, sha: str, file_url: str, page_url: str, link_title: str,
                 release_date: str, sheet_name: str, unit: str, series: dict, info: dict,
                 log: RunLog) -> None:
    _, rows = read_csv_rows(LEDGER_CSV)
    for name, label, gloss in SERIES:
        key = f"sheet{SHEET_POSITION}:{label}"
        if any(r.get("series_key_or_code") == key and f"sha256={sha}" in r.get("notes", "")
               for r in rows):
            log.info(f"Ledger already has {key} for this file (same sha256); not added again")
            continue
        qs = sorted(k for k, v in series[name].items() if v is not None)
        notes = "; ".join([
            f"file={rel(raw)}", f"sheet_position={SHEET_POSITION}", f"sheet_name={sheet_name}",
            f"row_label={label}", f"row_excel={info['row_index'][name]}",
            f"label_col_excel={info['label_col'][name]}",
            f"header_rows_excel={','.join(map(str, info['header_rows']))}",
            "orientation=periods_in_columns", f"annual_cols_skipped={info['n_annual_cols_skipped']}",
            f"release_page={page_url or 'not recorded (file saved by hand; add it to this note)'}",
            f"release_date={release_date or 'not found'}",
            f"interim_file={rel(QUARTERLY_CSV)}; interim_column={name}", f"sha256={sha}"])
        append_csv_row(LEDGER_CSV, LEDGER_FIELDS, {
            "series_name": f"BOP {label} ({gloss}), BPM6, quarterly",
            "provider": "State Administration of Foreign Exchange (SAFE)",
            "original_file_name": (link_title or Path(urlparse(file_url).path).name)
                                  + ("" if (link_title or "").lower().endswith((".xlsx", ".xls"))
                                     else Path(urlparse(file_url).path).suffix),
            "download_date": dt.date.today().isoformat(),
            "source_url_or_access_note": file_url or "downloaded by hand from the release page",
            "series_key_or_code": key,
            "frequency": "quarterly",
            "unit": unit or "not found in sheet",
            "currency": "USD" if "美元" in unit else "see unit",
            "seasonal_adjustment_status": "as published (no adjustment stated in the file)",
            "coverage_start": f"{qs[0][0]}-Q{qs[0][1]}" if qs else "",
            "coverage_end": f"{qs[-1][0]}-Q{qs[-1][1]}" if qs else "",
            "acquisition_method": "file_download",
            "layer": "rep",
            "license_or_access_restriction": "Public official statistics (SAFE website); "
                                             "reproduction with attribution",
            "transformation_script": SCRIPT_REL,
            "notes": notes}, log)
        log.info(f"Ledger row written: {key}")


# --------------------------------------------------------------------------------------
# Main
# --------------------------------------------------------------------------------------
def acquire(args, log: RunLog) -> tuple[Path, str, str, str, str]:
    """Returns (raw path, file URL, release page URL, link title, release date)."""
    if args.file:
        p = Path(args.file)
        p = p if p.is_absolute() else (ROOT / p)
        if not p.exists():
            raise FileNotFoundError(f"{p} not found")
        if RAW_DIR.resolve() not in p.resolve().parents:
            raise ValueError(f"{rel(p)} must be saved in data/raw/safe/ first (untouched)")
        log.info(f"Using a file already saved by hand: {rel(p)}")
        return p, "", "", p.name, ""
    import requests
    with requests.Session() as s:
        list_html = http_get(s, LIST_URL, log).decode("utf-8", errors="replace")
        page_url, seen = find_release_link(list_html, LIST_URL)
        if not page_url:
            raise RuntimeError(f"release '{RELEASE_TITLE}' not found on {LIST_URL}; "
                               f"BPM6-like links seen: {seen}")
        log.info(f"Release page: {page_url}")
        page_html = http_get(s, page_url, log).decode("utf-8", errors="replace")
        file_url, title, date = find_excel_link(page_html, page_url)
        if not file_url:
            raise RuntimeError(f"no .xlsx/.xls link on {page_url}")
        log.info(f"Excel link: {file_url} (title '{title}', release date {date or 'not found'})")
        content = http_get(s, file_url, log)
    if not content.startswith(b"PK"):          # .xlsx files are ZIP archives
        raise RuntimeError(f"{file_url} did not return an Excel file (first bytes "
                           f"{content[:15]!r}); the site may be blocking scripted downloads. "
                           "Use the --file route (README_week2, Troubleshooting)")
    raw, _ = save_raw(content, file_url, date, log)
    return raw, file_url, page_url, title, date


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description="Collect SAFE BPM6 BOP (Study A, Week 2)")
    ap.add_argument("--file", help="process a file already saved in data/raw/safe/")
    args = ap.parse_args(argv)
    log = RunLog("collect_bop")
    try:
        log.info(f"Python {platform.python_version()} on {platform.system()} {platform.release()}")
        raw, file_url, page_url, title, date = acquire(args, log)
        sha = sha256_file(raw)
        log.info(f"Raw file {rel(raw)} sha256={sha}")
        sheet_name, rows, _ = read_sheet(raw, log)
        log.info(f"Reading sheet {SHEET_POSITION} '{sheet_name}': {len(rows)} rows")
        unit = find_unit(rows)
        if not unit:
            log.warn("no unit line (单位) found in the first 30 rows of the sheet")
        elif UNIT_EXPECTED not in norm(unit):
            log.warn(f"unit line is '{unit}', not {UNIT_EXPECTED} (the BOP unit of the original study); "
                     "check that sheet 4 is the USD quarterly table")
        else:
            log.info(f"CHECK PASS  unit line: {unit}")
        series, info = extract_series(rows, log)
        log.info(f"Header rows (Excel numbering) {info['header_rows']}; "
                 f"{info['n_quarter_cols']} quarterly columns, "
                 f"{info['n_annual_cols_skipped']} annual columns skipped")
        for n, _, _ in SERIES:
            log.info(f"  {n}: row {info['row_index'][n]} (Excel), "
                     f"{sum(v is not None for v in series[n].values())} numeric quarters")
        if not check_coverage(series, log):
            raise ValueError("sample quarters missing; outputs not written")
        monthly = monthly_candidates(series)
        write_outputs(series, monthly)
        log.info(f"Wrote {rel(QUARTERLY_CSV)} and {rel(MONTHLY_CSV)} ({len(monthly)} months, "
                 f"{len(SERIES) * len(CONVERSIONS)} candidates)")
        log_transformation("bop_parse_quarter_headers", raw, sha, QUARTERLY_CSV,
                           "Sheet 4 column headers parsed to quarters; annual columns skipped",
                           info["n_quarter_cols"], "W2-04", log)
        for conv, desc in [("hold", "quarterly value held in each month of the quarter"),
                           ("div3", "quarterly value divided by 3 in each month of the quarter"),
                           ("ytd", "cumulative year-to-date value, held in each month of the quarter")]:
            log_transformation(f"bop_monthly_{conv}", raw, sha, MONTHLY_CSV, desc,
                               len(monthly) * len(SERIES), "W2-04", log)
        write_ledger(raw, sha, file_url, page_url, title, date, sheet_name, unit, series, info, log)
        log.info("Note: the current-account row was checked to be the same under BPM5 and "
                 "BPM6 for the sample (decision W2-12)")
    except Exception as exc:
        log.error(f"{type(exc).__name__}: {exc}")
        for ln in traceback.format_exc().splitlines()[-6:]:
            log.info(f"  {ln}")
    status = log.finish()
    print(f"Run log: {log.path}")
    return 1 if status == "FAIL" else 0


if __name__ == "__main__":
    sys.exit(main())
