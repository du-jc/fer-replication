"""
code/python/collect_cpi.py -- Week 4, Study A (Replication Plan s.5, "Deflator is not a price level")

Collects a fixed-base consumer price index for China from two public sources:

  IMF data portal  SDMX 3.0, dataflow IMF.STA/CPI, key CHN.CPI._T.IX.M      layer rep   (W4-05)
                   (Blueprint B.2; no key needed so far; an optional IMF_API_KEY in .env
                   is sent as a subscription-key header if present)
  FRED             series CHNCPIALLMINMEI (OECD MEI, via the FRED API)      layer check (W4-05)
                   (Blueprint B.2; needs a free FRED_API_KEY in .env)
  NBS              居民消费价格指数 (上年同月=100), the published year-on-year CPI
                   (this month vs the same month a year earlier; NOT 上年同期=100, which is
                   the cumulative January-to-month average),
                   a CSV exported by hand from data.stats.gov.cn (月度数据) and saved
                   untouched in data/raw/nbs/                                  layer check (W4-14)
                   Optional: used if a file is there. It has the same definition as the
                   original pi, so 08 compares the original pi with it directly.

For each source the script:
  1. downloads the series and saves the response untouched in data/raw/imf/ or data/raw/fred/
     (a new file only if the content changed; an existing file is never overwritten);
  2. parses the monthly observations and checks that every month 1999-01..2011-12 is present
     (1999 is needed for the year-on-year check against the original pi; 2000 is the base year);
  3. writes one ledger row (acquisition_method = api) with reading instructions in `notes`;
  4. writes data/interim/cpi_monthly.csv (month, cpi_imf, cpi_fred) and logs each step in
     data/metadata/transformation_log.csv.
The IMF series is required (ERROR if it fails); the FRED series is a cross-check (WARN if it
cannot be collected, e.g. no key). Python only collects: the rebasing, the deflation and the
comparisons are done in R (08_corrections.R).

Rules (Blueprint Part 0): no data value is typed in this file; API keys are read from .env and
never written to a log, the ledger or a file name; every run writes outputs/logs/collect_cpi_<ts>.log.

Usage (project root, .venv active):
  python code/python/collect_cpi.py                       download both
  python code/python/collect_cpi.py --no-fred             IMF only
  python code/python/collect_cpi.py --check-key           only check FRED_API_KEY's format
  python code/python/collect_cpi.py --imf-file data/raw/imf/<name>.csv
  python code/python/collect_cpi.py --fred-file data/raw/fred/<name>.json
  python code/python/collect_cpi.py --nbs-file data/raw/nbs/<name>.csv   (default: newest file there)
                                   process files already saved there (e.g. saved from a browser)
"""
from __future__ import annotations

import argparse
import csv
import datetime as dt
import hashlib
import io
import json
import os
import platform
import re
import sys
import time
import traceback
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
RAW_IMF = ROOT / "data" / "raw" / "imf"
RAW_FRED = ROOT / "data" / "raw" / "fred"
RAW_NBS = ROOT / "data" / "raw" / "nbs"
CPI_CSV = ROOT / "data" / "interim" / "cpi_monthly.csv"
LEDGER_CSV = ROOT / "data" / "metadata" / "source_ledger.csv"
TRANSFORM_CSV = ROOT / "data" / "metadata" / "transformation_log.csv"
LOG_DIR = ROOT / "outputs" / "logs"
ENV_FILE = ROOT / ".env"
SCRIPT_REL = "code/python/collect_cpi.py"

# ---- Specification (Blueprint B.2; Week 4 chat, W4-05). Codes and URLs, not data.
IMF_BASE = "https://api.imf.org/external/sdmx/3.0"
IMF_FLOW = ("IMF.STA", "CPI")                    # agency, dataflow; version "~" = latest
IMF_KEY = "CHN.CPI._T.IX.M"                      # China, CPI, all items, index, monthly
IMF_ACCEPT = ["application/vnd.sdmx.data+csv;version=2.0.0", "text/csv"]
FRED_BASE = "https://api.stlouisfed.org/fred"
FRED_SERIES = "CHNCPIALLMINMEI"
FRED_KEY_LENGTH = 32                             # FRED docs: 32-character lower-case alphanumeric
FRED_DEMO_KEY = "abcdefghijklmnopqrstuvwxyz123456"   # the placeholder in FRED's documentation
NEED_FIRST, NEED_LAST = "1999-01", "2011-12"
# NBS export (data.stats.gov.cn, 月度数据): the row label of the series, matched after removing
# all whitespace; the words of the header row and of the source line. Labels, not data.
NBS_LABEL = "居民消费价格指数(上年同月=100)"     # 上年同期=100 is the cumulative series (W4-14)
NBS_HEADER_WORD = "指标"
NBS_SOURCE_WORD = "国家统计局"
HTTP_HEADERS = {"User-Agent": "Mozilla/5.0 (academic replication; fer-replication)"}
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
    global ROOT, RAW_IMF, RAW_FRED, RAW_NBS, CPI_CSV, LEDGER_CSV, TRANSFORM_CSV, LOG_DIR, ENV_FILE
    ROOT = Path(root)
    RAW_IMF = ROOT / "data" / "raw" / "imf"
    RAW_FRED = ROOT / "data" / "raw" / "fred"
    RAW_NBS = ROOT / "data" / "raw" / "nbs"
    CPI_CSV = ROOT / "data" / "interim" / "cpi_monthly.csv"
    LEDGER_CSV = ROOT / "data" / "metadata" / "source_ledger.csv"
    TRANSFORM_CSV = ROOT / "data" / "metadata" / "transformation_log.csv"
    LOG_DIR = ROOT / "outputs" / "logs"
    ENV_FILE = ROOT / ".env"


# --------------------------------------------------------------------------------------
# Run log (same format as the other scripts)
# --------------------------------------------------------------------------------------
class RunLog:
    def __init__(self, script_name: str):
        LOG_DIR.mkdir(parents=True, exist_ok=True)
        self.path = LOG_DIR / f"{script_name}_{dt.datetime.now():%Y%m%d-%H%M%S}.log"
        self._fh = open(self.path, "w", encoding="utf-8", newline="\n")
        self.warnings = 0
        self.errors = 0
        self.secrets: list[str] = []

    def _write(self, level: str, msg: str) -> None:
        for s in self.secrets:                       # never print a key
            if s:
                msg = msg.replace(s, "REDACTED")
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
# Helpers (same conventions as collect_bop.py)
# --------------------------------------------------------------------------------------
def sha256_bytes(b: bytes) -> str:
    return hashlib.sha256(b).hexdigest()


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


def read_env() -> dict:
    """Keys from .env (python-dotenv if installed, else a plain KEY=VALUE reader)."""
    out = {}
    if ENV_FILE.exists():
        try:
            from dotenv import dotenv_values
            out = {k: v for k, v in dotenv_values(ENV_FILE).items() if v}
        except ImportError:
            for ln in ENV_FILE.read_text(encoding="utf-8").splitlines():
                m = re.match(r"^\s*([A-Za-z_][A-Za-z0-9_]*)\s*=\s*(.*?)\s*$", ln)
                if m and not ln.lstrip().startswith("#"):
                    out[m.group(1)] = m.group(2).strip("'\"")
    for k in ("FRED_API_KEY", "IMF_API_KEY"):
        if os.environ.get(k):
            out[k] = os.environ[k]
    return out


def http_get(session, url: str, params: dict, headers: dict, log: RunLog):
    last = None
    for attempt in range(1, RETRIES + 1):
        try:
            r = session.get(url, params=params, headers={**HTTP_HEADERS, **headers}, timeout=60)
            log.info(f"GET {url} -> HTTP {r.status_code}, {len(r.content)} bytes")
            if r.status_code in (400, 401, 403, 404):
                return r                               # not worth retrying; caller decides
            r.raise_for_status()
            return r
        except Exception as exc:
            last = exc
            log.warn(f"GET {url} attempt {attempt}/{RETRIES} failed: {exc}")
            time.sleep(2 * attempt)
    raise RuntimeError(f"could not download {url}: {last}")


def save_raw(content: bytes, folder: Path, stem: str, ext: str, log: RunLog) -> Path:
    """Save untouched; if a file with the same content exists, reuse it (no new vintage)."""
    folder.mkdir(parents=True, exist_ok=True)
    sha = sha256_bytes(content)
    for f in sorted(folder.glob("*")):
        if f.is_file() and sha256_bytes(f.read_bytes()) == sha:
            log.info(f"Same content already saved as {rel(f)}; no new vintage")
            return f
    dest = folder / f"{stem}_dl{dt.date.today():%Y%m%d}_{sha[:8]}{ext}"
    with open(dest, "xb") as f:                  # 'x' refuses to overwrite
        f.write(content)
    log.info(f"Saved new vintage {rel(dest)} (sha256 {sha[:16]}...)")
    return dest


def inside(p: Path, folder: Path) -> Path:
    p = p if p.is_absolute() else ROOT / p
    if not p.exists():
        raise FileNotFoundError(f"{p} not found")
    if folder.resolve() not in p.resolve().parents:
        raise ValueError(f"{rel(p)} must be saved in {rel(folder)}/ first (untouched)")
    return p


# --------------------------------------------------------------------------------------
# Parsing
# --------------------------------------------------------------------------------------
_MONTH_RE = re.compile(r"^(\d{4})\s*-?\s*M?\s*(\d{1,2})(?:-\d{1,2})?$", re.IGNORECASE)


def parse_month(text: str) -> str | None:
    """'2000-01', '2000-M01', '2000M1', '2000-01-01' -> '2000-01'; anything else -> None."""
    m = _MONTH_RE.match((text or "").strip())
    if not m or not 1 <= int(m.group(2)) <= 12:
        return None
    return f"{m.group(1)}-{int(m.group(2)):02d}"


def parse_imf_csv(content: bytes) -> tuple[dict, dict]:
    """SDMX-CSV -> ({month: value}, info). One series expected (the key is fully specified)."""
    text = content.decode("utf-8-sig", errors="replace")
    rd = csv.DictReader(io.StringIO(text))
    cols = {c.upper(): c for c in (rd.fieldnames or [])}
    if "TIME_PERIOD" not in cols or "OBS_VALUE" not in cols:
        raise ValueError(f"IMF response has no TIME_PERIOD/OBS_VALUE columns; columns: {rd.fieldnames}; "
                         f"first characters: {text[:120]!r}")
    out: dict = {}
    other: dict = {}
    bad = 0
    for r in rd:
        m = parse_month(r[cols["TIME_PERIOD"]])
        v = (r[cols["OBS_VALUE"]] or "").strip()
        if m is None:
            bad += 1
            continue
        if m in out:
            raise ValueError(f"IMF response has month {m} twice (more than one series?)")
        out[m] = float(v) if v not in ("", "NaN", "NA") else None
        for k, c in cols.items():
            if k not in ("TIME_PERIOD", "OBS_VALUE") and (r[c] or "").strip():
                other.setdefault(k, set()).add(r[c].strip())
    info = {"columns": list(rd.fieldnames or []), "unparsed_periods": bad,
            "constant_columns": {k: sorted(v)[0] for k, v in other.items() if len(v) == 1}}
    return out, info


def parse_fred_json(content: bytes) -> dict:
    """FRED observations JSON -> {month: value}; '.' (missing) -> None."""
    d = json.loads(content.decode("utf-8"))
    if "observations" not in d:
        raise ValueError(f"FRED response has no 'observations': {str(d)[:200]}")
    out = {}
    for o in d["observations"]:
        m = parse_month(o.get("date", "")[:7])
        if m is None:
            continue
        v = o.get("value", ".")
        out[m] = None if v in (".", "", None) else float(v)
    return out


def months_between(a: str, b: str) -> list[str]:
    y, m = int(a[:4]), int(a[5:])
    out = []
    while f"{y}-{m:02d}" <= b:
        out.append(f"{y}-{m:02d}")
        y, m = (y + 1, 1) if m == 12 else (y, m + 1)
    return out


def check_coverage(series: dict, name: str, log: RunLog, required: bool) -> bool:
    need = months_between(NEED_FIRST, NEED_LAST)
    miss = [m for m in need if series.get(m) is None]
    have = sorted(m for m, v in series.items() if v is not None)
    if miss:
        (log.error if required else log.warn)(
            f"CHECK FAIL  {name}: {len(miss)} month(s) of {NEED_FIRST}..{NEED_LAST} missing: {miss[:6]}")
        return False
    log.info(f"CHECK PASS  {name}: every month {NEED_FIRST}..{NEED_LAST} present "
             f"(file covers {have[0]}..{have[-1]}, {len(have)} months)")
    nonpos = [m for m in have if series[m] <= 0]
    if nonpos:
        log.warn(f"{name}: {len(nonpos)} value(s) <= 0 (an index should be positive), first {nonpos[0]}")
    return True


# --------------------------------------------------------------------------------------
# Sources
# --------------------------------------------------------------------------------------
_NBS_MONTH_RE = re.compile(r"^(\d{4})年(\d{1,2})月$")


def nbs_norm(x: str) -> str:
    """Compare NBS labels safely: full-width -> half-width, every whitespace (incl. tabs) removed."""
    import unicodedata
    return re.sub(r"\s+", "", unicodedata.normalize("NFKC", x or ""))


def parse_nbs_csv(content: bytes) -> tuple[dict, dict]:
    """NBS 月度数据 export -> ({month: value}, info). Months run in columns (newest first) in the
    row whose first cell is 指标; the series is the one row labelled NBS_LABEL."""
    text = content.decode("utf-8-sig", errors="strict")
    rows = list(csv.reader(io.StringIO(text)))
    hdr = [i for i, r in enumerate(rows) if r and nbs_norm(r[0]) == NBS_HEADER_WORD]
    if len(hdr) != 1:
        raise ValueError(f"NBS file: {len(hdr)} header rows starting with {NBS_HEADER_WORD} (need 1)")
    head = rows[hdr[0]]
    cols = {}
    for j, c in enumerate(head[1:], start=1):
        m = _NBS_MONTH_RE.match(nbs_norm(c))
        if m and 1 <= int(m.group(2)) <= 12:
            cols[j] = f"{m.group(1)}-{int(m.group(2)):02d}"
        elif nbs_norm(c):
            raise ValueError(f"NBS file: header cell {c!r} is not a month like 2000年1月")
    if len(set(cols.values())) != len(cols):
        raise ValueError("NBS file: a month appears twice in the header")
    labels = [nbs_norm(r[0]) for r in rows[hdr[0] + 1:] if r]
    hits = [r for r in rows[hdr[0] + 1:] if r and nbs_norm(r[0]) == nbs_norm(NBS_LABEL)]
    if len(hits) != 1:
        raise ValueError(f"NBS file: row {NBS_LABEL} found {len(hits)} times (need 1); row labels: {labels[:8]}")
    out = {}
    for j, month in cols.items():
        v = nbs_norm(hits[0][j]) if j < len(hits[0]) else ""
        out[month] = float(v) if v not in ("", "--", "...") else None
    footer = [" ".join(r).strip() for r in rows[hdr[0] + 1:] if r and nbs_norm(r[0]) != nbs_norm(NBS_LABEL)]
    info = {"label": hits[0][0].strip(), "n_months": len(cols), "footer": footer,
            "title": " | ".join(r[0].strip() for r in rows[:hdr[0]] if r),
            "source_ok": any(NBS_SOURCE_WORD in f for f in footer)}
    return out, info


def find_nbs(args, log: RunLog) -> Path | None:
    """--nbs-file, else the newest .csv in data/raw/nbs/, else None (the NBS check is optional)."""
    if args.nbs_file:
        return inside(Path(args.nbs_file), RAW_NBS)
    files = sorted((p for p in RAW_NBS.glob("*.csv") if p.is_file()), key=lambda p: p.stat().st_mtime)
    if not files:
        log.info(f"NBS: no file in {rel(RAW_NBS)}/; the NBS check is skipped (optional; README_week4 Step 4)")
        return None
    if len(files) > 1:
        log.info(f"NBS: {len(files)} files in {rel(RAW_NBS)}/; using the newest, {files[-1].name} "
                 "(choose another with --nbs-file)")
    return files[-1]


def imf_url() -> str:
    return f"{IMF_BASE}/data/dataflow/{IMF_FLOW[0]}/{IMF_FLOW[1]}/~/{IMF_KEY}"


def get_imf(args, env: dict, log: RunLog) -> tuple[Path, str]:
    if args.imf_file:
        p = inside(Path(args.imf_file), RAW_IMF)
        log.info(f"IMF: using a file already saved: {rel(p)}")
        return p, "file saved by hand (source URL not recorded; add it to the ledger note)"
    import requests
    headers_extra = {"Ocp-Apim-Subscription-Key": env["IMF_API_KEY"]} if env.get("IMF_API_KEY") else {}
    if headers_extra:
        log.info("IMF: IMF_API_KEY found in .env; sent as a subscription-key header")
    with requests.Session() as s:
        last = ""
        for acc in IMF_ACCEPT:
            r = http_get(s, imf_url(), {}, {"Accept": acc, **headers_extra}, log)
            head = r.content[:400].decode("utf-8", errors="replace").upper()
            if r.status_code == 200 and "OBS_VALUE" in head and "TIME_PERIOD" in head:
                log.info(f"IMF: CSV received with Accept: {acc}")
                return save_raw(r.content, RAW_IMF, f"IMF_CPI_{IMF_KEY}", ".csv", log), imf_url()
            last = f"HTTP {r.status_code}, first characters {r.content[:120]!r}"
            log.info(f"IMF: Accept {acc} gave no SDMX-CSV ({last})")
    raise RuntimeError(f"IMF did not return SDMX-CSV for {imf_url()} ({last}). If the portal now needs a key, "
                       "add IMF_API_KEY to .env; or save the CSV from data.imf.org into data/raw/imf/ and "
                       "rerun with --imf-file (README_week4, Troubleshooting)")


def fred_key_problem(key: str) -> str:
    """'' if the key has the documented FRED format (32 lower-case letters/digits), else a
    description of the problem. The description never contains the key itself."""
    if not key:
        return "FRED_API_KEY is missing or empty in .env"
    if key != key.strip():
        return "FRED_API_KEY has spaces or a line break around it; remove them"
    if key[:1] in "'\"" or key[-1:] in "'\"":
        return "FRED_API_KEY is wrapped in quotes; write it without quotes"
    if len(key) != FRED_KEY_LENGTH:
        return (f"FRED_API_KEY has {len(key)} characters, expected {FRED_KEY_LENGTH} "
                "(copied only part of it, or with extra characters?)")
    if key == FRED_DEMO_KEY:
        return "FRED_API_KEY is the demonstration key from FRED's documentation; use your own"
    if not re.fullmatch(r"[a-z0-9]+", key):
        kinds = []
        if re.search(r"[A-Z]", key):
            kinds.append("upper-case letters")
        if re.search(r"[^A-Za-z0-9]", key):
            kinds.append("characters other than letters and digits")
        return f"FRED_API_KEY contains {' and '.join(kinds)}; a FRED key is lower-case letters and digits only"
    return ""


def get_fred(args, env: dict, log: RunLog) -> tuple[Path | None, str, dict]:
    if args.fred_file:
        p = inside(Path(args.fred_file), RAW_FRED)
        log.info(f"FRED: using a file already saved: {rel(p)}")
        return p, "file saved by hand", {}
    key = env.get("FRED_API_KEY", "")
    if not key:
        log.warn("FRED: no FRED_API_KEY in .env; the FRED cross-check is skipped (see README_week4 Step 2)")
        return None, "", {}
    log.secrets.append(key)
    problem = fred_key_problem(key)
    if problem:
        log.warn(f"FRED: {problem}; the FRED cross-check is skipped (see README_week4 Step 2)")
        return None, "", {}
    log.info(f"CHECK PASS  FRED_API_KEY format: {FRED_KEY_LENGTH} lower-case letters/digits")
    import requests
    with requests.Session() as s:
        meta_r = http_get(s, f"{FRED_BASE}/series", {"series_id": FRED_SERIES, "api_key": key,
                                                      "file_type": "json"}, {}, log)
        meta = {}
        if meta_r.status_code == 200:
            ss = meta_r.json().get("seriess", [])
            meta = ss[0] if ss else {}
        else:
            log.warn(f"FRED series metadata: HTTP {meta_r.status_code}")
        r = http_get(s, f"{FRED_BASE}/series/observations",
                     {"series_id": FRED_SERIES, "api_key": key, "file_type": "json"}, {}, log)
        if r.status_code != 200:
            raise RuntimeError(f"FRED observations: HTTP {r.status_code}: {r.text[:200]}")
    url = f"{FRED_BASE}/series/observations?series_id={FRED_SERIES}&file_type=json (api_key redacted)"
    return save_raw(r.content, RAW_FRED, f"FRED_{FRED_SERIES}", ".json", log), url, meta


# --------------------------------------------------------------------------------------
# Ledger and interim file
# --------------------------------------------------------------------------------------
def write_ledger(row: dict, key: str, sha: str, log: RunLog) -> None:
    _, rows = read_csv_rows(LEDGER_CSV)
    if any(r.get("series_key_or_code") == key and f"sha256={sha}" in r.get("notes", "") for r in rows):
        log.info(f"Ledger already has {key} for this file (same sha256); not added again")
        return
    append_csv_row(LEDGER_CSV, LEDGER_FIELDS, row)
    log.info(f"Ledger row written: {key} (layer {row['layer']})")


def span(series: dict) -> tuple[str, str]:
    have = sorted(m for m, v in series.items() if v is not None)
    return (have[0], have[-1]) if have else ("", "")


def write_interim(imf: dict, fred: dict, nbs: dict | None = None) -> int:
    nbs = nbs or {}
    months = sorted(set(imf) | set(fred) | set(nbs))
    CPI_CSV.parent.mkdir(parents=True, exist_ok=True)
    fmt = lambda v: "" if v is None else repr(float(v))
    with open(CPI_CSV, "w", encoding="utf-8", newline="") as f:
        w = csv.writer(f, lineterminator="\n")
        w.writerow(["month", "cpi_imf", "cpi_fred", "cpi_nbs_yoy"])
        for m in months:
            w.writerow([m, fmt(imf.get(m)), fmt(fred.get(m)), fmt(nbs.get(m))])
    return len(months)


def collect_nbs(args, log: RunLog) -> dict:
    """Register the hand-exported NBS file (layer check). Any failure is a WARN, never an ERROR."""
    try:
        path = find_nbs(args, log)
        if path is None:
            return {}
        content = path.read_bytes()
        sha = sha256_bytes(content)
        nbs, info = parse_nbs_csv(content)
        log.info(f"NBS {rel(path)}: row '{info['label']}', {info['n_months']} monthly columns; {info['title']}")
        if not info["source_ok"]:
            log.warn(f"NBS file: no source line naming {NBS_SOURCE_WORD}; footer: {info['footer'][:2]}")
        check_coverage(nbs, "cpi_nbs_yoy", log, required=False)
        a, b = span(nbs)
        write_ledger({
            "series_name": f"{info['label']} (CPI, same month of the previous year = 100)",
            "provider": "National Bureau of Statistics of China (NBS), data.stats.gov.cn",
            "original_file_name": path.name, "download_date": dt.date.fromtimestamp(path.stat().st_mtime).isoformat(),
            "source_url_or_access_note": "https://data.stats.gov.cn (月度数据), exported by hand as CSV and saved untouched",
            "series_key_or_code": f"nbs:{NBS_LABEL}", "frequency": "monthly",
            "unit": "index, same month of the previous year = 100", "currency": "not applicable (index)",
            "seasonal_adjustment_status": "not seasonally adjusted (as published)",
            "coverage_start": a, "coverage_end": b, "acquisition_method": "file_download", "layer": "check",
            "license_or_access_restriction": "Public official statistics (NBS); cite the National Bureau of Statistics",
            "transformation_script": SCRIPT_REL,
            "notes": "; ".join([f"file={rel(path)}", "format=nbs-csv (months in columns, newest first)",
                                f"header_row_first_cell={NBS_HEADER_WORD}", f"row_label={NBS_LABEL}",
                                "download_date_is=file modification date",
                                f"footer={' | '.join(info['footer'])[:160]}", f"interim_file={rel(CPI_CSV)}",
                                "interim_column=cpi_nbs_yoy",
                                "used_for=check of the original pi and of the IMF-implied year-on-year rate",
                                f"sha256={sha}"])}, f"nbs:{NBS_LABEL}", sha, log)
        log_transformation("cpi_nbs_parse", path, sha, CPI_CSV,
                           "NBS export: month columns (YYYY年M月, newest first) -> month, value", len(nbs), "W4-14")
        return nbs
    except Exception as exc:
        log.warn(f"NBS check not collected: {type(exc).__name__}: {exc}")
        return {}


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description="Collect China CPI: IMF (rep); FRED and NBS (check); Study A, Week 4")
    ap.add_argument("--imf-file", help="process an IMF SDMX-CSV already saved in data/raw/imf/")
    ap.add_argument("--fred-file", help="process a FRED observations JSON already saved in data/raw/fred/")
    ap.add_argument("--no-fred", action="store_true", help="skip the FRED cross-check")
    ap.add_argument("--nbs-file", help="the NBS CSV in data/raw/nbs/ (default: the newest file there)")
    ap.add_argument("--check-key", action="store_true",
                    help="only check the format of FRED_API_KEY in .env; download nothing")
    args = ap.parse_args(argv)
    log = RunLog("collect_cpi")
    try:
        log.info(f"Python {platform.python_version()} on {platform.system()} {platform.release()}")
        env = read_env()
        log.secrets += [v for k, v in env.items() if k.endswith("_KEY")]
        if args.check_key:
            log.info(f".env: {'found' if ENV_FILE.exists() else 'NOT FOUND'} at {rel(ENV_FILE)}")
            problem = fred_key_problem(env.get("FRED_API_KEY", ""))
            if problem:
                log.error(f"CHECK FAIL  {problem}")
            else:
                log.info(f"CHECK PASS  FRED_API_KEY format: {FRED_KEY_LENGTH} lower-case letters/digits")
            status = log.finish()
            print(f"Run log: {log.path}")
            return 1 if status == "FAIL" else 0
        # ---- IMF (required) ----
        raw, url = get_imf(args, env, log)
        content = raw.read_bytes()
        sha = sha256_bytes(content)
        imf, info = parse_imf_csv(content)
        log.info(f"IMF {rel(raw)}: {len(imf)} monthly rows; constant columns {info['constant_columns']}")
        if info["unparsed_periods"]:
            log.warn(f"IMF: {info['unparsed_periods']} row(s) with a non-monthly TIME_PERIOD skipped")
        if not check_coverage(imf, "cpi_imf", log, required=True):
            raise ValueError("IMF CPI incomplete for the sample; nothing written")
        a, b = span(imf)
        unit = info["constant_columns"].get("UNIT", info["constant_columns"].get("UNIT_MEASURE", ""))
        write_ledger({
            "series_name": "Consumer price index, all items, China (IMF CPI dataflow)",
            "provider": "International Monetary Fund (IMF data portal)",
            "original_file_name": raw.name, "download_date": dt.date.today().isoformat(),
            "source_url_or_access_note": url, "series_key_or_code": f"{IMF_FLOW[0]}/{IMF_FLOW[1]}/{IMF_KEY}",
            "frequency": "monthly", "unit": unit or "index (base as published; see the raw file)",
            "currency": "not applicable (index)", "seasonal_adjustment_status": "as published",
            "coverage_start": a, "coverage_end": b, "acquisition_method": "api", "layer": "rep",
            "license_or_access_restriction": "IMF data, free of charge; cite the IMF data portal (IMF terms of use)",
            "transformation_script": SCRIPT_REL,
            "notes": "; ".join([f"file={rel(raw)}", "format=sdmx-csv", "time_col=TIME_PERIOD",
                                "value_col=OBS_VALUE", f"interim_file={rel(CPI_CSV)}", "interim_column=cpi_imf",
                                "used_for=fixed-base deflator (08_corrections.R, rebased to 2000=100)",
                                f"sha256={sha}"])}, f"{IMF_FLOW[0]}/{IMF_FLOW[1]}/{IMF_KEY}", sha, log)
        # ---- FRED (cross-check) ----
        fred: dict = {}
        if not args.no_fred:
            try:
                fraw, furl, meta = get_fred(args, env, log)
                if fraw is not None:
                    fc = fraw.read_bytes()
                    fsha = sha256_bytes(fc)
                    fred = parse_fred_json(fc)
                    log.info(f"FRED {rel(fraw)}: {len(fred)} monthly rows; units '{meta.get('units', 'not requested')}', "
                             f"last updated {meta.get('last_updated', 'n/a')}")
                    check_coverage(fred, "cpi_fred", log, required=False)
                    fa, fb = span(fred)
                    write_ledger({
                        "series_name": meta.get("title", f"FRED {FRED_SERIES}"),
                        "provider": "FRED, Federal Reserve Bank of St. Louis (source: OECD Main Economic Indicators)",
                        "original_file_name": fraw.name, "download_date": dt.date.today().isoformat(),
                        "source_url_or_access_note": furl, "series_key_or_code": FRED_SERIES,
                        "frequency": meta.get("frequency", "monthly"), "unit": meta.get("units", "see raw file"),
                        "currency": "not applicable (index)",
                        "seasonal_adjustment_status": meta.get("seasonal_adjustment", "as published"),
                        "coverage_start": fa, "coverage_end": fb, "acquisition_method": "api", "layer": "check",
                        "license_or_access_restriction": "OECD data via FRED; cite OECD and FRED as FRED requests",
                        "transformation_script": SCRIPT_REL,
                        "notes": "; ".join([f"file={rel(fraw)}", "format=fred-json", "date_field=date",
                                            "value_field=value (. = missing)", f"interim_file={rel(CPI_CSV)}",
                                            "interim_column=cpi_fred", "used_for=cross-check of the IMF CPI",
                                            f"sha256={fsha}"])}, FRED_SERIES, fsha, log)
                    log_transformation("cpi_fred_parse", fraw, fsha, CPI_CSV,
                                       "FRED observations JSON -> month, value ('.' -> blank)", len(fred), "W4-05")
            except Exception as exc:          # the check layer never blocks the rep layer
                log.warn(f"FRED cross-check not collected: {type(exc).__name__}: {exc}")
        # ---- NBS published year-on-year (optional check) ----
        nbs = collect_nbs(args, log)
        n = write_interim(imf, fred, nbs)
        log.info(f"Wrote {rel(CPI_CSV)} ({n} months; cpi_fred {'filled' if fred else 'empty'}; "
                 f"cpi_nbs_yoy {'filled' if nbs else 'empty'})")
        log_transformation("cpi_imf_parse", raw, sha, CPI_CSV,
                           "IMF SDMX-CSV -> month (TIME_PERIOD), value (OBS_VALUE)", len(imf), "W4-05")
        log.info("Next: Rscript code/R/08_corrections.R (rebasing and deflation are done in R)")
    except Exception as exc:
        log.error(f"{type(exc).__name__}: {exc}")
        for ln in traceback.format_exc().splitlines()[-6:]:
            log.info(f"  {ln}")
    status = log.finish()
    print(f"Run log: {log.path}")
    return 1 if status == "FAIL" else 0


if __name__ == "__main__":
    sys.exit(main())
