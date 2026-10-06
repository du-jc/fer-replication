#!/usr/bin/env python
"""
extract_original.py -- Study A, Week 1 (Blueprint A.2 step A1; Replication Plan sections 1, 2 and 4)

Reads the original document (.docx) saved untouched in data/raw/original/ and writes:

  data/interim/original_appendix.csv    Appendix I, one row per month, month labels kept as TEXT
  data/metadata/original_targets.csv    every cell of every captioned table (Tables 3.1-4.11),
                                      one row per numeric token, with flags
  config/original_label_map.csv         DRAFTED on the first run: every label cell that is empty or
                                      holds an image/equation object, with a proposed label by
                                      position. You confirm each row against the original document.
  data/metadata/source_ledger.csv     one row for the .docx (acquisition_method=document_extraction)
  data/metadata/transformation_log.csv  one row per conversion step
  outputs/logs/extract_original_<timestamp>.log

Rules this script follows (Blueprint Part 0):
  * The .docx is opened read-only and never saved.
  * No data value is typed anywhere in this file. Every value comes from the document.
  * The label map is never overwritten. New unmapped label cells are appended as new rows.

Usage (from the project root, with .venv active):
  python code/python/extract_original.py
  python code/python/extract_original.py --docx data/raw/original/<file>.docx
"""
from __future__ import annotations

import argparse
import csv
import datetime as dt
import hashlib
import platform
import re
import sys
import traceback
from dataclasses import dataclass, field
from pathlib import Path

# --------------------------------------------------------------------------------------
# Paths and specifications (from the Blueprint and the Replication Plan, not from data)
# --------------------------------------------------------------------------------------
ROOT = Path(__file__).resolve().parents[2]
RAW_ORIGINAL_DIR = ROOT / "data" / "raw" / "original"
APPENDIX_CSV = ROOT / "data" / "interim" / "original_appendix.csv"      # decision W1-01
TARGETS_CSV = ROOT / "data" / "metadata" / "original_targets.csv"
LEDGER_CSV = ROOT / "data" / "metadata" / "source_ledger.csv"
TRANSFORM_CSV = ROOT / "data" / "metadata" / "transformation_log.csv"
LABEL_MAP_CSV = ROOT / "config" / "original_label_map.csv"
LOG_DIR = ROOT / "outputs" / "logs"
SCRIPT_REL = "code/python/extract_original.py"



def configure_paths(root: Path) -> None:
    """Point every path at another project root (used by the tests in tests/)."""
    global ROOT, RAW_ORIGINAL_DIR, APPENDIX_CSV, TARGETS_CSV, LEDGER_CSV, TRANSFORM_CSV
    global LABEL_MAP_CSV, LOG_DIR
    ROOT = Path(root)
    RAW_ORIGINAL_DIR = ROOT / "data" / "raw" / "original"
    APPENDIX_CSV = ROOT / "data" / "interim" / "original_appendix.csv"
    TARGETS_CSV = ROOT / "data" / "metadata" / "original_targets.csv"
    LEDGER_CSV = ROOT / "data" / "metadata" / "source_ledger.csv"
    TRANSFORM_CSV = ROOT / "data" / "metadata" / "transformation_log.csv"
    LABEL_MAP_CSV = ROOT / "config" / "original_label_map.csv"
    LOG_DIR = ROOT / "outputs" / "logs"


# Replication Plan section 2.1 / Blueprint A.2: 144 months, Jan 2000 - Dec 2011
EXPECTED_APPENDIX_ROWS = 144
EXPECTED_FIRST_MONTH = "2000-01"
EXPECTED_LAST_MONTH = "2011-12"
# Replication Plan section 1: the tables used as targets
EXPECTED_TABLES = [f"T3.{i}" for i in range(1, 10)] + [f"T4.{i}" for i in range(1, 12)]

# Access note for the ledger. Decision W1-02: the original document is not committed or redistributed.
LICENSE_NOTE = ("Unpublished original study document (co-author's); used for replication only. "
                "Not committed to git and not redistributed (decision W1-02).")

# Label sets used ONLY to propose labels for empty/image label cells (Replication Plan s.4).
# They are variable names from the original document and the Plan, not data. Every proposal is flagged
# for you to confirm. 'pi' is the ASCII name used for the original document's pi column.
_CH3 = ["FER", "GDP", "ER", "AER", "NX", "pi", "FDI", "FDB"]
_APP = ["FER", "GDP", "ER", "AER", "NX", "pi", "FDI", "FDB", "i"]
_UNIT = ["FER", "GDP", "ER", "NX", "pi", "FDI", "FDB", "i", "NDF", "BOP"]
# Tables 4.9-4.10: the image label is the double-hatted DER (Plan s.3.1: DER_hathat)
_REG = ["DER_hathat", "DNDF", "DGDP", "DNX", "DFDB", "DFDI"]  # required coefficient rows
LABEL_SETS = {"APP_I": _APP, "T3.1": _APP, "T4.1": _UNIT, "T4.2": _UNIT,
              "T4.9": _REG, "T4.10": _REG}
LABEL_SETS.update({f"T3.{i}": _CH3 for i in range(2, 10)})

LABEL_MAP_FIELDS = ["table_id", "caption", "axis", "row_idx", "col_idx", "cell_content",
                    "cell_text", "omml_text", "neighbor_before", "neighbor_after",
                    "first_cells", "proposed_label", "proposal_rule", "confirmed_label",
                    "confirmed", "confirmed_date", "note", "docx_sha256"]
TARGET_FIELDS = ["target_id", "table_id", "caption", "table_note", "row_idx", "col_idx", "token_idx",
                 "row_label", "row_label_source", "col_label", "col_label_source", "element",
                 "cell_text", "value_text", "stars", "dagger", "paren", "missing", "percent",
                 "thousands_sep", "numbers_in_text", "labels_confirmed"]
LEDGER_FIELDS = ["series_name", "provider", "original_file_name", "download_date",
                 "source_url_or_access_note", "series_key_or_code", "frequency", "unit",
                 "currency", "seasonal_adjustment_status", "coverage_start", "coverage_end",
                 "acquisition_method", "layer", "license_or_access_restriction",
                 "transformation_script", "notes"]
TRANSFORM_FIELDS = ["timestamp_utc", "script", "step", "input_file", "input_sha256",
                    "output_file", "description", "n_affected", "decision_ref"]

W_NS = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
M_NS = "http://schemas.openxmlformats.org/officeDocument/2006/math"
GRAPHIC_TAGS = {"drawing", "pict", "object"}          # images, VML, OLE (e.g. MathType)


# --------------------------------------------------------------------------------------
# Run log (every script writes one to outputs/logs/)
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
# Text helpers
# --------------------------------------------------------------------------------------
_MINUS = {"\u2212": "-", "\u2013": "-", "\uff0d": "-"}   # minus sign, en dash, full-width
_NUM_RE = re.compile(r"[-+]?(?:\d+\.?\d*|\.\d+)(?:[eE][-+]?\d+)?")
_THOUSANDS_RE = re.compile(r"[-+]?\d{1,3}(?:,\d{3})+(?:\.\d+)?")
_CAPTION_RE = re.compile(r"^\s*Table\s*(\d+)\s*[.\uff0e]\s*(\d+)")
_APP_I_RE = re.compile(r"^\s*App(?:endix|\.)?\s*I(?![IVX])", re.IGNORECASE)
_SECTION_END_RE = re.compile(
    r"^\s*(App(?:endix|\.)?\s*(II|III|IV|V)\b|CHAPTER\b|REFERENCES?\b|BIBLIOGRAPHY\b|ACKNOWLEDG)",
    re.IGNORECASE)
_MONTH_RE = re.compile(r"^(\d{4})\s*[.\uff0e]\s*(\d{1,2})$")
_ELLIPSIS = {"\u2026", "...", "\u22ef"}


def clean_text(s: str) -> str:
    """Normalise whitespace only (keeps the characters the original document printed)."""
    s = s.replace("\u00a0", " ").replace("\u3000", " ")
    return re.sub(r"[ \t]+", " ", s).strip()


def normalize_label(s: str) -> str:
    """Label used for matching and column names: no spaces, pi symbol -> 'pi'."""
    s = clean_text(s).replace("\u03c0", "pi").replace("\U0001d70b", "pi")
    return re.sub(r"\s+", "", s)


@dataclass
class Parsed:
    element: str            # value | paren | text | empty
    value_text: str = ""    # normalised numeric text (e.g. '-0.0264'), '' if not numeric
    stars: int = 0
    dagger: int = 0
    paren: int = 0
    percent: int = 0
    thousands_sep: int = 0
    minus_normalised: int = 0
    missing: int = 0
    numbers_in_text: str = ""


def parse_numeric(line: str) -> Parsed:
    """Parse ONE line of a cell, e.g. '0.4041 **', '(0.2196 )', '-.0007', '1.6e+06', '0.6647+'.
    A line counts as numeric only if nothing but a number, stars, a dagger, % and brackets
    is left. Anything else stays text (its numbers are listed in numbers_in_text)."""
    raw = clean_text(line)
    if raw == "":
        return Parsed("empty", missing=1)
    if raw in _ELLIPSIS:
        return Parsed("text", missing=1)
    p = Parsed("text")
    s = raw
    for k, v in _MINUS.items():
        if k in s:
            p.minus_normalised = 1
            s = s.replace(k, v)
    p.stars = s.count("*")
    p.dagger = s.count("\u2020")
    p.percent = int("%" in s)
    core = s.replace("*", "").replace("\u2020", "").replace("%", "").strip()
    if core.startswith("(") and core.endswith(")"):
        p.paren = 1
        core = core[1:-1]
    core = core.replace(" ", "")
    if _THOUSANDS_RE.fullmatch(core):
        p.thousands_sep = 1
        core = core.replace(",", "")
    if _NUM_RE.fullmatch(core):
        p.element = "paren" if p.paren else "value"
        p.value_text = core
        return p
    # not a pure number: keep as text, list the numbers it contains
    p.stars = p.dagger = p.paren = p.percent = p.thousands_sep = 0
    p.numbers_in_text = ";".join(_NUM_RE.findall(s.replace(",", "")))
    return p


def parse_month_label(label: str) -> str | None:
    """'2000.1' -> '2000-01'; '2000.10' -> '2000-10'. Reads TEXT, never a float
    (as a float, 2000.10 == 2000.1, which is why month labels must stay text)."""
    m = _MONTH_RE.match(clean_text(label))
    if not m:
        return None
    year, month = int(m.group(1)), int(m.group(2))
    if not 1 <= month <= 12:
        return None
    return f"{year:04d}-{month:02d}"


def sha256_file(path: Path) -> str:
    h = hashlib.sha256()
    with open(path, "rb") as f:
        for chunk in iter(lambda: f.read(1 << 20), b""):
            h.update(chunk)
    return h.hexdigest()


# --------------------------------------------------------------------------------------
# Reading the Word document
# --------------------------------------------------------------------------------------
@dataclass
class Cell:
    text: str               # cleaned cell text, paragraphs joined by '\n'
    content: str            # text | empty | image | ole_object | omml | mixed forms like text+image
    omml_text: str
    merged_left: bool       # continuation of a horizontally merged cell
    merged_up: bool         # continuation of a vertically merged cell


@dataclass
class TableBlock:
    ordinal: int            # position among all tables in the document (1-based)
    table_id: str = ""
    caption: str = ""
    note: str = ""
    in_appendix: bool = False
    grid: list[list[Cell]] = field(default_factory=list)


def inspect_tc(tc) -> tuple[str, str]:
    """Look inside a cell's XML for images, OLE objects and Word equations (OMML)."""
    kinds, omml = set(), []
    for el in tc.iter():
        tag = el.tag if isinstance(el.tag, str) else ""
        local = tag.rsplit("}", 1)[-1]
        if tag.startswith("{" + W_NS) and local in GRAPHIC_TAGS:
            kinds.add("ole_object" if local == "object" else "image")
        elif tag == f"{{{M_NS}}}oMath":
            kinds.add("omml")
        elif tag == f"{{{M_NS}}}t" and el.text:
            omml.append(el.text)
    return "+".join(sorted(kinds)), "".join(omml)


def table_to_grid(table) -> list[list[Cell]]:
    """python-docx returns one _Cell per grid column; merged cells are the SAME underlying
    <w:tc> element repeated. We keep references to every tc so identity checks are safe."""
    grid, keep_alive = [], []
    prev_row_tcs: list = []
    for row in table.rows:
        cells = list(row.cells)
        tcs = [c._tc for c in cells]
        keep_alive.extend(tcs)
        out = []
        for j, c in enumerate(cells):
            tc = tcs[j]
            merged_left = j > 0 and tc is tcs[j - 1]
            merged_up = j < len(prev_row_tcs) and tc is prev_row_tcs[j]
            text = "\n".join(clean_text(p.text) for p in c.paragraphs).strip("\n")
            graphic, omml = inspect_tc(tc)
            if text and graphic:
                content = "text+" + graphic
            elif graphic:
                content = graphic
            else:
                content = "text" if text else "empty"
            out.append(Cell(text, content, omml, merged_left, merged_up))
        grid.append(out)
        prev_row_tcs = tcs
    return grid


def read_blocks(doc, log: RunLog) -> list[TableBlock]:
    """Walk the document body in order (paragraphs and tables interleaved), attach captions
    (nearest 'Table X.Y ...' paragraph since the previous table, or the paragraph right after),
    notes ('Note...' right after a table) and the Appendix I flag."""
    from docx.table import Table
    from docx.text.paragraph import Paragraph

    blocks: list[TableBlock] = []
    captions_since_last: list[str] = []
    in_appendix = False
    last_table: TableBlock | None = None
    after_table = 0          # paragraphs seen since the last table
    for child in doc.element.body.iterchildren():
        local = child.tag.rsplit("}", 1)[-1]
        if local == "p":
            text = clean_text(Paragraph(child, doc).text)
            if not text:
                continue
            after_table += 1
            if _APP_I_RE.match(text):
                in_appendix = True
            elif _SECTION_END_RE.match(text):
                in_appendix = False
            if _CAPTION_RE.match(text) and len(text) < 160:
                if (last_table is not None and after_table == 1 and not last_table.caption
                        and not last_table.in_appendix):
                    last_table.caption = text          # caption placed below the table
                else:
                    captions_since_last.append(text)
            elif last_table is not None and after_table <= 3 and text.lower().startswith("note"):
                last_table.note = (last_table.note + " " + text).strip()
        elif local == "tbl":
            tb = TableBlock(ordinal=len(blocks) + 1)
            tb.grid = table_to_grid(Table(child, doc))
            if captions_since_last:
                tb.caption = captions_since_last[-1]
            tb.in_appendix = in_appendix and not tb.caption
            blocks.append(tb)
            last_table, after_table, captions_since_last = tb, 0, []
    assign_table_ids(blocks, log)
    return blocks


def assign_table_ids(blocks: list[TableBlock], log: RunLog) -> None:
    seen: dict[str, int] = {}
    app_n = other_n = 0
    for tb in blocks:
        m = _CAPTION_RE.match(tb.caption) if tb.caption else None
        if m:
            base = f"T{int(m.group(1))}.{int(m.group(2))}"
            seen[base] = seen.get(base, 0) + 1
            tb.table_id = base if seen[base] == 1 else f"{base}_part{seen[base]}"
            if seen[base] > 1:
                log.warn(f"Caption {base} used by more than one table; this one is {tb.table_id}")
        elif tb.in_appendix:
            app_n += 1
            tb.table_id = f"APP_I_{app_n}"
        else:
            other_n += 1
            tb.table_id = f"UNCAPTIONED_{other_n}"


# --------------------------------------------------------------------------------------
# Table structure: header rows, label cells, standard-error rows
# --------------------------------------------------------------------------------------
def is_numeric_cell(cell: Cell) -> bool:
    return any(parse_numeric(ln).element in ("value", "paren") for ln in cell.text.split("\n"))


def header_row_count(grid: list[list[Cell]]) -> int:
    """Leading rows with no numeric entry outside the first column are header rows."""
    n = 0
    for row in grid:
        if any(is_numeric_cell(c) for c in row[1:] if not c.merged_left):
            break
        n += 1
    return n


def has_letters(s: str) -> bool:
    return bool(re.search(r"[A-Za-z\u0370-\u03ff\u4e00-\u9fff]", s))


def is_paren_row(row: list[Cell]) -> bool:
    """A row whose first cell is empty and whose entries are all in parentheses: a row of
    standard errors (or statistics) under the coefficient row above it."""
    if row[0].content != "empty":
        return False
    entries = []
    for c in row[1:]:
        if c.merged_left:
            continue
        for ln in c.text.split("\n"):
            p = parse_numeric(ln)
            if p.element != "empty":
                entries.append(p.element)
    return bool(entries) and all(e == "paren" for e in entries)


def label_needs_map(cell: Cell) -> bool:
    """A label cell goes into the label map if it is empty or holds a graphic or an equation
    (the equation-image labels in Replication Plan s.4). Text labels without letters, such as
    the month labels '2000.1', are ordinary labels."""
    if cell.merged_left or cell.merged_up:
        return False
    graphic = any(k in cell.content for k in ("image", "ole_object", "omml"))
    return graphic or cell.content == "empty"


@dataclass
class LabelCell:
    table_id: str
    caption: str
    axis: str               # row (first column) | col (header rows)
    row_idx: int
    col_idx: int
    cell: Cell
    neighbor_before: str = ""
    neighbor_after: str = ""
    first_cells: str = ""
    proposed_label: str = ""
    proposal_rule: str = "manual"

    @property
    def key(self) -> tuple:
        return (self.table_id, self.axis, self.row_idx, self.col_idx)


def find_label_cells(tb: TableBlock) -> list[LabelCell]:
    grid, out = tb.grid, []
    if not grid:
        return out
    nh = header_row_count(grid)
    ncol = max(len(r) for r in grid)
    # row labels (first column of data rows)
    for i in range(nh, len(grid)):
        row = grid[i]
        has_data = any(c.text for c in row[1:] if not c.merged_left)
        if not has_data or is_paren_row(row) or not label_needs_map(row[0]):
            continue
        firsts = [c.text.replace("\n", " ") for c in row[1:] if c.text and not c.merged_left][:3]
        out.append(LabelCell(tb.table_id, tb.caption, "row", i, 0, row[0],
                             first_cells=" | ".join(firsts)))
    # column labels (header rows, columns >= 1)
    for i in range(nh):
        for j in range(1, len(grid[i])):
            c = grid[i][j]
            col_has_data = any(j < len(r) and r[j].text for r in grid[nh:])
            if not col_has_data or not label_needs_map(c):
                continue
            # a blank upper header cell above a filled lower header cell is just layout
            if c.content == "empty" and any(has_letters(grid[k][j].text) for k in range(nh) if k != i):
                continue
            firsts = [r[j].text.replace("\n", " ") for r in grid[nh:] if j < len(r) and r[j].text][:3]
            out.append(LabelCell(tb.table_id, tb.caption, "col", i, j, c,
                                 first_cells=" | ".join(firsts)))
    _fill_neighbors(tb, out, nh, ncol)
    return out


def _axis_labels(tb: TableBlock, axis: str, nh: int, header_row: int = 0) -> list[tuple[int, str]]:
    """(position, label) along an axis; skips paren rows and merged continuations."""
    grid = tb.grid
    if axis == "row":
        return [(i, grid[i][0].text) for i in range(nh, len(grid))
                if not is_paren_row(grid[i]) and any(c.text for c in grid[i][1:])]
    row = grid[header_row] if grid else []
    return [(j, row[j].text) for j in range(1, len(row)) if not row[j].merged_left]


def _fill_neighbors(tb: TableBlock, cells: list[LabelCell], nh: int, ncol: int) -> None:
    for lc in cells:
        pos = lc.row_idx if lc.axis == "row" else lc.col_idx
        seq = _axis_labels(tb, lc.axis, nh, lc.row_idx if lc.axis == "col" else 0)
        before = [t for p, t in seq if p < pos and has_letters(t)]
        after = [t for p, t in seq if p > pos and has_letters(t)]
        lc.neighbor_before = before[-1].replace("\n", " ") if before else ""
        lc.neighbor_after = after[0].replace("\n", " ") if after else ""


def propose_labels(tb: TableBlock, cells: list[LabelCell]) -> None:
    """Proposal by position. For each axis of the table:
       missing = expected labels (LABEL_SETS) not already printed on that axis.
       one blank and one missing label        -> rule 'unique_missing'
       k blanks and k missing labels (k > 1)  -> rule 'order_of_missing' (in expected order)
       otherwise                              -> rule 'manual' (you fill it in)
    If the labels on the axis carry a 'D' prefix (first differences), proposals get it too."""
    base_id = tb.table_id.split("_part")[0]
    expected = LABEL_SETS.get("APP_I" if tb.table_id.startswith("APP_I") else base_id)
    if not expected:
        return
    nh = header_row_count(tb.grid)
    for axis in ("row", "col"):
        axis_cells = [c for c in cells if c.axis == axis]
        if not axis_cells:
            continue
        hdr = axis_cells[0].row_idx if axis == "col" else 0
        # positions of the label cells themselves ('D' + an image is not a printed label)
        own = {c.row_idx if axis == "row" else c.col_idx for c in axis_cells}
        printed = [normalize_label(t) for p, t in _axis_labels(tb, axis, nh, hdr)
                   if p not in own and has_letters(t)]
        n_d = sum(1 for lab in printed if lab.startswith("D") and lab[1:] in expected)
        n_plain = sum(1 for lab in printed if lab in expected)
        prefix = "D" if n_d > n_plain else ""
        present = {lab[len(prefix):] if prefix and lab.startswith(prefix) else lab for lab in printed}
        missing = [e for e in expected if e not in present]
        if len(axis_cells) == 1 and len(missing) == 1:
            axis_cells[0].proposed_label = prefix + missing[0]
            axis_cells[0].proposal_rule = "unique_missing"
        elif len(axis_cells) == len(missing) > 1:
            ordered = sorted(axis_cells, key=lambda c: (c.row_idx, c.col_idx))
            for c, lab in zip(ordered, missing):
                c.proposed_label = prefix + lab
                c.proposal_rule = "order_of_missing"


# --------------------------------------------------------------------------------------
# Label map file (drafted once, then only appended to; you edit confirmed_label/confirmed)
# --------------------------------------------------------------------------------------
def read_label_map(path: Path) -> dict[tuple, dict]:
    if not path.exists():
        return {}
    with open(path, encoding="utf-8-sig", newline="") as f:
        rows = list(csv.DictReader(f))
    out = {}
    for r in rows:
        r = {k.strip(): (v or "").strip() for k, v in r.items() if k}
        key = (r["table_id"], r["axis"], int(r["row_idx"]), int(r["col_idx"]))
        out[key] = r
    return out


def label_map_row(lc: LabelCell, docx_sha: str) -> dict:
    return {"table_id": lc.table_id, "caption": lc.caption, "axis": lc.axis,
            "row_idx": lc.row_idx, "col_idx": lc.col_idx, "cell_content": lc.cell.content,
            "cell_text": lc.cell.text.replace("\n", " "), "omml_text": lc.cell.omml_text,
            "neighbor_before": lc.neighbor_before, "neighbor_after": lc.neighbor_after,
            "first_cells": lc.first_cells, "proposed_label": lc.proposed_label,
            "proposal_rule": lc.proposal_rule, "confirmed_label": "", "confirmed": "no",
            "confirmed_date": "", "note": "", "docx_sha256": docx_sha}


def sync_label_map(label_cells: list[LabelCell], docx_sha: str, log: RunLog) -> dict[tuple, dict]:
    """First run: write the draft. Later runs: keep every existing row as you edited it,
    append rows for label cells not yet in the file, warn about rows no longer found."""
    existing = read_label_map(LABEL_MAP_CSV)
    found = {lc.key: lc for lc in label_cells}
    new_rows = [label_map_row(lc, docx_sha) for k, lc in found.items() if k not in existing]
    LABEL_MAP_CSV.parent.mkdir(parents=True, exist_ok=True)
    if not existing:
        with open(LABEL_MAP_CSV, "w", encoding="utf-8", newline="") as f:
            w = csv.DictWriter(f, fieldnames=LABEL_MAP_FIELDS, lineterminator="\n")
            w.writeheader()
            w.writerows(new_rows)
        log.info(f"Label map drafted: {LABEL_MAP_CSV.relative_to(ROOT)} ({len(new_rows)} rows)")
    elif new_rows:
        with open(LABEL_MAP_CSV, "a", encoding="utf-8", newline="") as f:
            csv.DictWriter(f, fieldnames=LABEL_MAP_FIELDS, lineterminator="\n").writerows(new_rows)
        log.warn(f"Appended {len(new_rows)} new label cells to the label map; confirm them")
    stale = [k for k in existing if k not in found]
    for k in stale:
        log.warn(f"Label map row not found in the document any more: {k}")
    shas = {r.get("docx_sha256", "") for r in existing.values()} - {"", docx_sha}
    if shas:
        log.warn("Label map rows were drafted from a different .docx (sha256 differs). "
                 "Re-check the positions against this file.")
    current = read_label_map(LABEL_MAP_CSV)
    return current


def resolve_label(key: tuple, lmap: dict[tuple, dict], fallback: str) -> tuple[str, str]:
    """Returns (label, source). source: map_confirmed | map_proposed | unmapped."""
    r = lmap.get(key)
    if r is None:
        return fallback, "unmapped"
    if r.get("confirmed", "").strip().lower() == "yes" and r.get("confirmed_label", "").strip():
        return r["confirmed_label"].strip(), "map_confirmed"
    if r.get("proposed_label", "").strip():
        return r["proposed_label"].strip(), "map_proposed"
    return fallback, "unmapped"


# --------------------------------------------------------------------------------------
# Labels for every row and column of a table
# --------------------------------------------------------------------------------------
def table_labels(tb: TableBlock, lmap: dict[tuple, dict]):
    """Returns (nh, row_labels{i: (label, source)}, col_labels{j: (label, source)})."""
    grid = tb.grid
    nh = header_row_count(grid)
    ncol = max((len(r) for r in grid), default=0)
    col_labels = {}
    for j in range(1, ncol):
        parts, sources = [], set()
        for i in range(nh):
            if j >= len(grid[i]):
                continue
            c = grid[i][j]
            key = (tb.table_id, "col", i, j)
            if key in lmap:
                lab, src = resolve_label(key, lmap, f"UNMAPPED_r{i}c{j}")
            elif c.merged_left:
                k = j
                while k > 0 and grid[i][k].merged_left:
                    k -= 1
                lab, src = grid[i][k].text.replace("\n", " "), "document"
                key_k = (tb.table_id, "col", i, k)
                if key_k in lmap:
                    lab, src = resolve_label(key_k, lmap, f"UNMAPPED_r{i}c{k}")
            else:
                lab, src = c.text.replace("\n", " "), "document"
            if lab and lab not in parts:
                parts.append(lab)
                sources.add(src)
        src = "document" if sources <= {"document"} else sorted(sources - {"document"})[0]
        col_labels[j] = (" | ".join(parts), src)
    row_labels = {}
    prev = ("", "document")
    for i in range(nh, len(grid)):
        row = grid[i]
        key = (tb.table_id, "row", i, 0)
        if key in lmap:
            row_labels[i] = resolve_label(key, lmap, f"UNMAPPED_r{i}")
        elif is_paren_row(row):
            row_labels[i] = (f"{prev[0]} (paren)", "derived_paren_row")
            continue
        else:
            row_labels[i] = (row[0].text.replace("\n", " "), "document")
        prev = row_labels[i]
    return nh, row_labels, col_labels


# --------------------------------------------------------------------------------------
# Targets (every cell of every captioned table)
# --------------------------------------------------------------------------------------
def build_targets(blocks: list[TableBlock], lmap: dict[tuple, dict]) -> tuple[list[dict], dict]:
    rows, counts = [], {"minus_normalised": 0, "thousands_sep": 0}
    for tb in blocks:
        if tb.in_appendix or tb.table_id.startswith("APP_I"):
            continue
        nh, rlab, clab = table_labels(tb, lmap)
        for i in range(nh, len(tb.grid)):
            row_label, row_src = rlab.get(i, ("", "document"))
            for j in range(1, len(tb.grid[i])):
                c = tb.grid[i][j]
                if c.merged_left or c.merged_up:
                    continue
                col_label, col_src = clab.get(j, ("", "document"))
                lines = c.text.split("\n") if c.text else [""]
                for t, ln in enumerate(lines):
                    p = parse_numeric(ln)
                    counts["minus_normalised"] += p.minus_normalised
                    counts["thousands_sep"] += p.thousands_sep
                    confirmed = all(s in ("document", "map_confirmed", "derived_paren_row")
                                    for s in (row_src, col_src))
                    rows.append({
                        "target_id": f"{tb.table_id}_r{i}c{j}t{t}", "table_id": tb.table_id,
                        "caption": tb.caption, "table_note": tb.note, "row_idx": i, "col_idx": j, "token_idx": t,
                        "row_label": row_label, "row_label_source": row_src,
                        "col_label": col_label, "col_label_source": col_src,
                        "element": p.element, "cell_text": ln, "value_text": p.value_text,
                        "stars": p.stars, "dagger": p.dagger, "paren": p.paren,
                        "missing": p.missing, "percent": p.percent,
                        "thousands_sep": p.thousands_sep, "numbers_in_text": p.numbers_in_text,
                        "labels_confirmed": "yes" if confirmed else "no"})
    return rows, counts


# --------------------------------------------------------------------------------------
# Appendix I
# --------------------------------------------------------------------------------------
def build_appendix(blocks: list[TableBlock], lmap: dict[tuple, dict], log: RunLog):
    """Returns (columns, records, stats). Month labels stay text; 'month' is derived."""
    app = [tb for tb in blocks if tb.table_id.startswith("APP_I")]
    if not app:
        # fallback: uncaptioned tables whose first header cell is 'Month'
        app = [tb for tb in blocks if tb.table_id.startswith("UNCAPTIONED")
               and tb.grid and normalize_label(tb.grid[0][0].text).lower() == "month"]
        if app:
            log.warn("No 'App. I' heading found; using uncaptioned tables headed 'Month'")
    if not app:
        log.error("Appendix I tables not found")
        return [], [], {}
    stats = {"repeated_headers": 0, "minus_normalised": 0, "thousands_sep": 0,
             "labels_from_map": 0, "unparsed_cells": 0}
    columns: list[str] | None = None
    records = []
    for tb in app:
        nh, _, clab = table_labels(tb, lmap)
        if nh == 0:
            log.error(f"{tb.table_id}: no header row found")
            continue
        names = []
        for j in range(1, len(tb.grid[0])):
            lab, src = clab.get(j, (f"col{j}", "document"))
            if src.startswith("map"):
                stats["labels_from_map"] += 1
            if src in ("unmapped", "map_proposed"):
                log.warn(f"{tb.table_id} column {j}: label '{lab}' is {src}")
            names.append(normalize_label(lab.split(" | ")[-1]) or f"col{j}")
        if columns is None:
            columns = names
        elif names != columns:
            log.error(f"{tb.table_id}: header {names} differs from first appendix table {columns}")
        for i in range(nh, len(tb.grid)):
            row = tb.grid[i]
            first = clean_text(row[0].text)
            if normalize_label(first).lower() == "month":
                stats["repeated_headers"] += 1
                continue
            if not any(c.text for c in row):
                continue
            values = []
            for j in range(1, len(row)):
                p = parse_numeric(row[j].text)
                stats["minus_normalised"] += p.minus_normalised
                stats["thousands_sep"] += p.thousands_sep
                if p.element == "value":
                    values.append(p.value_text)
                else:
                    stats["unparsed_cells"] += 1
                    values.append(row[j].text)
            records.append({"source_table": tb.table_id, "source_row": i,
                            "month_label": first, "month": parse_month_label(first) or "",
                            "values": values})
    for n, r in enumerate(records, start=1):
        r["row_in_appendix"] = n
    return columns or [], records, stats


def check_appendix(columns: list[str], records: list[dict], stats: dict, log: RunLog) -> bool:
    """Structural checks only (counts and calendar), per Replication Plan s.2.1."""
    checks = []
    checks.append(("rows == %d" % EXPECTED_APPENDIX_ROWS, len(records) == EXPECTED_APPENDIX_ROWS,
                   f"found {len(records)}"))
    bad_labels = [r["month_label"] for r in records if not r["month"]]
    checks.append(("every month label reads as YYYY.M", not bad_labels,
                   f"unreadable: {bad_labels[:5]}"))
    months = [r["month"] for r in records if r["month"]]
    checks.append(("first month " + EXPECTED_FIRST_MONTH,
                   bool(months) and months[0] == EXPECTED_FIRST_MONTH,
                   f"found {months[0] if months else None}"))
    checks.append(("last month " + EXPECTED_LAST_MONTH,
                   bool(months) and months[-1] == EXPECTED_LAST_MONTH,
                   f"found {months[-1] if months else None}"))
    idx = [int(m[:4]) * 12 + int(m[5:]) for m in months]
    gaps = [months[k + 1] for k in range(len(idx) - 1) if idx[k + 1] - idx[k] != 1]
    checks.append(("months consecutive, no duplicates", not gaps, f"breaks at {gaps[:5]}"))
    widths = {len(r["values"]) for r in records}
    checks.append(("same number of values in every row", widths == {len(columns)},
                   f"widths {sorted(widths)} vs {len(columns)} columns"))
    checks.append(("every value cell is a plain number", stats.get("unparsed_cells", 1) == 0,
                   f"{stats.get('unparsed_cells')} cells not numeric"))
    checks.append(("column names unique and non-empty",
                   len(set(columns)) == len(columns) and all(columns), f"{columns}"))
    ok = True
    for name, passed, detail in checks:
        if passed:
            log.info(f"CHECK PASS  {name}")
        else:
            ok = False
            log.error(f"CHECK FAIL  {name}: {detail}")
    return ok


# --------------------------------------------------------------------------------------
# Writers
# --------------------------------------------------------------------------------------
def write_appendix_csv(columns: list[str], records: list[dict]) -> None:
    APPENDIX_CSV.parent.mkdir(parents=True, exist_ok=True)
    header = ["row_in_appendix", "source_table", "source_row", "month_label", "month"] + columns
    with open(APPENDIX_CSV, "w", encoding="utf-8", newline="") as f:
        w = csv.writer(f, quoting=csv.QUOTE_ALL, lineterminator="\n")
        w.writerow(header)
        for r in records:
            w.writerow([r["row_in_appendix"], r["source_table"], r["source_row"],
                        r["month_label"], r["month"]] + r["values"])


def write_targets_csv(rows: list[dict]) -> None:
    TARGETS_CSV.parent.mkdir(parents=True, exist_ok=True)
    with open(TARGETS_CSV, "w", encoding="utf-8", newline="") as f:
        w = csv.DictWriter(f, fieldnames=TARGET_FIELDS, quoting=csv.QUOTE_ALL, lineterminator="\n")
        w.writeheader()
        w.writerows(rows)


def append_csv_row(path: Path, fields: list[str], row: dict, log: RunLog) -> None:
    new = not path.exists()
    if new:
        log.warn(f"{path.relative_to(ROOT)} did not exist; creating it with its header "
                 "(run init_config_and_metadata.ps1 first next time)")
        path.parent.mkdir(parents=True, exist_ok=True)
    else:
        with open(path, encoding="utf-8-sig", newline="") as f:
            header = next(csv.reader(f), [])
        if header != fields:
            raise RuntimeError(f"Header of {path} does not match the expected fields")
    with open(path, "a", encoding="utf-8", newline="") as f:
        w = csv.DictWriter(f, fieldnames=fields, lineterminator="\n")
        if new:
            w.writeheader()
        w.writerow(row)


def log_transformation(step: str, docx: Path, sha: str, output: Path, description: str,
                       n: int, decision: str, log: RunLog) -> None:
    """Append one row to transformation_log.csv unless the identical step is already there."""
    key = (SCRIPT_REL, step, sha, str(output.relative_to(ROOT)), str(n))
    if TRANSFORM_CSV.exists():
        with open(TRANSFORM_CSV, encoding="utf-8-sig", newline="") as f:
            for r in csv.DictReader(f):
                if (r["script"], r["step"], r["input_sha256"], r["output_file"], r["n_affected"]) == key:
                    return
    append_csv_row(TRANSFORM_CSV, TRANSFORM_FIELDS, {
        "timestamp_utc": dt.datetime.now(dt.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
        "script": SCRIPT_REL, "step": step, "input_file": str(docx.relative_to(ROOT)),
        "input_sha256": sha, "output_file": str(output.relative_to(ROOT)),
        "description": description, "n_affected": n, "decision_ref": decision}, log)


def write_ledger_row(docx: Path, sha: str, blocks: list[TableBlock], records: list[dict],
                     n_conf: int, n_map: int, log: RunLog) -> None:
    if LEDGER_CSV.exists():
        with open(LEDGER_CSV, encoding="utf-8-sig", newline="") as f:
            if any(f"sha256={sha}" in r.get("notes", "") for r in csv.DictReader(f)):
                log.info("Ledger already has a row for this .docx (same sha256); not added again")
                return
    app_notes = [tb.note for tb in blocks if tb.table_id.startswith("APP_I") and tb.note]
    t31 = [tb.note for tb in blocks if tb.table_id == "T3.1" and tb.note]
    unit = (app_notes or t31 or ["see the unit note under Table 3.1 of the original document"])[0]
    months = [r["month"] for r in records if r["month"]]
    tables = [tb.table_id for tb in blocks if tb.table_id.startswith("T")]
    mtime = dt.datetime.fromtimestamp(docx.stat().st_mtime).strftime("%Y-%m-%d")
    append_csv_row(LEDGER_CSV, LEDGER_FIELDS, {
        "series_name": "Original document Appendix I (9 variables) and Tables 3.1-4.11",
        "provider": "Original study document underlying Du, Ge & Qiao (2014)",
        "original_file_name": docx.name,
        "download_date": dt.date.today().isoformat(),
        "source_url_or_access_note": "Original Word file saved as .docx by the project author; "
                                     "not publicly available",
        "series_key_or_code": "App. I; " + ",".join(tables),
        "frequency": "monthly (App. I); period statistics and test tables",
        "unit": unit, "currency": "as in unit note",
        "seasonal_adjustment_status": "not stated in original document",
        "coverage_start": months[0] if months else "", "coverage_end": months[-1] if months else "",
        "acquisition_method": "document_extraction", "layer": "rep",
        "license_or_access_restriction": LICENSE_NOTE,
        "transformation_script": SCRIPT_REL,
        "notes": (f"appendix_csv=data/interim/original_appendix.csv; "
                  f"targets_csv=data/metadata/original_targets.csv; "
                  f"label_map=config/original_label_map.csv; month_label=text; month=YYYY-MM; "
                  f"all_fields_quoted=yes; na=empty; encoding=utf-8; file_mtime={mtime}; "
                  f"label_map_confirmed_at_registration={n_conf}/{n_map}; sha256={sha}")}, log)
    log.info("Ledger row written (acquisition_method=document_extraction, layer=rep)")


# --------------------------------------------------------------------------------------
# Main
# --------------------------------------------------------------------------------------
def find_docx(arg: str | None, log: RunLog) -> Path:
    if arg:
        p = (ROOT / arg).resolve() if not Path(arg).is_absolute() else Path(arg)
        if not p.exists():
            raise FileNotFoundError(f"--docx file not found: {p}")
    else:
        cands = [p for p in RAW_ORIGINAL_DIR.glob("*.docx") if not p.name.startswith("~$")]
        if not cands:
            docs = list(RAW_ORIGINAL_DIR.glob("*.doc"))
            hint = " (found a .doc: open it in Word and Save As .docx)" if docs else ""
            raise FileNotFoundError(f"No .docx in {RAW_ORIGINAL_DIR}{hint}")
        if len(cands) > 1:
            raise RuntimeError(f"More than one .docx in {RAW_ORIGINAL_DIR}: "
                               f"{[c.name for c in cands]}. Use --docx to choose one.")
        p = cands[0]
    if RAW_ORIGINAL_DIR.resolve() not in p.resolve().parents:
        raise ValueError(f"Save the original document inside {RAW_ORIGINAL_DIR} (raw files live in data/raw/)")
    if p.suffix.lower() != ".docx":
        raise ValueError("The original document must be a .docx file (Word 2007+ format)")
    return p


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description="Extract Appendix I and all tables from the original document (.docx)")
    ap.add_argument("--docx", help="path to the original document (.docx) (default: the one file in data/raw/original/)")
    args = ap.parse_args(argv)
    try:
        sys.stdout.reconfigure(errors="replace")
    except AttributeError:
        pass
    log = RunLog("extract_original")
    try:
        import docx as python_docx
        log.info(f"Python {platform.python_version()}; python-docx {python_docx.__version__}")
        path = find_docx(args.docx, log)
        sha = sha256_file(path)
        log.info(f"Original document file: {path.relative_to(ROOT)}  sha256={sha}")
        doc = python_docx.Document(str(path))          # read-only use; never saved

        blocks = read_blocks(doc, log)
        log.info(f"Tables in document: {len(blocks)}")
        for tb in blocks:
            nrow = len(tb.grid)
            ncol = max((len(r) for r in tb.grid), default=0)
            log.info(f"  {tb.table_id:<14} {nrow:>4} x {ncol:<3} {tb.caption[:70]}")
        found = {tb.table_id for tb in blocks}
        missing_tables = [t for t in EXPECTED_TABLES if t not in found]
        if missing_tables:
            log.warn(f"Expected tables not found by caption: {missing_tables}")
        else:
            log.info("All expected tables found: Tables 3.1-3.9 and 4.1-4.11")

        # label cells -> draft/append label map -> read back (with your confirmations)
        label_cells = []
        for tb in blocks:
            lcs = find_label_cells(tb)
            propose_labels(tb, lcs)
            label_cells.extend(lcs)
        lmap = sync_label_map(label_cells, sha, log)
        relevant = [lmap[lc.key] for lc in label_cells if lc.key in lmap]
        n_conf = sum(1 for r in relevant if r.get("confirmed", "").lower() == "yes"
                     and r.get("confirmed_label", "").strip())
        log.info(f"Label cells needing a label: {len(label_cells)}; confirmed: {n_conf}")
        for lc in label_cells:
            r = lmap.get(lc.key, {})
            state = "confirmed" if r.get("confirmed", "").lower() == "yes" else "TO CONFIRM"
            log.info(f"  {lc.table_id:<10} {lc.axis} r{lc.row_idx} c{lc.col_idx} "
                     f"[{lc.cell.content}] proposed='{lc.proposed_label}' "
                     f"({lc.proposal_rule}) {state}")
        if n_conf < len(label_cells):
            log.warn(f"{len(label_cells) - n_conf} label(s) still to confirm in "
                     f"{LABEL_MAP_CSV.relative_to(ROOT)}")

        # appendix
        columns, records, stats = build_appendix(blocks, lmap, log)
        ok = check_appendix(columns, records, stats, log)
        write_appendix_csv(columns, records)
        log.info(f"Wrote {APPENDIX_CSV.relative_to(ROOT)}: {len(records)} rows, columns {columns}")

        # targets
        targets, tcounts = build_targets(blocks, lmap)
        write_targets_csv(targets)
        by_el: dict[str, int] = {}
        for t in targets:
            by_el[t["element"]] = by_el.get(t["element"], 0) + 1
        n_unconf = sum(1 for t in targets if t["labels_confirmed"] == "no")
        log.info(f"Wrote {TARGETS_CSV.relative_to(ROOT)}: {len(targets)} rows {by_el}; "
                 f"rows with an unconfirmed label: {n_unconf}")

        # transformation log (one row per conversion step; repeated runs are not duplicated)
        steps = [
            ("appendix_month_iso", APPENDIX_CSV, "month_label kept as text; month (YYYY-MM) "
             "parsed from the text 'YYYY.M'", len(records), "W1-01"),
            ("appendix_drop_repeated_headers", APPENDIX_CSV, "header rows repeated inside "
             "appendix tables dropped", stats.get("repeated_headers", 0), "W1-01"),
            ("normalise_minus_sign", APPENDIX_CSV, "Unicode minus/dash replaced by '-' in "
             "numeric cells", stats.get("minus_normalised", 0), "W1-01"),
            ("remove_thousands_separator", APPENDIX_CSV, "',' thousands separators removed "
             "in numeric cells", stats.get("thousands_sep", 0), "W1-01"),
            ("apply_label_map_appendix", APPENDIX_CSV, "appendix column labels filled from "
             "config/original_label_map.csv", stats.get("labels_from_map", 0), "W1-04"),
            ("targets_tokenise", TARGETS_CSV, "every cell of Tables 3.1-4.11 split into lines; "
             "numbers, stars, daggers, parentheses flagged; minus signs normalised "
             f"({tcounts['minus_normalised']}), separators removed ({tcounts['thousands_sep']})",
             len(targets), "W1-04"),
        ]
        for step, out, desc, n, dec in steps:
            log_transformation(step, path, sha, out, desc, n, dec, log)

        if ok:
            write_ledger_row(path, sha, blocks, records, n_conf, len(label_cells), log)
        else:
            log.error("Appendix checks failed: ledger row NOT written. Fix, then rerun.")
    except Exception as exc:                              # noqa: BLE001
        log.error(f"{type(exc).__name__}: {exc}")
        for ln in traceback.format_exc().splitlines():
            log.error("  " + ln)
    status = log.finish()
    print(f"Run log: {log.path}")
    return 1 if status == "FAIL" else 0


if __name__ == "__main__":
    sys.exit(main())
