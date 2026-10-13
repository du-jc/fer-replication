"""Compare the outputs of two complete runs of the Study A pipeline (clean-clone test, W4-38).

Usage (Command Prompt, from the project root):
    python code\\python\\compare_runs.py --ref . --new %TEMP%\\fer_clean

--ref  root of the reference project (your working copy)
--new  root of the clean clone after its full run

What it compares:
  * every file in outputs/tables/ (both directions: a file present in only one run is a failure);
    - byte-identical files (SHA-256)                      -> IDENTICAL
    - otherwise CSVs are compared cell by cell; numbers that differ by no more than --tol
      (relative) count as equal                           -> EQUAL_WITHIN_TOL
    - run metadata listed in RUN_METADATA (run date, commit, time stamps of a selection)
      are ignored                                         -> EQUAL_EXCEPT_RUN_METADATA
    - if cells differ only because the rows are in another order, the rows are compared
      after sorting                                       -> EQUAL_AFTER_SORT
    - anything else                                       -> DIFFERS (FAIL)
  RUN_METADATA and the sorted comparison were added after the first comparison of W4-38
  (amendment recorded in W4-38): they classify differences that are not results.
  * files in FROZEN_INPUTS (kept in outputs/tables but written by no script) are listed, not compared;
  * outputs/figures/: SHA-256 only, reported but never a failure (PNG files can carry
    rendering metadata).

Writes outputs/logs/compare_runs_<timestamp>.log in the reference project and ends with
"STATUS OK" or "STATUS FAIL". Prints no data values: a differing cell is reported by file,
row and column only.
"""
import argparse
import csv
import datetime as dt
import hashlib
import sys
from pathlib import Path

# Run metadata that differs between runs without being a result (W4-38): the run date and the commit
# of the run (note_numbers.csv rows), and the time stamps written when a selection rule ran.
RUN_METADATA = {
    "note_numbers.csv": ("rows", {"date_outputs", "git_commit"}),
    "dfgls_selected_spec.csv": ("cols", {"fixed_at"}),
    "selected_inputs.csv": ("cols", {"selected_at"}),
}
# Files kept in outputs/tables that no script of the pipeline writes (frozen inputs). They are
# copied into the clean clone by hand and are reported, not compared.
FROZEN_INPUTS = {"note_T7_expectation_long_fullrun.csv"}  # first-rerun step table, W4-31


def sha256(path: Path) -> str:
    h = hashlib.sha256()
    with open(path, "rb") as fh:
        for chunk in iter(lambda: fh.read(1 << 20), b""):
            h.update(chunk)
    return h.hexdigest()


def read_csv(path: Path):
    with open(path, newline="", encoding="utf-8") as fh:
        return list(csv.reader(fh))


def num(x: str):
    try:
        return float(x)
    except ValueError:
        return None


def _cmp_rows(ra, rb, header, tol, meta):
    """Cell-by-cell comparison. Returns (diffs, within_tol, skipped_metadata)."""
    kind, names = meta if meta else (None, set())
    name_col = header.index("name") if "name" in header else None
    skip_cols = {header.index(c) for c in names if kind == "cols" and c in header}
    diffs, within, skipped = [], False, False
    for i, (x, y) in enumerate(zip(ra, rb)):
        if len(x) != len(y):
            diffs.append(f"row {i + 1}: column count")
            continue
        row_meta = kind == "rows" and name_col is not None and i > 0 and x[name_col] in names
        for j, (u, v) in enumerate(zip(x, y)):
            if u == v:
                continue
            if row_meta or (i > 0 and j in skip_cols):
                skipped = True
                continue
            nu, nv = num(u), num(v)
            if nu is not None and nv is not None and abs(nu - nv) <= tol * max(1.0, abs(nu), abs(nv)):
                within = True
                continue
            col = header[j] if j < len(header) else str(j + 1)
            diffs.append(f"row {i + 1}, column '{col}'")
    return diffs, within, skipped


def compare_csv(a: Path, b: Path, tol: float, meta=None):
    """Return (status, list of 'row r, column c' strings that differ)."""
    ra, rb = read_csv(a), read_csv(b)
    if len(ra) != len(rb):
        return "DIFFERS", [f"row count {len(ra)} vs {len(rb)}"]
    header = ra[0] if ra else []
    diffs, within, skipped = _cmp_rows(ra, rb, header, tol, meta)
    if diffs and ra and rb and ra[0] == rb[0]:
        # Same rows in another order? Compare after sorting the data rows (header kept first).
        d2, w2, s2 = _cmp_rows([ra[0]] + sorted(ra[1:]), [rb[0]] + sorted(rb[1:]), header, tol, meta)
        if not d2:
            return "EQUAL_AFTER_SORT", [f"rows in another order; equal after sorting"
                                         f"{' (within tolerance)' if w2 else ''}"
                                         f"{' (run metadata ignored)' if s2 else ''}"]
    if diffs:
        return "DIFFERS", diffs
    if within:
        return "EQUAL_WITHIN_TOL", []
    if skipped:
        return "EQUAL_EXCEPT_RUN_METADATA", []
    return "IDENTICAL", []


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--ref", required=True)
    ap.add_argument("--new", required=True)
    ap.add_argument("--tol", type=float, default=1e-10, help="relative tolerance for numbers (default 1e-10)")
    args = ap.parse_args()
    ref, new = Path(args.ref).resolve(), Path(args.new).resolve()

    stamp = dt.datetime.now().strftime("%Y%m%d-%H%M%S")
    log_dir = ref / "outputs" / "logs"
    log_dir.mkdir(parents=True, exist_ok=True)
    log_path = log_dir / f"compare_runs_{stamp}.log"
    lines = [f"compare_runs.py  {stamp}", f"reference: {ref}", f"new run:   {new}", f"tolerance: {args.tol}", ""]
    fail = False
    if ref == new:
        lines += ["ERROR --ref and --new are the same folder: a run cannot be compared with itself.",
                  "      Run this from the working copy's root: --ref . --new <clone folder>", "", "STATUS FAIL"]
        log_path.write_text("\n".join(lines) + "\n", encoding="utf-8")
        print("\n".join(lines))
        return 1

    for sub, strict in (("outputs/tables", True), ("outputs/figures", False)):
        da, db = ref / sub, new / sub
        fa = {p.name for p in da.glob("*") if p.is_file()} if da.exists() else set()
        fb = {p.name for p in db.glob("*") if p.is_file()} if db.exists() else set()
        lines.append(f"== {sub}: {len(fa)} files in reference, {len(fb)} in new run")
        for name in sorted((fa | fb) & FROZEN_INPUTS):
            lines.append(f"  FROZEN    {name} (input kept by hand, not compared)")
        fa, fb = fa - FROZEN_INPUTS, fb - FROZEN_INPUTS
        for name in sorted(fa ^ fb):
            where = "reference only" if name in fa else "new run only"
            lines.append(f"  MISSING   {name} ({where})")
            fail = fail or strict
        counts = {}
        for name in sorted(fa & fb):
            pa, pb = da / name, db / name
            if sha256(pa) == sha256(pb):
                status, detail = "IDENTICAL", []
            elif strict and name.lower().endswith(".csv"):
                status, detail = compare_csv(pa, pb, args.tol, RUN_METADATA.get(name))
            else:
                status, detail = ("DIFFERS" if strict else "DIFFERS_BINARY"), []
            counts[status] = counts.get(status, 0) + 1
            if status != "IDENTICAL":
                lines.append(f"  {status:<26}{name}")
                for d in detail[:20]:
                    lines.append(f"      {d}")
                if len(detail) > 20:
                    lines.append(f"      ... {len(detail) - 20} more")
            if strict and status == "DIFFERS":
                fail = True
        lines.append("  summary: " + ", ".join(f"{k} {v}" for k, v in sorted(counts.items())))
        lines.append("")

    lines.append("STATUS FAIL" if fail else "STATUS OK")
    log_path.write_text("\n".join(lines) + "\n", encoding="utf-8")
    print("\n".join(lines))
    print(f"\nlog written: {log_path}")
    return 1 if fail else 0


if __name__ == "__main__":
    sys.exit(main())
