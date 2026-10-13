"""Write docs/decisions_index.csv: the number, title, date and status of every entry in the
decision log, for Appendix A of the replication note.

The decision log itself (docs/decisions.md) stays private (W1-09). The index carries only what
Appendix A prints: no evidence, no options, no data values. It is committed, so a reader who
clones the repository gets the same Appendix A as the author.

Usage (Command Prompt, from the project root), after every change to docs/decisions.md:
    .venv\\Scripts\\python code\\python\\make_decisions_index.py

Writes outputs/logs/make_decisions_index_<timestamp>.log, ending in STATUS OK or STATUS FAIL.
"""
import csv
import datetime as dt
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SRC = ROOT / "docs" / "decisions.md"
OUT = ROOT / "docs" / "decisions_index.csv"
HEAD = re.compile(r"^## (W\d+-\d+) (.+?)\s*$")
FIELD = re.compile(r"^- \*\*(Date|Date received|Status)\*\*:\s*(.*)$")


def main() -> int:
    stamp = dt.datetime.now().strftime("%Y%m%d-%H%M%S")
    log_dir = ROOT / "outputs" / "logs"
    log_dir.mkdir(parents=True, exist_ok=True)
    log = [f"make_decisions_index.py  {stamp}"]
    status = "OK"

    if not SRC.exists():
        log += [f"ERROR {SRC.relative_to(ROOT)} not found (it is private, W1-09; run this in the author's copy)",
                "STATUS FAIL"]
        (log_dir / f"make_decisions_index_{stamp}.log").write_text("\n".join(log) + "\n", encoding="utf-8")
        print("\n".join(log))
        return 1

    rows, cur, in_code = [], None, False
    for line in SRC.read_text(encoding="utf-8").splitlines():
        if line.lstrip().startswith("```"):
            in_code = not in_code          # entries quoted inside code blocks are not entries
            continue
        if in_code:
            continue
        m = HEAD.match(line)
        if m:
            cur = {"decision": m.group(1), "title": m.group(2), "date": "", "status": ""}
            rows.append(cur)
            continue
        f = FIELD.match(line)
        if f and cur is not None:
            key = "date" if f.group(1).startswith("Date") else "status"
            if not cur[key]:
                cur[key] = f.group(2).strip()

    ids = [r["decision"] for r in rows]
    dups = sorted({i for i in ids if ids.count(i) > 1})
    if dups:
        log.append("WARN  decision numbers that appear more than once: " + ", ".join(dups))
        status = "WARN"
    templates = [r["decision"] for r in rows if "<" in r["decision"]]
    rows = [r for r in rows if r["decision"] not in templates]

    with open(OUT, "w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(fh, fieldnames=["decision", "title", "date", "status"])
        w.writeheader()
        w.writerows(rows)
    log.append(f"CHECK PASS  {len(rows)} entries read from docs/decisions.md")
    log.append(f"Wrote {OUT.relative_to(ROOT)} (decision, title, date, status; commit it)")
    log.append(f"STATUS {status}")
    (log_dir / f"make_decisions_index_{stamp}.log").write_text("\n".join(log) + "\n", encoding="utf-8")
    print("\n".join(log))
    return 0


if __name__ == "__main__":
    sys.exit(main())
