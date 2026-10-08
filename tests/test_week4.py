"""
tests/test_week4.py -- Week 4 tests of code/python/collect_cpi.py (Study A)

Builds SYNTHETIC files shaped like an IMF SDMX-CSV response and a FRED observations JSON,
and runs the collector on them in file mode (no network). Every number is a placeholder made
from a formula of the month's position; none is real data.

Run from the project root:   python -m unittest tests.test_week4 -v
"""
import csv
import importlib.util
import json
import shutil
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]


def load(name):
    spec = importlib.util.spec_from_file_location(name, ROOT / "code" / "python" / f"{name}.py")
    mod = importlib.util.module_from_spec(spec)
    sys.modules[name] = mod
    spec.loader.exec_module(mod)
    return mod


def read(path):
    with open(path, encoding="utf-8-sig", newline="") as f:
        return list(csv.DictReader(f))


def months(a=1998, b=2012):
    return [f"{y}-{m:02d}" for y in range(a, b + 1) for m in range(1, 13)]


def placeholder(i):
    return round(50 + i * 0.25, 4)            # synthetic index level, not data


def imf_csv(path, period_style="M", drop=None):
    with open(path, "w", encoding="utf-8", newline="") as f:
        w = csv.writer(f)
        w.writerow(["STRUCTURE", "STRUCTURE_ID", "ACTION", "COUNTRY", "INDEX_TYPE", "COICOP_1999",
                    "TYPE_OF_TRANSFORMATION", "FREQUENCY", "TIME_PERIOD", "OBS_VALUE"])
        for i, m in enumerate(months()):
            if m == drop:
                continue
            tp = m.replace("-", "-M") if period_style == "M" else m
            w.writerow(["dataflow", "IMF.STA:CPI(5.0.0)", "I", "CHN", "CPI", "_T", "IX", "M", tp, placeholder(i)])


def fred_json(path):
    obs = [{"date": f"{m}-01", "value": "." if i == 0 else str(placeholder(i) * 2)} for i, m in enumerate(months())]
    path.write_text(json.dumps({"observations": obs}), encoding="utf-8")


def nbs_csv(path, label="居民消费价格指数 (上年同月=100) ", first=1995, last=2015, drop=None):
    """Synthetic file shaped like the data.stats.gov.cn export: months in columns, newest first,
    tab characters inside cells, a trailing empty cell, note and source lines. Placeholder values."""
    ms = [m for m in months(first, last) if m != drop][::-1]
    head = ["指标\t"] + [f"{int(m[:4])}年{int(m[5:])}月\t" for m in ms] + [""]
    vals = [label + "\t"] + [f"{100 + (i % 7) / 10:.1f}\t" for i, _ in enumerate(ms)] + [""]
    with open(path, "w", encoding="utf-8-sig", newline="") as f:
        w = csv.writer(f)
        w.writerow(["数据库：月度数据 "])
        w.writerow([f"时间：{first}年01月-{last}年12月 "])
        w.writerow(head)
        w.writerow(vals)
        w.writerow(["注：synthetic note"])
        w.writerow(["数据来源：国家统计局"])
    return ms


class CpiCase(unittest.TestCase):
    def setUp(self):
        self.tmp = Path(tempfile.mkdtemp())
        for d in ["data/raw/imf", "data/raw/fred", "data/raw/nbs", "data/interim", "data/metadata", "outputs/logs"]:
            (self.tmp / d).mkdir(parents=True, exist_ok=True)
        self.cc = load("collect_cpi")
        self.cc.configure_paths(self.tmp)

    def tearDown(self):
        shutil.rmtree(self.tmp, ignore_errors=True)

    def test_month_parser(self):
        p = self.cc.parse_month
        self.assertEqual(p("2000-M01"), "2000-01")
        self.assertEqual(p("2000M1"), "2000-01")
        self.assertEqual(p("2000-01"), "2000-01")
        self.assertEqual(p("2000-12-01"), "2000-12")
        self.assertIsNone(p("2000-Q1"))
        self.assertIsNone(p("2000-M13"))

    def test_end_to_end_file_mode(self):
        imf = self.tmp / "data/raw/imf/imf.csv"
        fred = self.tmp / "data/raw/fred/fred.json"
        imf_csv(imf)
        fred_json(fred)
        before = (imf.read_bytes(), fred.read_bytes())
        self.assertEqual(self.cc.main(["--imf-file", str(imf), "--fred-file", str(fred)]), 0)
        self.assertEqual((imf.read_bytes(), fred.read_bytes()), before, "raw files must stay untouched")
        rows = {r["month"]: r for r in read(self.tmp / "data/interim/cpi_monthly.csv")}
        ms = months()
        for i in (1, 30, 100):
            self.assertAlmostEqual(float(rows[ms[i]]["cpi_imf"]), placeholder(i))
            self.assertAlmostEqual(float(rows[ms[i]]["cpi_fred"]), placeholder(i) * 2)
        self.assertEqual(rows[ms[0]]["cpi_fred"], "", "FRED '.' must become blank")
        ledger = read(self.tmp / "data/metadata/source_ledger.csv")
        self.assertEqual([r["layer"] for r in ledger], ["rep", "check"])
        self.assertTrue(all(r["acquisition_method"] == "api" for r in ledger))
        n_tr = len(read(self.tmp / "data/metadata/transformation_log.csv"))
        self.assertEqual(self.cc.main(["--imf-file", str(imf), "--fred-file", str(fred)]), 0)
        self.assertEqual(len(read(self.tmp / "data/metadata/source_ledger.csv")), 2, "rerun adds no ledger row")
        self.assertEqual(len(read(self.tmp / "data/metadata/transformation_log.csv")), n_tr)

    def test_plain_month_periods(self):
        imf = self.tmp / "data/raw/imf/imf.csv"
        imf_csv(imf, period_style="plain")
        self.assertEqual(self.cc.main(["--imf-file", str(imf), "--no-fred"]), 0)

    def test_missing_sample_month_fails(self):
        imf = self.tmp / "data/raw/imf/imf.csv"
        imf_csv(imf, drop="2003-04")
        self.assertEqual(self.cc.main(["--imf-file", str(imf), "--no-fred"]), 1)
        self.assertFalse((self.tmp / "data/interim/cpi_monthly.csv").exists())

    def test_no_fred_key_is_a_warning_not_a_failure(self):
        imf = self.tmp / "data/raw/imf/imf.csv"
        imf_csv(imf)
        self.cc.read_env = lambda: {}                  # no .env keys
        self.assertEqual(self.cc.main(["--imf-file", str(imf)]), 0)
        self.assertEqual(len(read(self.tmp / "data/metadata/source_ledger.csv")), 1)

    def test_file_outside_raw_folder_refused(self):
        other = self.tmp / "elsewhere.csv"
        imf_csv(other)
        self.assertEqual(self.cc.main(["--imf-file", str(other), "--no-fred"]), 1)

    def test_key_never_logged(self):
        log = self.cc.RunLog("secret_test")
        log.secrets.append("abc123SECRET")
        log.info("url?api_key=abc123SECRET")
        log.finish()
        self.assertNotIn("abc123SECRET", log.path.read_text(encoding="utf-8"))

    def test_fred_key_format(self):
        ok = "0123456789abcdef0123456789abcdef"         # synthetic, 32 characters
        f = self.cc.fred_key_problem
        self.assertEqual(f(ok), "")
        self.assertIn("31 characters", f(ok[:-1]))
        self.assertIn("33 characters", f(ok + "a"))
        self.assertIn("upper-case", f(ok.upper()))
        self.assertIn("other than letters", f(ok[:-1] + "-"))
        self.assertIn("quotes", f('"' + ok + '"'))
        self.assertIn("spaces", f(ok + " "))
        self.assertIn("demonstration", f(self.cc.FRED_DEMO_KEY))
        self.assertIn("missing", f(""))
        for bad in (ok[:-1], ok.upper(), '"' + ok + '"'):
            self.assertNotIn(bad.strip('"'), f(bad), "the message must not reveal the key")

    def test_check_key_option(self):
        good = "0123456789abcdef0123456789abcdef"
        self.cc.read_env = lambda: {"FRED_API_KEY": good}
        self.assertEqual(self.cc.main(["--check-key"]), 0)
        self.assertFalse((self.tmp / "data/interim/cpi_monthly.csv").exists(), "--check-key downloads nothing")
        self.cc.read_env = lambda: {"FRED_API_KEY": good[:-2]}
        self.assertEqual(self.cc.main(["--check-key"]), 1)

    def test_bad_key_skips_fred_only(self):
        imf = self.tmp / "data/raw/imf/imf.csv"
        imf_csv(imf)
        self.cc.read_env = lambda: {"FRED_API_KEY": "tooshort"}
        self.assertEqual(self.cc.main(["--imf-file", str(imf)]), 0)
        self.assertEqual(len(read(self.tmp / "data/metadata/source_ledger.csv")), 1, "IMF only")

    def test_nbs_parse(self):
        p = self.tmp / "data/raw/nbs/nbs.csv"
        ms = nbs_csv(p)
        out, info = self.cc.parse_nbs_csv(p.read_bytes())
        self.assertEqual(len(out), len(ms))
        self.assertTrue(info["source_ok"])
        for i in (0, 10, 100):                       # newest first in the file
            self.assertAlmostEqual(out[ms[i]], 100 + (i % 7) / 10)
        self.assertEqual(sorted(out)[0], "1995-01")

    def test_nbs_wrong_label_refused(self):
        p = self.tmp / "data/raw/nbs/nbs.csv"
        nbs_csv(p, label="居民消费价格指数 (上年同期=100) ")     # the cumulative series: refused
        with self.assertRaises(ValueError):
            self.cc.parse_nbs_csv(p.read_bytes())

    def test_nbs_end_to_end_found_automatically(self):
        imf = self.tmp / "data/raw/imf/imf.csv"
        imf_csv(imf)
        nbs = self.tmp / "data/raw/nbs/nbs.csv"
        nbs_csv(nbs)
        before = nbs.read_bytes()
        self.assertEqual(self.cc.main(["--imf-file", str(imf), "--no-fred"]), 0)
        self.assertEqual(nbs.read_bytes(), before, "raw file must stay untouched")
        rows = {r["month"]: r for r in read(self.tmp / "data/interim/cpi_monthly.csv")}
        self.assertNotEqual(rows["2006-07"]["cpi_nbs_yoy"], "")
        self.assertEqual([r["layer"] for r in read(self.tmp / "data/metadata/source_ledger.csv")], ["rep", "check"])
        self.assertEqual(self.cc.main(["--imf-file", str(imf), "--no-fred"]), 0)
        self.assertEqual(len(read(self.tmp / "data/metadata/source_ledger.csv")), 2, "rerun adds no ledger row")

    def test_nbs_problem_is_only_a_warning(self):
        imf = self.tmp / "data/raw/imf/imf.csv"
        imf_csv(imf)
        nbs_csv(self.tmp / "data/raw/nbs/nbs.csv", drop="2003-04")   # a sample month missing
        self.assertEqual(self.cc.main(["--imf-file", str(imf), "--no-fred"]), 0)
        (self.tmp / "data/raw/nbs/nbs.csv").write_text("not an NBS file", encoding="utf-8")
        self.assertEqual(self.cc.main(["--imf-file", str(imf), "--no-fred"]), 0)

    def test_bad_imf_response(self):
        with self.assertRaises(ValueError):
            self.cc.parse_imf_csv(b"<html>blocked</html>")


if __name__ == "__main__":
    unittest.main()
