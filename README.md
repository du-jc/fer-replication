# fer-replication

**A replication of Du, Ge and Qiao (2014), "Impact of renminbi exchange rate expectation on China's foreign exchange reserves: an empirical analysis", *International Journal of Sustainable Economy*.**

This repository rebuilds the article's analysis of how expectations about the renminbi–US dollar exchange rate relate to China's foreign exchange reserves, 2000–2011. It does this from the original data and documents, with every step done by code. It then corrects the problems found in the original analysis, one at a time, and reports how each correction changes the results.

> **Purpose and status.** This is a record of a full replication and reproducibility procedure carried out with AI tools (Claude Code, Anthropic). It is **not intended for submission or publication**: much of the text of the replication note was drafted by the AI tool. The AI supplied no data value, estimate or result. Every number is produced by the code in this repository, run by the author. See [`docs/ai_use.md`](docs/ai_use.md).
>
> Study A is complete at tag **`studyA-v1.0`**. A separate new study with data for 2000–2025 (Study B) is in a different repository.

- **Replication note:** [`docs/replication_note.pdf`](docs/replication_note.pdf) (source: [`docs/replication_note.qmd`](docs/replication_note.qmd); to render it yourself, see "How to run", step 4)
- **Status:** complete, version 1.0 (tag `studyA-v1.0`, [DATE OF W4-39])
- **Author of this replication:** Jiancheng Du

------------------------------------------------------------------------

## Contents

```
fer-replication/
├── README.md, LICENSE, .gitignore, .env.example, .Renviron
├── renv.lock, requirements.txt            # pinned R and Python environments
├── create_skeleton.ps1, init_config_and_metadata.ps1, check_history.ps1
├── config/
│   ├── original_label_map.csv             # labels of equation images in the original tables
│   └── wind_request.csv                   # record of the Wind NDF request
├── data/
│   ├── raw/                               # source files, untouched; public sources included
│   │   ├── original/                      #   NOT in the repository (see Data)
│   │   └── proprietary/                   #   NOT in the repository (see Data)
│   ├── interim/                           # NOT in the repository; rebuilt by the code
│   └── metadata/                          # source_ledger.csv, transformation_log.csv, original_targets.csv
├── code/
│   ├── python/                            # collection and extraction
│   └── R/                                 # estimation, tests, tables
├── outputs/
│   ├── tables/                            # every result table (CSV)
│   ├── figures/
│   └── logs/                              # one run log per script run
├── docs/
│   ├── replication_note.qmd               # the replication note (Quarto source)
│   ├── replication_note.pdf               # the rendered note
│   ├── references.bib
│   ├── decisions_index.csv                # number, title, date, status of each decision (Appendix A)
│   └── ai_use.md                          # AI-use disclosure
└── tests/
```

**Python collects; R estimates.**

| Script | What it does |
|---|---|
| `code/python/extract_original.py` | Extracts Appendix I (the data) and every test and estimation table (the reproduction targets) from the original document |
| `code/python/register_wind.py` | Checks the Wind NDF export, records it in the source ledger, and writes the monthly series |
| `code/python/collect_bop.py`, `collect_cpi.py` | Download the SAFE balance of payments and the IMF, FRED and NBS consumer price data, and record each vintage in the ledger |
| `code/python/compare_runs.py` | Compares the result tables of two complete runs (reproducibility test) |
| `code/python/make_decisions_index.py` | Writes `docs/decisions_index.csv` (number, title, date and status of each decision) from the private decision log, for Appendix A of the note |
| `code/R/01_import_verify.R` | Builds the analysis panel and verifies the extracted data against the original's descriptive tables |
| `code/R/02_unit_roots.R` | DF-GLS and ADF unit-root tests |
| `code/R/03_select_bop_ndf.R` | Selects the BOP and NDF series by a rule fixed before estimation |
| `code/R/04_model_I.R`, `05_model_II.R` | The article's two models, per period, for every undocumented specification choice |
| `code/R/06_diagnostics_chow.R` | Durbin–Watson, Breusch–Godfrey and Chow tests |
| `code/R/07_classify.R` | Classifies every estimate as reproduced, partially reproduced or not reproduced |
| `code/R/08_corrections.R` | The corrections: data, bootstrap of the generated regressor, pooled model with Newey–West tests |
| `code/R/09_outputs.R`, `10_paper_tables.R` | Tables and figures for the note, and `note_numbers.csv` |
| `code/R/check_dw_bounds.R`, `check_deflate_scope.R` | Two diagnostic checks cited in the note |
| `code/R/common.R`, `models.R`, `corrections.R` | Shared functions |

------------------------------------------------------------------------

## Data

| Source | Content | In this repository? |
|---|---|---|
| Original document (unpublished, by the article's authors) | Appendix I: nine monthly variables, 2000–2011; Tables 3.2–4.11 | **No.** It is unpublished work of the original authors. `original_targets.csv` (the numbers read from its tables) is [included / not included; W4-35]. |
| Wind, 12-month USD/CNY non-deliverable forward | Daily quotes, Wind code `G0008386` | **No.** Proprietary. |
| SAFE | Quarterly current account, BPM6 presentation | Yes, with release date and checksum |
| IMF | Consumer price index, China (fixed base) | Yes |
| FRED | Consumer price index, China (check only) | Yes |
| NBS | Consumer price index, same month of the previous year = 100 (check only) | Yes |

Every file is listed in `data/metadata/source_ledger.csv`, with its source, download date, checksum and licence.

**Obtaining the two inputs that are not included.**

- **Original document.** Request it from the author, Jiancheng Du (see Contact). Save it as `data/raw/original/original_document.docx`. [Keep this sentence only if the co-authors agree to share the document on request; W4-36.]
- **Wind NDF.** Export from a Wind terminal the daily series `G0008386` (无本金交割远期外汇(NDF):美元兑人民币:12个月) covering at least December 1999 to December 2011, as an `.xlsx` file, unchanged. Save it in `data/raw/proprietary/`.

**Without the Wind export the pipeline does not run:** the first R script stops, because the NDF is part of the analysis panel.

------------------------------------------------------------------------

## Requirements

- Windows (the commands below use Command Prompt), Git, VS Code (optional)
- R 4.5 with `renv`; Python 3.13 (the version used; 3.11 at least); Quarto with a LaTeX engine (for the PDF of the note)
- Package versions are pinned in `renv.lock` and `requirements.txt`.

------------------------------------------------------------------------

## How to run

From the repository root:

**1. Environment**

```
python -m venv .venv
.venv\Scripts\python -m pip install -r requirements.txt
Rscript -e "renv::restore(prompt = FALSE)"
```

If the CRAN mirror recorded in `renv.lock` is unreachable, restore from another mirror. The package versions still come from the lockfile:

```
set RENV_CONFIG_REPOS_OVERRIDE=https://cloud.r-project.org
Rscript -e "renv::restore(prompt = FALSE)"
```

**2. Add the two inputs** (see Data). Then extract and register them:

```
.venv\Scripts\python code\python\extract_original.py
.venv\Scripts\python code\python\register_wind.py --unit yuan --frequency daily
.venv\Scripts\python code\python\collect_bop.py --file SAFE_BOP_BPM6_timeseries_release20260929_dl20261006_9922402b.xlsx
.venv\Scripts\python code\python\collect_cpi.py --imf-file IMF_CPI_CHN.CPI._T.IX.M_dl20261006_a747a112.csv --fred-file [FRED FILE NAME] --nbs-file NBS_CPI_yoy_monthly_199801-201512.csv
```

**3. Run the analysis, in this order**

```
Rscript code\R\01_import_verify.R
Rscript code\R\02_unit_roots.R
Rscript code\R\03_select_bop_ndf.R
Rscript code\R\04_model_I.R
Rscript code\R\05_model_II.R
Rscript code\R\06_diagnostics_chow.R
Rscript code\R\07_classify.R
Rscript code\R\08_corrections.R
Rscript code\R\09_outputs.R
Rscript code\R\10_paper_tables.R
Rscript code\R\check_dw_bounds.R
Rscript code\R\check_deflate_scope.R
```

**4. Render the note**

```
quarto render docs\replication_note.qmd --to pdf
```

**Notes on the run:**

- Each script writes a log to `outputs/logs/` that ends in a status: OK, WARN or FAIL. Check it before running the next script. A WARN with `errors=0` is expected where the warnings are known: `01_import_verify.R` warns about the cells of the original's descriptive tables that do not match, which the note reports.
- The bootstrap in `08_corrections.R` uses a fixed seed, so reruns give identical results. It is also the slowest step.
- `collect_bop.py` and `collect_cpi.py` convert the stored public raw files into the monthly series in `data/interim/`, so they must run before `01_import_verify.R`. With the file options shown, they process the files in the package and download nothing. Without them, they fetch the current release, which may be a later vintage.
- The note takes every number from the result tables when it renders. No number in its text is typed by hand.

**Reproducibility check.** A complete run from a fresh clone, with only the two inputs added, reproduced every result table of the author's run, as compared by `compare_runs.py` (decision W4-38). To repeat it:

```
python code\python\compare_runs.py --ref <first run> --new <second run>
```

------------------------------------------------------------------------

## Documentation

- **`docs/replication_note.pdf`** (source `docs/replication_note.qmd`): the replication note. It covers the reproduction, the corrections, their effect on the conclusions, and how to reproduce every result.
- **`docs/ai_use.md`**: how AI tools were used, and the rules and safeguards that applied.
- **Decision log.** Every analytic choice (data series, specifications, classification rules, corrections) was recorded, dated and committed before the estimates it governs. The note's Appendix A lists the decisions by number and title, from `docs/decisions_index.csv`. The log itself is not in the repository and is available on request.
- **Run logs** in `outputs/logs/` and the git history document each step.

------------------------------------------------------------------------

## Licence

- **Code:** MIT (see `LICENSE`).
- **Text and figures** (the replication note, `outputs/figures/`): CC BY 4.0.
- **Data files** keep their providers' terms, as recorded in `data/metadata/source_ledger.csv`.

## Citation

**The original article:**

> Du, J., Ge, J. and Qiao, J. (2014). Impact of renminbi exchange rate expectation on China's foreign exchange reserves: an empirical analysis. *International Journal of Sustainable Economy*, 6(4), 327–344.

**This replication:**

> Du, J. (2026). Renminbi exchange-rate expectations and China's foreign exchange reserves: a replication of Du, Ge and Qiao (2014). Replication package, version studyA-v1.0. https://github.com/du-jc/fer-replication

## Contact

Jiancheng Du, jiancheng.du@okstate.edu. The author of this replication is the first author of the original article.
