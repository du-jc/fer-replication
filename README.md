# fer-replication

Replication of Du, J., Ge, J. and Qiao, J. (2014), "Impact of Renminbi Exchange Rate Expectation on China's Foreign Exchange Reserves: An Empirical Analysis", *International Journal of Sustainable Economy*, <volume(issue)>, <pages>. <DOI or link>

**Replication note:** [`docs/replication_note.qmd`](docs/replication_note.qmd) (rendered: <link to the HTML or PDF release>)
**Status:** <e.g. "complete, version 1.0 (<date>)">
**Author of this replication:** <name, affiliation, contact>

<Two or three sentences in your own words: what was reproduced, what was corrected, where the reader finds the result. Cite numbers only from `outputs/tables/note_numbers.csv`.>

## Contents

```
fer-replication/
├── README.md, LICENSE, .gitignore, .env.example, renv.lock, requirements.txt
├── create_skeleton.ps1, init_config_and_metadata.ps1, check_history.ps1
├── config/                 original_label_map.csv (image labels, confirmed by hand), wind_request.csv
├── data/
│   ├── raw/safe/           SAFE BPM6 balance-of-payments file (public, untouched)
│   ├── raw/imf/, raw/fred/ CPI files (public, untouched)
│   ├── raw/original/       NOT in this repository (see "Data not redistributed")
│   ├── raw/proprietary/    NOT in this repository (Wind)
│   ├── interim/            NOT in this repository (rebuilt by the code)
│   └── metadata/           source_ledger.csv, transformation_log.csv, original_targets.csv
├── code/python/            extraction and collection: extract_original, collect_bop, register_wind,
│                           collect_cpi
├── code/R/                 estimation: 00_packages ... 09_outputs, with common.R, models.R, corrections.R
├── tests/                  Python unittest and R testthat tests (simulated data only)
├── outputs/tables/         every result table (statistics only)
├── outputs/figures/        figures used in the note
├── outputs/logs/           a run log for every script run, and the weekly summaries
└── docs/                   replication_note.qmd (and .pdf, .tex, .html, .docx), paper_tables.qmd, references.bib
```

## Data

| Source | Series | In this repository? | How to obtain |
|---|---|---|---|
| Original document (Appendix I, Tables 3.2-4.11) | nine monthly variables, 2000-2011; all reproduction targets | targets only (`data/metadata/original_targets.csv`); the document is not | <how a reader can obtain the original document, or "available from the authors on request"> |
| SAFE | BOP, BPM6, current account | yes (`data/raw/safe/`) | `python code/python/collect_bop.py` |
| Wind (subscription) | 12-month USD/CNY NDF, daily | **no** (licence) | a Wind terminal; series code and name in `data/metadata/source_ledger.csv`; save the export in `data/raw/proprietary/` and run `python code/python/register_wind.py` |
| IMF data portal | CPI, all items, index | yes (`data/raw/imf/`) | `python code/python/collect_cpi.py` |
| FRED (OECD MEI) | CPI, all items, index (cross-check) | yes (`data/raw/fred/`) | `python code/python/collect_cpi.py` with a free `FRED_API_KEY` in `.env` |

Every raw file has a row in `data/metadata/source_ledger.csv` (provider, code, download date, coverage, licence, reading instructions); every conversion has a row in `data/metadata/transformation_log.csv`.

**Data not redistributed.** <Your wording: the original document and the Wind export are not included; what a reader without them can and cannot rerun.>

## How to run

Windows 10/11, Command Prompt, from the project root. Requirements: Python 3.11+, R 4.x, Git, Quarto.

```
python -m venv .venv
.venv\Scripts\activate
pip install -r requirements.txt
Rscript -e "renv::restore()"

python -m unittest discover -s tests -v
Rscript -e "testthat::test_dir('tests/testthat')"

python code\python\extract_original.py
python code\python\collect_bop.py
python code\python\register_wind.py
python code\python\collect_cpi.py
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
quarto install tinytex
quarto render docs\replication_note.qmd
```

Each script writes a log to `outputs/logs/`. <Approximate total run time on your machine.>

## Results

<Pointer to the note and to the key tables: `outputs/tables/note_T7_expectation.csv` (the expectation coefficient through every correction), `note_T1_reproduction.csv`, `note_T2_classification.csv`.>

## Decisions

Every choice made during the replication is recorded in a numbered decision log (W1-01 onward) with the options considered and the evidence; code comments and the replication note cite these numbers, and Appendix A of the note lists them. <W1-09: the log itself is not in this repository; state whether a cleaned version is published or available on request.>

## Licence

Code: <licence, e.g. MIT>. Public data files keep the terms of their providers (see `license_or_access_restriction` in the ledger).

## Citation

<How to cite this replication; optionally a CITATION.cff file.>
