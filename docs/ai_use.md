# Use of AI tools in this replication

*Replication of Du, Ge and Qiao (2014), "Impact of renminbi exchange rate expectation on China's foreign exchange reserves: an empirical analysis" (Study A). Disclosure statement, part of the replication package (`docs/ai_use.md`).*

------------------------------------------------------------------------

## 1. Summary

The author used one AI tool, **Claude Code** (Anthropic), throughout this replication, from September to October 2026. It was used to draft planning documents, write the data-collection and estimation code, propose options for analytic decisions, and draft much of the text of the replication note. It was used under a fixed rule: **the AI did not supply any data value, estimate or result.** Every number in the replication note is produced by code that reads the original sources, and the author ran that code locally. Every analytic decision was taken by the author and recorded in a dated decision log before the estimates it governs were computed. The author is responsible for the code, the decisions, the results and the text.

**This report is not intended for submission or publication.** Much of its text was drafted by the AI tool. Its purpose is to carry out and document a full replication and reproducibility procedure using AI tools (section 2).

## 2. Purpose of the replication and the role of AI in it

This replication was carried out mainly to conduct a full replication and reproducibility procedure: rebuilding a published empirical analysis from its original data and documents, with every step done by code, recorded and checkable by others. A central part of that purpose was to carry out the procedure with AI tools. The project is therefore also a test of whether an AI-assisted workflow, under fixed rules and with the author in control of every decision, can produce a replication that is reproducible and auditable. The rules and safeguards below are part of that procedure, not only a description of how the tool was used.

For that reason, the replication note is a record of the procedure and its results, not a manuscript. It is not intended for submission to a journal or for publication, because much of its text was drafted by the AI tool rather than written by the author. Its numbers, however, are not AI-drafted: every one is produced by the author's runs of the code, as described below. Any later version prepared for submission would have to be rewritten by the author and would carry its own disclosure.

## 3. What the AI did and what the author did

| Task | AI | Author |
|---|---|---|
| Planning documents (Replication Plan, Code and Data Blueprint) | Drafted on request | Set the questions and scope, revised and approved |
| Data collection code (Python) and estimation code (R) | Wrote the code and the tests | Ran all code locally, reported errors, reviewed and committed it to git |
| Data | None supplied | Provided the original document; obtained the Wind NDF export; ran the scripts that download the public data (SAFE, IMF, FRED, NBS) |
| Analytic decisions (data series, specifications, classification rules, corrections) | Proposed options, with the evidence for each | Chose, dated and recorded each decision in the decision log |
| The classification rule for "reproduced" (W1-07) | — | Defined by the author |
| Diagnosing results and errors | Read result tables and run logs uploaded by the author, and proposed explanations | Accepted or rejected each explanation; post-estimation diagnoses are declared as such in the note |
| Text of the replication note | Drafted much of the text, with every number inserted by code at render time | Reviewed, edited and approved the text |

## 4. Rules under which the AI was used

The following rules were set by the author in the instructions of the AI project and applied throughout:

1. The AI does not supply, recall or type data values, results or conclusions. All values come from code reading the sources.
2. Python collects the data; R estimates. Every script writes a run log.
3. When the author reports an error, only the affected function is changed.
4. Web searches by the AI are limited to confirming data endpoints, series codes and software package details.
5. Where the planning documents are ambiguous, the AI asks the author before writing code.
6. Interpretations in the note are marked as interpretations ("Our interpretation suggests …").

## 5. Safeguards

The safeguards serve two purposes: to catch errors in AI-written code or text, and to prevent the replication from being steered towards a preferred result. The second matters because the author is the first author of the original article.

- **Decisions recorded before estimation.** Every rule that selects data, specifications or classifications was written in the decision log, dated, and committed to git before the estimates it governs were run. Where a choice was changed after estimation, the change, its trigger and its timing are declared in the note.
- **Checks in the code.** Scripts verify their own inputs and outputs and log the result (for example, completeness of downloaded series, equality of R and Stata-equivalent estimators, and equality of the pooled and per-period estimates where they must agree). Unit tests cover the estimation functions.
- **Determinism.** The bootstrap uses a fixed seed. Two complete runs produced identical result files, verified by file hashes.
- **No typed numbers in the text.** Every number in the note is read from the result tables by code when the note is rendered. Words that state a result (for example "significant" or "rejects") are also computed from the tables where possible.
- **Author review.** The author ran every script, read every log, and checked the rendered note against the result tables.

## 6. Errors made by the AI

The AI made errors during the project. They included mistakes in describing the source documents, incomplete hypotheses, and inaccurate statements about the timing of a decision. All were found by the author's review or by the checks above, and corrected in the documents concerned. Errors in code were caught by the scripts' checks and tests, by failed runs, or by the author, and fixed function by function. No AI-proposed explanation enters the note without being marked as an interpretation and supported by output of the code.

## 7. Materials shared with the AI

- The original document on which the article is based, an unpublished work of the original authors, and the published article.
- The planning documents and the decision log.
- Result tables, run logs and the source of the replication note, as produced by the author's runs.



## 8. Records

The decision log, the run logs and the git history document each step and are available on request. The code and the public data are in the replication package.

------------------------------------------------------------------------

