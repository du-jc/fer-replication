#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
knitr::opts_knit$set(root.dir = normalizePath(".."))
#
#
#
# Bibliography entries for the software cited (nocite above), written from the installed versions
knitr::write_bib(c("base", "urca", "lmtest", "sandwich", "strucchange", "car", "flextable", "knitr"),
                 file = file.path("docs", "packages.bib"))
#
#
#
# Every number in the text comes from a table written by the project's code, through the
# functions below. Nothing is typed. A missing file or row prints as [missing: ...].
options(knitr.table.format = "pipe")   # pandoc wraps long pipe tables in LaTeX and Word
knitr::opts_chunk$set(out.width = "100%")   # figures fit the text width in every format
tbl_dir <- file.path("outputs", "tables")
fig_dir <- file.path("outputs", "figures")
rd <- function(f) {
  p <- file.path(tbl_dir, f)
  if (!file.exists(p)) return(NULL)
  utils::read.csv(p, check.names = FALSE, encoding = "UTF-8", stringsAsFactors = FALSE)
}
miss <- function(what) paste0("[missing: ", what, "]")
f <- function(x, d = 2) ifelse(is.na(x), "n.i.", formatC(as.numeric(x), format = "f", digits = d, big.mark = ","))
p_txt <- function(p) ifelse(is.na(p), "n.i.", ifelse(p < 0.001, "< 0.001", paste("=", formatC(p, format = "f", digits = 3))))
stars <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "\\*\\*\\*", ifelse(p < 0.01, "\\*\\*", ifelse(p < 0.05, "\\*", ifelse(p < 0.10, "\u2020", "")))))

nums <- rd("note_numbers.csv")
num  <- function(name) { v <- nums$value[nums$name == name]; if (length(v) == 1) v else miss(name) }
numv <- function(name) suppressWarnings(as.numeric(num(name)))

# T2: reproduction counts by model and data version
T2 <- rd("note_T2_classification.csv")
cls <- function(model, version, col) { x <- T2[T2$model == model & T2$version == version, col]; if (length(x) == 1) x else miss(paste(model, version, col)) }

# T3: Durbin-Watson and Breusch-Godfrey comparisons, summed over the rows selected
T3 <- rd("note_T3_dwbg.csv")
dwbg <- function(col, test = NULL, version = NULL, convention = NULL) {
  x <- T3
  if (!is.null(test)) x <- x[x$test == test, ]
  if (!is.null(version)) x <- x[x$version == version, ]
  if (!is.null(convention)) x <- x[x$convention == convention, ]
  sum(x[[col]])
}

# T7 (long): the expectation coefficient at each correction step. step = the row number ("0", "1", "1b",
# "1c", ..., "4c", ..., "7"), matched exactly against the column step_id, so "4" does not also pick "4c";
# contains = text that tells apart rows with the same number (step 2: "wild"/"block"; step 7: the variant).
T7 <- rd("note_T7_expectation_long.csv")
tf <- function(x) toupper(as.character(x)) %in% c("TRUE", "T")
t7 <- function(model, step, period, what = "cell", contains = NULL, d = 2) {
  id <- trimws(step)
  hit <- if ("step_id" %in% names(T7)) T7$step_id == id else startsWith(T7$step, step)
  x <- T7[T7$model == model & hit & T7$period == period, ]
  if (!is.null(contains)) x <- x[grepl(contains, x$step, fixed = TRUE), ]
  if (nrow(x) != 1) return(miss(paste(model, step, period, contains)))
  if ("applies" %in% names(x) && !tf(x$applies)) return("not applicable")
  # marks: the stars column (row 0: as printed in the original, W4-28; other rows: from p), escaped for markdown
  mk <- if ("stars" %in% names(x)) gsub("*", "\\*", ifelse(is.na(x$stars), "", x$stars), fixed = TRUE) else stars(x$p)
  switch(what, coef = f(x$coef, d), se = f(x$se, d), p = p_txt(x$p), stars = mk, sign = x$sign,
         cell = if (is.na(x$coef)) "not identified" else paste0(f(x$coef, d), mk, " (SE ", f(x$se, d), ")"))
}

# T8: pooled-model Wald tests
T8 <- rd("note_T8_pooled_tests.csv")
wt <- function(model, version, type, test, regimes, what = "full") {
  x <- T8[T8$model == model & T8$version == version & T8$type == type & grepl(test, T8$test, fixed = TRUE) & T8$regimes == regimes, ]
  if (nrow(x) != 1) return(miss(paste(model, version, type, test, regimes)))
  switch(what, p = p_txt(x$p_F), full = sprintf("*F*(%d, %d) = %s, *p* %s", as.integer(x$q), as.integer(x$df2), f(x$F, 2), p_txt(x$p_F)))
}

# T12: CPI checks; T13: Period 1 variation; T5: unit roots
T12 <- rd("note_T12_cpi_check.csv")
T13 <- rd("note_T13_period1.csv")
p1 <- function(version, col, d = 3) { x <- T13[T13$version == version, col]; if (length(x) == 1) f(x, d) else miss(paste(version, col)) }
tf <- function(x) toupper(as.character(x)) %in% c("TRUE", "T")

# Relabelling check (cell level) and bootstrap summaries
RL <- rd("corr_relabel.csv")
rl_se_only_miss <- function() {
  if (is.null(RL)) return(miss("corr_relabel.csv"))
  x <- RL[!tf(RL$stars_match_if_se), ]
  if (!nrow(x)) return("none")
  paste(sprintf("Model %s, Period %d, %s", x$model, x$period, x$term), collapse = "; ")
}
BT <- rd("corr_bootstrap.csv")
# s2c: bootstrap with a stage-2 constant; by default as in the analysis (W4-20: only on the corrected data,
# s2c_of() in code/R/corrections.R)
bt <- function(version, scheme, period, col, term = "DER_hathat", d = 2, s2c = version == "corrected") {
  B <- if ("stage2_const" %in% names(BT)) BT[tf(BT$stage2_const) == s2c, ] else BT
  x <- B[B$version == version & B$scheme == scheme & B$period == period & B$term == term, col]
  if (length(x) != 1) return(miss(paste(version, scheme, period, col)))
  if (col %in% c("p_boot", "p_naive")) p_txt(x) else if (is.numeric(x)) f(x, d) else x
}
bt_min_ok <- function() if (is.null(BT)) miss("corr_bootstrap.csv") else min(BT$n_ok)
sel <- rd("selected_inputs.csv")
sel_col <- function(what) if (is.null(sel)) miss("selected_inputs.csv") else sel$selected_column[toupper(sel$input) == what]

# Verification of the extracted appendix against original Tables 3.2-3.9 (Week 2)
V3 <- rd("verify_T3_summary.csv")

# Tables: every table is drawn with flextable (HTML, Word and PDF alike), with fixed column widths
# that add up to the text width, so wide tables fit the page. Tables 1a/1b come with a status file
# (R/P/N per reproduced cell, rules in W1-07): those cells are shaded green / yellow / red.
SHADE <- c(R = "#C6EFCE", P = "#FFEB9C", N = "#FFC7CE")
# Optional: widths = relative column widths (overrides the automatic ones); size = font size;
# left = columns aligned left (default: the first only); pad = space on each side of a cell (pt). In the
# PDF, flextable ignores cell padding, so pad is passed as the column gap (tabcolsep) and the widths are
# reduced by that gap, keeping the table inside the text width.
ftab <- function(d, digits = 3, status = NULL, width_in = 6.3, widths = NULL, size = NULL, left = 1L, pad = 2) {
  if (is.null(d)) return(miss("table"))
  d <- as.data.frame(d, stringsAsFactors = FALSE)
  d$line <- NULL
  for (v in names(d)) {
    x <- d[[v]]
    if (is.numeric(x)) x <- ifelse(is.na(x), "", ifelse(abs(x - round(x)) < 1e-12, formatC(x, format = "d", big.mark = ","),
                                                   formatC(x, format = "f", digits = digits)))
    x <- as.character(x); x[is.na(x)] <- ""
    d[[v]] <- x
  }
  labels <- gsub("_", " ", names(d))
  if (!is.null(status) || identical(names(d)[1], "row")) labels[1] <- " "
  labels <- sub(" (published|reproduced)$", "\n\\1", labels)          # two-line headers in Tables 1a/1b
  if (!requireNamespace("flextable", quietly = TRUE)) {
    names(d) <- labels
    return(knitr::kable(d, align = c("l", rep("c", ncol(d) - 1)), row.names = FALSE))
  }
  ft <- flextable::theme_booktabs(flextable::flextable(d))    # horizontal rules only, as in JCRE articles
  ft <- flextable::set_header_labels(ft, values = stats::setNames(as.list(labels), names(d)))
  if (!is.null(status))
    for (cl in intersect(setdiff(names(status), c("row", "line")), names(d)))
      for (k in names(SHADE)) {
        i <- which(status[[cl]] == k)
        if (length(i)) ft <- flextable::bg(ft, i = i, j = cl, bg = SHADE[[k]])
      }
  ft <- flextable::align(ft, j = setdiff(seq_len(ncol(d)), left), align = "center", part = "all")
  ft <- flextable::align(ft, j = left, align = "left", part = "all")
  ft <- flextable::fontsize(ft, size = if (!is.null(size)) size else if (ncol(d) > 7) 7.5 else 8.5, part = "all")
  ft <- flextable::padding(ft, padding.top = 1, padding.bottom = 1, padding.left = pad, padding.right = pad, part = "all")
  # widths in proportion to the longest entry of each column (header words included), within limits
  ew <- function(x) { u <- nchar(gsub("[^A-Z0-9_]", "", x)); nchar(x) + 0.4 * u }   # capitals and digits are wider
  longest <- function(v, h) max(ew(c(v, strsplit(h, "[ \n]")[[1]])), 3)
  w <- pmin(pmax(mapply(longest, d, labels), 4), 45)
  if (!is.null(status) || all(grepl("^P[1-4]", names(d)[-1])))          # period columns: equal widths
    w <- c(min(w[1], 22), rep(max(w[-1]), ncol(d) - 1))
  if (!is.null(widths)) w <- widths
  avail <- width_in - ncol(d) * 2 * pad / 72
  ft <- flextable::width(ft, j = seq_len(ncol(d)), width = avail * w / sum(w))
  flextable::set_table_properties(ft, layout = "fixed", opts_pdf = list(tabcolsep = pad))
}
ptab <- function(name, ...) {
  d <- rd(paste0(name, ".csv"))
  if (is.null(d)) return(miss(name))
  d[] <- lapply(d, as.character)
  ftab(d, status = rd(paste0(name, "_status.csv")), ...)
}

pnotes <- function(name) {
  n <- rd(paste0(name, "_notes.csv"))
  if (is.null(n)) return(invisible())
  cat(paste0("^", n$letter, "^ ", n$note, collapse = "  \n"), "\n")
}
# Figures: path relative to docs/ (where Quarto and LaTeX look for them); error = FALSE because
# knitr would otherwise check the path from the project root, where R's working directory is
fig <- function(file) knitr::include_graphics(file.path("..", fig_dir, file), rel_path = FALSE, error = FALSE)
hv <- num("headline_version")

# T1: published and reproduced cells of Tables 1a/1b. Numbers are parsed from the cell text that
# 10_paper_tables.R writes ("coef (se)stars"); stars and spaces are dropped. what:
#   ratio  = ours / published coefficient
#   gap    = |ours - published| / published SE
#   gap100 = |ours/100 - published| / published SE  (ours rescaled by 1/100; W4-26)
#   p_orig = two-sided normal p-value of the published coefficient / published SE
T1 <- rd("note_T1_reproduction.csv")
t1 <- function(model, period, term, what) {
  x <- T1[T1$model == model & T1$period == period & T1$term == term, ]
  if (nrow(x) != 1) return(NA_real_)
  o   <- as.numeric(gsub("[^0-9.eE-]", "", x$original))
  ose <- as.numeric(x$original_se)
  m   <- regmatches(x$ours, regexec("^\\s*(-?[0-9.]+)\\s*\\(([0-9.]+)\\)", x$ours))[[1]]
  b   <- as.numeric(m[2])
  switch(what, ratio = b / o, gap = abs(b - o) / ose, gap100 = abs(b / 100 - o) / ose,
         p_orig = 2 * pnorm(-abs(o / ose)), NA_real_)
}
# The four ARIMA constants (Periods 1 and 3, both models): ratio and the gap after rescaling by 1/100
cons100 <- if (is.null(T1)) NULL else {
  k <- T1[T1$term == "_cons", c("model", "period")]
  data.frame(model = k$model, period = k$period,
             ratio  = mapply(function(m, p) t1(m, p, "_cons", "ratio"),  k$model, k$period),
             gap100 = mapply(function(m, p) t1(m, p, "_cons", "gap100"), k$model, k$period))
}
c100_list <- function(d = 2) if (is.null(cons100)) miss("note_T1_reproduction.csv") else
  paste(sprintf("Model %s, Period %d: %s", cons100$model, cons100$period, f(cons100$gap100, d)), collapse = "; ")
# Expectation term of Model I: number of periods and number where the reproduced sign differs
n_exp      <- if (is.null(T1)) miss("T1") else sum(T1$model == "I" & T1$term == "DER_hathat")
n_exp_sign <- if (is.null(T1)) miss("T1") else sum(T1$model == "I" & T1$term == "DER_hathat" & grepl("sign differs", T1$reason))

# W4-25: n and k implied by the Durbin-Watson bounds printed in original Table 4.3
DWB <- rd("check_dw_bounds.csv")
dwb <- function(period, what) {
  x <- DWB[DWB$period == period, ]
  if (nrow(x) != 1) return(miss(paste("check_dw_bounds", period, what)))
  nk <- regmatches(x$exact_match_nk, regexec("n=([0-9]+),k=([0-9]+)", x$exact_match_nk))[[1]]
  switch(what, n = nk[2], k = nk[3], n_full = x$n_full, n_within = x$n_within, k_stage1 = x$k_stage1,
         miss(what))
}

#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: tbl-periods
#| tbl-cap: "Sample periods. Source: Du, Ge and Qiao (2014); original document, Chapter 3."
source(file.path("code", "R", "common.R"))
ftab(data.frame(Period = PERIODS$period, From = PERIODS$start, To = PERIODS$end,
                        Months = sapply(PERIODS$period, function(p) sum(in_period(SAMPLE_MONTHS, p)))), width_in = 4)
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: tbl-variables
#| tbl-cap: "Variables. Source: original Appendix I; SAFE; Wind; labels corrected where the original is inaccurate (@sec-corrections)."
ftab(rd("note_T10_variables.csv"))
#
#
#
#
#
#
#
#
#
#
#
#| label: tbl-sources
#| tbl-cap: "Data sources. Coverage: first and last observation in the file as downloaded. Layer rep: used in the analysis; check: used only to verify another series. Source: data/metadata/source_ledger.csv of the replication package."
s <- rd("note_T9_sources.csv")
# Layout only; every value is the ledger's. Coverage start and end share one column, underscores
# become spaces, and long codes get break points (a space after each comma; a line break after the
# last "/"), so that every entry wraps inside its column.
brk <- function(x) {
  x <- gsub(",(?=\\S)", ", ", x, perl = TRUE)
  ifelse(nchar(x) > 16 & !grepl(" ", x), sub("/(?=[^/]*$)", "/\n", x, perl = TRUE), x)
}
# A long path inside a sentence (no spaces) gets a line break after its middle "/".
brk_words <- function(x) vapply(strsplit(x, " ", fixed = TRUE), function(w) {
  long <- nchar(w) > 16 & grepl("/.", w)
  w[long] <- vapply(w[long], function(t) {
    pos <- gregexpr("/", t)[[1]]; pos <- pos[pos < nchar(t)]
    p <- pos[which.min(abs(pos - nchar(t) / 2))]
    paste0(substr(t, 1, p), "\n", substr(t, p + 1, nchar(t)))
  }, "")
  paste(w, collapse = " ")
}, "")
ftab(data.frame(Series = s$series_name, Provider = s$provider, Code = brk(s$series_key_or_code),
                Frequency = s$frequency, Coverage = paste0(s$coverage_start, " to\n", s$coverage_end),
                Method = gsub("_", " ", s$acquisition_method), Layer = s$layer,
                Licence_or_access = brk_words(s$license_or_access_restriction)),
     widths = c(1.0, 0.98, 1.36, 0.6, 0.78, 0.72, 0.36, 0.9), size = 7.5, left = 1:8, pad = 3)
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: tbl-unitroot
#| tbl-cap: "Unit-root tests. Source: original Tables 4.1 and 4.2; own calculations."
ptab("paper_T5")
#
#
#
#| results: asis
pnotes("paper_T5")
#
#
#
#
#
#
#
#| label: tbl-repro-a
#| tbl-cap: "Published and reproduced estimates, Periods 1 and 2. Source: original Table 4.9 (= article Table 4); own estimates."
ptab("paper_T1a")
#
#
#
#| results: asis
pnotes("paper_T1a")
#
#
#
#| label: tbl-repro-b
#| tbl-cap: "Published and reproduced estimates, Periods 3 and 4. Source: original Table 4.10 (= article Table 4); own estimates."
ptab("paper_T1b")
#
#
#
#| results: asis
pnotes("paper_T1b")
#
#
#
#| label: fig-gap
#| fig-cap: "Gap between reproduced and published Model I estimates, in published standard errors (headline specification). Dashed lines: one standard error. Cells beyond the clipped axis are marked with triangles and labelled with their gap. Source: own estimates; original Tables 4.9-4.10."
fig("fig1_std_gap.png")
#
#
#
#
#
#
#
#
#
#| label: tbl-classification
#| tbl-cap: "Reproduction status by model and data version (primary convention). Model I: headline specification. Source: own classification (rules in W1-07)."
ftab(T2, digits = 2)
#
#
#
#
#
#
#
#
#
#
#
#| label: tbl-chow
#| tbl-cap: "Chow tests between adjacent periods. Source: original Table 4.11; own calculations."
ptab("paper_T6")
#
#
#
#| results: asis
pnotes("paper_T6")
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: fig-ci
#| fig-cap: "Expectation coefficient of Model I by period: 95% intervals from the conventional standard error, the wild and the block bootstrap (percentile), and the pooled model with Newey-West standard errors. Headline specification. Source: own estimates."
fig("fig2_expectation_ci.png")
#
#
#
#| label: fig-draws
#| fig-cap: "Wild-bootstrap distribution of the Model I expectation coefficient by period. Solid line: point estimate; dashed lines: conventional 95% interval. Source: own estimates."
fig("fig3_bootstrap_draws.png")
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: tbl-cpi
#| tbl-cap: "Consumer-price checks, 2000-2011. Within print: gap no larger than half a unit of the last decimal printed in the original document. Source: original Appendix I; IMF; FRED; NBS; own calculations."
ftab(T12, digits = 4)
#
#
#
#| label: fig-cpi
#| fig-cap: "Gaps between the original inflation series, the NBS published year-on-year CPI and the year-on-year rate implied by the IMF CPI index, 2000-2011. Source: original Appendix I; NBS; IMF; own calculations."
fig("fig4_cpi_gap.png")
#
#
#
#
#
#
#
#| label: tbl-period1
#| tbl-cap: "Period 1: variation of the exchange rate, its change, and the stage-3 expectation term, by data version. Source: own calculations."
ftab(T13, digits = 4)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: tbl-pooled
#| tbl-cap: "Pooled models, 2000-2011, with regime interactions and Newey-West standard errors; corrected data. Source: own estimates."
ptab("paper_T4")
#
#
#
#| results: asis
pnotes("paper_T4")
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: tbl-corrected
#| tbl-cap: "Corrected estimates. Source: own estimates."
ptab("paper_T3")
#
#
#
#| results: asis
pnotes("paper_T3")
#
#
#
#
#
#
#
#| label: tbl-steps-1
#| tbl-cap: "Model I: the expectation coefficient through the corrections. Source: original Tables 4.9-4.10; own estimates."
ptab("paper_T2a")
#
#
#
#| results: asis
pnotes("paper_T2a")
#
#
#
#| label: tbl-steps-2
#| tbl-cap: "Model II: the NDF coefficient through the corrections. Source: original Tables 4.9-4.10; own estimates."
ptab("paper_T2b")
#
#
#
#| results: asis
pnotes("paper_T2b")
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| label: tbl-decisions
#| tbl-cap: "Decisions in the author's decision log. Each entry gives the options considered, the evidence and the date."
# The log itself stays on the author's computer (W1-09); a copy without it prints a notice instead
dpath <- file.path("docs", "decisions.md")
dl <- if (file.exists(dpath)) readLines(dpath, encoding = "UTF-8", warn = FALSE) else "## W0-00 [decision log not available in this copy]"
h <- grep("^## W[0-9]+-[0-9]+ ", dl, value = TRUE)
ftab(data.frame(Decision = sub("^## (W[0-9]+-[0-9]+) .*$", "\\1", h),
                Title = sub("^## W[0-9]+-[0-9]+ ", "", h)))
#
#
#
#
