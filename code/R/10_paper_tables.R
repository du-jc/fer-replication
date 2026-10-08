# code/R/10_paper_tables.R -- Week 4, Study A (Replication Plan s.4-s.5; JCRE guidelines for replications)
#
# Builds the tables of the replication note in the layout of a published paper: coefficients with
# significance marks, standard errors in parentheses on the line below, one column per period and
# model, and footnotes marked with letters. It estimates nothing: every number is read from a
# table written by 04-09 and only formatted. Following the JCRE guidelines, the original and the
# reproduced estimates stand side by side, footnotes use letters, and no square brackets appear.
#
#   paper_T1a / T1b  Reproduction of original Tables 4.9 / 4.10 (published | reproduced, per period
#                    and model), headline Model I specification, primary convention
#   paper_T2a / T2b  The expectation coefficient through the corrections (Model I / Model II)
#   paper_T3         Corrected estimates (version 'corrected', level form), original estimators,
#                    with the wild-bootstrap SE of the expectation term on a third line
#   paper_T4         Pooled 2000-2011 models with regime interactions, Newey-West, and the tests
#   paper_T5         Unit-root tests: original DF-GLS, our DF-GLS, ADF
#   paper_T6         Chow tests: published, classic in differences, in levels, robust Wald
# Each table: outputs/tables/paper_T*.csv, with its footnotes in paper_T*_notes.csv (letter, text).
# docs/paper_tables.qmd renders all of them (HTML and Word) for checking.
#
# Usage:   Rscript code/R/10_paper_tables.R

source(file.path("code", "R", "common.R"))
source(file.path("code", "R", "models.R"))
source(file.path("code", "R", "corrections.R"))

# Significance marks: the legend of original Tables 4.9-4.10, read from their notes (W1-07)
sig_note <- function() sprintf(paste("Significance (legend of original Tables 4.9-4.10): \u2020 p < %s, * p < %s, ** p < %s, *** p < %s",
                                     "(two-sided; normal for the ARIMA periods, t for OLS)."),
                               STAR_CUTS[["dagger"]], STAR_CUTS[["one"]], STAR_CUTS[["two"]], STAR_CUTS[["three"]])
PERIOD_NOTE <- paste("Periods:", paste(sprintf("%d = %s to %s", PERIODS$period, PERIODS$start, PERIODS$end),
                                       collapse = "; "), "(Replication Plan s.3.3).")
TERM_ORDER <- c("DER_hathat", "DNDF", "DGDP", "DNX", "DFDB", "DFDI", "_cons", "L.ar")
TERM_LABEL <- c(DER_hathat = "Expected DER (stage 3)", DNDF = "DNDF", DGDP = "DGDP", DNX = "DNX",
                DFDB = "DFDB", DFDI = "DFDI", `_cons` = "Constant", L.ar = "AR(1)")

rd <- function(name, lg, required = TRUE) {
  p <- file.path(PATHS$tables, name)
  if (!file.exists(p)) {
    if (required) stop(p, " not found: run the script that writes it first")
    lg$warn(p, " not found; the table that needs it is skipped"); return(NULL)
  }
  d <- read_chr_csv(p)
  for (v in intersect(c("period", "lag", "coef", "se", "p", "n", "target", "target_se", "regime", "se_nw", "p_nw",
                        "se_boot", "p_boot", "q", "F", "df2", "p_F", "nw_lag", "dfgls_tau", "original_tau",
                        "dfgls_cv5", "adf_tau", "adf_cv5", "k"), names(d))) d[[v]] <- to_num(d[[v]])
  d
}
p_txt <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "< 0.001", paste("=", sprintf("%.3f", p))))
STAR_CUTS <- DEFAULT_CUTS        # replaced in main() by the legend of original Tables 4.9-4.10 (W1-07)
mark <- function(p) ifelse(is.na(p), "", sig_label(sig_level(p, STAR_CUTS)))
num_d <- function(x, d) {                         # d may differ per element (decimals as printed)
  d <- rep_len(as.integer(d), length(x))
  ifelse(is.na(x), "", sprintf("%.*f", d, x))
}
decs_of <- function(txt) { k <- printed_decimals(gsub("[^0-9.-]", "", txt)); ifelse(k > 0, k, 3L) }
out_table <- function(df, notes, name, lg) {
  write_table(df, paste0(name, ".csv"), lg)
  write_table(data.frame(letter = letters[seq_along(notes)], note = notes, stringsAsFactors = FALSE),
              paste0(name, "_notes.csv"), lg)
}

# Two lines per term: coefficient with marks, then the SE in parentheses
stack_cells <- function(cells, keys_rows, cols) {
  # cells: data.frame(term, col, coef_txt, se_txt); returns rows = term x {coef, se}
  out <- list()
  for (tm in keys_rows) {
    r1 <- c(row = TERM_LABEL[[tm]] %||% tm, line = "coefficient")
    r2 <- c(row = "", line = "se")
    for (cl in cols) {
      x <- cells[cells$term == tm & cells$col == cl, , drop = FALSE]
      r1[[cl]] <- if (nrow(x)) x$coef_txt[1] else ""
      r2[[cl]] <- if (nrow(x) && nzchar(x$se_txt[1])) paste0("(", x$se_txt[1], ")") else ""
    }
    if (any(nzchar(r1[cols]))) out <- c(out, list(r1, r2))
  }
  as.data.frame(do.call(rbind, out), stringsAsFactors = FALSE)
}
`%||%` <- function(a, b) if (is.null(a) || !length(a) || is.na(a)) b else a

# Classification of each reproduced cell (R/P/N), in the same shape as the stacked table:
# the coefficient line and the SE line of a term get the class of that term's cell (W1-07)
status_matrix <- function(tab, x, cols) {
  code <- function(cl) ifelse(grepl("^not", cl), "N", ifelse(grepl("^partial", cl), "P", ifelse(grepl("^reproduced", cl), "R", "")))
  st <- tab
  st[, cols] <- ""
  term <- NA_character_
  for (i in seq_len(nrow(tab))) {
    if (tab$line[i] == "coefficient") term <- names(TERM_LABEL)[match(tab$row[i], TERM_LABEL)]
    for (cl in grep("_reproduced$", cols, value = TRUE)) {
      pp <- as.integer(substr(cl, 2, 2)); mm <- strsplit(cl, "_")[[1]][2]
      k <- which(x$term == term & x$period == pp & x$model == mm)
      if (length(k) == 1 && nzchar(tab[[cl]][i])) st[[cl]][i] <- code(x$class[k])
    }
  }
  st
}

# ---- T1: reproduction side by side -----------------------------------------------------------------
table_reproduction <- function(h, lg) {
  cc <- rd("classify_cells.csv", lg)
  cc <- cc[cc$convention == CONVENTIONS[1] & cc$version == h$version &
             ((cc$model == "I" & cc$spec == h$spec & cc$lag == h$lag) | cc$model == "II"), ]
  nn <- rd("classify_N.csv", lg, required = FALSE)
  for (part in list(list(id = "a", per = 1:2, tbl = "4.9"), list(id = "b", per = 3:4, tbl = "4.10"))) {
    x <- cc[cc$period %in% part$per, ]
    cols <- as.vector(t(outer(paste0("P", part$per), c("I", "II"), paste, sep = "_")))
    cols <- as.vector(t(outer(cols, c("published", "reproduced"), paste, sep = "_")))
    d <- decs_of(x$target_text)
    cells <- rbind(
      data.frame(term = x$term, col = paste0("P", x$period, "_", x$model, "_published"),
                 coef_txt = trimws(x$target_text), se_txt = num_d(x$target_se, d), stringsAsFactors = FALSE),
      data.frame(term = x$term, col = paste0("P", x$period, "_", x$model, "_reproduced"),
                 coef_txt = ifelse(x$status == "estimated", paste0(num_d(x$coef, d), mark(x$p)), "n.i."),
                 se_txt = ifelse(x$status == "estimated", num_d(x$se, d), ""), stringsAsFactors = FALSE))
    tab <- stack_cells(cells, intersect(TERM_ORDER, unique(x$term)), cols)
    st <- status_matrix(tab, x, cols)
    nrow_n <- c(row = "N", line = "n")
    for (cl in cols) {
      pp <- as.integer(substr(cl, 2, 2)); mm <- strsplit(cl, "_")[[1]][2]; kind <- strsplit(cl, "_")[[1]][3]
      if (kind == "reproduced") {
        v <- unique(x$n[x$period == pp & x$model == mm]); nrow_n[[cl]] <- if (length(v)) as.character(v[1]) else ""
      } else if (!is.null(nn) && all(c("model", "period", "target") %in% names(nn))) {
        v <- unique(nn$target[nn$model == mm & to_num(nn$period) == pp]); nrow_n[[cl]] <- if (length(v)) as.character(v[1]) else ""
      } else nrow_n[[cl]] <- ""
    }
    est <- c(row = "Estimator", line = "estimator")
    for (cl in cols) est[[cl]] <- if (as.integer(substr(cl, 2, 2)) %in% ARIMA_PERIODS) "ARIMA" else "OLS"
    tab <- rbind(tab, as.data.frame(t(nrow_n), stringsAsFactors = FALSE), as.data.frame(t(est), stringsAsFactors = FALSE))
    st <- rbind(st, st[rep(NA_integer_, 2), , drop = FALSE])
    st[nrow(st) - 1:0, ] <- ""
    write_table(st, paste0("paper_T1", part$id, "_status.csv"), lg)
    notes <- c(
      sprintf("Published: original Table %s as printed (marks as printed); the numbers in parentheses are those printed below each coefficient, which the article labels t-statistics (see the section 'Standard errors labelled as t-statistics').", part$tbl),
      sprintf("Reproduced: our estimates, Model I with the %s stage-2 regressor and %d lag(s) on %s data (rank 1 under the pre-specified rule W3-02; result W3-07); Model II on the same data; differences on the %s convention. Standard errors in parentheses: OPG for ARIMA periods, OLS otherwise.",
              h$spec, h$lag, h$version, CONVENTIONS[1]),
      "n.i. = not identified: the regressor has no variation in that period (the exchange rate was pegged in Period 1).",
      "Estimator: ARIMA = regression in first differences with AR(1) errors (ARIMA(1,1,0) in levels), exact maximum likelihood; OLS otherwise.",
      "Our coefficients are rounded to the decimals printed in the original.",
      "Shading of the reproduced estimates (classification W1-07, on sign and star level only; significant = p < 0.10): green = reproduced (same sign and same star level, or both insignificant); yellow = partially reproduced (same sign, both significant, different star level); red = not reproduced (significance flips, signs differ with either significant, or not identified).",
      sig_note(), PERIOD_NOTE)
    out_table(tab, notes, paste0("paper_T1", part$id), lg)
  }
}

# ---- T2: expectation coefficient through the corrections -------------------------------------------
table_steps <- function(lg) {
  L <- rd("note_T7_expectation_long.csv", lg)
  for (mdl in intersect(c("I", "II"), unique(L$model))) {
    x <- L[L$model == mdl, ]
    steps <- unique(x$step)
    rows <- list()
    for (st in steps) {
      y <- x[x$step == st, ]
      r1 <- c(step = st, line = "coefficient"); r2 <- c(step = "", line = "se")
      for (p in PERIODS$period) {
        k <- which(y$period == p)
        na <- "applies" %in% names(y) && !as_lgl(y$applies[1])           # step absent for this model (Model II: 1c, 4c)
        r1[[paste0("P", p)]] <- if (na) "\u2014" else if (length(k) && !is.na(y$coef[k])) paste0(num_d(y$coef[k], 3), mark(y$p[k])) else "n.i."
        r2[[paste0("P", p)]] <- if (!na && length(k) && !is.na(y$se[k])) paste0("(", num_d(y$se[k], 3), ")") else ""
      }
      rows <- c(rows, list(r1, r2))
    }
    tab <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
    notes <- c(
      sprintf("Coefficient on %s in each period (step 7: in each regime of the pooled model), standard error below in parentheses.",
              if (mdl == "I") "the stage-3 expected change of the exchange rate (Model I)" else "DNDF (Model II)"),
      "Step 0: original, with the numbers in parentheses read as standard errors. Step 1: our reproduction (headline data version). Step 1b: the same on raw data. Steps 3-6 add one correction at a time (README_week4 s.2.1): 1b to 3 isolates the GDP correction, 3 to 4 the fixed-base CPI.",
      if (mdl == "I") "Step 1c is a diagnostic, not a correction (decision W4-19): the reproduction of step 1 re-estimated with a constant in stage 2, to test whether the original's stage 2 had one; the reproduction itself (step 1) is unchanged. Step 4c is a correction outside Plan s.5 (decision W4-20): step 4 with a constant in stage 2; for Model I, steps 5, 6 and the pooled model on the corrected data build on 4c." else "Steps 1c and 4c (a stage-2 constant, decisions W4-19 and W4-20) concern stage 2 of Model I; Model II has no stage 2, so they do not apply (\u2014).",
      if (mdl == "I") "Steps 2 and 5: bootstrap standard error of the whole three-stage chain (999 replications; wild: Rademacher; block: moving blocks within periods); marks from the centred bootstrap p-value." else "No bootstrap for Model II: its expectation regressor is observed, not generated.",
      "Step 6: coefficients are elasticities (log differences of FER and of the exchange-rate terms). Step 7: pooled OLS 2000-2011 with regime-specific constants and expectation terms; full = all coefficients regime-specific; exp_only = controls common; Newey-West standard errors.",
      "n.i. = not identified.", sig_note(), PERIOD_NOTE)
    out_table(tab, notes, paste0("paper_T2", if (mdl == "I") "a" else "b"), lg)
  }
}

# ---- T3: corrected estimates, full models ----------------------------------------------------------
table_corrected <- function(h, lg) {
  pe <- keep_s2c(rd("corr_period_estimates.csv", lg))       # Model I with a stage-2 constant (W4-20)
  bt <- keep_s2c(rd("corr_bootstrap.csv", lg))
  x <- pe[pe$version == "corrected" & pe$form == "level" & pe$convention == CONVENTIONS[1], ]
  cols <- as.vector(t(outer(paste0("P", PERIODS$period), c("I", "II"), paste, sep = "_")))
  rows <- list()
  for (tm in intersect(TERM_ORDER, unique(x$term))) {
    r1 <- c(row = TERM_LABEL[[tm]], line = "coefficient"); r2 <- c(row = "", line = "se")
    r3 <- c(row = "", line = "bootstrap se")
    for (cl in cols) {
      pp <- as.integer(substr(cl, 2, 2)); mm <- sub("^P[1-4]_", "", cl)
      y <- x[x$model == mm & x$period == pp & x$term == tm, ]
      est <- nrow(y) && y$status[1] == "estimated"
      r1[[cl]] <- if (!nrow(y)) "" else if (est) paste0(num_d(y$coef[1], 3), mark(y$p[1])) else "n.i."
      r2[[cl]] <- if (est) paste0("(", num_d(y$se[1], 3), ")") else ""
      b <- bt[bt$version == "corrected" & bt$scheme == BOOT_SCHEMES[1] & bt$period == pp & bt$term == tm, ]
      r3[[cl]] <- if (mm == "I" && tm == "DER_hathat" && nrow(b)) paste0("(", num_d(b$se_boot[1], 3), ")", mark(b$p_boot[1])) else ""
    }
    rows <- c(rows, list(r1, r2), if (any(nzchar(r3[cols]))) list(r3))
  }
  nrow_n <- c(row = "N", line = "n")
  for (cl in cols) {
    pp <- as.integer(substr(cl, 2, 2)); mm <- sub("^P[1-4]_", "", cl)
    v <- unique(x$n[x$model == mm & x$period == pp]); nrow_n[[cl]] <- if (length(v)) as.character(v[1]) else ""
  }
  tab <- as.data.frame(do.call(rbind, c(rows, list(nrow_n))), stringsAsFactors = FALSE)
  notes <- c(
    "Data version 'corrected': discrete quarterly GDP; FER, GDP, NX, FDI, FDB and BOP divided by the IMF consumer price index (2000 = 100); exchange rates, inflation and the interest rate not deflated.",
    sprintf("Model I: %s stage-2 regressor, %d lag(s) (the headline specification)%s; same estimators as the original (ARIMA(1,0,0) by ML in Periods 1 and 3, OLS in Periods 2 and 4).",
            h$spec, h$lag, if (s2c_of("corrected")) ", with a constant in stage 2 (step 4c, a correction outside Plan s.5; decision W4-20)" else ""),
    "Second line: conventional standard error (OPG in the ARIMA periods). Third line, expectation term of Model I only: wild-bootstrap standard error of the whole three-stage chain (999 replications), marks from the centred bootstrap p-value.",
    "n.i. = not identified (no variation of the exchange rate under the Period 1 peg when it is not deflated).", sig_note(), PERIOD_NOTE)
  out_table(tab, notes, "paper_T3", lg)
}

# ---- T4: pooled models and tests -------------------------------------------------------------------------
table_pooled <- function(lg) {
  pc <- keep_s2c(rd("corr_pooled_coefs.csv", lg))          # Model I with a stage-2 constant (W4-20)
  pt <- keep_s2c(rd("corr_pooled_tests.csv", lg))
  pc <- pc[pc$version == "corrected" & pc$form == "level" & pc$status == "estimated", ]
  pt <- pt[pt$version == "corrected" & pt$form == "level" & pt$nw == "rule", ]
  cols <- c("I_full", "I_exp_only", "II_full", "II_exp_only")
  ev <- c(I = "DER_hathat", II = "DNDF")
  rows <- list()
  add2 <- function(label, getter) {
    r1 <- c(row = label, line = "coefficient"); r2 <- c(row = "", line = "se")
    for (cl in cols) {
      mm <- sub("_.*$", "", cl); ty <- sub("^[I]+_", "", cl)
      y <- getter(mm, ty)
      r1[[cl]] <- if (nrow(y)) paste0(num_d(y$coef[1], 3), mark(y$p_nw[1])) else ""
      r2[[cl]] <- if (nrow(y)) paste0("(", num_d(y$se_nw[1], 3), ")") else ""
    }
    rows <<- c(rows, list(r1, r2))
  }
  for (r in PERIODS$period)
    add2(sprintf("Expectation term, regime %d", r), function(mm, ty) pc[pc$model == mm & pc$type == ty & pc$term == ev[[mm]] & pc$regime %in% r, ])
  for (v in c("DGDP", "DNX", "DFDB", "DFDI"))
    add2(sprintf("%s (common)", v), function(mm, ty) pc[pc$model == mm & pc$type == ty & pc$term == v & is.na(pc$regime), ])
  test_row <- function(label, pattern, regimes) {
    r <- c(row = label, line = "test")
    for (cl in cols) {
      mm <- sub("_.*$", "", cl); ty <- sub("^[I]+_", "", cl)
      y <- pt[pt$model == mm & pt$type == ty & grepl(pattern, pt$test, fixed = TRUE) & pt$regimes == regimes, ]
      r[[cl]] <- if (nrow(y) && !is.na(y$F[1])) sprintf("F(%d, %d) = %s, p %s", as.integer(y$q[1]), as.integer(y$df2[1]),
                                                       num_d(y$F[1], 2), p_txt(y$p_F[1])) else ""
    }
    rows <<- c(rows, list(r))
  }
  for (pr in c("1-2", "2-3", "3-4")) test_row(sprintf("All coefficients equal, regimes %s", pr), "robust Chow", pr)
  test_row("Controls equal in all regimes", "controls equal", "1-2-3-4")
  test_row("Expectation term equal in all regimes", "expectation equal", "1-2-3-4")
  nr <- c(row = "N (Newey-West lag)", line = "n")
  for (cl in cols) {
    mm <- sub("_.*$", "", cl); ty <- sub("^[I]+_", "", cl)
    y <- pt[pt$model == mm & pt$type == ty, ]
    nr[[cl]] <- if (nrow(y)) sprintf("%d (%d)", as.integer(y$n[1]), as.integer(y$nw_lag[1])) else ""
  }
  tab <- as.data.frame(do.call(rbind, c(rows, list(nr))), stringsAsFactors = FALSE)
  notes <- c(
    "Pooled OLS over 2000-2011, data version 'corrected', level form, with regime-specific constants. full: every coefficient regime-specific (point estimates equal the separate per-period OLS fits; controls not shown); exp_only: only the constant and the expectation term regime-specific, controls common.",
    "Newey-West standard errors in parentheses: Bartlett kernel, lag floor(4(n/100)^(2/9)), no prewhitening, small-sample factor n/(n-k); p-values from t(n-k). Results with the automatic bandwidth are in corr_pooled_coefs.csv and corr_pooled_tests.csv.",
    "Tests: Wald tests with the Newey-West covariance, reported as F = W/q with (q, n-k) degrees of freedom. A term not identified in a regime is left out of the restrictions.",
    sprintf("Model I uses the stage-3 expected change of the exchange rate from the headline specification%s; its pooled standard errors do not account for the generated regressor (see Table 3 for the bootstrap).",
            if (s2c_of("corrected")) ", with a constant in stage 2 (decision W4-20)" else ""),
    sig_note(), PERIOD_NOTE)
  out_table(tab, notes, "paper_T4", lg)
}

# ---- T5: unit roots --------------------------------------------------------------------------------------
table_unitroot <- function(lg) {
  u <- rd("corr_unitroot.csv", lg, required = FALSE)
  if (is.null(u)) return()
  yn <- function(x) ifelse(toupper(x) %in% c("TRUE", "T"), "yes", "no")
  tab <- data.frame(variable = ifelse(u$series == "diff", paste0("D", u$var), u$var), lags = as.integer(u$k),
                    original_dfgls = num_d(u$original_tau, 3), dfgls = num_d(u$dfgls_tau, 3), dfgls_cv5 = num_d(u$dfgls_cv5, 3),
                    dfgls_rejects = yn(u$dfgls_rejects_5pct), adf = num_d(u$adf_tau, 3), adf_cv5 = num_d(u$adf_cv5, 3),
                    adf_rejects = yn(u$adf_rejects_5pct), original_stationary = u$original_judgement, stringsAsFactors = FALSE)
  tab <- tab[order(u$series != "level", u$table_id), ]
  notes <- c(
    sprintf("Original DF-GLS: original Tables 4.1 (levels) and 4.2 (first differences). Our DF-GLS and ADF use the specification fixed in Week 2 (%s) and the same number of lags.", u$spec[1]),
    "Critical values at 5%: DF-GLS from urca (Elliott, Rothenberg and Stock 1996 tables), ADF from urca (Dickey-Fuller tables). The original's critical values (Stata dfgls, finite-sample) differ by construction; compare the statistics.",
    "Rejects: the unit-root null is rejected at 5%.")
  out_table(tab, notes, "paper_T5", lg)
}

# ---- T6: Chow tests ----------------------------------------------------------------------------------------
table_chow <- function(lg) {
  t4 <- rd("note_T4_chow.csv", lg, required = FALSE)
  if (is.null(t4)) return()
  tab <- data.frame(model = t4$model, periods = t4$pair, published = t4$original,
                    classic_differences = sprintf("F(%s) = %s", t4$classic_diff_df, t4$classic_diff_F),
                    classic_levels = sprintf("F(%s) = %s", t4$levels_df, t4$levels_F),
                    robust_wald = sprintf("F(%s) = %s, p = %s", t4$robust_wald_df, t4$robust_wald_F, t4$robust_wald_p),
                    stringsAsFactors = FALSE)
  notes <- c(
    "Published: original Table 4.11 as printed.",
    "Classic: Chow F with a common error variance, on the model as estimated in first differences, and on the same regressors in levels (whose degrees of freedom are compared with the published ones in the text).",
    "Robust Wald: equality of all coefficients of the two regimes in the fully interacted pooled model (Table 4), Newey-West covariance, F = W/q.")
  out_table(tab, notes, "paper_T6", lg)
}

main <- function(lg) {
  STAR_CUTS <<- read_star_legend(c("T4.9", "T4.10"), lg)
  rank <- rd("classify_rank_model_I.csv", lg)
  r <- rank[rank$convention == CONVENTIONS[1] & to_num(rank$rank) == 1, ]
  if (nrow(r) != 1) stop("no single headline row in classify_rank_model_I.csv")
  h <- list(spec = r$spec, lag = as.integer(r$lag), version = r$version)
  lg$info(sprintf("Headline Model I: %s, lag %d, %s data (%s convention)", h$spec, h$lag, h$version, CONVENTIONS[1]))
  table_reproduction(h, lg)
  table_steps(lg)
  table_corrected(h, lg)
  table_pooled(lg)
  table_unitroot(lg)
  table_chow(lg)
  lg$info("Next: quarto render docs/paper_tables.qmd (HTML and Word, for checking)")
}

run_script("10_paper_tables", main)
