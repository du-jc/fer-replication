# code/R/09_outputs.R -- Week 4, Study A (Blueprint A.2 step A5; Replication Plan s.4, s.5)
#
# Turns the Week 3 and Week 4 result tables into the tables, figures and numbers that
# docs/replication_note.qmd reads. It estimates nothing: every value is read from a table
# written by 04-08 (or, for the CPI figure, from the interim data files), then selected,
# formatted or plotted. The note's narrative cites values through note_numbers.csv, so no
# number is typed into the note.
#
# Selection rules (fixed; W4-06):
#   Model I   = the headline specification of 07 (rank 1, primary convention, W3-07)
#   version   = the headline's data version for the reproduction; "corrected" for corrections
#   convention = the primary one (CONVENTIONS[1], W3-01)
#   stage 2   = without a constant, except Model I on the corrected data (W4-20; s2c_of() and
#               keep_s2c() in corrections.R); row 1c of T7 is the W4-19 diagnostic
#
# Outputs: outputs/tables/note_*.csv, outputs/figures/fig*.png, run log in outputs/logs/.
# Usage:   Rscript code/R/09_outputs.R

source(file.path("code", "R", "common.R"))
source(file.path("code", "R", "models.R"))
source(file.path("code", "R", "corrections.R"))

FIG_DIR <- file.path("outputs", "figures")

tab <- function(name, lg, required = TRUE) {
  p <- file.path(PATHS$tables, name)
  if (!file.exists(p)) {
    if (required) stop(p, " not found: run the script that writes it first")
    lg$warn(p, " not found; the parts that use it are skipped")
    return(NULL)
  }
  d <- read_chr_csv(p)
  num <- c("period", "lag", "coef", "se", "p", "n", "target", "target_se", "std_gap", "reproduced", "partially",
           "not_reproduced", "n_cells", "mean_std_gap", "rank", "F", "df1", "df2", "target_F", "target_df1",
           "target_df2", "q", "W", "p_F", "p_chi2", "se_nw", "p_nw", "regime", "se_boot", "p_boot", "ci_lo", "ci_hi",
           "se_naive", "p_naive", "se_ratio", "n_ok", "B", "seed", "nw_lag", "draw", "replication",
           "log10_F_ratio", "ours", "mean_abs_gap", "max_abs_gap", "n_within_print", "correlation",
           "std_gap_without", "std_gap_with", "target_F")
  for (v in intersect(num, names(d))) d[[v]] <- to_num(d[[v]])
  d
}
lgl <- function(x) toupper(x) %in% c("TRUE", "T", "1", "YES")
STAR_CUTS <- DEFAULT_CUTS        # replaced in main() by the legend of original Tables 4.9-4.10 (W1-07)
star <- function(p) ifelse(is.na(p), "", sig_label(sig_level(p, STAR_CUTS)))
cell <- function(b, se, p, d = 3) ifelse(is.na(b), "n.i.", sprintf("%s (%s)%s", fmt(b, d), fmt(se, d), star(p)))

NUM <- new.env()
put <- function(name, value, source) {
  NUM$rows[[length(NUM$rows) + 1]] <- data.frame(name = name, value = as.character(value), source = source,
                                                 stringsAsFactors = FALSE)
}

git_commit <- function() {
  x <- tryCatch(suppressWarnings(system2("git", c("rev-parse", "--short", "HEAD"), stdout = TRUE, stderr = FALSE)),
                error = function(e) character(0))
  if (length(x) == 1 && nzchar(x)) x else "unknown"
}

main <- function(lg) {
  NUM$rows <- list()
  STAR_CUTS <<- read_star_legend(c("T4.9", "T4.10"), lg)
  dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)
  conv <- CONVENTIONS[1]
  rank <- tab("classify_rank_model_I.csv", lg)
  h <- rank[rank$convention == conv & rank$rank == 1, ]
  if (nrow(h) != 1) stop("no single headline row in classify_rank_model_I.csv")
  hv <- h$version
  put("headline_spec", h$spec, "classify_rank_model_I.csv"); put("headline_lag", h$lag, "classify_rank_model_I.csv")
  put("headline_version", hv, "classify_rank_model_I.csv"); put("primary_convention", conv, "models.R")
  put("headline_reproduced", h$reproduced, "classify_rank_model_I.csv"); put("headline_n_cells", h$n_cells, "classify_rank_model_I.csv")
  put("headline_partially", h$partially, "classify_rank_model_I.csv")
  put("headline_not_reproduced", h$not_reproduced, "classify_rank_model_I.csv")
  put("headline_mean_std_gap", fmt(h$mean_std_gap, 2), "classify_rank_model_I.csv")
  put("headline_decided_by", h$tie_broken_by, "classify_rank_model_I.csv")
  # W3-14: reproduced counts rest largely on cells where both estimates are insignificant;
  # report the two subsets with the count (columns written by 07_classify.R)
  for (v in intersect(c("reproduced_both_insignificant", "reproduced_within_one_se"), names(h)))
    put(paste0("headline_", v), h[[v]], "classify_rank_model_I.csv")
  put("n_model_I_combinations", sum(rank$convention == conv), "classify_rank_model_I.csv")

  # ---- T1 reproduction status, cell by cell -----------------------------------------------
  cc <- tab("classify_cells.csv", lg)
  sel_I <- cc$model == "I" & cc$spec == h$spec & cc$lag == h$lag
  t1 <- cc[cc$convention == conv & cc$version == hv & (sel_I | cc$model == "II"), ]
  t1 <- t1[order(t1$model, t1$period, t1$term), ]
  t1o <- data.frame(model = t1$model, period = t1$period, term = t1$term, original = t1$target_text,
                    original_se = fmt(t1$target_se, 3), ours = cell(t1$coef, t1$se, t1$p),
                    class = t1$class, reason = t1$reason, stringsAsFactors = FALSE)
  write_table(t1o, "note_T1_reproduction.csv", lg)

  # ---- T2 classification summary ------------------------------------------------------------------
  cs <- tab("classify_summary.csv", lg)
  t2 <- cs[cs$convention == conv & (cs$model == "II" | (cs$spec == h$spec & cs$lag == h$lag)), ]
  t2 <- t2[order(t2$model, t2$version), intersect(c("model", "version", "n_cells", "reproduced", "reproduced_both_insignificant",
                                                  "reproduced_within_one_se", "partially", "not_reproduced", "mean_std_gap"), names(t2))]
  write_table(t2, "note_T2_classification.csv", lg)
  for (i in seq_len(nrow(t2))) {
    k <- sprintf("model%s_%s", t2$model[i], t2$version[i])
    put(paste0(k, "_reproduced"), t2$reproduced[i], "classify_summary.csv")
    put(paste0(k, "_partially"), t2$partially[i], "classify_summary.csv")
    put(paste0(k, "_not_reproduced"), t2$not_reproduced[i], "classify_summary.csv")
    put(paste0(k, "_n_cells"), t2$n_cells[i], "classify_summary.csv")
    for (v in intersect(c("reproduced_both_insignificant", "reproduced_within_one_se"), names(t2)))
      put(paste0(k, "_", v), t2[[v]][i], "classify_summary.csv")
  }
  nn <- tab("classify_N.csv", lg, required = FALSE)
  if (!is.null(nn)) {
    x <- nn[nn$convention == conv, ]
    put("N_match", sum(lgl(x$n_match)), "classify_N.csv"); put("N_compared", nrow(x), "classify_N.csv")
  }

  # ---- T3 DW / BG ---------------------------------------------------------------------------------------
  dv <- tab("diag_vs_targets.csv", lg)
  key <- paste(dv$table_id, dv$version, dv$convention)
  t3 <- do.call(rbind, lapply(split(dv, key), function(d) data.frame(
    table_id = d$table_id[1], test = d$test[1], version = d$version[1], convention = d$convention[1],
    n = nrow(d), within_print = sum(lgl(d$within_print)), same_decision = sum(lgl(d$same_decision)),
    stringsAsFactors = FALSE)))
  t3 <- t3[order(t3$table_id, t3$version, t3$convention), ]
  write_table(t3, "note_T3_dwbg.csv", lg)
  put("dwbg_n", sum(t3$n), "diag_vs_targets.csv"); put("dwbg_within_print", sum(t3$within_print), "diag_vs_targets.csv")
  put("dwbg_same_decision", sum(t3$same_decision), "diag_vs_targets.csv")

  # ---- T4 Chow: original, classic (Week 3) and robust Wald (Week 4) -----------------------------------------
  cv <- tab("chow_vs_targets.csv", lg)
  pt <- keep_s2c(tab("corr_pooled_tests.csv", lg))        # one stage 2 per version (W4-20)
  cvs <- cv[cv$version == hv & (cv$model == "II" | cv$variant == "levels" | (cv$spec == h$spec & cv$lag == h$lag)), ]
  t4 <- do.call(rbind, lapply(split(cvs, paste(cvs$model, cvs$pair)), function(d) {
    g <- function(v, what) { x <- d[d$variant == v, ]; if (!nrow(x)) NA else x[[what]][1] }
    w <- pt[pt$model == d$model[1] & pt$version == hv & pt$form == "level" & pt$type == "full" & pt$nw == "rule" &
              pt$regimes == d$pair[1] & grepl("robust Chow", pt$test), ]
    data.frame(model = d$model[1], pair = d$pair[1], original = d$target_text[1],
               classic_diff_F = fmt(g("diff_full", "F"), 2),
               classic_diff_df = sprintf("%s,%s", g("diff_full", "df1"), g("diff_full", "df2")),
               classic_diff_df_match = g("diff_full", "df_match"),
               levels_F = fmt(g("levels", "F"), 2), levels_df = sprintf("%s,%s", g("levels", "df1"), g("levels", "df2")),
               levels_df_match = g("levels", "df_match"),
               robust_wald_F = if (nrow(w)) fmt(w$F[1], 2) else NA, robust_wald_df = if (nrow(w)) sprintf("%d,%d", w$q[1], w$df2[1]) else NA,
               robust_wald_p = if (nrow(w)) fmt(w$p_F[1], 4) else NA, stringsAsFactors = FALSE)
  }))
  write_table(t4, "note_T4_chow.csv", lg)
  put("chow_levels_df_match", sum(lgl(cv$df_match[cv$variant == "levels"])), "chow_vs_targets.csv")
  put("chow_levels_compared", sum(cv$variant == "levels"), "chow_vs_targets.csv")
  # The note's sentence: "the published F exceeds the largest recalculated one by a factor of at least
  # 10^x" (W3-13). x = min over model-pairs of log10(F_original / max over variants, versions, specs and
  # lags of F_ours), rounded DOWN to one decimal so that "at least" stays true.
  cvf <- cv[is.finite(cv$F) & cv$F > 0 & is.finite(cv$target_F) & cv$target_F > 0, ]
  mr <- vapply(split(cvf, paste(cvf$model, cvf$pair)), function(d) log10(d$target_F[1] / max(d$F)), numeric(1))
  put("chow_min_log10_ratio", if (length(mr)) fmt(floor(10 * min(mr)) / 10, 1) else NA, "chow_vs_targets.csv")

  # ---- T5 unit roots, T6 relabel, CPI and Period 1 tables -------------------------------------------------
  ur <- tab("corr_unitroot.csv", lg, required = FALSE)
  if (!is.null(ur)) {
    write_table(ur[, c("table_id", "var", "series", "k", "original_tau", "dfgls_tau", "dfgls_cv5", "dfgls_rejects_5pct",
                       "adf_tau", "adf_cv5", "adf_rejects_5pct", "original_judgement")], "note_T5_unitroot.csv", lg)
    put("unitroot_n", nrow(ur), "corr_unitroot.csv")
    put("unitroot_agree", sum(lgl(ur$dfgls_rejects_5pct) == lgl(ur$adf_rejects_5pct)), "corr_unitroot.csv")
  }
  rl <- tab("corr_relabel.csv", lg)
  t6 <- do.call(rbind, lapply(split(rl, rl$table_id), function(d) data.frame(table_id = d$table_id[1], n_cells = nrow(d),
    stars_match_if_se = sum(lgl(d$stars_match_if_se)), stars_match_if_t = sum(lgl(d$stars_match_if_t)))))
  write_table(t6, "note_T6_relabel.csv", lg)
  put("relabel_n", sum(t6$n_cells), "corr_relabel.csv"); put("relabel_match_se", sum(t6$stars_match_if_se), "corr_relabel.csv")
  put("relabel_match_t", sum(t6$stars_match_if_t), "corr_relabel.csv")
  ck <- tab("corr_cpi_check.csv", lg)
  write_table(ck, "note_T12_cpi_check.csv", lg)
  ckr <- function(pattern) ck[grepl(pattern, ck$check, fixed = TRUE), , drop = FALSE]
  x <- ckr("original pi vs IMF")
  put("cpi_pi_max_abs_gap", fmt(x$max_abs_gap, 3), "corr_cpi_check.csv")
  put("cpi_pi_within_print", x$n_within_print, "corr_cpi_check.csv"); put("cpi_pi_n", x$n, "corr_cpi_check.csv")
  x <- ckr("IMF vs FRED")
  if (nrow(x)) put("cpi_imf_fred_max_abs_gap", fmt(x$max_abs_gap, 4), "corr_cpi_check.csv")
  x <- ckr("original pi vs NBS")
  if (nrow(x)) {
    put("cpi_pi_nbs_within_print", x$n_within_print, "corr_cpi_check.csv")
    put("cpi_pi_nbs_max_abs_gap", fmt(x$max_abs_gap, 3), "corr_cpi_check.csv")
  }
  x <- ckr("NBS published year-on-year vs IMF")
  if (nrow(x)) {
    put("cpi_nbs_imf_within_print", x$n_within_print, "corr_cpi_check.csv")
    put("cpi_nbs_imf_max_abs_gap", fmt(x$max_abs_gap, 3), "corr_cpi_check.csv")
  }
  p1 <- tab("corr_period1.csv", lg)
  write_table(p1, "note_T13_period1.csv", lg)

  # ---- T7 the expectation coefficient through every correction (effect on conclusions) ---------------
  pe <- tab("corr_period_estimates.csv", lg)
  bt <- tab("corr_bootstrap.csv", lg)
  pc <- keep_s2c(tab("corr_pooled_coefs.csv", lg))        # one stage 2 per version (W4-20)
  tg <- read_estimate_targets(lg)
  long <- list()
  # step_id: the row number of the step ("0", "1", "1b", "1c", ..., "7"), matched exactly by the note's t7()
  # applies = FALSE: the step does not exist for this model (1c and 4c of Model II, which has no stage 2)
  addl <- function(model, step, period, coef, se, p, s2c = NA, applies = TRUE) {
    if (!length(period)) return()
    long[[length(long) + 1]] <<- data.frame(model = model, step_id = sub(" .*$", "", step), step = step, period = period,
                                            stage2_const = s2c, applies = applies, coef = coef, se = se, p = p,
                                            stringsAsFactors = FALSE)
  }
  na_row <- function(model, step) addl(model, step, PERIODS$period, NA_real_, NA_real_, NA_real_, NA, FALSE)
  S1C <- "1c reproduction + stage-2 constant (diagnostic W4-19)"
  S4C <- "4c + stage-2 constant (outside Plan s.5, W4-20)"
  for (mdl in intersect(c("I", "II"), unique(pe$model))) {
    ev <- if (mdl == "I") "DER_hathat" else "DNDF"
    th <- tg[tg$model == mdl & tg$term == ev, ]
    addl(mdl, "0 original (parentheses read as SE)", th$period, th$target, th$target_se, 2 * stats::pnorm(-abs(th$target / th$target_se)))
    s2 <- if (mdl == "I") function(v) v else function(v) NA       # Model II has no stage 2
    pv <- function(ver, form, s2c = FALSE) {
      x <- pe[pe$model == mdl & pe$term == ev & pe$version == ver & pe$form == form & pe$convention == conv, ]
      if (mdl == "I") x <- x[as_lgl(x$stage2_const) == s2c, ]
      x[order(x$period), ]
    }
    btv <- function(ver, sc, s2c) {
      b <- bt[bt$version == ver & bt$scheme == sc & bt$term == ev & as_lgl(bt$stage2_const) == s2c, ]
      b[order(b$period), ]
    }
    x <- pv(hv, "level"); addl(mdl, sprintf("1 reproduction (%s, naive SE)", hv), x$period, x$coef, x$se, x$p, s2(FALSE))
    # Step 3 is built on raw data. If the headline is not raw, this row separates the effect of
    # the article's deflation (1 -> 1b) from that of the GDP correction (1b -> 3).
    if (hv != "raw") { x <- pv("raw", "level"); addl(mdl, "1b reproduction (raw, naive SE)", x$period, x$coef, x$se, x$p, s2(FALSE)) }
    # 1c: the W4-19 diagnostic (the headline with a stage-2 constant); the headline stays row 1
    if (mdl == "I") { x <- pv(hv, "level", TRUE); addl(mdl, S1C, x$period, x$coef, x$se, x$p, TRUE) } else na_row(mdl, S1C)
    if (mdl == "I") for (sc in BOOT_SCHEMES) {
      b <- btv(hv, sc, FALSE)
      addl(mdl, sprintf("2 reproduction (%s), %s bootstrap SE", hv, sc), b$period, b$coef, b$se_boot, b$p_boot, FALSE)
    }
    x <- pv("gdpq", "level"); addl(mdl, "3 + discrete quarterly GDP (raw)", x$period, x$coef, x$se, x$p, s2(FALSE))
    x <- pv("corrected", "level"); addl(mdl, "4 + fixed-base CPI, ER/NDF undeflated", x$period, x$coef, x$se, x$p, s2(FALSE))
    # 4c (W4-20): Model I on the corrected data with a stage-2 constant; steps 5-7 of Model I build on it
    s2c <- mdl == "I" && s2c_of("corrected")
    if (mdl == "I") { x <- pv("corrected", "level", TRUE); addl(mdl, S4C, x$period, x$coef, x$se, x$p, TRUE) } else na_row(mdl, S4C)
    lab <- if (s2c) " + stage-2 constant" else ""
    if (mdl == "I") {
      b <- btv("corrected", BOOT_SCHEMES[1], s2c)
      addl(mdl, sprintf("5 corrected%s, %s bootstrap SE", lab, BOOT_SCHEMES[1]), b$period, b$coef, b$se_boot, b$p_boot, s2c)
    }
    x <- pv("corrected", "logdiff", s2c); addl(mdl, sprintf("6 corrected%s, log differences", lab), x$period, x$coef, x$se, x$p, s2(s2c))
    for (ty in POOLED_TYPES) for (ver in unique(c(hv, "corrected"))) {
      x <- pc[pc$model == mdl & pc$term == ev & pc$type == ty & pc$version == ver & pc$form == "level" & pc$status == "estimated", ]
      x <- x[order(x$regime), ]
      sv <- mdl == "I" && s2c_of(ver)
      addl(mdl, sprintf("7 pooled %s, Newey-West (%s%s)", ty, ver, if (sv) " + stage-2 constant" else ""),
           x$regime, x$coef, x$se_nw, x$p_nw, s2(sv))
    }
  }
  long <- do.call(rbind, long)
  long$stars <- star(long$p)
  long$sign <- ifelse(is.na(long$coef), NA, ifelse(long$coef > 0, "+", ifelse(long$coef < 0, "-", "0")))
  write_table(long, "note_T7_expectation_long.csv", lg)
  wide <- do.call(rbind, lapply(split(long, paste(long$model, long$step)), function(d) {
    r <- data.frame(model = d$model[1], step_id = d$step_id[1], step = d$step[1], stringsAsFactors = FALSE)
    for (p in PERIODS$period) {
      k <- which(d$period == p)
      r[[paste0("P", p)]] <- if (!d$applies[1]) "\u2014" else if (length(k)) cell(d$coef[k], d$se[k], d$p[k]) else "n.i."
    }
    r
  }))
  ord <- unique(paste(long$model, long$step))                            # the order of the steps above, per model
  wide <- wide[order(wide$model, match(paste(wide$model, wide$step), ord)), ]
  write_table(wide, "note_T7_expectation.csv", lg)

  # ---- T8 pooled tests, T11 sign convention -----------------------------------------------------------------
  t8 <- pt[pt$nw == "rule" & pt$form == "level" & pt$version %in% unique(c(hv, "corrected")),
           c("model", "version", "stage2_const", "type", "test", "regimes", "vars_tested", "q", "F", "df2", "p_F", "nw_lag", "n")]
  write_table(t8, "note_T8_pooled_tests.csv", lg)
  put("nw_lag_rule", paste(unique(pt$nw_lag), collapse = "/"), "corr_pooled_tests.csv")
  sg <- long[!is.na(long$coef), c("model", "step", "period", "coef", "stars", "sign")]
  sg$reading <- ifelse(sg$sign == "+", "a rise (expected RMB depreciation) goes with higher reserve accumulation",
                ifelse(sg$sign == "-", "a rise (expected RMB depreciation) goes with lower reserve accumulation", "zero"))
  write_table(sg, "note_T11_sign.csv", lg)

  # ---- T9 sources, T10 variable labels ---------------------------------------------------------------------
  led <- read_chr_csv(file.path("data", "metadata", "source_ledger.csv"))
  write_table(led[, intersect(c("series_name", "provider", "series_key_or_code", "frequency", "coverage_start", "coverage_end",
                                "acquisition_method", "layer", "license_or_access_restriction"), names(led))], "note_T9_sources.csv", lg)
  vl <- data.frame(
    symbol = c("FER", "ER", "NDF", "BOP", "GDP", "pi", "i", "NX", "FDI", "FDB"),
    label = c("foreign exchange reserves", "exchange rate, RMB per USD (a rise is a depreciation of the RMB)",
              "12-month USD/CNY non-deliverable forward, RMB per USD (Wind series in the source ledger)",
              "balance of payments, current account (SAFE BPM6 row 1), quarterly converted to monthly (W2-10)",
              "GDP: cumulative year-to-date in the appendix; discrete quarterly in versions gdpq and corrected",
              "CPI, same month of the previous year = 100 (a growth factor, not a price level)",
              "nominal one-year deposit rate (labelled 'real' in the article)", "net exports",
              "foreign direct investment", "external debt stock"),
    source = c("original Appendix I", "original Appendix I", "Wind", "SAFE", "original Appendix I", "original Appendix I",
               "original Appendix I", "original Appendix I", "original Appendix I", "original Appendix I"), stringsAsFactors = FALSE)
  write_table(vl, "note_T10_variables.csv", lg)

  # ---- numbers from the W4-19 diagnostic (rule fixed before the run; stage2_rule in corrections.R) -----------
  dg <- tab("corr_stage2_const_diag.csv", lg, required = FALSE)
  if (!is.null(dg)) {
    r <- stage2_rule(dg)
    put("s2c_outcome", r$outcome, "corr_stage2_const_diag.csv")
    put("s2c_outcome_text", S2C_OUTCOME_TEXT[[r$outcome]], "corr_stage2_const_diag.csv")
    put("s2c_n_cells", r$n_cells, "corr_stage2_const_diag.csv")
    put("s2c_n_changed", r$n_changed, "corr_stage2_const_diag.csv")
    put("s2c_n_changed_other", r$n_changed_other, "corr_stage2_const_diag.csv")
    for (i in seq_len(nrow(r$cons))) {
      put(sprintf("s2c_P%d_cons_std_gap_without", r$cons$period[i]), fmt(r$cons$std_gap_without[i], 2), "corr_stage2_const_diag.csv")
      put(sprintf("s2c_P%d_cons_std_gap_with", r$cons$period[i]), fmt(r$cons$std_gap_with[i], 2), "corr_stage2_const_diag.csv")
    }
    lg$info(sprintf("W4-19 outcome (%s): %s", r$outcome, S2C_OUTCOME_TEXT[[r$outcome]]))
  }

  # ---- numbers from the bootstrap ------------------------------------------------------------------------------
  put("boot_B", bt$B[1], "corr_bootstrap.csv"); put("boot_seed", bt$seed[1], "corr_bootstrap.csv")
  put("boot_schemes", paste(unique(bt$scheme), collapse = ", "), "corr_bootstrap.csv")
  if (bt$B[1] < BOOT_B) lg$warn(sprintf("corr_bootstrap.csv comes from a trial run (B = %d); rerun 08 without --B", bt$B[1]))
  put("git_commit", git_commit(), "git rev-parse"); put("r_version", R.version.string, "R")
  put("date_outputs", format(Sys.Date()), "09_outputs.R")

  # ---- figures -----------------------------------------------------------------------------------------------------
  png(file.path(FIG_DIR, "fig1_std_gap.png"), width = 1800, height = 1400, res = 200)
  f1 <- t1[t1$model == "I" & t1$status == "estimated" & !is.na(t1$target_se), ]
  g <- (f1$coef - f1$target) / f1$target_se
  lab <- sprintf("P%d %s", f1$period, f1$term)
  o <- order(f1$period, f1$term)
  # Axis clipped at +/- CLIP original SEs so that the cells near zero stay readable; cells beyond
  # the clip are drawn as triangles at the edge, labelled with their actual gap.
  CLIP <- 5
  gc <- pmax(pmin(g, CLIP), -CLIP)
  out <- is.finite(g) & abs(g) > CLIP
  graphics::par(mar = c(5.5, 9, 2, 5))
  graphics::plot(gc[o], seq_along(o), yaxt = "n", pch = ifelse(out[o], ifelse(g[o] > 0, 17, 25), 19),
                 bg = "black", xlab = "(ours - original) / original SE", ylab = "",
                 main = sprintf("Model I headline (%s, lag %s, %s): gap in original SEs", h$spec, h$lag, hv),
                 xlim = c(-CLIP, CLIP), cex.main = 0.9)
  graphics::axis(2, at = seq_along(o), labels = lab[o], las = 1, cex.axis = 0.7)
  graphics::abline(v = c(-1, 0, 1), lty = c(2, 1, 2), col = "grey50")
  if (any(out[o])) {
    k <- which(out[o])
    graphics::text(gc[o][k], k, labels = sprintf("%.0f", g[o][k]), pos = ifelse(g[o][k] > 0, 2, 4), cex = 0.7)
    graphics::mtext(sprintf("axis clipped at +/-%d; triangles: cells beyond, labelled with their gap", CLIP),
                    side = 1, line = 4.3, adj = 1, cex = 0.6)
  }
  grDevices::dev.off()

  png(file.path(FIG_DIR, "fig2_expectation_ci.png"), width = 2000, height = 1200, res = 200)
  steps <- c(sprintf("1 reproduction (%s, naive SE)", hv), sprintf("2 reproduction (%s), %s bootstrap SE", hv, BOOT_SCHEMES),
             sprintf("7 pooled full, Newey-West (%s)", hv))
  d2 <- long[long$model == "I" & long$step %in% steps & !is.na(long$coef), ]
  pos <- d2$period + (match(d2$step, steps) - 2.5) * 0.15
  lo <- d2$coef - 1.96 * d2$se; hi <- d2$coef + 1.96 * d2$se
  bi <- grepl("bootstrap", d2$step)
  for (i in which(bi)) {
    b <- bt[bt$version == hv & !as_lgl(bt$stage2_const) & bt$term == "DER_hathat" & bt$period == d2$period[i] &
              bt$scheme == sub(".*\\), (\\w+) bootstrap SE$", "\\1", d2$step[i]), ]
    if (nrow(b)) { lo[i] <- b$ci_lo; hi[i] <- b$ci_hi }
  }
  graphics::par(mar = c(4, 4, 2, 1))
  graphics::plot(pos, d2$coef, pch = 19, col = match(d2$step, steps), xlim = c(0.5, 4.5),
                 ylim = range(c(lo, hi), finite = TRUE), xaxt = "n", xlab = "period / regime",
                 ylab = "coefficient on DER_hathat", main = "Expectation coefficient: 95% intervals", cex.main = 0.9)
  graphics::segments(pos, lo, pos, hi, col = match(d2$step, steps))
  graphics::axis(1, at = 1:4)
  graphics::abline(h = 0, lty = 2, col = "grey50")
  graphics::legend("topleft", legend = sub("^\\d ", "", steps), col = seq_along(steps), pch = 19, cex = 0.6, bty = "n")
  grDevices::dev.off()

  dr <- tab("corr_bootstrap_draws.csv", lg, required = FALSE)
  if (!is.null(dr)) {
    png(file.path(FIG_DIR, "fig3_bootstrap_draws.png"), width = 2000, height = 1200, res = 200)
    graphics::par(mfrow = c(1, 4), mar = c(4, 3, 2, 1))
    for (p in PERIODS$period) {
      x <- dr$draw[dr$version == hv & !as_lgl(dr$stage2_const) & dr$scheme == BOOT_SCHEMES[1] & dr$period == p]
      x <- x[is.finite(x)]
      if (length(x) < 2) { graphics::plot.new(); graphics::title(sprintf("P%d: not identified", p), cex.main = 0.8); next }
      b <- bt[bt$version == hv & !as_lgl(bt$stage2_const) & bt$scheme == BOOT_SCHEMES[1] & bt$term == "DER_hathat" & bt$period == p, ]
      xl <- range(c(x, if (nrow(b)) b$coef + c(-1, 1) * 1.96 * b$se_naive), finite = TRUE)   # red lines always inside
      graphics::hist(x, breaks = 30, main = sprintf("P%d (%s, %s)", p, hv, BOOT_SCHEMES[1]), xlab = "DER_hathat draws",
                     cex.main = 0.8, col = "grey85", border = "white", xlim = xl)
      if (nrow(b)) graphics::abline(v = c(b$coef, b$coef - 1.96 * b$se_naive, b$coef + 1.96 * b$se_naive),
                                    lty = c(1, 2, 2), col = c("black", "red", "red"))
    }
    grDevices::dev.off()
  }

  cpi <- read_cpi(lg)
  panel <- read_panel(lg)
  yoy <- yoy_index(cpi$cpi_imf, cpi$month)[match(panel$month, cpi$month)]
  nbs <- cpi$cpi_nbs_yoy[match(panel$month, cpi$month)]
  panels <- list(list(g = panel$pi - yoy, t = "Original pi minus the year-on-year change of the IMF CPI"))
  if (any(is.finite(nbs))) panels <- c(panels, list(
    list(g = panel$pi - nbs, t = "Original pi minus the NBS published year-on-year CPI"),
    list(g = nbs - yoy, t = "NBS published year-on-year minus the IMF CPI year-on-year")))
  png(file.path(FIG_DIR, "fig4_cpi_gap.png"), width = 2000, height = 700 * length(panels), res = 200)
  graphics::par(mfrow = c(length(panels), 1), mar = c(3, 4, 2, 1))
  yl <- range(unlist(lapply(panels, `[[`, "g")), 0, finite = TRUE)
  at <- which(substr(panel$month, 6, 7) == "01")
  for (pp in panels) {
    graphics::plot(seq_along(panel$month), pp$g, type = "h", xaxt = "n", xlab = "", ylab = "percentage points",
                   ylim = yl, main = pp$t, cex.main = 0.9)
    graphics::axis(1, at = at, labels = substr(panel$month[at], 1, 4), cex.axis = 0.7)
    graphics::abline(h = 0, col = "grey50")
  }
  grDevices::dev.off()
  lg$info(sprintf("Wrote 4 figures to %s", FIG_DIR))

  nums <- do.call(rbind, NUM$rows)
  write_table(nums, "note_numbers.csv", lg)
  lg$info(sprintf("HEADLINE for the note: Model I %s, lag %s, %s data (%s convention); %s of %s cells reproduced",
                  h$spec, h$lag, hv, conv, h$reproduced, h$n_cells))
  lg$info("Next: quarto render docs/replication_note.qmd")
}

run_script("09_outputs", main)
