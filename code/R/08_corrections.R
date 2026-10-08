# code/R/08_corrections.R -- Week 4, Study A (Blueprint A.2 step A5; Replication Plan s.5)
#
# Applies the corrections of Replication Plan s.5 and writes one table per correction.
# Model I is always the HEADLINE specification (spec and lag) named by 07_classify.R under the
# primary convention (classify_rank_model_I.csv, rank 1; rule W3-02, result W3-07).
#
#   Plan s.5 issue                          what this script does                     table (outputs/tables/)
#   SEs labelled as t-statistics            stars implied by coef/SE vs by the number
#                                           read as t, against the printed stars       corr_relabel.csv
#   level coefficients read as percentages  log-difference form (elasticities)        corr_period_estimates.csv
#   year-to-date GDP                        discrete quarterly GDP (version gdpq)      corr_period_estimates.csv
#   deflator is not a price level           fixed-base IMF CPI; ER/NDF undeflated
#                                           (version corrected); CPI checks            corr_cpi_check.csv
#   artificial Period 1 ER variation        variation of ER, DER, DER_hathat in P1     corr_period1.csv
#   generated regressors                    wild (primary) and block bootstrap of the
#                                           whole three-stage chain                   corr_bootstrap.csv (+ _draws)
#   no power in Period 2                    pooled 2000-2011 model, regime interactions,
#                                           Newey-West                                 corr_pooled_coefs.csv
#   implausible Chow statistics             robust Wald tests of equal coefficients in
#                                           adjacent regimes (pooled, NW)              corr_pooled_tests.csv
#   unit-root test mislabelled              DF-GLS and ADF side by side                corr_unitroot.csv
#   (sign convention, "real" rate, NDF source: labels and text, written by 09_outputs.R)
#
#   Week 4 rerun, outside Plan s.5 (stage 2 of Model I with a constant):
#   W4-19 diagnostic                        headline re-estimated with a stage-2 constant,
#                                           every Model I cell classified again, rule
#                                           (a)/(b)/(c) fixed before the run           corr_stage2_const_diag.csv
#   W4-20 step 4c                           Model I on the corrected data with a stage-2
#                                           constant; bootstrap, log differences and the
#                                           pooled model of the corrected data use it  stage2_const column
#
# Every estimate is computed for the four data versions (raw, deflated, gdpq, corrected) and
# both forms (level, logdiff); per-period estimates under both conventions; the pooled model
# and the bootstrap under the primary convention only (W4-04). Model I per-period and pooled
# estimates are computed with and without a stage-2 constant (column stage2_const; Model II has
# no stage 2, so its stage2_const is NA). Which one each version uses: s2c_of() in corrections.R.
#
# Usage:   Rscript code/R/08_corrections.R            (B = 999 replications per scheme)
#          Rscript code/R/08_corrections.R --B 99     (quick trial run; not for the note)

source(file.path("code", "R", "common.R"))
source(file.path("code", "R", "models.R"))
source(file.path("code", "R", "corrections.R"))

parse_args <- function() {
  a <- commandArgs(trailingOnly = TRUE)
  B <- BOOT_B
  i <- match("--B", a)
  if (!is.na(i)) {
    B <- suppressWarnings(as.integer(a[i + 1]))
    if (is.na(B) || B < 19) stop("--B needs an integer of at least 19")
  }
  list(B = B)
}

read_headline <- function(lg) {
  p <- file.path(PATHS$tables, "classify_rank_model_I.csv")
  r <- read_chr_csv(p)
  r <- r[r$convention == CONVENTIONS[1] & to_num(r$rank) == 1, , drop = FALSE]
  if (nrow(r) != 1) stop(p, ": no single rank-1 row for convention ", CONVENTIONS[1], " (run 07_classify.R)")
  h <- list(spec = r$spec, lag = as.integer(r$lag), version = r$version)
  lg$info(sprintf("HEADLINE Model I specification (07_classify.R, W3-07): %s, lag %d, %s data",
                  h$spec, h$lag, h$version))
  h
}

# ---- transformation log (R side; same columns as the Python scripts) ---------------------------
log_transformation_r <- function(step, input, output, description, n, decision, lg) {
  if (!file.exists(PATHS$transform)) { lg$warn(PATHS$transform, " not found; transformation not logged"); return() }
  tl <- read_chr_csv(PATHS$transform)
  hash <- paste0("md5:", unname(tools::md5sum(input)))
  script <- "code/R/08_corrections.R"
  if (any(tl$script == script & tl$step == step & tl$input_sha256 == hash & tl$n_affected == as.character(n))) return()
  row <- stats::setNames(as.list(rep("", ncol(tl))), names(tl))
  vals <- list(timestamp_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), script = script,
               step = step, input_file = input, input_sha256 = hash, output_file = output,
               description = description, n_affected = as.character(n), decision_ref = decision)
  for (k in intersect(names(vals), names(row))) row[[k]] <- vals[[k]]
  con <- file(PATHS$transform, open = "ab")       # binary: keeps "\n" line ends on Windows too
  on.exit(close(con))
  utils::write.table(as.data.frame(row, check.names = FALSE, stringsAsFactors = FALSE), con,
                     sep = ",", row.names = FALSE, col.names = FALSE, qmethod = "double", eol = "\n")
}

# ---- 1. data corrections and the CPI checks -----------------------------------------------------
corrections_data <- function(panel, cpi, sel, lg) {
  panel <- prepare_corrections(panel, cpi, lg)
  out <- data.frame(month = panel$month, GDP_q = panel$GDP_q, cpi_2000 = panel$cpi_2000)
  utils::write.csv(out, CORR_PANEL, row.names = FALSE, fileEncoding = "UTF-8")
  lg$info(sprintf("Wrote %s (discrete quarterly GDP, CPI 2000 = 100; git-ignored)", CORR_PANEL))
  log_transformation_r("gdp_ytd_to_discrete_quarterly", PATHS$panel, CORR_PANEL,
                       "Year-to-date GDP -> discrete quarterly GDP (YTD(q) - YTD(q-1)), held in each month of the quarter",
                       nrow(panel), "W4-05", lg)
  log_transformation_r("cpi_rebase_2000", CPI_FILE, CORR_PANEL,
                       "IMF CPI rebased to its 2000 mean = 100 (the deflator of version 'corrected')",
                       nrow(panel), "W4-05", lg)
  panel
}

cpi_checks <- function(panel, cpi, lg) {
  yoy_imf <- yoy_index(cpi$cpi_imf, cpi$month)
  yoy_fred <- yoy_index(cpi$cpi_fred, cpi$month)
  i <- match(panel$month, cpi$month)
  app <- if (file.exists(PATHS$appendix)) read_chr_csv(PATHS$appendix) else NULL
  tol <- if (!is.null(app) && "pi" %in% names(app)) print_tolerance(app$pi[match(panel$month, app$month)]) else NA_real_
  g_original <- panel$pi - yoy_imf[i]
  rows <- list(data.frame(check = "original pi vs IMF CPI year-on-year", n = sum(is.finite(g_original)),
                          mean_abs_gap = mean(abs(g_original), na.rm = TRUE), max_abs_gap = max(abs(g_original), na.rm = TRUE),
                          n_within_print = sum(abs(g_original) <= tol, na.rm = TRUE),
                          correlation = suppressWarnings(stats::cor(panel$pi, yoy_imf[i], use = "complete.obs"))))
  if (any(is.finite(yoy_fred))) {
    g_f <- (yoy_imf - yoy_fred)[i]
    lv <- (cpi$cpi_imf / cpi$cpi_fred)[i]
    rows[[2]] <- data.frame(check = "IMF vs FRED CPI year-on-year (sample months)", n = sum(is.finite(g_f)),
                            mean_abs_gap = mean(abs(g_f), na.rm = TRUE), max_abs_gap = max(abs(g_f), na.rm = TRUE),
                            n_within_print = NA_integer_,
                            correlation = suppressWarnings(stats::cor(yoy_imf[i], yoy_fred[i], use = "complete.obs")))
    rows[[3]] <- data.frame(check = "IMF / FRED index ratio (constant if same index, other base)", n = sum(is.finite(lv)),
                            mean_abs_gap = stats::sd(lv, na.rm = TRUE) / mean(lv, na.rm = TRUE),
                            max_abs_gap = diff(range(lv, na.rm = TRUE)) / mean(lv, na.rm = TRUE),
                            n_within_print = NA_integer_, correlation = NA_real_)
  } else lg$warn("FRED check column empty: the IMF CPI is not cross-checked (collect_cpi.py WARN)")
  # NBS published year-on-year (W4-14): same definition as the original pi, so compared directly
  nbs <- cpi$cpi_nbs_yoy[i]
  if (any(is.finite(nbs))) {
    g_n <- panel$pi - nbs
    g_ni <- nbs - yoy_imf[i]
    rows[[length(rows) + 1]] <- data.frame(check = "original pi vs NBS published year-on-year", n = sum(is.finite(g_n)),
      mean_abs_gap = mean(abs(g_n), na.rm = TRUE), max_abs_gap = max(abs(g_n), na.rm = TRUE),
      n_within_print = sum(abs(g_n) <= tol, na.rm = TRUE),
      correlation = suppressWarnings(stats::cor(panel$pi, nbs, use = "complete.obs")))
    rows[[length(rows) + 1]] <- data.frame(check = "NBS published year-on-year vs IMF CPI year-on-year", n = sum(is.finite(g_ni)),
      mean_abs_gap = mean(abs(g_ni), na.rm = TRUE), max_abs_gap = max(abs(g_ni), na.rm = TRUE),
      n_within_print = sum(abs(g_ni) <= tol, na.rm = TRUE),
      correlation = suppressWarnings(stats::cor(nbs, yoy_imf[i], use = "complete.obs")))
    # month by month, for the note's Deflator section; git-ignored because it holds the original pi
    gaps <- data.frame(month = panel$month, period = panel$period, pi_original = panel$pi, yoy_nbs = nbs,
                       yoy_imf = yoy_imf[i], gap_original_nbs = g_n, gap_original_imf = g_original, gap_nbs_imf = g_ni,
                       tolerance = tol)
    gp <- file.path("data", "interim", "cpi_gaps.csv")
    utils::write.csv(gaps, gp, row.names = FALSE, fileEncoding = "UTF-8")
    lg$info(sprintf("Wrote %s (month by month; git-ignored: it holds the original pi)", gp))
    off <- which(abs(g_n) > tol)
    if (length(off)) {
      o <- off[order(-abs(g_n[off]))][seq_len(min(8, length(off)))]
      lg$info(sprintf("  original pi differs from the NBS published rate beyond print in %d month(s); largest: %s", length(off),
                      paste(sprintf("%s (%s)", panel$month[o], fmt(g_n[o], 2)), collapse = ", ")))
    }
  } else lg$info("NBS year-on-year column empty: original pi not compared with the published rate (optional; W4-14)")
  out <- do.call(rbind, rows)
  write_table(out, "corr_cpi_check.csv", lg)
  for (k in seq_len(nrow(out)))
    lg$info(sprintf("  CPI check: %s: n %d, mean |gap| %s, max |gap| %s%s", out$check[k], out$n[k],
                    fmt(out$mean_abs_gap[k], 4), fmt(out$max_abs_gap[k], 4),
                    if (!is.na(out$n_within_print[k])) sprintf(", %d within the printed decimals of pi", out$n_within_print[k]) else ""))
}

# ---- 2. SEs labelled as t-statistics ---------------------------------------------------------------
relabel <- function(lg) {
  cuts <- read_star_legend(c("T4.9", "T4.10"), lg)        # meanings of the marks, from the tables' notes (W1-07)
  x <- relabel_check(read_estimate_targets(lg), cuts)
  write_table(x[, c("table_id", "model", "period", "term", "target_text", "target", "target_se", "target_level",
                    "implied_t_if_se", "p_if_se", "level_if_se", "stars_match_if_se",
                    "p_if_t", "level_if_t", "stars_match_if_t")], "corr_relabel.csv", lg)
  lg$info(sprintf("Stars implied by the parentheses: read as SE match the printed stars in %d of %d cells; read as t in %d of %d",
                  sum(x$stars_match_if_se), nrow(x), sum(x$stars_match_if_t), nrow(x)))
}

# ---- 3. per-period estimates, all versions x forms x conventions --------------------------------------
period_estimates <- function(panel, sel, h, lg) {
  has_ndf <- !is.na(sel$ndf) && sel$ndf %in% names(panel)
  est <- list()
  for (ver in CORR_VERSIONS) for (form in FORMS) for (conv in CONVENTIONS) {
    base <- tryCatch(make_base(panel, ver, form, sel, conv), error = function(e) e)
    if (inherits(base, "error")) { lg$warn(sprintf("%s/%s/%s: %s", ver, form, conv, conditionMessage(base))); next }
    for (s2c in c(FALSE, TRUE)) {                 # stage 2 without / with a constant (W4-19, W4-20)
      b3 <- model_I_chain(base, conv, h$spec, h$lag, s2c)$base
      for (mdl in c("I", if (has_ndf && !s2c) "II")) {
        xv <- if (mdl == "I") STAGE3_X else MODEL2_X
        b <- if (mdl == "I") b3 else base
        for (p in PERIODS$period) {
          rows <- eq_rows(b, p, xv)
          if (length(rows) < length(xv) + 2) next
          keys <- data.frame(model = mdl, version = ver, form = form, convention = conv,
                             spec = if (mdl == "I") h$spec else NA_character_,
                             lag = if (mdl == "I") h$lag else NA_integer_,
                             stage2_const = if (mdl == "I") s2c else NA, period = p, stringsAsFactors = FALSE)
          f <- fit_dfer(b$DFER[rows], as.matrix(b[rows, xv]), p)
          est[[length(est) + 1]] <- tidy_fit(f, keys)
        }
      }
    }
  }
  est <- do.call(rbind, est)
  no_s2c <- is.na(est$stage2_const) | !est$stage2_const      # the Week 3 stage 2 (no constant)
  write_table(est, "corr_period_estimates.csv", lg)
  # regression check: raw/deflated, level form reproduce the Week 3 estimates exactly
  for (mdl in c("I", "II")) {
    p3 <- file.path(PATHS$tables, sprintf("model_%s_estimates.csv", mdl))
    if (!file.exists(p3)) next
    w3 <- read_chr_csv(p3)
    w3 <- w3[w3$status == "estimated", ]
    if (mdl == "I") w3 <- w3[w3$spec == h$spec & as.integer(w3$lag) == h$lag, ]
    m <- merge(w3[, c("version", "convention", "period", "term", "coef")],
               est[no_s2c & est$model == mdl & est$form == "level" & est$status == "estimated",
                   c("version", "convention", "period", "term", "coef")],
               by = c("version", "convention", "period", "term"), suffixes = c("_w3", "_w4"))
    if (!nrow(m)) next
    gap <- max(abs(to_num(m$coef_w3) - m$coef_w4) / pmax(1, abs(m$coef_w4)))
    if (gap < 1e-6) lg$info(sprintf("CHECK PASS  Model %s raw/deflated level estimates equal Week 3 (%d coefficients)", mdl, nrow(m)))
    else lg$error(sprintf("Model %s: Week 4 re-estimates differ from Week 3 (max relative gap %g)", mdl, gap))
  }
  key <- if ("II" %in% est$model) c("DER_hathat", "DNDF") else "DER_hathat"
  for (tm in key) for (form in FORMS) for (s2c in if (tm == "DER_hathat") c(FALSE, TRUE) else NA) {
    x <- est[est$term == tm & est$form == form & est$convention == CONVENTIONS[1] &
               (if (is.na(s2c)) TRUE else est$stage2_const %in% s2c), ]
    for (ver in CORR_VERSIONS) {
      y <- x[x$version == ver, ]; y <- y[order(y$period), ]
      if (!nrow(y)) next
      lg$info(sprintf("  %-10s %-7s %-9s %-4s %s", tm, form, ver, if (isTRUE(s2c)) "+s2c" else "",
                      paste(sprintf("P%d %s [%s]%s", y$period, fmt(y$coef, 4), fmt(y$se, 4),
                                    ifelse(is.na(y$p), "", sig_label(sig_level(y$p), ascii = TRUE))), collapse = "  ")))
    }
  }
  est
}

# ---- 3b. W4-19: did the original's stage 2 have a constant? ---------------------------------------------
# Headline specification and data version, primary convention, level form: every Model I cell is
# classified against the original (W1-07, the star legend of the tables' notes, as in 07_classify.R)
# without and with a stage-2 constant, and the rule of W4-19 (stage2_rule, fixed before the run)
# is applied. The headline itself is not changed (W3-04).
stage2_const_diag <- function(est, h, lg) {
  cuts <- read_star_legend(c("T4.9", "T4.10"), lg)
  tg <- read_estimate_targets(lg)
  tg <- tg[tg$model == "I" & tg$term != "N", , drop = FALSE]
  e <- est[est$model == "I" & est$version == h$version & est$form == "level" & est$convention == CONVENTIONS[1], ]
  one <- function(s2c) {
    x <- e[e$stage2_const %in% s2c, c("period", "term", "coef", "se", "p", "status")]
    m <- merge(tg[, c("table_id", "period", "term", "target", "target_text", "target_se", "target_level")], x,
               by = c("period", "term"), all.x = TRUE)
    m$status[is.na(m$status)] <- "not_estimated"
    cbind(m, classify_cell(m$target, m$target_se, m$target_level, m$coef, m$p, m$status, cuts))
  }
  w0 <- one(FALSE); w1 <- one(TRUE)
  k <- c("table_id", "period", "term")
  d <- merge(w0[, c(k, "target_text", "target_se", "coef", "se", "p", "std_gap", "class")],
             w1[, c(k, "coef", "se", "p", "std_gap", "class")], by = k, suffixes = c("_without", "_with"))
  d <- d[order(d$period, d$term), ]
  d <- data.frame(model = "I", version = h$version, form = "level", convention = CONVENTIONS[1], spec = h$spec,
                  lag = h$lag, d, class_changes = d$class_without != d$class_with, stringsAsFactors = FALSE)
  # check: without the constant, the classes equal those of 07_classify.R (Week 3)
  pc <- file.path(PATHS$tables, "classify_cells.csv")
  if (file.exists(pc)) {
    cc <- read_chr_csv(pc)
    cc <- cc[cc$model == "I" & cc$version == h$version & cc$convention == CONVENTIONS[1] & cc$spec == h$spec &
               to_num(cc$lag) %in% h$lag, ]
    m <- merge(d[, c("period", "term", "class_without")], data.frame(period = as.integer(cc$period), term = cc$term,
               class_w3 = cc$class, stringsAsFactors = FALSE), by = c("period", "term"))
    if (nrow(m) && all(m$class_without == m$class_w3))
      lg$info(sprintf("CHECK PASS  without the stage-2 constant, all %d Model I classes equal classify_cells.csv", nrow(m)))
    else lg$error(sprintf("without the stage-2 constant, %d of %d Model I classes differ from classify_cells.csv",
                          sum(m$class_without != m$class_w3), nrow(m)))
  }
  write_table(d, "corr_stage2_const_diag.csv", lg)
  r <- stage2_rule(d)
  for (i in seq_len(nrow(r$cons)))
    lg$info(sprintf("  Period %d constant: std_gap %s without, %s with the stage-2 constant", r$cons$period[i],
                    fmt(r$cons$std_gap_without[i], 2), fmt(r$cons$std_gap_with[i], 2)))
  if (r$constants_found != length(S2C_RULE_PERIODS))
    lg$warn(sprintf("found %d of the %d constants the rule needs (periods %s)", r$constants_found,
                    length(S2C_RULE_PERIODS), paste(S2C_RULE_PERIODS, collapse = ", ")))
  lg$info(sprintf("  Model I cells whose class changes: %d of %d (%d other than the Period %s constants)",
                  r$n_changed, r$n_cells, r$n_changed_other, paste(S2C_RULE_PERIODS, collapse = " and ")))
  lg$info(sprintf("W4-19 OUTCOME (%s): %s", r$outcome, S2C_OUTCOME_TEXT[[r$outcome]]))
  invisible(r)
}

# ---- 4. Period 1: where the expectation variation comes from ------------------------------------------
period1_identification <- function(panel, sel, h, lg) {
  rows <- list()
  for (ver in CORR_VERSIONS) {
    base <- make_base(panel, ver, "level", sel, CONVENTIONS[1])
    b3 <- model_I_chain(base, CONVENTIONS[1], h$spec, h$lag, s2c_of(ver))$base      # W4-20 on corrected
    d <- corr_version_panel(panel, ver, sel)
    r <- which(base$period == 1 & stats::complete.cases(base[, c("DER", "Dpi")]))
    r2 <- if (stats::sd(base$DER[r]) > 0) summary(stats::lm(base$DER[r] ~ base$Dpi[r]))$r.squared else NA_real_
    rows[[length(rows) + 1]] <- data.frame(version = ver, stage2_const = s2c_of(ver), sd_ER_level = stats::sd(d$ER[d$period == 1]),
      sd_DER = stats::sd(base$DER[r]), sd_DER_hathat = stats::sd(b3$DER_hathat[base$period == 1], na.rm = TRUE),
      r2_DER_on_Dpi = r2, stringsAsFactors = FALSE)
  }
  out <- do.call(rbind, rows)
  write_table(out, "corr_period1.csv", lg)
  for (i in seq_len(nrow(out)))
    lg$info(sprintf("  Period 1 %-9s sd(ER) %s  sd(DER) %s  sd(DER_hathat) %s  R2(DER on Dpi) %s", out$version[i],
                    fmt(out$sd_ER_level[i], 4), fmt(out$sd_DER[i], 4), fmt(out$sd_DER_hathat[i], 4), fmt(out$r2_DER_on_Dpi[i], 3)))
}

# ---- 5. unit roots: DF-GLS and ADF side by side ---------------------------------------------------------
unit_roots <- function(panel, lg) {
  p1 <- file.path(PATHS$tables, "dfgls_table4_1_4_2.csv"); p2 <- file.path(PATHS$tables, "dfgls_selected_spec.csv")
  if (!file.exists(p1) || !file.exists(p2)) { lg$warn("Week 2 DF-GLS tables not found; corr_unitroot.csv not written"); return() }
  t <- read_chr_csv(p1); s <- read_chr_csv(p2)
  sv <- col_of(s, "version", p2); sd <- col_of(s, c("det", "deterministics"), p2)
  sc <- col_of(s, c("conv", "sample"), p2); sr <- col_of(s, "rule", p2)
  dat <- if (sv == "deflated") deflate(panel, ORIGINAL_VARS) else panel
  type <- if (sd == "trend") "trend" else "drift"
  cv <- t(vapply(seq_len(nrow(t)), function(i) {
    y <- dat[[t$var[i]]]; if (t$series[i] == "diff") y <- diff(y)
    u <- urca::ur.df(y, type = type, lags = as.integer(t$k[i]))
    c(u@cval[1, "5pct"], as.numeric(u@teststat[1]))
  }, numeric(2)))
  out <- data.frame(table_id = t$table_id, var = t$var, series = t$series, k = t$k,
                    dfgls_tau = to_num(t$tau), original_tau = to_num(col_of(t, c("target", "original_tau"), p1)),
                    dfgls_cv5 = to_num(t$urca_cv5),
                    dfgls_rejects_5pct = to_num(t$tau) < to_num(t$urca_cv5),
                    adf_tau = cv[, 2], adf_cv5 = cv[, 1], adf_rejects_5pct = cv[, 2] < cv[, 1],
                    original_judgement = col_of(t, c("original_judgement", "thesis_judgement"), p1),
                    spec = paste(sv, sd, sc, sr, sep = "/"),
                    stringsAsFactors = FALSE)
  if (max(abs(out$adf_tau - to_num(t$adf_tau_same_k)), na.rm = TRUE) > 1e-8)
    lg$error("ADF tau differs from dfgls_table4_1_4_2.csv (adf_tau_same_k)")
  else lg$info("CHECK PASS  ADF tau equals the Week 2 value at the same lag")
  write_table(out, "corr_unitroot.csv", lg)
  lg$info(sprintf("Unit roots (spec %s): DF-GLS and ADF agree on rejection at 5%% in %d of %d series",
                  out$spec[1], sum(out$dfgls_rejects_5pct == out$adf_rejects_5pct), nrow(out)))
}

# ---- 6. pooled model with regime interactions and Newey-West ----------------------------------------------
pooled <- function(panel, sel, h, lg) {
  has_ndf <- !is.na(sel$ndf) && sel$ndf %in% names(panel)
  coefs <- list(); tests <- list(); chk <- 0
  conv <- CONVENTIONS[1]
  for (ver in CORR_VERSIONS) for (form in FORMS) {
    base <- tryCatch(make_base(panel, ver, form, sel, conv), error = function(e) NULL)
    if (is.null(base)) next
    for (s2c in c(FALSE, TRUE)) {                     # Model I stage 2 without / with a constant
    b3 <- model_I_chain(base, conv, h$spec, h$lag, s2c)$base
    for (mdl in c("I", if (has_ndf && !s2c) "II")) {
      xv <- if (mdl == "I") STAGE3_X else MODEL2_X
      b <- if (mdl == "I") b3 else base
      expvar <- xv[1]; controls <- xv[-1]
      rows <- which(!is.na(b$period) & stats::complete.cases(b[, c("DFER", xv)]))
      y <- b$DFER[rows]
      for (type in POOLED_TYPES) {
        d <- pooled_design(b, rows, expvar, controls, type)
        pf <- fit_pooled(y, d$X)
        fit <- pf$fit
        n <- length(y); L <- nw_lag_rule(n)
        V <- nw_vcov(fit, L); Va <- sandwich::NeweyWest(fit, lag = NULL, prewhite = FALSE, adjust = TRUE)
        Vo <- stats::vcov(fit)
        bb <- stats::coef(fit); dfr <- fit$df.residual
        se <- sqrt(diag(V)); se_a <- sqrt(diag(Va)); se_o <- sqrt(diag(Vo))
        keys <- data.frame(model = mdl, version = ver, form = form, convention = conv, type = type,
                           spec = if (mdl == "I") h$spec else NA_character_, lag = if (mdl == "I") h$lag else NA_integer_,
                           stage2_const = if (mdl == "I") s2c else NA, n = n, nw_lag = L, stringsAsFactors = FALSE)
        nm <- names(bb)
        coefs[[length(coefs) + 1]] <- cbind(keys[rep(1, length(bb)), ], data.frame(
          regime = regime_of(nm),
          term = sub("^R[1-4]:", "", nm), coef = unname(bb), se_nw = unname(se), t_nw = unname(bb / se),
          p_nw = 2 * stats::pt(-abs(unname(bb / se)), dfr), se_nw_auto = unname(se_a),
          p_nw_auto = 2 * stats::pt(-abs(unname(bb / se_a)), dfr), se_ols = unname(se_o),
          status = "estimated", stringsAsFactors = FALSE), row.names = NULL)
        if (length(d$dropped) || length(pf$aliased)) {
          nd <- c(d$dropped, pf$aliased)
          coefs[[length(coefs) + 1]] <- cbind(keys[rep(1, length(nd)), ], data.frame(
            regime = regime_of(nd), term = sub("^R[1-4]:", "", nd),
            coef = NA_real_, se_nw = NA_real_, t_nw = NA_real_, p_nw = NA_real_, se_nw_auto = NA_real_,
            p_nw_auto = NA_real_, se_ols = NA_real_, status = "not_identified", stringsAsFactors = FALSE), row.names = NULL)
        }
        # Wald tests with the NW covariance (rule lag) and with the automatic bandwidth
        add_test <- function(name, regimes, vars) {
          er <- equality_R(nm, vars, regimes)
          for (vv in list(list("rule", V), list("auto", Va))) {
            w <- wald_test(bb, vv[[2]], er$R, dfr)
            tests[[length(tests) + 1]] <<- cbind(keys, data.frame(test = name, nw = vv[[1]],
              regimes = paste(regimes, collapse = "-"), vars_tested = paste(er$vars_used, collapse = ";"),
              stringsAsFactors = FALSE), w)
          }
        }
        if (type == "full") {
          for (pr in list(1:2, 2:3, 3:4)) add_test("all coefficients equal (robust Chow)", pr, c("_cons", xv))
          add_test("controls equal in all regimes", 1:4, controls)
          add_test("expectation equal in all regimes", 1:4, expvar)
          # check: the fully interacted pooled OLS equals the four separate OLS fits
          for (p in PERIODS$period) {
            rp <- rows[b$period[rows] == p]
            f <- fit_ols(b$DFER[rp], as.matrix(b[rp, xv]))
            cf <- stats::setNames(f$coef$coef, paste0("R", p, ":", f$coef$term))
            k <- intersect(names(cf), nm)
            chk <- max(chk, abs(cf[k] - bb[k]) / pmax(1, abs(bb[k])))
          }
        } else add_test("expectation equal in all regimes", 1:4, expvar)
      }
    }
    }
  }
  if (chk < 1e-6) lg$info("CHECK PASS  fully interacted pooled OLS equals the separate per-period OLS fits")
  else lg$error(sprintf("pooled full-interaction coefficients differ from per-period OLS (max relative gap %g)", chk))
  coefs <- do.call(rbind, coefs); tests <- do.call(rbind, tests)
  write_table(coefs, "corr_pooled_coefs.csv", lg)
  write_table(tests, "corr_pooled_tests.csv", lg)
  lg$info(sprintf("Newey-West lag by rule floor(4(n/100)^(2/9)): %s (n = %s)",
                  paste(unique(coefs$nw_lag), collapse = "/"), paste(unique(coefs$n), collapse = "/")))
  for (mdl in unique(coefs$model)) {
    ev <- if (mdl == "I") "DER_hathat" else "DNDF"
    for (type in POOLED_TYPES) {
      x <- keep_s2c(coefs[coefs$model == mdl & coefs$term == ev & coefs$type == type & coefs$form == "level", ])
      for (ver in CORR_VERSIONS) {
        y <- x[x$version == ver, ]; y <- y[order(y$regime), ]
        if (!nrow(y)) next
        lg$info(sprintf("  pooled %-8s %-2s %-9s %-4s %s", type, mdl, ver, if (mdl == "I" && s2c_of(ver)) "+s2c" else "",
                        paste(sprintf("R%d %s [%s]%s", y$regime, fmt(y$coef, 4), fmt(y$se_nw, 4),
                                      ifelse(is.na(y$p_nw), "", sig_label(sig_level(y$p_nw), ascii = TRUE))), collapse = "  ")))
      }
    }
  }
}

# ---- 7. bootstrap of the generated regressors ---------------------------------------------------------------
bootstrap <- function(panel, sel, h, B, lg) {
  conv <- CONVENTIONS[1]
  # runs: the headline version without a stage-2 constant (reproduction, step 2); the corrected data
  # without one (as in the first Week 4 run, for comparison) and with one (step 5, W4-20)
  runs <- unique(rbind(data.frame(version = h$version, s2c = FALSE), data.frame(version = "corrected", s2c = c(FALSE, TRUE))))
  lg$info(sprintf("Bootstrap: %d replications per scheme (%s), seed %d, runs %s, level form, convention %s",
                  B, paste(BOOT_SCHEMES, collapse = ", "), BOOT_SEED,
                  paste(sprintf("%s%s", runs$version, ifelse(runs$s2c, "+s2c", "")), collapse = ", "), conv))
  summ <- list(); draws <- list()
  for (ri in seq_len(nrow(runs))) {
    ver <- runs$version[ri]; s2c <- runs$s2c[ri]
    base <- make_base(panel, ver, "level", sel, conv)
    ch <- model_I_chain(base, conv, h$spec, h$lag, s2c)
    naive <- do.call(rbind, lapply(PERIODS$period, function(p) {
      rows <- eq_rows(ch$base, p, STAGE3_X)
      if (length(rows) < length(STAGE3_X) + 2) return(NULL)
      f <- fit_dfer(ch$base$DFER[rows], as.matrix(ch$base[rows, STAGE3_X]), p)
      if (!isTRUE(f$ok)) return(NULL)
      data.frame(period = p, term = f$coef$term, se_naive = f$coef$se, p_naive = f$coef$p, stringsAsFactors = FALSE)
    }))
    for (scheme in BOOT_SCHEMES) {
      t0 <- Sys.time()
      r <- boot_chain(base, conv, h$spec, h$lag, scheme, B, BOOT_SEED, lg, stage2_const = s2c)
      s <- merge(boot_summary(r$draws, r$orig), naive, by = c("period", "term"), all.x = TRUE)
      s$se_ratio <- s$se_boot / s$se_naive
      s$stars_naive <- sig_label(sig_level(s$p_naive))
      s$stars_boot <- sig_label(sig_level(s$p_boot))
      s$block_length <- if (scheme == "block") vapply(s$period, function(p) block_length(r$pools_n[p]), 1L) else NA_integer_
      s$n_pool <- if (scheme == "block") r$pools_n[s$period] else NA_integer_
      summ[[length(summ) + 1]] <- cbind(data.frame(model = "I", version = ver, form = "level", convention = conv,
                                                   spec = h$spec, lag = h$lag, stage2_const = s2c, scheme = scheme, B = B,
                                                   seed = BOOT_SEED, stringsAsFactors = FALSE), s)
      k <- grep(":DER_hathat$", colnames(r$draws), value = TRUE)
      if (length(k)) draws[[length(draws) + 1]] <- data.frame(model = "I", version = ver, stage2_const = s2c, scheme = scheme,
        replication = rep(seq_len(B), length(k)), period = rep(as.integer(sub("^P([1-4]):.*$", "\\1", k)), each = B),
        term = "DER_hathat", draw = as.numeric(r$draws[, k]), stringsAsFactors = FALSE)
      fail <- s[s$n_ok < 0.9 * B, ]
      if (nrow(fail)) lg$warn(sprintf("%s%s/%s: %d coefficient(s) with fewer than 90%% successful replications (see n_ok)",
                                      ver, if (s2c) "+s2c" else "", scheme, nrow(fail)))
      x <- s[s$term == "DER_hathat", ]; x <- x[order(x$period), ]
      lg$info(sprintf("  %-9s %-4s %-5s DER_hathat: %s  (%.0f s)", ver, if (s2c) "+s2c" else "", scheme,
                      paste(sprintf("P%d se %s -> %s (x%s)%s", x$period, fmt(x$se_naive, 4), fmt(x$se_boot, 4),
                                    fmt(x$se_ratio, 2), ifelse(is.na(x$p_boot), "", sig_label(sig_level(x$p_boot), ascii = TRUE))),
                            collapse = "  "), as.numeric(difftime(Sys.time(), t0, units = "secs"))))
    }
  }
  summ <- do.call(rbind, summ)
  write_table(summ, "corr_bootstrap.csv", lg)
  if (length(draws)) write_table(do.call(rbind, draws), "corr_bootstrap_draws.csv", lg)
  if (B < BOOT_B) lg$warn(sprintf("B = %d is a trial run; rerun without --B (B = %d) before writing the note", B, BOOT_B))
}

main <- function(lg) {
  args <- parse_args()
  if (!requireNamespace("sandwich", quietly = TRUE)) stop("package sandwich is not installed")
  lg$info("sandwich ", as.character(utils::packageVersion("sandwich")))
  sel <- read_selected_inputs(lg)
  if (is.na(sel$bop)) stop("selected_inputs.csv names no BOP column (W2-10)")
  panel <- read_panel(lg)
  if (!is.na(sel$ndf) && (!sel$ndf %in% names(panel) || anyNA(panel[[sel$ndf]]))) {
    lg$warn("NDF absent or incomplete: Model II parts skipped"); sel$ndf <- NA_character_
  }
  h <- read_headline(lg)
  cpi <- read_cpi(lg)
  lg$info("== 1. Data corrections: discrete quarterly GDP; fixed-base CPI (Plan s.5)")
  panel <- corrections_data(panel, cpi, sel, lg)
  cpi_checks(panel, cpi, lg)
  lg$info("== 2. Standard errors labelled as t-statistics (Plan s.5)")
  relabel(lg)
  lg$info("== 3. Per-period estimates: versions x forms x conventions (expectation term, primary convention)")
  est <- period_estimates(panel, sel, h, lg)
  lg$info("== 3b. W4-19 diagnostic: headline with a stage-2 constant (rule fixed before the run)")
  stage2_const_diag(est, h, lg)
  lg$info("== 4. Period 1: variation of the exchange rate and of the expectation term")
  period1_identification(panel, sel, h, lg)
  lg$info("== 5. Unit roots: DF-GLS and ADF")
  unit_roots(panel, lg)
  lg$info("== 6. Pooled model with regime interactions, Newey-West (primary convention)")
  pooled(panel, sel, h, lg)
  lg$info("== 7. Bootstrap of the three-stage chain (generated regressors)")
  bootstrap(panel, sel, h, args$B, lg)
  lg$info("Next: 09_outputs.R")
}

run_script("08_corrections", main)
