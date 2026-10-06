# code/R/06_diagnostics_chow.R -- Week 3, Study A (Blueprint A.2 step A4; Replication Plan s.3.3)
#
# Reproduces the diagnostic tests of the original study and reports each next to its target:
#   Table 4.3 / 4.4  DW / BG of the stage-1 regressions (4-1)          Model I
#   Table 4.5 / 4.6  DW / BG of the stage-3 DFER regressions (4-3)     Model I
#   Table 4.7 / 4.8  DW / BG of the Model II DFER regressions (4-13)
#   Table 4.11       Chow tests between adjacent periods, Models I and II
#
# The original study tests for autocorrelation on OLS residuals BEFORE choosing ARIMA errors, so
# every DW and BG here comes from an OLS fit, in all four periods.
#   DW   d = sum (e_t - e_{t-1})^2 / sum e_t^2; judged with the dL/dU printed in the original document
#        (the only bounds we have from the source), plus the exact p-value (lmtest::dwtest)
#   BG   order 1, LM = n R^2 ~ chi2(1), missing lagged residuals set to 0 (Stata's default
#        for estat bgodfrey; lmtest::bgtest fill = 0)                        (W3-06)
#   Chow F = [(RSS_pooled - RSS_1 - RSS_2)/k] / [(RSS_1 + RSS_2)/(n1 + n2 - 2k)], OLS,
#        k = parameters incl. the constant, adjacent periods only                (W3-05)
#        "as specified": the estimating equation in differences, full convention;
#        diagnostic variants: within convention, and the same equation in LEVELS. The
#        variants only show which construction reproduces the original's degrees of freedom
#        and size of F; they are not corrections (those are Week 4, Plan s.5).
#
# Every DW/BG/Chow is computed for both data versions and both conventions; Model I stage-3
# tests for both stage-2 specs and lags 1-3. Model II parts are skipped if the NDF is absent.
#
# Outputs (outputs/tables/): diag_dw_bg.csv, diag_vs_targets.csv, chow.csv, chow_vs_targets.csv
# Usage:   Rscript code/R/06_diagnostics_chow.R

source(file.path("code", "R", "common.R"))
source(file.path("code", "R", "models.R"))

TEST_TABLES <- list(stage1 = c(dw = "T4.3", bg = "T4.4"), stage3_I = c(dw = "T4.5", bg = "T4.6"),
                    model_II = c(dw = "T4.7", bg = "T4.8"))

# ---- targets -----------------------------------------------------------------------------
dw_bg_targets <- function(lg) {
  tg <- read_targets(unlist(TEST_TABLES))
  out <- list()
  for (eq in names(TEST_TABLES)) for (test in c("dw", "bg")) {
    tid <- TEST_TABLES[[eq]][[test]]
    d <- tg[tg$table_id == tid, , drop = FALSE]
    if (!nrow(d)) { lg$warn(tid, " not found in original_targets.csv"); next }
    rm <- row_model_period(d$row_label)
    d <- cbind(d, rm)
    d <- d[!is.na(d$period), , drop = FALSE]
    cn <- d$col_norm
    role <- ifelse(grepl("durbin|dstatistic", cn), "stat",
            ifelse(cn == "chi2", "stat",
            ifelse(grepl("^prob", cn), "p",
            ifelse(grepl("^dl", cn), "dL",
            ifelse(grepl("^du", cn), "dU",
            ifelse(grepl("^judge", cn), "judge",
            ifelse(grepl("reject", cn), "decision", NA_character_)))))))
    d$role <- role
    for (p in sort(unique(d$period))) {
      x <- d[d$period == p, , drop = FALSE]
      g <- function(r, num = TRUE) {
        y <- x[x$role %in% r, , drop = FALSE]
        if (!nrow(y)) return(if (num) NA_real_ else "")
        if (num) to_num(y$value_text[1]) else trimws(y$cell_text[1])
      }
      out[[length(out) + 1]] <- data.frame(
        equation = eq, test = test, table_id = tid, period = p,
        target = g("stat"), target_text = g("stat", FALSE), target_p = g("p"),
        dL = g("dL"), dU = g("dU"), original_judge = g("judge", FALSE),
        original_decision = g("decision", FALSE),
        labels_confirmed = paste(unique(x$labels_confirmed), collapse = "/"), stringsAsFactors = FALSE)
    }
  }
  out <- do.call(rbind, out)
  lg$info(sprintf("DW/BG targets: %d rows from %s", nrow(out), paste(unique(out$table_id), collapse = ", ")))
  out
}

chow_targets <- function(lg) {
  tg <- read_targets("T4.11")
  if (!nrow(tg)) { lg$warn("Table 4.11 not found in original_targets.csv"); return(NULL) }
  l <- tolower(tg$row_label)
  tg$model <- ifelse(grepl("model\\s*ii\\b", l), "II", ifelse(grepl("model\\s*i\\b", l), "I", NA))
  tg$p1 <- suppressWarnings(as.integer(sub(".*period\\s*([1-4])\\s*and\\s*([1-4]).*", "\\1", l)))
  tg$p2 <- suppressWarnings(as.integer(sub(".*period\\s*([1-4])\\s*and\\s*([1-4]).*", "\\2", l)))
  tg <- tg[!is.na(tg$model) & grepl("period\\s*[1-4]\\s*and\\s*[1-4]", l), , drop = FALSE]
  out <- list()
  for (key in unique(paste(tg$model, tg$p1, tg$p2))) {
    x <- tg[paste(tg$model, tg$p1, tg$p2) == key, , drop = FALSE]
    ftxt <- trimws(x$cell_text[x$col_norm == "f"][1])
    m <- regmatches(ftxt, regexec("F\\s*\\(\\s*([0-9]+)\\s*,\\s*([0-9]+)\\s*\\)\\s*=\\s*([-+0-9.eE]+)", ftxt))[[1]]
    ptxt <- x[x$col_norm == "p", , drop = FALSE]
    out[[length(out) + 1]] <- data.frame(
      model = x$model[1], pair = sprintf("%d-%d", x$p1[1], x$p2[1]), target_text = ftxt,
      target_df1 = if (length(m)) as.integer(m[2]) else NA_integer_,
      target_df2 = if (length(m)) as.integer(m[3]) else NA_integer_,
      target_F = if (length(m)) to_num(m[4]) else NA_real_,
      target_p = if (nrow(ptxt)) to_num(ptxt$value_text[1]) else NA_real_,
      original_decision = trimws(x$cell_text[grepl("reject", x$col_norm)][1]),
      labels_confirmed = paste(unique(x$labels_confirmed), collapse = "/"), stringsAsFactors = FALSE)
  }
  out <- do.call(rbind, out)
  bad <- sum(is.na(out$target_df1))
  lg$info(sprintf("Chow targets: %d rows from Table 4.11%s", nrow(out),
                  if (bad) sprintf(" (%d F cell(s) not parsed: see target_text)", bad) else ""))
  out
}

# ---- tests -------------------------------------------------------------------------------
dw_bg_row <- function(f, keys) {
  if (is.null(f$fit)) return(NULL)
  e <- stats::residuals(f$fit)
  dwp <- tryCatch(unname(lmtest::dwtest(f$fit, alternative = "two.sided")$p.value), error = function(e) NA_real_)
  bg <- tryCatch(lmtest::bgtest(f$fit, order = 1, type = "Chisq", fill = 0), error = function(e) NULL)
  cbind(keys, n = length(e), k_regressors = f$k - 1L, dropped = paste(f$dropped, collapse = ";"),
        dw = dw_stat(e), dw_p_exact = dwp,
        bg_chi2 = if (is.null(bg)) NA_real_ else unname(bg$statistic),
        bg_p = if (is.null(bg)) NA_real_ else unname(bg$p.value), stringsAsFactors = FALSE)
}

chow_check <- function(y1, X1, y2, X2) {
  if (!requireNamespace("strucchange", quietly = TRUE)) return(NA_real_)
  d <- data.frame(y = c(y1, y2), rbind(X1, X2))
  tryCatch(unname(strucchange::sctest(y ~ ., data = d, type = "Chow", point = length(y1))$statistic),
           error = function(e) NA_real_)
}

# ---- data builders -----------------------------------------------------------------------
eq_data <- function(b, p, xvars) {
  rows <- eq_rows(b, p, xvars)
  list(y = b$DFER[rows], X = as.matrix(b[rows, xvars, drop = FALSE]))
}
levels_data <- function(dat, p, xvars) {
  rows <- which(dat$period == p & stats::complete.cases(dat[, c("FER", xvars)]))
  list(y = dat$FER[rows], X = as.matrix(dat[rows, xvars, drop = FALSE]))
}
screened <- function(A, B) {        # drop columns without variation in either sample
  keep <- vapply(seq_len(ncol(A$X)), function(j) stats::sd(A$X[, j]) > 1e-12 && stats::sd(B$X[, j]) > 1e-12, TRUE)
  list(A = list(y = A$y, X = A$X[, keep, drop = FALSE]), B = list(y = B$y, X = B$X[, keep, drop = FALSE]),
       dropped = colnames(A$X)[!keep])
}

main <- function(lg) {
  sel <- read_selected_inputs(lg)
  panel <- read_panel(lg)
  if (is.na(sel$bop) || !sel$bop %in% names(panel)) stop("selected BOP column missing (W2-10)")
  has_ndf <- !is.na(sel$ndf) && sel$ndf %in% names(panel) && !anyNA(panel[[sel$ndf]])
  if (!has_ndf) lg$warn("NDF absent: Model II diagnostics and Chow tests skipped (as in 05_model_II.R)")
  if (!requireNamespace("strucchange", quietly = TRUE))
    lg$warn("package strucchange not installed: the Chow cross-check is skipped")
  pairs <- list(c(1L, 2L), c(2L, 3L), c(3L, 4L))
  diag <- list(); chow <- list()
  for (ver in VERSIONS) {
    dat <- version_panel(panel, ver, c(ORIGINAL_VARS, sel$bop, if (has_ndf) sel$ndf))
    dat$period <- panel$period
    for (conv in CONVENTIONS) {
      base <- build_base(dat, conv, sel$bop, if (has_ndf) sel$ndf else NA_character_)
      # stage 1 (Table 4.3 / 4.4)
      for (p in PERIODS$period) {
        rows <- which(base$period == p & stats::complete.cases(base[, c("DER", STAGE1_X)]))
        f <- fit_ols(base$DER[rows], as.matrix(base[rows, STAGE1_X]))
        diag[[length(diag) + 1]] <- dw_bg_row(f, data.frame(equation = "stage1", model = "I", version = ver,
          convention = conv, spec = NA_character_, lag = NA_integer_, period = p))
      }
      # Model I stage 3 (Table 4.5 / 4.6) and Chow (Table 4.11)
      for (spec in SPECS) for (j in LAGS) {
        b <- model_I_chain(base, conv, spec, j)$base
        for (p in PERIODS$period) {
          e <- eq_data(b, p, STAGE3_X)
          if (length(e$y) < length(STAGE3_X) + 2) next
          f <- fit_ols(e$y, e$X)
          diag[[length(diag) + 1]] <- dw_bg_row(f, data.frame(equation = "stage3_I", model = "I", version = ver,
            convention = conv, spec = spec, lag = j, period = p))
        }
        for (pr in pairs) {
          s <- screened(eq_data(b, pr[1], STAGE3_X), eq_data(b, pr[2], STAGE3_X))
          chow[[length(chow) + 1]] <- cbind(data.frame(model = "I", variant = paste0("diff_", conv), version = ver,
            spec = spec, lag = j, pair = sprintf("%d-%d", pr[1], pr[2]), dropped = paste(s$dropped, collapse = ";")),
            chow_f(s$A$y, s$A$X, s$B$y, s$B$X), strucchange_F = chow_check(s$A$y, s$A$X, s$B$y, s$B$X))
        }
      }
      # Model II (Table 4.7 / 4.8) and Chow
      if (has_ndf) {
        for (p in PERIODS$period) {
          e <- eq_data(base, p, MODEL2_X)
          if (length(e$y) < length(MODEL2_X) + 2) next
          f <- fit_ols(e$y, e$X)
          diag[[length(diag) + 1]] <- dw_bg_row(f, data.frame(equation = "model_II", model = "II", version = ver,
            convention = conv, spec = NA_character_, lag = NA_integer_, period = p))
        }
        for (pr in pairs) {
          s <- screened(eq_data(base, pr[1], MODEL2_X), eq_data(base, pr[2], MODEL2_X))
          chow[[length(chow) + 1]] <- cbind(data.frame(model = "II", variant = paste0("diff_", conv), version = ver,
            spec = NA_character_, lag = NA_integer_, pair = sprintf("%d-%d", pr[1], pr[2]),
            dropped = paste(s$dropped, collapse = ";")),
            chow_f(s$A$y, s$A$X, s$B$y, s$B$X), strucchange_F = chow_check(s$A$y, s$A$X, s$B$y, s$B$X))
        }
      }
    }
    # levels variant (no differencing, so no convention): FER on the level counterparts
    lv <- list(I = c("ER", "NX", "FDB", "FDI"), II = c(if (has_ndf) sel$ndf, "GDP", "NX", "FDB", "FDI"))
    if (!has_ndf) lv$II <- NULL
    for (mdl in names(lv)) for (pr in pairs) {
      s <- screened(levels_data(dat, pr[1], lv[[mdl]]), levels_data(dat, pr[2], lv[[mdl]]))
      chow[[length(chow) + 1]] <- cbind(data.frame(model = mdl, variant = "levels", version = ver,
        spec = NA_character_, lag = NA_integer_, pair = sprintf("%d-%d", pr[1], pr[2]),
        dropped = paste(s$dropped, collapse = ";")),
        chow_f(s$A$y, s$A$X, s$B$y, s$B$X), strucchange_F = chow_check(s$A$y, s$A$X, s$B$y, s$B$X))
    }
  }
  diag <- do.call(rbind, diag); chow <- do.call(rbind, chow)

  # ---- checks ----
  dmax <- suppressWarnings(max(abs(chow$F - chow$strucchange_F), na.rm = TRUE))
  if (is.finite(dmax)) {
    rel <- suppressWarnings(max(abs(chow$F - chow$strucchange_F) / pmax(1, abs(chow$F)), na.rm = TRUE))
    if (rel < 1e-6) lg$info(sprintf("CHECK PASS  Chow F by hand equals strucchange::sctest (%d tests)",
                                    sum(!is.na(chow$strucchange_F))))
    else lg$error(sprintf("Chow F by hand differs from strucchange::sctest (max relative gap %g)", rel))
  }
  nd <- sum(!is.na(chow$dropped) & nzchar(chow$dropped))
  if (nd) lg$info(sprintf("%d Chow test(s) dropped a regressor without variation (column 'dropped')", nd))
  write_table(diag, "diag_dw_bg.csv", lg)
  write_table(chow, "chow.csv", lg)

  # ---- DW / BG next to targets ----
  tg <- dw_bg_targets(lg)
  long <- rbind(
    cbind(diag[, setdiff(names(diag), c("bg_chi2", "bg_p", "dw_p_exact"))], test = "dw",
          ours = diag$dw, ours_p = diag$dw_p_exact),
    cbind(diag[, setdiff(names(diag), c("bg_chi2", "bg_p", "dw_p_exact"))], test = "bg",
          ours = diag$bg_chi2, ours_p = diag$bg_p))
  vt <- merge(long, tg, by = c("equation", "test", "period"))
  vt$gap <- vt$ours - vt$target
  vt$within_print <- !is.na(vt$gap) & abs(vt$gap) <= print_tolerance(vt$target_text)
  vt$our_decision <- ifelse(vt$test == "dw", dw_judge(vt$ours, vt$dL, vt$dU),
                            ifelse(is.na(vt$ours_p), NA_character_, ifelse(vt$ours_p < 0.05, "reject", "fail to reject")))
  vt$original_decision_norm <- norm_decision(vt$original_decision)
  vt$same_decision <- vt$our_decision == vt$original_decision_norm
  vt <- vt[order(vt$table_id, vt$version, vt$convention, vt$spec, vt$lag, vt$period), ]
  write_table(vt, "diag_vs_targets.csv", lg)
  for (tid in unique(vt$table_id)) for (ver in VERSIONS) for (conv in CONVENTIONS) {
    x <- vt[vt$table_id == tid & vt$version == ver & vt$convention == conv, ]
    if (!nrow(x)) next
    lg$info(sprintf("%s %-8s %-6s: %d of %d statistics within print, %d of %d decisions as in the original",
                    tid, ver, conv, sum(x$within_print), nrow(x), sum(x$same_decision, na.rm = TRUE), nrow(x)))
  }
  s1 <- unique(diag[diag$equation == "stage1", c("convention", "period", "n")])
  for (conv in CONVENTIONS)
    lg$info(sprintf("Stage-1 n by period (%s): %s", conv,
                    paste(s1$n[s1$convention == conv][order(s1$period[s1$convention == conv])], collapse = "/")))

  # ---- Chow next to targets ----
  ct <- chow_targets(lg)
  if (!is.null(ct)) {
    cv <- merge(chow, ct, by = c("model", "pair"))
    cv$df_match <- cv$df1 == cv$target_df1 & cv$df2 == cv$target_df2
    cv$log10_F_ratio <- log10(cv$F / cv$target_F)
    cv <- cv[order(cv$model, cv$pair, cv$variant, cv$version, cv$spec, cv$lag), ]
    write_table(cv, "chow_vs_targets.csv", lg)
    for (key in unique(paste(cv$model, cv$pair))) {
      x <- cv[paste(cv$model, cv$pair) == key, ]
      lg$info(sprintf("Chow Model %s, periods %s: original %s", x$model[1], x$pair[1], x$target_text[1]))
      for (v in unique(x$variant)) for (ver in VERSIONS) {
        y <- x[x$variant == v & x$version == ver, ]
        if (!nrow(y)) next
        lg$info(sprintf("    %-11s %-8s df (%s)  df match: %s  F range %s .. %s",
                        v, ver, paste(unique(sprintf("%d,%d", y$df1, y$df2)), collapse = " | "),
                        if (all(y$df_match, na.rm = TRUE)) "yes" else if (any(y$df_match, na.rm = TRUE)) "some" else "no",
                        fmt(min(y$F, na.rm = TRUE), 2), fmt(max(y$F, na.rm = TRUE), 2)))
      }
    }
  }
  lg$info("Chow assumptions to check in the tables: equal error variance in both periods (var_ratio, ",
          "var_ratio_p), no autocorrelation (DW/BG above), and more observations than parameters in each period.")
}

run_script("06_diagnostics_chow", main)
