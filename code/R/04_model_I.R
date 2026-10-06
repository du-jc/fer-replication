# code/R/04_model_I.R -- Week 3, Study A (Blueprint A.2 step A4; Replication Plan s.3.1, s.3.3)
#
# Model I, three stages, for every combination of
#   data version  raw | deflated                         (Plan s.3.3)
#   convention    full (primary) | within                (W3-01, settles W2-05)
#   stage 2       primary (DER_hat on its lags) | alternative (DER on its lags)   (Plan s.3.1)
#   lag order j   1 | 2 | 3                               (Plan s.3.1)
#   period        1 | 2 | 3 | 4
# = 2 x 2 x 2 x 3 x 4 = 96 stage-3 regressions.
#
#   Stage 1 (4-1)  DER = b0 + b1 DBOP + b2 Di + b3 Dpi + b4 DGDP + x          OLS
#   Stage 2 (4-2)  Z(t) = sum_{i=1..j} a_i Z(t-i) + e, no constant            OLS
#                  Z = DER_hat (primary) or DER (alternative); fitted -> DER_hathat
#   Stage 3 (4-3)  DFER = t0 + t1 DER_hathat + t2 DNX + t3 DFDB + t4 DFDI + y
#                  Periods 1, 3: AR(1) errors, exact ML (= Stata arima, arima(1,1,0))
#                  Periods 2, 4: OLS
# Standard errors in stages 2-3 ignore that DER_hathat is estimated (the generated-regressor
# problem); they are reproduced as published and corrected in Week 4 (Plan s.5).
#
# BOP enters as the column selected in Week 2 (outputs/tables/selected_inputs.csv, W2-10).
#
# Outputs (outputs/tables/): model_I_stage1.csv, model_I_stage2.csv, model_I_estimates.csv,
#   model_I_fit.csv, model_I_arma_order.csv; run log outputs/logs/04_model_I_<ts>.log
# Usage:   Rscript code/R/04_model_I.R

source(file.path("code", "R", "common.R"))
source(file.path("code", "R", "models.R"))

# Estimate the stage-3 equation of one period; returns list(est, fit, arma) rows
stage3_period <- function(b, p, keys) {
  rows <- eq_rows(b, p, STAGE3_X)
  keys$first_month <- if (length(rows)) b$month[rows[1]] else NA_character_
  keys$last_month <- if (length(rows)) b$month[rows[length(rows)]] else NA_character_
  if (length(rows) < length(STAGE3_X) + 2)
    return(list(msg = sprintf("only %d usable months; not estimated", length(rows))))
  y <- b$DFER[rows]; X <- as.matrix(b[rows, STAGE3_X])
  f <- fit_dfer(y, X, p)
  list(f = f, est = tidy_fit(f, keys), fit = fit_summary_row(f, keys),
       arma = if (p %in% ARIMA_PERIODS) arma_order_row(y, X, keys) else NULL)
}

main <- function(lg) {
  sel <- read_selected_inputs(lg)
  if (is.na(sel$bop)) stop("selected_inputs.csv names no BOP column (W2-10)")
  panel <- read_panel(lg)
  if (!sel$bop %in% names(panel)) stop("panel has no column ", sel$bop)
  lg$info(sprintf("Grid: %d versions x %d conventions x %d stage-2 specs x %d lags x 4 periods",
                  length(VERSIONS), length(CONVENTIONS), length(SPECS), length(LAGS)))
  lg$info("ARIMA periods (AR(1) errors, exact ML; OPG SEs as in Stata, OIM SEs as in R): ",
          paste(ARIMA_PERIODS, collapse = ", "), "; other periods OLS")

  est <- list(); fits <- list(); st1 <- list(); st2 <- list(); arma <- list()
  ll_bad <- 0L; nonconv <- 0L; failed <- 0L
  for (ver in VERSIONS) {
    dat <- version_panel(panel, ver, c(ORIGINAL_VARS, sel$bop))
    for (conv in CONVENTIONS) {
      base <- build_base(dat, conv, sel$bop)
      for (spec in SPECS) for (j in LAGS) {
        ch <- model_I_chain(base, conv, spec, j)
        if (spec == SPECS[1] && j == LAGS[1])      # stage 1 does not depend on spec or lag
          st1[[length(st1) + 1]] <- cbind(version = ver, convention = conv, ch$stage1)
        if (!is.null(ch$stage2))
          st2[[length(st2) + 1]] <- cbind(version = ver, convention = conv, spec = spec, lag = j, ch$stage2)
        for (p in PERIODS$period) {
          keys <- data.frame(model = "I", version = ver, convention = conv, spec = spec, lag = j,
                             period = p, stringsAsFactors = FALSE)
          r <- stage3_period(ch$base, p, keys)
          tag <- sprintf("I %s/%s/%s/j=%d P%d", ver, conv, spec, j, p)
          if (is.null(r$f)) { failed <- failed + 1L; lg$warn(tag, ": ", r$msg); next }
          if (!isTRUE(r$f$ok)) {
            failed <- failed + 1L
            lg$warn(tag, ": estimation failed: ", r$f$msg)
          } else if (r$f$estimator != "ols") {
            if (!isTRUE(r$f$converged)) nonconv <- nonconv + 1L
            if (is.na(r$f$ll_check) || abs(r$f$ll_check) > 1e-6) ll_bad <- ll_bad + 1L
          }
          est[[length(est) + 1]] <- r$est
          fits[[length(fits) + 1]] <- r$fit
          if (!is.null(r$arma)) arma[[length(arma) + 1]] <- r$arma
        }
      }
    }
  }
  est <- do.call(rbind, est); fits <- do.call(rbind, fits)
  write_table(do.call(rbind, st1), "model_I_stage1.csv", lg)
  write_table(do.call(rbind, st2), "model_I_stage2.csv", lg)
  write_table(est, "model_I_estimates.csv", lg)
  write_table(fits, "model_I_fit.csv", lg)
  write_table(do.call(rbind, arma), "model_I_arma_order.csv", lg)
  report_model(est, fits, ll_bad, nonconv, failed, "DER_hathat", lg)
  lg$info("Next: 05_model_II.R, then 06_diagnostics_chow.R and 07_classify.R")
}

run_script("04_model_I", main)
