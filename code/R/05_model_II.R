# code/R/05_model_II.R -- Week 3, Study A (Blueprint A.2 step A4; Replication Plan s.3.2, s.3.3)
#
# Model II (4-13)..(4-16), one equation per period:
#   DFER = g0 + g1 DNDF + g2 DGDP + g3 DNX + g4 DFDB + g5 DFDI + w
#   Periods 1, 3: AR(1) errors, exact ML (= Stata arima, arima(1,1,0)); Periods 2, 4: OLS
# for data version raw | deflated and convention full (primary) | within (W3-01):
# 2 x 2 x 4 = 16 regressions. There is no stage 2, so no spec or lag dimension.
#
# NDF enters as the column selected in Week 2 (selected_inputs.csv, W2-10): the 12-month
# USD/CNY NDF from Wind, reported "as specified" (Plan s.2.2). If the NDF is absent (no NDF
# row in selected_inputs.csv, no such column in the panel, or missing sample values), the
# script writes nothing, says why, and ends with STATUS WARN and exit code 0.
#
# Outputs (outputs/tables/): model_II_estimates.csv, model_II_fit.csv, model_II_arma_order.csv
# Usage:   Rscript code/R/05_model_II.R

source(file.path("code", "R", "common.R"))
source(file.path("code", "R", "models.R"))

# Returns a reason string if Model II cannot be estimated, "" otherwise
ndf_absent <- function(sel, panel) {
  if (is.na(sel$ndf)) return("selected_inputs.csv has no NDF row")
  if (!sel$ndf %in% names(panel)) return(sprintf("the panel has no column %s", sel$ndf))
  if (anyNA(panel[[sel$ndf]])) return(sprintf("%s has missing values in 2000-01..2011-12", sel$ndf))
  ""
}

main <- function(lg) {
  sel <- read_selected_inputs(lg)
  panel <- read_panel(lg)
  why <- ndf_absent(sel, panel)
  if (nzchar(why)) {
    lg$warn("Model II skipped: ", why, ". Nothing written. Model I (04) is unaffected; ",
            "06 and 07 skip their Model II parts in the same way.")
    return(invisible())
  }
  lg$info("ARIMA periods (AR(1) errors, exact ML; OPG and OIM SEs): ", paste(ARIMA_PERIODS, collapse = ", "),
          "; other periods OLS")
  est <- list(); fits <- list(); arma <- list()
  ll_bad <- 0L; nonconv <- 0L; failed <- 0L
  for (ver in VERSIONS) {
    dat <- version_panel(panel, ver, c(ORIGINAL_VARS, sel$bop, sel$ndf))
    for (conv in CONVENTIONS) {
      b <- build_base(dat, conv, if (is.na(sel$bop)) "FER" else sel$bop, sel$ndf)
      for (p in PERIODS$period) {
        keys <- data.frame(model = "II", version = ver, convention = conv, spec = NA_character_,
                           lag = NA_integer_, period = p, stringsAsFactors = FALSE)
        rows <- eq_rows(b, p, MODEL2_X)
        keys$first_month <- if (length(rows)) b$month[rows[1]] else NA_character_
        keys$last_month <- if (length(rows)) b$month[rows[length(rows)]] else NA_character_
        tag <- sprintf("II %s/%s P%d", ver, conv, p)
        if (length(rows) < length(MODEL2_X) + 2) {
          failed <- failed + 1L; lg$warn(tag, ": only ", length(rows), " usable months; not estimated"); next
        }
        y <- b$DFER[rows]; X <- as.matrix(b[rows, MODEL2_X])
        f <- fit_dfer(y, X, p)
        if (!isTRUE(f$ok)) {
          failed <- failed + 1L; lg$warn(tag, ": estimation failed: ", f$msg)
        } else if (f$estimator != "ols") {
          if (!isTRUE(f$converged)) nonconv <- nonconv + 1L
          if (is.na(f$ll_check) || abs(f$ll_check) > 1e-6) ll_bad <- ll_bad + 1L
        }
        est[[length(est) + 1]] <- tidy_fit(f, keys)
        fits[[length(fits) + 1]] <- fit_summary_row(f, keys)
        if (p %in% ARIMA_PERIODS) arma[[length(arma) + 1]] <- arma_order_row(y, X, keys)
      }
    }
  }
  est <- do.call(rbind, est); fits <- do.call(rbind, fits)
  write_table(est, "model_II_estimates.csv", lg)
  write_table(fits, "model_II_fit.csv", lg)
  write_table(do.call(rbind, arma), "model_II_arma_order.csv", lg)
  report_model(est, fits, ll_bad, nonconv, failed, "DNDF", lg)
  lg$info("Sign convention: NDF and ER are RMB per USD, so a fall is an appreciation (Plan s.5).")
}

run_script("05_model_II", main)
