# code/R/check_dw_bounds.R -- Week 4 rerun, W3-09 "dL/dU check" (Study A)
#
# Original Table 4.3 prints, for each period's stage-1 regression (DER on DBOP, Di, Dpi, DGDP and a
# constant), the 5% Durbin-Watson bounds dL and dU it used. The bounds depend only on the number of
# observations n and the number of regressors k (excluding the constant), so they reveal the n the
# original had. Instead of reading a printed DW table, this script computes the exact bounds:
#
#   A = the n x n Durbin-Watson matrix, eigenvalues l_j = 2(1 - cos(pi (j-1)/n)), j = 1..n (l_1 = 0).
#   With a constant and k further regressors (k' = k + 1):
#     d_L = sum_{i=1}^{n-k'} l_{i+1}  z_i^2 / sum z_i^2      (the n-k' smallest non-zero eigenvalues)
#     d_U = sum_{i=1}^{n-k'} l_{i+k'} z_i^2 / sum z_i^2      (the n-k' largest)
#   The 5% bound c solves P(sum (l - c) z^2 < 0) = 0.05, computed with Imhof's (1961) formula.
#   This is how the published tables (Durbin-Watson 1951; Savin-White 1977) were built.
#
# For each Table 4.3 row it lists every (n, k) whose bounds, rounded to the printed decimals, equal
# the printed dL and dU, and compares n with the stage-1 sample under both conventions (W3-01).
# It checks itself against two textbook entries computed by the same method (see CHECK lines).
#
# Output: outputs/tables/check_dw_bounds.csv; run log in outputs/logs/.
# Usage:  Rscript code/R/check_dw_bounds.R

source(file.path("code", "R", "common.R"))
source(file.path("code", "R", "models.R"))

DW_N <- 6:200          # range of the Savin-White tables
DW_K <- 1:6            # regressors excluding the constant
ALPHA <- 0.05

dw_eigen <- function(n) 2 * (1 - cos(pi * (seq_len(n) - 1) / n))

# P(sum w_j z_j^2 < 0), z ~ N(0,1) independent (Imhof 1961, with x = 0)
imhof_below0 <- function(w) {
  w <- w[abs(w) > 1e-14]
  f <- function(u) {
    th <- 0.5 * colSums(atan(outer(w, u)))
    rho <- exp(0.25 * colSums(log1p(outer(w^2, u^2))))
    sin(th) / (u * rho)
  }
  # integrate over [0, 1] after u = t / (1 - t): the slowly decaying tail (few eigenvalues, small n)
  # becomes a finite interval
  g <- function(t) { u <- t / (1 - t); ifelse(t >= 1, 0, f(u) / (1 - t)^2) }
  v <- stats::integrate(g, 0, 1, subdivisions = 5000L, rel.tol = 1e-9, stop.on.error = FALSE)$value
  1 - (0.5 + v / pi)
}

dw_bound <- function(n, k, which = c("L", "U")) {
  which <- match.arg(which)
  kp <- k + 1L
  m <- n - kp
  if (m < 2) return(NA_real_)          # with one eigenvalue left, d is a constant
  l <- dw_eigen(n)
  w <- if (which == "L") l[2:(m + 1)] else l[(kp + 1):n]
  g <- function(c) imhof_below0(w - c) - ALPHA
  tryCatch(stats::uniroot(g, c(min(w) + 1e-9, max(w) - 1e-9), tol = 1e-10)$root, error = function(e) NA_real_)
}

main <- function(lg) {
  # self-check: two entries computed independently by the same exact method; the bounds of
  # textbook tables agree with them to the 3 printed decimals (rounded half up)
  chk <- data.frame(n = c(15L, 20L), k = c(1L, 2L))
  chk$dL <- mapply(dw_bound, chk$n, chk$k, MoreArgs = list(which = "L"))
  chk$dU <- mapply(dw_bound, chk$n, chk$k, MoreArgs = list(which = "U"))
  for (i in seq_len(nrow(chk)))
    lg$info(sprintf("CHECK  n = %d, k = %d: dL %.3f, dU %.3f (compare with any DW table)", chk$n[i], chk$k[i], chk$dL[i], chk$dU[i]))

  tg <- read_targets("T4.3")
  tg <- tg[tg$element == "value", ]
  lab <- tolower(tg$col_label)
  pick <- function(pat) { x <- tg[grepl(pat, lab), , drop = FALSE]; x[order(as.integer(x$row_idx)), , drop = FALSE] }
  dl <- pick("^dl\\s*\\("); du <- pick("^du\\s*\\(")    # not "durbin-watson ..."
  if (!nrow(dl) || nrow(dl) != nrow(du)) stop("Table 4.3: dL/dU columns not found (col_label 'dL (5%)', 'dU (5%)')")
  per <- row_model_period(dl$row_label)$period

  # stage-1 n per period and convention, from the panel (as in 06_diagnostics_chow.R)
  sel <- read_selected_inputs(lg)
  panel <- version_panel(read_panel(lg), "deflated", c(ORIGINAL_VARS, sel$bop))
  n_conv <- sapply(CONVENTIONS, function(conv) {
    b <- build_base(panel, conv, sel$bop)
    vapply(PERIODS$period, function(p) sum(b$period == p & stats::complete.cases(b[, c("DER", STAGE1_X)])), 1L)
  })
  lg$info(sprintf("Stage-1 n by period (k = %d regressors + constant): %s", length(STAGE1_X),
                  paste(sprintf("%s %s", colnames(n_conv), apply(n_conv, 2, paste, collapse = "/")), collapse = "; ")))

  lg$info(sprintf("Computing exact 5%% bounds for n = %d..%d, k = %d..%d ...", min(DW_N), max(DW_N), min(DW_K), max(DW_K)))
  grid <- expand.grid(n = DW_N, k = DW_K)
  grid <- grid[grid$n - grid$k - 1 >= 2, ]
  grid$dL <- mapply(dw_bound, grid$n, grid$k, MoreArgs = list(which = "L"))
  grid$dU <- mapply(dw_bound, grid$n, grid$k, MoreArgs = list(which = "U"))
  utils::write.csv(grid, file.path(PATHS$tables, "dw_bounds_exact.csv"), row.names = FALSE)
  lg$info(sprintf("Wrote %s (%d rows)", file.path(PATHS$tables, "dw_bounds_exact.csv"), nrow(grid)))

  if (anyNA(grid$dL) || anyNA(grid$dU)) lg$warn(sprintf("%d (n, k) pairs without a bound (numerical failure)", sum(is.na(grid$dL) | is.na(grid$dU))))
  grid <- grid[!is.na(grid$dL) & !is.na(grid$dU), ]
  out <- list()
  for (i in seq_len(nrow(dl))) {
    tl <- dl$value_text[i]; tu <- du$value_text[i]
    d <- max(printed_decimals(c(tl, tu)))
    hit <- grid[round(grid$dL, d) == round(as.numeric(tl), d) & round(grid$dU, d) == round(as.numeric(tu), d), ]
    near <- grid[order(abs(grid$dL - as.numeric(tl)) + abs(grid$dU - as.numeric(tu))), ][1:3, ]
    out[[i]] <- data.frame(period = per[i], printed_dL = tl, printed_dU = tu,
                           exact_match_nk = if (nrow(hit)) paste(sprintf("n=%d,k=%d", hit$n, hit$k), collapse = "; ") else "none",
                           closest_nk = paste(sprintf("n=%d,k=%d (%.3f/%.3f)", near$n, near$k, near$dL, near$dU), collapse = "; "),
                           n_full = n_conv[per[i], "full"], n_within = n_conv[per[i], "within"],
                           k_stage1 = length(STAGE1_X), stringsAsFactors = FALSE)
    lg$info(sprintf("  Period %d: printed dL %s, dU %s -> exact match %s; stage-1 n full %d, within %d",
                    per[i], tl, tu, out[[i]]$exact_match_nk, out[[i]]$n_full, out[[i]]$n_within))
    if (!nrow(hit)) lg$info(sprintf("    closest: %s", out[[i]]$closest_nk))
  }
  out <- do.call(rbind, out)
  write_table(out, "check_dw_bounds.csv", lg)
}

run_script("check_dw_bounds", main)
