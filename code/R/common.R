# code/R/common.R -- shared helpers for the Study A R scripts (Week 2 onward)
#
# Sourced at the top of 01_import_verify.R, 02_unit_roots.R and 03_select_bop_ndf.R.
# Contains no data values: only paths, the period definitions of the Replication Plan
# (s.3.3), functions, and the constants of the DF-GLS method itself (ERS 1996).
# Base R + urca + lmtest only, so every step is visible.

if (!file.exists(file.path("code", "R", "common.R"))) {
  stop("Run from the project root (the folder that contains code/ and data/).")
}

# ---- paths ------------------------------------------------------------------------------
PATHS <- list(
  appendix     = file.path("data", "interim", "original_appendix.csv"),
  targets      = file.path("data", "metadata", "original_targets.csv"),
  label_map    = file.path("config", "original_label_map.csv"),
  ndf_monthly  = file.path("data", "interim", "ndf_monthly.csv"),
  bop_monthly  = file.path("data", "interim", "bop_monthly_candidates.csv"),
  panel        = file.path("data", "interim", "study_a_monthly.csv"),   # git-ignored (W2-08)
  transform    = file.path("data", "metadata", "transformation_log.csv"),
  tables       = file.path("outputs", "tables"),
  logs         = file.path("outputs", "logs")
)
dir.create(PATHS$tables, recursive = TRUE, showWarnings = FALSE)
dir.create(PATHS$logs, recursive = TRUE, showWarnings = FALSE)

# Replication Plan s.3.3: the four periods (months inclusive)
PERIODS <- data.frame(
  period = 1:4,
  start  = c("2000-01", "2005-07", "2006-06", "2008-06"),
  end    = c("2005-06", "2006-05", "2008-05", "2011-12"),
  stringsAsFactors = FALSE
)
SAMPLE_MONTHS <- sprintf("%d-%02d", rep(2000:2011, each = 12), rep(1:12, 12))

# Original-study variables with data in Appendix I (column names written by extract_original.py)
ORIGINAL_VARS <- c("FER", "GDP", "ER", "AER", "NX", "pi", "FDI", "FDB", "i")

# ---- run log (same line format and STATUS line as the Python scripts) -------------------
new_log <- function(script) {
  lg <- new.env()
  lg$path <- file.path(PATHS$logs, sprintf("%s_%s.log", script, format(Sys.time(), "%Y%m%d-%H%M%S")))
  lg$con <- file(lg$path, open = "wt", encoding = "UTF-8")
  lg$n_warn <- 0L
  lg$n_err <- 0L
  lg$line <- function(level, msg) {
    for (m in strsplit(msg, "\n", fixed = TRUE)[[1]]) {
      l <- sprintf("%s %-5s %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), level, m)
      cat(l, "\n", sep = "")
      writeLines(l, lg$con)
    }
    flush(lg$con)
    if (level == "WARN") lg$n_warn <- lg$n_warn + 1L
    if (level == "ERROR") lg$n_err <- lg$n_err + 1L
  }
  lg$info  <- function(...) lg$line("INFO", paste0(...))
  lg$warn  <- function(...) lg$line("WARN", paste0(...))
  lg$error <- function(...) lg$line("ERROR", paste0(...))
  lg$finish <- function() {
    status <- if (lg$n_err > 0) "FAIL" else if (lg$n_warn > 0) "WARN" else "OK"
    lg$line("INFO", sprintf("STATUS: %s (warnings=%d, errors=%d)", status, lg$n_warn, lg$n_err))
    close(lg$con)
    cat("Run log:", lg$path, "\n")
    invisible(status)
  }
  lg$info(sprintf("%s on %s; urca %s, lmtest %s", R.version.string, R.version$platform,
                  as.character(utils::packageVersion("urca")),
                  as.character(utils::packageVersion("lmtest"))))
  lg
}

# Runs main(lg) inside tryCatch, closes the log, sets the exit code for Rscript.
run_script <- function(script, main) {
  lg <- new_log(script)
  tryCatch(main(lg), error = function(e) {
    lg$error(conditionMessage(e))
    cl <- conditionCall(e)
    if (!is.null(cl)) lg$info("  in: ", paste(deparse(cl), collapse = " ")[1])
  })
  status <- lg$finish()
  if (!interactive()) quit(save = "no", status = if (status == "FAIL") 1 else 0)
  invisible(status)
}

# ---- reading files ----------------------------------------------------------------------
read_chr_csv <- function(path) {
  if (!file.exists(path)) stop(sprintf("%s not found", path))
  utils::read.csv(path, colClasses = "character", check.names = FALSE,
                  encoding = "UTF-8", na.strings = character(0))
}

to_num <- function(x) suppressWarnings(as.numeric(x))

# Label normaliser used to match original-document labels to column names
norm_label <- function(x) {
  x <- gsub("\u03c0|\U0001d70b", "pi", x)
  tolower(gsub("[^A-Za-z0-9]", "", x))
}

# Original targets (original_targets.csv from extract_original.py), numeric cells only
read_targets <- function(tables = NULL) {
  t <- read_chr_csv(PATHS$targets)
  if (!is.null(tables)) t <- t[t$table_id %in% tables, , drop = FALSE]
  t$value <- ifelse(t$element == "value", to_num(t$value_text), NA_real_)
  t$row_norm <- norm_label(t$row_label)
  t$col_norm <- norm_label(t$col_label)
  t
}

# Number of decimals printed in a target (e.g. "-6.707" -> 3); tolerance = half a unit
# of the last printed digit (decision W2-07).
printed_decimals <- function(txt) {
  out <- rep(0L, length(txt))
  has <- grepl("\\.[0-9]+", txt)
  out[has] <- nchar(sub("^[^.]*\\.([0-9]+).*$", "\\1", txt[has]))
  out
}
print_tolerance <- function(txt) 0.5 * 10^(-printed_decimals(txt)) + 1e-9

# ---- data versions ----------------------------------------------------------------------
# Replication Plan s.2.1: the article's deflated data divide each variable by pi/100.
# pi itself is not deflated (it is the deflator).
deflate <- function(df, vars) {
  out <- df
  for (v in setdiff(vars, "pi")) out[[v]] <- df[[v]] / (df$pi / 100)
  out
}

# First difference on the full monthly series (decision W2-05); first month is NA
d1 <- function(x) c(NA_real_, diff(x))

in_period <- function(month, p) month >= PERIODS$start[p] & month <= PERIODS$end[p]

# ---- DF-GLS (Elliott, Rothenberg & Stock 1996) ------------------------------------------
# GLS detrending exactly as urca::ur.ers: abar = 1 + cbar/n, cbar = -7 (constant) or
# -13.5 (constant and trend), n = number of observations of y.
gls_detrend <- function(y, det) {
  n <- length(y)
  cbar <- if (det == "trend") -13.5 else -7
  a <- 1 + cbar / n
  ya <- c(y[1], y[-1] - a * y[-n])
  z1 <- c(1, rep(1 - a, n - 1))
  if (det == "trend") {
    tr <- seq_len(n)
    z2 <- c(1, tr[-1] - a * tr[-n])
    b <- stats::coef(stats::lm(ya ~ 0 + z1 + z2))
    y - b[1] - b[2] * tr
  } else {
    b <- stats::coef(stats::lm(ya ~ 0 + z1))
    y - b[1]
  }
}

# Schwert (1989) maximum lag used by Stata's dfgls: int(12 * (n/100)^(1/4)),
# n = number of observations of y
schwert_kmax <- function(n) as.integer(floor(12 * (n / 100)^0.25))

# One DF-GLS regression: dyd_t on yd_{t-1} and k lags of dyd_t, no deterministic terms,
# for t = first_t..n (1-based positions in y).
dfgls_fit <- function(yd, k, first_t) {
  n <- length(yd)
  tt <- first_t:n
  dy <- yd[tt] - yd[tt - 1]
  X <- matrix(yd[tt - 1], ncol = 1, dimnames = list(NULL, "ylag"))
  if (k > 0) for (j in seq_len(k)) {
    X <- cbind(X, yd[tt - j] - yd[tt - j - 1])
    colnames(X)[ncol(X)] <- paste0("dlag", j)
  }
  fit <- stats::lm(dy ~ 0 + X)
  cf <- summary(fit)$coefficients
  ssr <- sum(stats::residuals(fit)^2)
  N <- length(tt)
  list(tau = cf[1, 3], b0 = cf[1, 1], ssr = ssr, N = N,
       sum_ylag2 = sum(yd[tt - 1]^2),
       t_last = if (k > 0) cf[k + 1, 3] else NA_real_,
       p_last = if (k > 0) cf[k + 1, 4] else NA_real_)
}

# All lags 0..kmax for one series.
#   common = TRUE : every lag uses the sample available with kmax lags (Stata dfgls)
#   common = FALSE: each lag uses its own longest sample (urca::ur.ers with lag.max = k)
# Information criteria are always computed on the common sample (they are comparable
# only on one sample; Ng & Perron 2001).
dfgls_all_lags <- function(y, det, kmax = schwert_kmax(length(y))) {
  y <- as.numeric(y)
  if (anyNA(y)) stop("dfgls_all_lags: NA in series")
  yd <- gls_detrend(y, det)
  rows <- lapply(0:kmax, function(k) {
    fc <- dfgls_fit(yd, k, kmax + 2)
    fo <- dfgls_fit(yd, k, k + 2)
    s2 <- fc$ssr / fc$N
    tau_np <- fc$b0^2 * fc$sum_ylag2 / s2
    data.frame(k = k, tau_common = fc$tau, tau_own = fo$tau, N_common = fc$N, N_own = fo$N,
               maic = log(s2) + 2 * (tau_np + k) / fc$N,
               sic = log(s2) + k * log(fc$N) / fc$N,
               p_last_common = fc$p_last)
  })
  do.call(rbind, rows)
}

# Lag chosen by a rule from the output of dfgls_all_lags
choose_lag <- function(tab, rule) {
  if (rule == "maic") return(tab$k[which.min(tab$maic)])
  if (rule == "sic") return(tab$k[which.min(tab$sic)])
  if (rule == "seqt") {                   # Ng-Perron (1995) sequential t at 10%, from kmax down
    for (k in rev(tab$k[tab$k > 0])) if (tab$p_last_common[tab$k == k] < 0.10) return(k)
    return(0L)
  }
  if (grepl("^fixed_", rule)) {
    k <- as.integer(sub("^fixed_", "", rule))
    return(if (k %in% tab$k) k else NA_integer_)
  }
  stop("unknown lag rule ", rule)
}

# tau for one series under a full specification
dfgls_tau <- function(y, det, conv, rule) {
  tab <- dfgls_all_lags(y, det)
  k <- choose_lag(tab, rule)
  if (is.na(k)) return(list(tau = NA_real_, k = NA_integer_, N = NA_integer_, kmax = max(tab$k)))
  r <- tab[tab$k == k, ]
  list(tau = if (conv == "common") r$tau_common else r$tau_own, k = k,
       N = if (conv == "common") r$N_common else r$N_own, kmax = max(tab$k))
}

# urca's own statistic and critical values at lag k (own sample) -- the reference
urca_dfgls <- function(y, det, k) {
  u <- urca::ur.ers(as.numeric(y), type = "DF-GLS", model = det, lag.max = k)
  list(tau = as.numeric(u@teststat), cval = u@cval)
}

# ---- Durbin-Watson ----------------------------------------------------------------------
dw_stat <- function(res) sum(diff(res)^2) / sum(res^2)

# ---- small output helpers ---------------------------------------------------------------
write_table <- function(df, name, lg) {
  p <- file.path(PATHS$tables, name)
  utils::write.csv(df, p, row.names = FALSE, fileEncoding = "UTF-8", na = "")
  lg$info(sprintf("Wrote %s (%d rows)", p, nrow(df)))
  invisible(p)
}

fmt <- function(x, d = 3) ifelse(is.na(x), "NA", formatC(x, format = "f", digits = d))
