# code/R/corrections.R -- shared helpers for the Week 4 scripts (08, 09), Study A
#
# Sourced after common.R and models.R by 08_corrections.R, 09_outputs.R and the Week 4 tests.
# Contains no data values: only the corrections of Replication Plan s.5, the choices made in
# the Week 4 chat (W4-01 .. W4-06) and functions.
#
#   data versions (each adds one correction to the one before it)
#     raw        the original data as extracted (Week 3)
#     deflated   every variable except pi divided by pi/100, as the article did (Week 3)
#     gdpq       raw, with the year-to-date GDP replaced by discrete quarterly GDP     (Plan s.5)
#     corrected  gdpq, with FER, GDP, NX, FDI, FDB and BOP divided by a fixed-base CPI
#                (IMF, rebased to 2000 = 100); ER, NDF, pi and i are NOT deflated     (Plan s.5)
#   forms
#     level      first differences of levels, as published
#     logdiff    first differences of logs for the positive series (FER, ER, NDF, GDP, FDB,
#                FDI); NX, BOP, pi and i stay in first differences of levels          (Plan s.5)
#   bootstrap (generated regressors, Plan s.5; W4-01)
#     wild       Rademacher sign flips of the stage-1 residuals and the stage-3 innovations,
#                the same draw for both equations in a month; AR(1) errors rebuilt in the
#                ARIMA periods; regressors and time order fixed                         PRIMARY
#     block      moving blocks of whole months within each period                    SECONDARY
#   pooled model (no power in Period 2, Plan s.5; W4-02)
#     full       every coefficient regime-specific (= the four separate OLS fits)      PRIMARY
#     exp_only   the expectation term and the constant regime-specific, controls common
#     Newey-West errors, lag floor(4 (n/100)^(2/9)); automatic bandwidth as sensitivity
#   stage-2 constant (Week 4 rerun)
#     W4-19  diagnostic: the headline re-estimated with a constant in stage 2, read by a rule
#            fixed before the run (stage2_rule below); the headline itself is unchanged (W3-04)
#     W4-20  step 4c, outside Plan s.5: Model I on the corrected data has a stage-2 constant,
#            and the steps built on it (bootstrap, log differences, pooled) use it too

CORR_VERSIONS <- c("raw", "deflated", "gdpq", "corrected")
FORMS <- c("level", "logdiff")
CPI_FILE <- file.path("data", "interim", "cpi_monthly.csv")
CORR_PANEL <- file.path("data", "interim", "study_a_corrected.csv")   # git-ignored folder
CPI_BASE_YEAR <- "2000"
BOOT_B <- 999L
BOOT_SEED <- 20261001L
BOOT_SCHEMES <- c("wild", "block")                 # first = primary (W4-01)
POOLED_TYPES <- c("full", "exp_only")              # first = primary (W4-02)
DIFF_COLS <- c("DFER", "DER", "DBOP", "Di", "Dpi", "DGDP", "DNX", "DFDB", "DFDI", "DNDF")

# ---- months ------------------------------------------------------------------------------
month_shift <- function(m, k) {
  t <- as.integer(substr(m, 1, 4)) * 12L + as.integer(substr(m, 6, 7)) - 1L + as.integer(k)
  sprintf("%d-%02d", t %/% 12L, t %% 12L + 1L)
}

# ---- GDP: year-to-date -> discrete quarterly (Plan s.2.1, s.5) --------------------------
# The appendix holds, in every month of quarter q, the cumulative GDP of quarters 1..q of that
# year. The discrete value of quarter q is YTD(q) - YTD(q-1), with YTD(0) = 0 each January.
# It is held in each month of the quarter (the same timing as the published YTD value).
gdp_discrete <- function(month, ytd) {
  yr <- substr(month, 1, 4)
  q <- (as.integer(substr(month, 6, 7)) - 1L) %/% 3L + 1L
  key <- paste(yr, q)
  last_in_q <- tapply(ytd, key, function(x) x[length(x)])
  nonconst <- tapply(ytd, key, function(x) diff(range(x)) > 0)
  prev_key <- ifelse(q == 1L, NA_character_, paste(yr, q - 1L))
  prev <- ifelse(q == 1L, 0, unname(last_in_q[prev_key]))
  val <- as.numeric(unname(last_in_q[key])) - as.numeric(prev)
  list(value = val, quarters_nonconstant = names(nonconst)[nonconst],
       months_nonpositive = month[!is.na(val) & val <= 0])
}

# ---- CPI ----------------------------------------------------------------------------------
read_cpi <- function(lg) {
  if (!file.exists(CPI_FILE)) stop(CPI_FILE, " not found: run code/python/collect_cpi.py first")
  c0 <- read_chr_csv(CPI_FILE)
  if (!all(c("month", "cpi_imf") %in% names(c0))) stop(CPI_FILE, " lacks the columns month, cpi_imf")
  c0$cpi_imf <- to_num(c0$cpi_imf)
  c0$cpi_fred <- if ("cpi_fred" %in% names(c0)) to_num(c0$cpi_fred) else NA_real_
  c0$cpi_nbs_yoy <- if ("cpi_nbs_yoy" %in% names(c0)) to_num(c0$cpi_nbs_yoy) else NA_real_
  st <- function(x) if (all(is.na(x))) "empty" else "present"
  lg$info(sprintf("CPI file %s: %d months (%s..%s); FRED check column %s; NBS year-on-year column %s", CPI_FILE,
                  nrow(c0), min(c0$month), max(c0$month), st(c0$cpi_fred), st(c0$cpi_nbs_yoy)))
  c0
}
rebase_index <- function(x, month, base_year = CPI_BASE_YEAR) {
  b <- mean(x[substr(month, 1, 4) == base_year])
  if (!is.finite(b)) stop("no CPI values in the base year ", base_year)
  100 * x / b
}
yoy_index <- function(x, month) 100 * x / x[match(month_shift(month, -12L), month)]

# ---- data versions -------------------------------------------------------------------------
# Adds GDP_q (discrete quarterly GDP) and cpi_2000 (IMF CPI, 2000 = 100) to the panel.
prepare_corrections <- function(panel, cpi, lg) {
  g <- gdp_discrete(panel$month, panel$GDP)
  if (length(g$quarters_nonconstant))
    lg$warn(sprintf("GDP is not constant within %d quarter(s) (%s); the last month of each quarter is used",
                    length(g$quarters_nonconstant), paste(utils::head(g$quarters_nonconstant, 4), collapse = ", ")))
  else lg$info("CHECK PASS  appendix GDP is constant within every quarter (year-to-date, held)")
  if (length(g$months_nonpositive))
    lg$warn(sprintf("discrete quarterly GDP <= 0 in %d month(s), first %s: the log form will fail there",
                    length(g$months_nonpositive), g$months_nonpositive[1]))
  panel$GDP_q <- g$value
  idx <- match(panel$month, cpi$month)
  if (anyNA(idx)) stop("CPI file lacks sample month(s): ", paste(utils::head(panel$month[is.na(idx)], 4), collapse = ", "))
  base_rows <- substr(cpi$month, 1, 4) == CPI_BASE_YEAR
  if (anyNA(cpi$cpi_imf[base_rows]) || sum(base_rows) != 12) stop("CPI base year ", CPI_BASE_YEAR, " incomplete")
  panel$cpi_2000 <- rebase_index(cpi$cpi_imf, cpi$month)[idx]
  if (anyNA(panel$cpi_2000)) stop("IMF CPI missing in the sample")
  panel
}

real_vars <- function(sel) intersect(c("FER", "GDP", "NX", "FDI", "FDB", sel$bop), ORIGINAL_VARS_PLUS(sel))
ORIGINAL_VARS_PLUS <- function(sel) unique(c(ORIGINAL_VARS, sel$bop, if (!is.na(sel$ndf)) sel$ndf))

corr_version_panel <- function(panel, version, sel) {
  cols <- ORIGINAL_VARS_PLUS(sel)
  if (version %in% c("raw", "deflated")) return(version_panel(panel, version, cols))
  d <- panel
  d$GDP <- panel$GDP_q
  if (version == "corrected") for (v in real_vars(sel)) d[[v]] <- d[[v]] / (d$cpi_2000 / 100)
  if (!version %in% CORR_VERSIONS) stop("unknown data version ", version)
  d
}

log_vars <- function(sel) c("FER", "ER", "GDP", "FDB", "FDI", if (!is.na(sel$ndf)) sel$ndf)

form_panel <- function(dat, form, sel) {
  if (form == "level") return(dat)
  for (v in log_vars(sel)) {
    bad <- which(dat[[v]] <= 0)
    if (length(bad)) stop(sprintf("log form: %s <= 0 in %d month(s), first %s", v, length(bad), dat$month[bad[1]]))
    dat[[v]] <- log(dat[[v]])
  }
  dat
}

make_base <- function(panel, version, form, sel, conv) {
  d <- form_panel(corr_version_panel(panel, version, sel), form, sel)
  build_base(d, conv, sel$bop, if (!is.na(sel$ndf) && sel$ndf %in% names(d)) sel$ndf else NA_character_)
}

# ---- Replication Plan s.5: t-statistics versus standard errors ----------------------------
# For each original cell, the parentheses read as an SE give t = b/SE; read as a t-statistic
# they give t = the number itself. The star level each reading implies is compared with the
# printed stars (normal p-values; the original prints no degrees of freedom for every column).
relabel_check <- function(tg, cuts = DEFAULT_CUTS) {   # cuts: the star legend of the original tables (W1-07)
  x <- tg[tg$term != "N" & !is.na(tg$target_se), , drop = FALSE]
  x$implied_t_if_se <- x$target / x$target_se
  x$p_if_se <- 2 * stats::pnorm(-abs(x$implied_t_if_se))
  x$p_if_t <- 2 * stats::pnorm(-abs(x$target_se))
  x$level_if_se <- sig_level(x$p_if_se, cuts)
  x$level_if_t <- sig_level(x$p_if_t, cuts)
  x$stars_match_if_se <- x$level_if_se == x$target_level
  x$stars_match_if_t <- x$level_if_t == x$target_level
  x
}

# First column of d among several possible names (Week 2 tables were renamed once: thesis -> original)
col_of <- function(d, candidates, what) {
  k <- intersect(candidates, names(d))
  if (!length(k)) stop(sprintf("%s: none of the columns %s found (columns: %s)", what,
                               paste(candidates, collapse = "/"), paste(names(d), collapse = ", ")))
  d[[k[1]]]
}

# ---- bootstrap of the three-stage chain (W4-01) ----------------------------------------------
rademacher <- function(n) sample(c(-1, 1), n, replace = TRUE)

# Regression with AR(1) errors: rebuild y from the fitted part and sign-flipped innovations.
# u = y - Z b; e_1 = sqrt(1 - phi^2) u_1, e_t = u_t - phi u_{t-1}; e* = e v;
# u*_1 = e*_1 / sqrt(1 - phi^2), u*_t = phi u*_{t-1} + e*_t; y* = Z b + u*.
ar1_wild_y <- function(y, Z, beta, phi, v) {
  u <- as.numeric(y - Z %*% beta)
  n <- length(u)
  s <- sqrt(max(1 - phi^2, 1e-8))
  e <- c(u[1] * s, u[-1] - phi * u[-n])
  es <- e * v
  us <- numeric(n)
  us[1] <- es[1] / s
  if (n > 1) for (t in 2:n) us[t] <- phi * us[t - 1] + es[t]
  as.numeric(Z %*% beta) + us
}

# Everything the wild bootstrap needs from the original fits
boot_dgp <- function(ch) {
  b3 <- ch$base
  s1 <- lapply(PERIODS$period, function(p) {
    f <- ch$stage1_fits[[p]]
    list(rows = f$rows, fitted = as.numeric(f$fitted), resid = as.numeric(f$resid))
  })
  s3 <- lapply(PERIODS$period, function(p) {
    rows <- eq_rows(b3, p, STAGE3_X)
    if (length(rows) < length(STAGE3_X) + 2) return(NULL)
    y <- b3$DFER[rows]
    X <- as.matrix(b3[rows, STAGE3_X])
    f <- fit_dfer(y, X, p)
    if (!isTRUE(f$ok)) return(NULL)
    if (f$estimator == "ols") return(list(rows = rows, type = "ols", fitted = as.numeric(f$fitted),
                                          resid = as.numeric(f$resid), fit = f))
    cf <- stats::setNames(f$coef$coef, f$coef$term)
    Z <- cbind(`_cons` = 1, X[, setdiff(colnames(X), f$dropped), drop = FALSE])
    list(rows = rows, type = "ar1", y = y, Z = Z, beta = cf[colnames(Z)], phi = unname(cf["L.ar"]), fit = f)
  })
  list(s1 = s1, s3 = s3)
}

wild_replicate <- function(base, dgp, v) {
  bs <- base
  for (p in PERIODS$period) {
    s <- dgp$s1[[p]]
    if (length(s$rows)) bs$DER[s$rows] <- s$fitted + s$resid * v[s$rows]
    t3 <- dgp$s3[[p]]
    if (is.null(t3)) next
    bs$DFER[t3$rows] <- if (t3$type == "ols") t3$fitted + t3$resid * v[t3$rows]
                        else ar1_wild_y(t3$y, t3$Z, t3$beta, t3$phi, v[t3$rows])
  }
  bs
}

# Moving-block resampling of whole months inside each period (block length per period)
block_length <- function(n) max(2L, as.integer(round(n^(1 / 3))))
block_indices <- function(n, L) {
  L <- min(L, max(1L, n - 1L))
  starts <- sample.int(n - L + 1L, ceiling(n / L), replace = TRUE)
  unlist(lapply(starts, function(s) s:(s + L - 1L)))[seq_len(n)]
}
block_pools <- function(base) {
  cols <- intersect(c("DER", STAGE1_X, "DFER", setdiff(STAGE3_X, "DER_hathat")), names(base))
  lapply(PERIODS$period, function(p) which(base$period == p & stats::complete.cases(base[, cols])))
}
block_replicate <- function(base, pools) {
  bs <- base
  cols <- intersect(DIFF_COLS, names(base))
  for (p in PERIODS$period) {
    r <- pools[[p]]
    if (length(r) < 3) next
    idx <- block_indices(length(r), block_length(length(r)))
    bs[r, cols] <- base[r[idx], cols]
  }
  bs
}

# Stage-3 coefficients of every period as a named vector "P<p>:<term>"
stage3_coef_vector <- function(b3) {
  out <- c()
  for (p in PERIODS$period) {
    rows <- eq_rows(b3, p, STAGE3_X)
    if (length(rows) < length(STAGE3_X) + 2) next
    f <- fit_dfer(b3$DFER[rows], as.matrix(b3[rows, STAGE3_X]), p)
    if (!isTRUE(f$ok) || !nrow(f$coef)) next
    out <- c(out, stats::setNames(f$coef$coef, paste0("P", p, ":", f$coef$term)))
  }
  out
}

# B replications of the whole chain; returns the draws (B x terms) and the original estimates.
# stage2_const is passed to every re-estimation, so each replication uses the same stage 2 (W4-20).
boot_chain <- function(base, conv, spec, lag, scheme, B, seed, lg = NULL, stage2_const = FALSE) {
  ch <- model_I_chain(base, conv, spec, lag, stage2_const)
  orig <- stage3_coef_vector(ch$base)
  dgp <- if (scheme == "wild") boot_dgp(ch) else NULL
  pools <- if (scheme == "block") block_pools(base) else NULL
  set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  draws <- matrix(NA_real_, B, length(orig), dimnames = list(NULL, names(orig)))
  t0 <- Sys.time()
  for (b in seq_len(B)) {
    bs <- if (scheme == "wild") wild_replicate(base, dgp, rademacher(nrow(base))) else block_replicate(base, pools)
    cf <- tryCatch(suppressWarnings(stage3_coef_vector(model_I_chain(bs, conv, spec, lag, stage2_const)$base)),
                   error = function(e) c())
    keep <- intersect(names(cf), colnames(draws))
    draws[b, keep] <- cf[keep]
    if (!is.null(lg) && b %% 200 == 0)
      lg$info(sprintf("    %s: %d of %d replications (%.0f s)", scheme, b, B,
                      as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  }
  list(draws = draws, orig = orig, pools_n = if (!is.null(pools)) vapply(pools, length, 1L) else NULL)
}

# Summaries: SE, percentile interval, and the centred two-sided p-value
# p = share of |b* - b_hat| >= |b_hat| (the bootstrap distribution of b_hat - b under H0: b = 0)
boot_summary <- function(draws, orig) {
  do.call(rbind, lapply(colnames(draws), function(k) {
    d <- draws[, k]; d <- d[is.finite(d)]
    b <- orig[[k]]
    data.frame(period = as.integer(sub("^P([1-4]):.*$", "\\1", k)), term = sub("^P[1-4]:", "", k),
               coef = b, n_ok = length(d), se_boot = if (length(d) > 1) stats::sd(d) else NA_real_,
               ci_lo = if (length(d)) unname(stats::quantile(d, 0.025)) else NA_real_,
               ci_hi = if (length(d)) unname(stats::quantile(d, 0.975)) else NA_real_,
               p_boot = if (length(d)) mean(abs(d - b) >= abs(b)) else NA_real_, stringsAsFactors = FALSE)
  }))
}

# ---- pooled model with regime interactions and Newey-West (W4-02, W4-03) ----------------------
# Regime number of a pooled coefficient name ("R3:DNX" -> 3; a common term -> NA)
regime_of <- function(nm) {
  out <- rep(NA_integer_, length(nm))
  has <- grepl("^R[1-4]:", nm)
  out[has] <- as.integer(substr(nm[has], 2, 2))
  out
}

nw_lag_rule <- function(n) as.integer(floor(4 * (n / 100)^(2 / 9)))

# Design matrix. Column names "R<r>:<var>" for regime-specific terms, "<var>" for common ones.
# A regime-specific column whose variable has no variation inside that regime is dropped
# (not identified, e.g. raw DER_hathat in Period 1).
pooled_design <- function(b, rows, expvar, controls, type) {
  reg <- b$period[rows]
  cols <- list(); dropped <- character(0)
  for (r in PERIODS$period) cols[[paste0("R", r, ":_cons")]] <- as.numeric(reg == r)
  inter <- if (type == "full") c(expvar, controls) else expvar
  for (v in inter) for (r in PERIODS$period) {
    x <- b[[v]][rows]
    nm <- paste0("R", r, ":", v)
    xr <- x[reg == r]
    if (stats::sd(xr) > 1e-12 * max(1, mean(abs(xr)))) cols[[nm]] <- ifelse(reg == r, x, 0)
    else dropped <- c(dropped, nm)
  }
  if (type == "exp_only") for (v in controls) cols[[v]] <- b[[v]][rows]
  X <- do.call(cbind, cols)
  list(X = X, dropped = dropped)
}

fit_pooled <- function(y, X) {
  fit <- stats::lm(y ~ 0 + X)
  names(fit$coefficients) <- colnames(X)
  al <- names(stats::coef(fit))[is.na(stats::coef(fit))]
  if (length(al)) {
    X <- X[, setdiff(colnames(X), al), drop = FALSE]
    fit <- stats::lm(y ~ 0 + X)
    names(fit$coefficients) <- colnames(X)
  }
  list(fit = fit, aliased = al, X = X)
}

nw_vcov <- function(fit, lag) {
  V <- sandwich::NeweyWest(fit, lag = lag, prewhite = FALSE, adjust = TRUE)
  dimnames(V) <- list(names(stats::coef(fit)), names(stats::coef(fit)))
  V
}

wald_test <- function(b, V, R, df2) {
  if (is.null(R) || !nrow(R)) return(data.frame(q = 0L, W = NA_real_, p_chi2 = NA_real_, F = NA_real_,
                                                df2 = df2, p_F = NA_real_))
  Rb <- R %*% b
  M <- R %*% V %*% t(R)
  W <- as.numeric(t(Rb) %*% solve(M, Rb))
  q <- nrow(R)
  data.frame(q = q, W = W, p_chi2 = stats::pchisq(W, q, lower.tail = FALSE), F = W / q, df2 = df2,
             p_F = stats::pf(W / q, q, df2, lower.tail = FALSE))
}

# Restriction matrix: for each variable, its coefficient is equal in the given regimes
# (only regimes where it is identified). Returns list(R, vars_used).
equality_R <- function(bnames, vars, regimes) {
  rows <- list(); used <- character(0)
  for (v in vars) {
    nm <- paste0("R", regimes, ":", v)
    nm <- nm[nm %in% bnames]
    if (length(nm) < 2) next
    used <- c(used, v)
    for (k in nm[-1]) {
      r <- stats::setNames(rep(0, length(bnames)), bnames)
      r[nm[1]] <- 1; r[k] <- -1
      rows[[length(rows) + 1]] <- r
    }
  }
  list(R = if (length(rows)) do.call(rbind, rows) else NULL, vars_used = used)
}

# ---- stage-2 constant (W4-19, W4-20) ------------------------------------------------------------
# Which stage 2 the Model I results of a data version use: with a constant on the corrected data
# (W4-20), without one on every other version (the printed equation, W3-04).
S2C_CORRECTED <- TRUE
s2c_of <- function(version) version == "corrected" & S2C_CORRECTED
as_lgl <- function(x) toupper(as.character(x)) %in% c("TRUE", "T", "1")
# Rows of a result table that follow that choice: Model II rows (no stage 2) always; Model I rows
# whose stage2_const equals s2c_of(version). A table without the column is returned unchanged.
keep_s2c <- function(d) {
  if (is.null(d) || !"stage2_const" %in% names(d)) return(d)
  ok <- d$model != "I" | as_lgl(d$stage2_const) == s2c_of(d$version)
  d[ok, , drop = FALSE]
}

# W4-19 reading rule, fixed before the run (decisions.md W4-19). diag: one row per Model I cell
# with period, term, class_without, class_with, std_gap_without, std_gap_with.
#   (a) with the stage-2 constant, the Period 1 and Period 3 constants both lie within one
#       published SE of the published values, and no other Model I cell changes class;
#   (b) both constants move closer (|std_gap| smaller in both periods), but (a) does not hold;
#   (c) otherwise. A constant without a std_gap (not estimated, or no published SE) fails (a), (b).
S2C_RULE_PERIODS <- c(1L, 3L)
S2C_OUTCOME_TEXT <- c(
  a = "the missing stage-2 constant is the likely explanation of the constant gaps",
  b = "the missing stage-2 constant is a partial explanation of the constant gaps",
  c = "the missing stage-2 constant is ruled out as the explanation of the constant gaps")
stage2_rule <- function(diag) {
  k <- diag$term == "_cons" & diag$period %in% S2C_RULE_PERIODS
  cons <- diag[k, , drop = FALSE]
  changed <- diag$class_without != diag$class_with
  have <- nrow(cons) == length(S2C_RULE_PERIODS) && all(is.finite(cons$std_gap_with)) && all(is.finite(cons$std_gap_without))
  within <- have && all(cons$std_gap_with <= 1)
  closer <- have && all(cons$std_gap_with < cons$std_gap_without)
  outcome <- if (within && !any(changed[!k])) "a" else if (closer) "b" else "c"
  list(outcome = outcome, n_changed = sum(changed), n_changed_other = sum(changed[!k]),
       n_cells = nrow(diag), constants_found = nrow(cons), cons = cons)
}
