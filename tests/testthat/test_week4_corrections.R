# tests/testthat/test_week4_corrections.R -- Week 4 tests of code/R/corrections.R (Study A)
# Simulated series and toy numbers only (set.seed); no original, SAFE, Wind, IMF or FRED values.
# Run from the project root:   Rscript -e "testthat::test_dir('tests/testthat')"

root <- normalizePath(file.path("..", ".."))
owd <- setwd(root)
source(file.path("code", "R", "common.R"))
source(file.path("code", "R", "models.R"))
source(file.path("code", "R", "corrections.R"))
setwd(owd)

set.seed(20261001)
sim_panel <- function() {
  m <- SAMPLE_MONTHS
  pid <- integer(length(m)); for (k in 1:4) pid[in_period(m, k)] <- k
  data.frame(month = m, period = pid, FER = 1000 + cumsum(rnorm(144, 5)), ER = 8 + cumsum(rnorm(144, 0, 0.02)),
             bop = rnorm(144, 100, 20), i = 2 + cumsum(rnorm(144, 0, 0.05)), pi = 100 + rnorm(144),
             GDP = 1000 + cumsum(rnorm(144)), NX = rnorm(144, 20, 5), FDB = 1500 + cumsum(rnorm(144)),
             FDI = 30 + rnorm(144), ndf = 8 + cumsum(rnorm(144, 0, 0.02)), stringsAsFactors = FALSE)
}
sel <- list(bop = "bop", ndf = "ndf")

test_that("year-to-date GDP becomes discrete quarterly GDP, held in each month", {
  m <- sprintf("2000-%02d", 1:12)
  q <- c(10, 12, 15, 20)                           # toy quarterly values
  ytd <- rep(cumsum(q), each = 3)
  g <- gdp_discrete(m, ytd)
  expect_equal(g$value, rep(q, each = 3))
  expect_length(g$quarters_nonconstant, 0)
  ytd2 <- ytd; ytd2[2] <- ytd2[2] + 1              # not constant within Q1: flagged
  expect_equal(gdp_discrete(m, ytd2)$quarters_nonconstant, "2000 1")
})

test_that("month arithmetic, year-on-year and rebasing", {
  expect_equal(month_shift("2000-01", -12), "1999-01")
  expect_equal(month_shift("2000-01", -1), "1999-12")
  m <- c(sprintf("1999-%02d", 1:12), sprintf("2000-%02d", 1:12))
  x <- c(rep(100, 12), rep(103, 12))
  expect_equal(yoy_index(x, m)[13:24], rep(103, 12))
  expect_true(all(is.na(yoy_index(x, m)[1:12])))
  expect_equal(mean(rebase_index(x, m, "2000")[13:24]), 100)
})

test_that("corrected version deflates the real variables only; ER, NDF, pi, i untouched", {
  p <- sim_panel(); p$GDP_q <- p$GDP; p$cpi_2000 <- seq(95, 130, length.out = 144)
  d <- corr_version_panel(p, "corrected", sel)
  for (v in c("FER", "NX", "FDI", "FDB", "bop")) expect_equal(d[[v]], p[[v]] / (p$cpi_2000 / 100))
  for (v in c("ER", "ndf", "pi", "i")) expect_equal(d[[v]], p[[v]])
  g <- corr_version_panel(p, "gdpq", sel)
  expect_equal(g$FER, p$FER)
  expect_equal(corr_version_panel(p, "deflated", sel)$ER, p$ER / (p$pi / 100))
})

test_that("log form logs the positive series and refuses non-positive values", {
  p <- sim_panel()
  l <- form_panel(p, "logdiff", sel)
  expect_equal(l$FER, log(p$FER)); expect_equal(l$NX, p$NX); expect_equal(l$bop, p$bop)
  p$FDI[5] <- -1
  expect_error(form_panel(p, "logdiff", sel), "FDI <= 0")
})

test_that("AR(1) wild draw with all signs +1 returns the original data", {
  n <- 40; Z <- cbind(`_cons` = 1, x = rnorm(n)); u <- as.numeric(stats::arima.sim(list(ar = 0.6), n))
  y <- as.numeric(Z %*% c(1, 2)) + u
  expect_equal(ar1_wild_y(y, Z, c(1, 2), 0.6, rep(1, n)), y)
  # with random signs the innovations keep their absolute size
  v <- rademacher(n); ys <- ar1_wild_y(y, Z, c(1, 2), 0.6, v)
  us <- ys - as.numeric(Z %*% c(1, 2))
  e <- c(u[1] * sqrt(1 - 0.36), u[-1] - 0.6 * u[-n]); es <- c(us[1] * sqrt(1 - 0.36), us[-1] - 0.6 * us[-n])
  expect_equal(abs(es), abs(e))
})

test_that("wild replicate with all signs +1 reproduces the three-stage estimates", {
  p <- sim_panel()
  base <- build_base(p, "full", "bop")
  ch <- model_I_chain(base, "full", "primary", 1)
  bs <- wild_replicate(base, boot_dgp(ch), rep(1, nrow(base)))
  expect_equal(bs$DER, base$DER)
  expect_equal(bs$DFER, base$DFER, tolerance = 1e-8)
  expect_equal(stage3_coef_vector(model_I_chain(bs, "full", "primary", 1)$base),
               stage3_coef_vector(ch$base), tolerance = 1e-5)
})

test_that("moving blocks: right length, inside the pool, consecutive within blocks", {
  idx <- block_indices(20, 3)
  expect_length(idx, 20)
  expect_true(all(idx >= 1 & idx <= 20))
  expect_equal(block_length(64), 4L); expect_equal(block_length(10), 2L)
  p <- sim_panel(); base <- build_base(p, "full", "bop")
  bs <- block_replicate(base, block_pools(base))
  expect_equal(bs$period, base$period)
  expect_true(all(bs$DFER[base$period == 2] %in% base$DFER[base$period == 2]))
})

test_that("fully interacted pooled OLS equals the separate per-period OLS fits", {
  p <- sim_panel(); b <- build_base(p, "full", "bop", "ndf")
  xv <- MODEL2_X; rows <- which(stats::complete.cases(b[, c("DFER", xv)]))
  d <- pooled_design(b, rows, xv[1], xv[-1], "full")
  bb <- stats::coef(fit_pooled(b$DFER[rows], d$X)$fit)
  for (r in 1:4) {
    rr <- rows[b$period[rows] == r]
    f <- stats::coef(stats::lm(b$DFER[rr] ~ as.matrix(b[rr, xv])))
    expect_equal(unname(bb[paste0("R", r, ":", c("_cons", xv))]), unname(f), tolerance = 1e-8)
  }
  e <- pooled_design(b, rows, xv[1], xv[-1], "exp_only")
  expect_equal(ncol(e$X), 4 + 4 + length(xv) - 1)
  z <- b; z$DNDF[z$period == 1] <- 0              # no variation in regime 1 -> dropped, reported
  expect_equal(pooled_design(z, rows, "DNDF", xv[-1], "full")$dropped, "R1:DNDF")
})

test_that("Newey-West: the lag rule, and our wrapper equals sandwich", {
  expect_equal(nw_lag_rule(100), 4L); expect_equal(nw_lag_rule(141), floor(4 * 1.41^(2 / 9)))
  x <- rnorm(120); y <- 1 + x + as.numeric(stats::arima.sim(list(ar = 0.5), 120))
  fit <- stats::lm(y ~ x)
  expect_equal(unname(nw_vcov(fit, 3)), unname(sandwich::NeweyWest(fit, lag = 3, prewhite = FALSE, adjust = TRUE)))
})

test_that("Wald test equals car::linearHypothesis with the same covariance", {
  skip_if_not_installed("car")
  x1 <- rnorm(80); x2 <- rnorm(80); y <- 1 + x1 + 0.5 * x2 + rnorm(80)
  fit <- stats::lm(y ~ x1 + x2); V <- sandwich::NeweyWest(fit, lag = 2, prewhite = FALSE, adjust = TRUE)
  R <- rbind(c(0, 1, -1)); colnames(R) <- names(stats::coef(fit))
  w <- wald_test(stats::coef(fit), V, R, fit$df.residual)
  lh <- car::linearHypothesis(fit, "x1 = x2", vcov. = V, test = "Chisq")
  expect_equal(w$W, lh$Chisq[2], tolerance = 1e-8)
  expect_equal(w$p_chi2, lh[2, "Pr(>Chisq)"], tolerance = 1e-8)
})

test_that("equality restrictions use only regimes where a term is identified", {
  nm <- c("R1:_cons", "R2:_cons", "R3:_cons", "R2:x", "R3:x")
  e <- equality_R(nm, c("_cons", "x"), 1:3)
  expect_equal(nrow(e$R), 3)                      # _cons: 2 restrictions; x: 1 (R1:x not identified)
  expect_equal(e$vars_used, c("_cons", "x"))
  expect_null(equality_R(nm, "x", 1:2)$R)         # only R2:x exists among regimes 1-2
})

test_that("relabel check: parentheses read as SE and as t", {
  tg <- data.frame(term = c("a", "b", "N"), target = c(2, 0.5, 60), target_se = c(0.5, 2, NA),
                   target_level = c(4L, 0L, NA), stringsAsFactors = FALSE)
  x <- relabel_check(tg)
  expect_equal(nrow(x), 2)
  expect_equal(x$implied_t_if_se, c(4, 0.25))
  expect_equal(x$stars_match_if_se, c(TRUE, TRUE))  # t = 4 -> ***; t = 0.25 -> none
  expect_equal(x$level_if_t, sig_level(2 * stats::pnorm(-c(0.5, 2))))
})

test_that("bootstrap summary: centred p-value and percentile interval", {
  d <- matrix(c(1, 2, 3, 4, 5), ncol = 1, dimnames = list(NULL, "P2:DER_hathat"))
  s <- boot_summary(d, c(`P2:DER_hathat` = 3))
  expect_equal(s$period, 2L); expect_equal(s$term, "DER_hathat")
  expect_equal(s$p_boot, mean(abs(1:5 - 3) >= 3))
  expect_equal(s$se_boot, stats::sd(1:5))
})
