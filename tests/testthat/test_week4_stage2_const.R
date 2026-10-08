# tests/testthat/test_week4_stage2_const.R -- Week 4 rerun: the stage-2 constant (W4-19, W4-20), Study A
# Simulated series and toy numbers only (set.seed); no original, SAFE, Wind, IMF or FRED values.
# Run from the project root:   Rscript -e "testthat::test_dir('tests/testthat')"

root <- normalizePath(file.path("..", ".."))
owd <- setwd(root)
source(file.path("code", "R", "common.R"))
source(file.path("code", "R", "models.R"))
source(file.path("code", "R", "corrections.R"))
setwd(owd)

set.seed(20261008)
# A drifting exchange rate (as in the managed-appreciation periods), so the stage-2 constant matters
sim_base <- function(conv = "full") {
  m <- SAMPLE_MONTHS
  pid <- integer(length(m)); for (k in 1:4) pid[in_period(m, k)] <- k
  p <- data.frame(month = m, period = pid, FER = 1000 + cumsum(rnorm(144, 5)), ER = 8 + cumsum(rnorm(144, -0.02, 0.02)),
                  bop = rnorm(144, 100, 20), i = 2 + cumsum(rnorm(144, 0, 0.05)), pi = 100 + rnorm(144),
                  GDP = 1000 + cumsum(rnorm(144)), NX = rnorm(144, 20, 5), FDB = 1500 + cumsum(rnorm(144)),
                  FDI = 30 + rnorm(144), stringsAsFactors = FALSE)
  build_base(p, conv, "bop")
}
# Stage-2 regression of one period by hand: y = the stage-2 source, X = its lags 1..j
stage2_by_hand <- function(ch, base, spec, lag, p, conv = "full") {
  src <- if (spec == "primary") ch$base$DER_hat else base$DER
  L <- sapply(seq_len(lag), function(k) lag_conv(src, k, base$period, conv))
  L <- matrix(L, ncol = lag)
  r <- which(base$period == p & !is.na(src) & stats::complete.cases(L))
  list(y = src[r], X = L[r, , drop = FALSE], rows = r)
}
s2coef <- function(ch, p) { x <- ch$stage2[ch$stage2$period == p, ]; stats::setNames(x$coef, x$term) }

test_that("stage2_const = FALSE (the default) gives exactly the Week 3 chain", {
  b <- sim_base()
  for (spec in SPECS) {
    a <- model_I_chain(b, "full", spec, 2)
    z <- model_I_chain(b, "full", spec, 2, stage2_const = FALSE)
    expect_identical(a$base$DER_hathat, z$base$DER_hathat)
    for (p in 2:4) {                                # no constant: lm(y ~ 0 + lags)
      hb <- stage2_by_hand(a, b, spec, 2, p)
      expect_equal(unname(s2coef(a, p)), unname(stats::coef(stats::lm(hb$y ~ 0 + hb$X))), tolerance = 1e-6)
      expect_false("_cons" %in% names(s2coef(a, p)))
    }
  }
})

test_that("stage2_const = TRUE: stage 2 equals lm(y ~ lags) with an intercept, primary and alternative", {
  b <- sim_base()
  for (spec in SPECS) for (lag in 1:3) {
    ch <- model_I_chain(b, "full", spec, lag, stage2_const = TRUE)
    for (p in 2:4) {
      hb <- stage2_by_hand(ch, b, spec, lag, p)
      f <- stats::lm(hb$y ~ hb$X)
      expect_equal(unname(s2coef(ch, p)), unname(stats::coef(f)), tolerance = 1e-6)
      expect_equal(names(s2coef(ch, p))[1], "_cons")
      expect_equal(ch$base$DER_hathat[hb$rows], unname(stats::fitted(f)), tolerance = 1e-8)   # fitted value includes it
    }
  }
})

test_that("invariance: alternative, j = 1, OLS period -- stage-3 |t| of DER_hathat unchanged; tau1 * a unchanged", {
  b <- sim_base()
  for (p in setdiff(PERIODS$period, ARIMA_PERIODS)) {
    s3 <- function(s2c) {
      ch <- model_I_chain(b, "full", "alternative", 1, stage2_const = s2c)
      r <- eq_rows(ch$base, p, STAGE3_X)
      f <- fit_ols(ch$base$DFER[r], as.matrix(ch$base[r, STAGE3_X]))
      k <- f$coef$term == "DER_hathat"
      list(t = f$coef$stat[k], tau = f$coef$coef[k], a = s2coef(ch, p)[["L1"]])
    }
    w0 <- s3(FALSE); w1 <- s3(TRUE)
    # |t| is identical; its sign follows the sign of the stage-2 slope (the regressor is c + a DER(t-1)),
    # so t * sign(a) -- the t of the implied coefficient on DER(t-1) -- is identical
    expect_equal(abs(w1$t), abs(w0$t), tolerance = 1e-8)
    expect_equal(w1$t * sign(w1$a), w0$t * sign(w0$a), tolerance = 1e-8)
    expect_equal(w1$tau * w1$a, w0$tau * w0$a, tolerance = 1e-8)   # both = the stage-3 coefficient on DER(t-1)
    expect_false(isTRUE(all.equal(w1$tau, w0$tau)))                 # ... while the coefficient itself is rescaled
  }
})

test_that("wild bootstrap with all signs +1 returns the original data under stage2_const = TRUE", {
  b <- sim_base()
  ch <- model_I_chain(b, "full", "primary", 2, stage2_const = TRUE)
  bs <- wild_replicate(b, boot_dgp(ch), rep(1, nrow(b)))
  expect_equal(bs$DER, b$DER)
  expect_equal(bs$DFER, b$DFER, tolerance = 1e-8)
  expect_equal(stage3_coef_vector(model_I_chain(bs, "full", "primary", 2, stage2_const = TRUE)$base),
               stage3_coef_vector(ch$base), tolerance = 1e-5)
})

test_that("boot_chain passes stage2_const to every replication", {
  b <- sim_base()
  r0 <- boot_chain(b, "full", "alternative", 1, "wild", 3, 1L)
  r1 <- boot_chain(b, "full", "alternative", 1, "wild", 3, 1L, stage2_const = TRUE)
  ch1 <- model_I_chain(b, "full", "alternative", 1, stage2_const = TRUE)
  expect_equal(r1$orig, stage3_coef_vector(ch1$base))
  expect_false(isTRUE(all.equal(r1$orig, r0$orig)))
})

test_that("W4-19 reading rule: outcomes (a), (b), (c)", {
  d <- function(g0, g1, cls1 = c("reproduced", "reproduced")) data.frame(
    period = c(1, 3, 2, 2), term = c("_cons", "_cons", "DNX", "DFDB"),
    std_gap_without = c(g0, 1, 1), std_gap_with = c(g1, 1, 1),
    class_without = c("not reproduced", "not reproduced", "reproduced", "reproduced"),
    class_with = c("reproduced", "reproduced", cls1), stringsAsFactors = FALSE)
  expect_equal(stage2_rule(d(c(3, 4), c(0.5, 0.9)))$outcome, "a")
  expect_equal(stage2_rule(d(c(3, 4), c(0.5, 0.9), c("reproduced", "not reproduced")))$outcome, "b")  # another cell changes
  expect_equal(stage2_rule(d(c(3, 4), c(2, 3)))$outcome, "b")                                         # closer, not within 1
  expect_equal(stage2_rule(d(c(3, 4), c(2, 5)))$outcome, "c")                                         # one moves away
  expect_equal(stage2_rule(d(c(3, NA), c(0.5, 0.5)))$outcome, "c")                                    # a gap missing
  expect_equal(stage2_rule(d(c(3, 4), c(0.5, 0.9)))$n_changed_other, 0)
})

test_that("keep_s2c keeps one stage 2 per version: with a constant on corrected only (W4-20)", {
  x <- data.frame(model = c("I", "I", "I", "I", "II"), version = c("deflated", "deflated", "corrected", "corrected", "corrected"),
                  stage2_const = c("FALSE", "TRUE", "FALSE", "TRUE", NA), stringsAsFactors = FALSE)
  k <- keep_s2c(x)
  expect_equal(k$version, c("deflated", "corrected", "corrected"))
  expect_equal(k$stage2_const, c("FALSE", "TRUE", NA))
  expect_identical(keep_s2c(x[, 1:2]), x[, 1:2])
})
