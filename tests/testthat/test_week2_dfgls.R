# tests/testthat/test_week2_dfgls.R -- Week 2 tests of code/R/common.R (Study A)
# Uses simulated series only (set.seed); no original-study, SAFE or Wind values.
# Run from the project root:   Rscript -e "testthat::test_dir('tests/testthat')"

root <- normalizePath(file.path("..", ".."))
owd <- setwd(root)
source(file.path("code", "R", "common.R"))
setwd(owd)

set.seed(20260928)
rw <- cumsum(rnorm(144))                 # simulated random walk
ar <- as.numeric(stats::arima.sim(list(ar = 0.5), n = 144))

test_that("own-sample tau equals urca::ur.ers at every lag", {
  for (det in c("constant", "trend")) for (y in list(rw, ar)) {
    tab <- dfgls_all_lags(y, det, kmax = 6)
    for (k in 0:6) {
      expect_equal(tab$tau_own[tab$k == k], urca_dfgls(y, det, k)$tau, tolerance = 1e-10)
    }
  }
})

test_that("common sample: every lag uses n - kmax - 1 observations", {
  tab <- dfgls_all_lags(rw, "trend")
  kmax <- schwert_kmax(length(rw))
  expect_equal(max(tab$k), kmax)
  expect_true(all(tab$N_common == length(rw) - kmax - 1))
  expect_equal(tab$N_own, length(rw) - tab$k - 1)
  expect_equal(tab$tau_common[tab$k == kmax], tab$tau_own[tab$k == kmax])
})

test_that("Schwert maximum lag follows int(12 (n/100)^(1/4))", {
  for (n in c(50, 100, 143, 144, 300)) expect_equal(schwert_kmax(n), floor(12 * (n / 100)^0.25))
})

test_that("lag rules return a lag inside 0..kmax", {
  tab <- dfgls_all_lags(ar, "constant")
  for (r in c("maic", "sic", "seqt", "fixed_2")) {
    k <- choose_lag(tab, r)
    expect_true(k >= 0 && k <= max(tab$k))
  }
  expect_equal(choose_lag(tab, "fixed_2"), 2L)
  expect_true(is.na(choose_lag(tab, "fixed_99")))
})

test_that("tau is invariant to rescaling a series (why hold and div3 tie)", {
  a <- dfgls_tau(rw, "trend", "common", "maic")
  b <- dfgls_tau(rw / 3, "trend", "common", "maic")
  expect_equal(a$tau, b$tau, tolerance = 1e-10)
  expect_equal(a$k, b$k)
})

test_that("DW by hand equals lmtest::dwtest", {
  x <- rnorm(60); y <- 1 + 2 * x + rnorm(60)
  fit <- lm(y ~ x)
  expect_equal(dw_stat(residuals(fit)), unname(lmtest::dwtest(fit)$statistic), tolerance = 1e-12)
})

test_that("tolerance is half a unit of the last printed digit", {
  expect_equal(print_tolerance("-6.707"), 0.0005 + 1e-9)
  expect_equal(print_tolerance("99"), 0.5 + 1e-9)
  expect_equal(print_tolerance("0.00"), 0.005 + 1e-9)
})

test_that("deflate divides by pi/100 and leaves pi unchanged", {
  df <- data.frame(ER = c(8, 8), pi = c(100, 102), GDP = c(10, 20))
  out <- deflate(df, c("ER", "pi", "GDP"))
  expect_equal(out$pi, df$pi)
  expect_equal(out$ER, df$ER / (df$pi / 100))
})

test_that("first differences keep the first month as NA", {
  expect_equal(d1(c(1, 3, 6)), c(NA, 2, 3))
})
