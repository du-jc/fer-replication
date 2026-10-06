# tests/testthat/test_week3_models.R -- Week 3 tests of code/R/models.R (Study A)
# Simulated series only (set.seed); no values from the original document, SAFE or Wind.
# Run from the project root:   Rscript -e "testthat::test_dir('tests/testthat')"

root <- normalizePath(file.path("..", ".."))
owd <- setwd(root)
source(file.path("code", "R", "common.R"))
source(file.path("code", "R", "models.R"))
setwd(owd)

set.seed(20260929)
sim_ar1_reg <- function(n, phi = 0.5) {
  x1 <- cumsum(rnorm(n)); x2 <- cumsum(rnorm(n))
  u <- as.numeric(stats::arima.sim(list(ar = phi), n))
  y <- cumsum(0.4 + 1.5 * c(0, diff(x1)) - 0.7 * c(0, diff(x2)) + c(0, diff(u)))
  list(y = y, x1 = x1, x2 = x2)
}

test_that("differences and lags: full reaches into the previous period, within does not", {
  x <- c(1, 3, 6, 10, 15, 21)
  pid <- c(1, 1, 1, 2, 2, 2)
  expect_equal(d_conv(x, pid, "full"), c(NA, 2, 3, 4, 5, 6))
  expect_equal(d_conv(x, pid, "within"), c(NA, 2, 3, NA, 5, 6))
  expect_equal(lag_conv(x, 1, pid, "full"), c(NA, 1, 3, 6, 10, 15))
  expect_equal(lag_conv(x, 1, pid, "within"), c(NA, 1, 3, NA, 10, 15))
  expect_equal(lag_conv(x, 2, pid, "within"), c(NA, NA, 1, NA, NA, 10))
})

test_that("Stata's arima y x, arima(1,1,0) = R arima on differences with a constant", {
  s <- sim_ar1_reg(80)
  a <- stats::arima(diff(s$y), order = c(1, 0, 0), xreg = cbind(x1 = diff(s$x1), x2 = diff(s$x2)),
                    include.mean = TRUE, method = "ML")
  # the same model written in levels: d = 1 and a linear trend, whose difference is the constant
  b <- stats::arima(s$y, order = c(1, 1, 0), xreg = cbind(x1 = s$x1, x2 = s$x2, drift = seq_along(s$y)),
                    method = "ML")
  expect_equal(unname(coef(a)[c("ar1", "intercept", "x1", "x2")]),
               unname(coef(b)[c("ar1", "drift", "x1", "x2")]), tolerance = 1e-4)
  # R's arima with d = 1 and no trend has NO constant: a different model
  c0 <- stats::arima(s$y, order = c(1, 1, 0), xreg = cbind(x1 = s$x1, x2 = s$x2), method = "ML")
  expect_false(isTRUE(all.equal(unname(coef(a)["ar1"]), unname(coef(c0)["ar1"]), tolerance = 1e-3)))
})

test_that("own exact AR(1) log-likelihood equals stats::arima; scores are the gradient", {
  s <- sim_ar1_reg(90)
  y <- diff(s$y); X <- cbind(x1 = diff(s$x1), x2 = diff(s$x2))
  f <- fit_ar1_ml(y, X)
  expect_true(f$ok)
  expect_lt(abs(f$ll_check), 1e-6)
  b <- setNames(f$coef$coef, f$coef$term)
  Z <- cbind(`_cons` = 1, X)
  th <- c(b[colnames(Z)], b["L.ar"], s2 = f$sigma2)
  ll <- function(t) ar1_loglik_obs(y, Z, t[1:3], t[4], t[5])
  G <- ar1_scores(y, Z, th[1:3], th[4], th[5])
  h <- 1e-6
  Gn <- sapply(1:5, function(j) { e <- rep(0, 5); e[j] <- h; (ll(th + e) - ll(th - e)) / (2 * h) })
  expect_lt(max(abs(G - Gn)), 1e-5)
  # at the maximum the scores sum to (about) zero
  expect_lt(max(abs(colSums(G)) / sqrt(colSums(G^2))), 1e-2)
})

test_that("OPG and OIM standard errors agree in a large sample (asymptotic equivalence)", {
  s <- sim_ar1_reg(3000)
  f <- fit_ar1_ml(diff(s$y), cbind(x1 = diff(s$x1), x2 = diff(s$x2)))
  r <- f$coef$se_opg / f$coef$se_oim
  expect_true(all(abs(r - 1) < 0.15))
  expect_equal(f$coef$se, f$coef$se_opg)          # OPG is the reported SE (Stata default)
  expect_equal(f$coef$stat_type[1], "z")
})

test_that("a regressor without variation is reported as not identified, not dropped silently", {
  y <- rnorm(30); X <- cbind(DER_hathat = rep(0, 30), DNX = rnorm(30))
  f <- fit_ols(y, X)
  expect_equal(f$dropped, "DER_hathat")
  t <- tidy_fit(f, data.frame(model = "I", period = 1L))
  expect_equal(t$status[t$term == "DER_hathat"], "not_identified")
  g <- fit_ar1_ml(y, X)
  expect_equal(g$dropped, "DER_hathat")
  # without a constant, a nonzero constant column is kept; an all-zero one is dropped
  expect_equal(screen_regressors(cbind(a = rep(2, 5), b = rep(0, 5)), intercept = FALSE)$dropped, "b")
})

test_that("significance levels: default cut-offs, dagger = p < 0.10 (W1-07)", {
  expect_equal(sig_level(c(0.0005, 0.005, 0.03, 0.07, 0.2)), c(4L, 3L, 2L, 1L, 0L))
  expect_equal(sig_label(c(0L, 1L, 2L, 4L), ascii = TRUE), c("", "+", "*", "***"))
})

test_that("classification rule (W1-07: sign and star level; magnitude information only)", {
  # levels: 0 none, 1 dagger, 2 *, 3 **, 4 ***; significant = level >= 1 (p < 0.10)
  cl <- function(t, tl, b, p, st = "estimated") classify_cell(t, 0.5, tl, b, p, st)$class
  expect_equal(cl(1, 3L, 1.3, 0.005), "reproduced")              # same sign, ** and **
  expect_equal(cl(1, 3L, 9.0, 0.005), "reproduced")              # magnitude does not count
  expect_equal(cl(1, 0L, 0.2, 0.50), "reproduced")               # both insignificant
  expect_equal(cl(1, 0L, -0.2, 0.50), "reproduced")              # both insignificant, sign differs
  expect_equal(cl(1, 1L, 1.0, 0.07), "reproduced")               # dagger and dagger
  expect_equal(cl(1, 3L, 1.0, 0.03), "partially reproduced")     # ** vs *
  expect_equal(cl(1, 1L, 1.0, 0.0005), "partially reproduced")   # dagger vs ***: both significant
  expect_equal(cl(1, 3L, 1.0, 0.20), "not reproduced")           # significant -> insignificant
  expect_equal(cl(1, 0L, 1.0, 0.07), "not reproduced")           # insignificant -> significant (dagger)
  expect_equal(cl(1, 3L, -1.0, 0.005), "not reproduced")         # sign differs, both significant
  expect_equal(cl(1, 0L, -1.0, 0.005), "not reproduced")         # sign differs, ours significant
  x <- classify_cell(1, 0.5, 3L, NA, NA, "not_identified")
  expect_equal(c(x$class, x$reason), c("not reproduced", "not identified"))
  # magnitude kept as information
  y <- classify_cell(1, 0.5, 3L, 9.0, 0.005, "estimated")
  expect_false(y$within_one_se)
  expect_equal(y$std_gap, 16)
})

test_that("star legend is read from a table note (W1-07)", {
  cuts <- parse_star_legend("Note: t statistics in parentheses, * p < 0.05, ** p < 0.01, *** p < 0.001")
  expect_equal(unname(cuts), c(0.10, 0.05, 0.01, 0.001))
  # a different legend changes the levels assigned to the same p-values
  c2 <- parse_star_legend("* p<0.10, ** p<0.05, *** p<0.01")
  expect_null(c2)                                   # dagger (0.10) must lie above *, so not legible
  c3 <- parse_star_legend("* p<0.04, ** p<0.02, *** p<0.005")
  expect_equal(sig_level(c(0.03, 0.01, 0.001), c3), c(2L, 3L, 4L))
  expect_null(parse_star_legend("Note: t statistics in parentheses"))
  expect_null(parse_star_legend("* p < 0.05, ** p < 0.01"))
})

test_that("DW decision regions with given dL/dU", {
  expect_equal(dw_judge(c(0.5, 1.2, 2.0, 2.9, 3.6), 1.0, 1.5),
               c("reject", "invalid", "fail to reject", "invalid", "reject"))
  expect_equal(norm_decision(c("fail to reject", "Reject", "invalid")), c("fail to reject", "reject", "invalid"))
})

test_that("BG is n R^2 of the auxiliary regression (lmtest, fill = 0)", {
  x <- rnorm(40); y <- 1 + x + as.numeric(stats::arima.sim(list(ar = 0.4), 40))
  f <- fit_ols(y, cbind(x = x))
  e <- residuals(f$fit)
  aux <- lm(e ~ x + c(0, e[-40]))
  bg <- lmtest::bgtest(f$fit, order = 1, type = "Chisq", fill = 0)
  expect_equal(unname(bg$statistic), 40 * summary(aux)$r.squared, tolerance = 1e-8)
})

test_that("Chow F by hand equals strucchange::sctest", {
  skip_if_not_installed("strucchange")
  X1 <- cbind(a = rnorm(25), b = rnorm(25)); X2 <- cbind(a = rnorm(15), b = rnorm(15))
  y1 <- 1 + X1 %*% c(1, 2) + rnorm(25); y2 <- 2 + X2 %*% c(0, 2) + rnorm(15)
  ch <- chow_f(as.numeric(y1), X1, as.numeric(y2), X2)
  d <- data.frame(y = c(y1, y2), rbind(X1, X2))
  sc <- strucchange::sctest(y ~ ., data = d, type = "Chow", point = 25)
  expect_equal(ch$F, unname(sc$statistic), tolerance = 1e-8)
  expect_equal(c(ch$df1, ch$df2), c(3, 25 + 15 - 6))
})

test_that("Model I chain: sample sizes under each convention", {
  months <- SAMPLE_MONTHS
  pid <- integer(length(months))
  for (k in 1:4) pid[in_period(months, k)] <- k
  dat <- data.frame(month = months, period = pid, FER = cumsum(rnorm(144)), ER = cumsum(rnorm(144)),
                    bop = rnorm(144), i = cumsum(rnorm(144)), pi = 100 + rnorm(144), GDP = rnorm(144),
                    NX = rnorm(144), FDB = cumsum(rnorm(144)), FDI = rnorm(144))
  size <- as.vector(table(pid))
  for (conv in CONVENTIONS) for (j in 1:3) {
    b <- model_I_chain(build_base(dat, conv, "bop"), conv, "primary", j)$base
    n <- sapply(1:4, function(p) length(eq_rows(b, p, STAGE3_X)))
    expected <- if (conv == "full") c(size[1] - 1 - j, size[-1]) else size - 1 - j
    expect_equal(n, expected)
  }
  # stage 2 has no constant (equation 4-2)
  s2 <- model_I_chain(build_base(dat, "full", "bop"), "full", "primary", 2)$stage2
  expect_false("_cons" %in% s2$term)
})
