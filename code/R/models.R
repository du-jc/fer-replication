# code/R/models.R -- shared helpers for the Week 3 scripts (04-07), Study A
#
# Sourced after common.R by 04_model_I.R, 05_model_II.R, 06_diagnostics_chow.R, 07_classify.R.
# Contains no data values: only the model definitions of the Replication Plan (s.3), the
# choices made in the Week 3 chat (W3-01 .. W3-06), the classification rule W1-07, and
# functions.
#
#   data versions   raw | deflated (every variable except pi divided by pi/100; Plan s.2.1)
#   conventions     full   = differences and stage-2 lags may reach into the previous period
#                            (computed on the whole 2000-2011 series, then split)  PRIMARY
#                   within = each period is its own dataset: differences and lags start
#                            inside the period                                     SENSITIVITY
#                   (W3-01, which settles the Period 3 part of W2-05)
#   estimators      Periods 1 and 3: regression on differenced variables with AR(1) errors
#                   and a constant, exact ML  (= Stata  arima y x, arima(1,1,0));
#                   Periods 2 and 4: OLS.  (Blueprint A.2 step A4; Plan s.3.3)

VERSIONS    <- c("raw", "deflated")
CONVENTIONS <- c("full", "within")            # first = primary (W3-01)
ARIMA_PERIODS <- c(1L, 3L)                    # Plan s.3.3
SPECS <- c("primary", "alternative")          # Plan s.3.1
LAGS  <- 1:3                                  # Plan s.3.1: j = 1, 2, 3
STAGE1_X  <- c("DBOP", "Di", "Dpi", "DGDP")   # equation (4-1)
STAGE3_X  <- c("DER_hathat", "DNX", "DFDB", "DFDI")          # equation (4-3)
MODEL2_X  <- c("DNDF", "DGDP", "DNX", "DFDB", "DFDI")        # equation (4-13)
SELECTED_INPUTS <- file.path("outputs", "tables", "selected_inputs.csv")

# ---- inputs ------------------------------------------------------------------------------
# Selected BOP and NDF columns (03_select_bop_ndf.R, decision W2-10). Missing NDF -> NA.
read_selected_inputs <- function(lg) {
  if (!file.exists(SELECTED_INPUTS)) stop(SELECTED_INPUTS, " not found: run 03_select_bop_ndf.R first")
  s <- read_chr_csv(SELECTED_INPUTS)
  get <- function(what) {
    x <- s$selected_column[toupper(s$input) == what]
    if (length(x) == 1 && nzchar(x)) x else NA_character_
  }
  out <- list(bop = get("BOP"), ndf = get("NDF"))
  lg$info(sprintf("Selected inputs (W2-10): BOP = %s; NDF = %s", out$bop, out$ndf))
  out
}

# The monthly panel (01_import_verify.R), numeric columns converted explicitly
read_panel <- function(lg) {
  if (!file.exists(PATHS$panel)) stop(PATHS$panel, " not found: run 01_import_verify.R first")
  p <- read_chr_csv(PATHS$panel)
  for (v in setdiff(names(p), c("month", "month_label"))) p[[v]] <- to_num(p[[v]])
  if (!identical(p$month, SAMPLE_MONTHS)) stop("panel months are not 2000-01..2011-12 in order")
  p$period <- NA_integer_
  for (k in PERIODS$period) p$period[in_period(p$month, k)] <- k
  if (anyNA(p$period)) stop("a panel month falls outside the four periods")
  lg$info(sprintf("Panel %s: %d months x %d columns", PATHS$panel, nrow(p), ncol(p)))
  p
}

# Raw or deflated copy of the panel. vars = columns that exist and are to be deflated.
version_panel <- function(panel, version, vars) {
  vars <- intersect(vars, names(panel))
  if (version == "deflated") deflate(panel, vars) else panel
}

# ---- differences and lags under a convention (W3-01) ------------------------------------
# x is a full-length monthly vector, pid the period of each month.
d_conv <- function(x, pid, conv) {
  d <- c(NA_real_, diff(x))
  if (conv == "within") d[c(FALSE, pid[-1] != pid[-length(pid)])] <- NA_real_
  d
}
lag_conv <- function(x, k, pid, conv) {
  n <- length(x)
  out <- c(rep(NA_real_, k), x[seq_len(n - k)])
  if (conv == "within") {
    pl <- c(rep(NA_integer_, k), pid[seq_len(n - k)])
    out[is.na(pl) | pl != pid] <- NA_real_
  }
  out
}

# All first differences used by Models I and II, one row per month
build_base <- function(dat, conv, bop_col, ndf_col = NA_character_) {
  pid <- dat$period
  D <- function(col) d_conv(dat[[col]], pid, conv)
  b <- data.frame(month = dat$month, period = pid,
                  DFER = D("FER"), DER = D("ER"), DBOP = D(bop_col), Di = D("i"),
                  Dpi = D("pi"), DGDP = D("GDP"), DNX = D("NX"), DFDB = D("FDB"), DFDI = D("FDI"),
                  stringsAsFactors = FALSE)
  b$DNDF <- if (!is.na(ndf_col) && ndf_col %in% names(dat)) D(ndf_col) else NA_real_
  b
}

# ---- regressor screening -----------------------------------------------------------------
# A regressor with no variation in the sample is not identified: with a constant it is
# collinear with the constant; without one, an all-zero column carries no information.
# Raw DER is constant in Period 1 (Plan s.2.1), so this happens by construction there.
screen_regressors <- function(X, intercept) {
  keep <- vapply(seq_len(ncol(X)), function(j) {
    x <- X[, j]
    if (intercept) stats::sd(x) > 1e-12 * max(1, mean(abs(x))) else any(abs(x) > 1e-12)
  }, logical(1))
  list(X = X[, keep, drop = FALSE], dropped = colnames(X)[!keep])
}

# ---- significance --------------------------------------------------------------------------
# Star levels: 0 none, 1 dagger, 2 *, 3 **, 4 ***. Cut-offs are p-values. W1-07: the meaning
# of *, ** and *** is read from each original table's own note (original_targets.csv,
# column table_note; see read_star_legend); significant means p < 0.10, so the dagger,
# which the notes do not define, is p < 0.10. DEFAULT_CUTS (the conventional values) only
# label the "stars" column of 04/05; 07 classifies with the cut-offs read from the notes.
DEFAULT_CUTS <- c(dagger = 0.10, one = 0.05, two = 0.01, three = 0.001)
sig_level <- function(p, cuts = DEFAULT_CUTS) {
  ifelse(is.na(p), NA_integer_,
         ifelse(p < cuts[["three"]], 4L, ifelse(p < cuts[["two"]], 3L,
         ifelse(p < cuts[["one"]], 2L, ifelse(p < cuts[["dagger"]], 1L, 0L)))))
}
sig_label <- function(level, ascii = FALSE) {
  c("", if (ascii) "+" else "\u2020", "*", "**", "***")[level + 1L]   # ascii for the run logs
}

# Star legend of original tables from their notes, e.g. "* p < 0.05, ** p < 0.01, ...".
# All tables given must state the same three cut-offs; otherwise there is no legible legend,
# and W1-07 requires a new decision on how to read the stars, so the caller stops.
parse_star_legend <- function(note) {
  m <- regmatches(note, gregexpr("(\\*{1,3})\\s*p\\s*<\\s*(0?\\.[0-9]+)", note))[[1]]
  if (!length(m)) return(NULL)
  k <- nchar(sub("^(\\*+).*$", "\\1", m))
  v <- as.numeric(sub("^.*<\\s*", "", m))
  if (!setequal(k, 1:3) || anyDuplicated(k)) return(NULL)
  cuts <- c(dagger = 0.10, one = v[k == 1], two = v[k == 2], three = v[k == 3])
  if (!(cuts[["dagger"]] > cuts[["one"]] && cuts[["one"]] > cuts[["two"]] && cuts[["two"]] > cuts[["three"]])) return(NULL)
  cuts
}
read_star_legend <- function(tables, lg) {
  tg <- read_targets(tables)
  notes <- unique(tg$table_note[nzchar(trimws(tg$table_note))])
  legends <- lapply(notes, parse_star_legend)
  if (!length(notes) || any(vapply(legends, is.null, TRUE)) ||
      length(unique(lapply(legends, unname))) != 1)
    stop(sprintf("no single legible star legend in the notes of %s (W1-07: decide and record how to read the stars)",
                 paste(tables, collapse = ", ")))
  cuts <- legends[[1]]
  lg$info(sprintf("Star legend from the notes of %s (W1-07): * p<%s, ** p<%s, *** p<%s; dagger p<%s (significant)",
                  paste(tables, collapse = ", "), cuts[["one"]], cuts[["two"]], cuts[["three"]], cuts[["dagger"]]))
  cuts
}

# ---- OLS -------------------------------------------------------------------------------
# y numeric, X matrix with column names; intercept added as "_cons" when intercept = TRUE.
fit_ols <- function(y, X, intercept = TRUE) {
  sc <- screen_regressors(X, intercept)
  X1 <- sc$X
  if (intercept) X1 <- cbind(`_cons` = 1, X1)
  if (ncol(X1) == 0) {
    return(list(ok = TRUE, coef = data.frame(), resid = y, fitted = rep(0, length(y)), n = length(y),
                k = 0L, dropped = sc$dropped, fit = NULL, rss = sum(y^2), estimator = "ols"))
  }
  fit <- stats::lm(y ~ 0 + X1)
  names(fit$coefficients) <- colnames(X1)
  b <- stats::coef(fit)
  aliased <- names(b)[is.na(b)]
  cf <- summary(fit)$coefficients
  rownames(cf) <- sub("^X1", "", rownames(cf))
  df <- fit$df.residual
  coef <- data.frame(term = rownames(cf), coef = cf[, 1], se = cf[, 2], stat = cf[, 3],
                     p = cf[, 4], stat_type = "t", df = df, stringsAsFactors = FALSE)
  list(ok = TRUE, coef = coef, resid = stats::residuals(fit), fitted = stats::fitted(fit),
       n = length(y), k = ncol(X1) - length(aliased), dropped = c(sc$dropped, aliased),
       fit = fit, rss = sum(stats::residuals(fit)^2), estimator = "ols")
}

# ---- Regression with AR(1) errors, exact ML ----------------------------------------------
# y_t = c + x_t'b + u_t,  u_t = phi u_{t-1} + e_t,  e_t ~ N(0, s2)
# Stata: arima Y X, arima(1,1,0) differences Y and X, adds a constant (the drift) and
# estimates by exact (unconditional) ML with the Kalman filter; its default standard
# errors are OPG. R: stats::arima on the ALREADY differenced series with
# include.mean = TRUE and method = "ML" is the same model; R reports Hessian (OIM)
# standard errors, so the OPG ones are computed here from the per-observation scores.

# Per-observation log-likelihood of the model above (exact: first observation uses the
# stationary variance s2/(1 - phi^2)).
ar1_loglik_obs <- function(y, Z, beta, phi, s2) {
  u <- as.numeric(y - Z %*% beta)
  n <- length(u)
  e <- u[-1] - phi * u[-n]
  c(-0.5 * log(2 * pi) - 0.5 * log(s2 / (1 - phi^2)) - u[1]^2 * (1 - phi^2) / (2 * s2),
    -0.5 * log(2 * pi) - 0.5 * log(s2) - e^2 / (2 * s2))
}

# Analytic score of each observation w.r.t. (beta, phi, s2); rows = observations
ar1_scores <- function(y, Z, beta, phi, s2) {
  u <- as.numeric(y - Z %*% beta)
  n <- length(u)
  e <- u[-1] - phi * u[-n]
  Zs <- Z[-1, , drop = FALSE] - phi * Z[-n, , drop = FALSE]
  g_beta <- rbind((1 - phi^2) * u[1] * Z[1, ] / s2, e * Zs / s2)
  g_phi  <- c(-phi / (1 - phi^2) + phi * u[1]^2 / s2, e * u[-n] / s2)
  g_s2   <- c(-1 / (2 * s2) + u[1]^2 * (1 - phi^2) / (2 * s2^2), -1 / (2 * s2) + e^2 / (2 * s2^2))
  cbind(g_beta, phi = g_phi, s2 = g_s2)
}

fit_ar1_ml <- function(y, X, ar_order = 1L) {
  sc <- screen_regressors(X, intercept = TRUE)
  X1 <- sc$X
  fit <- tryCatch(
    stats::arima(y, order = c(ar_order, 0L, 0L), xreg = if (ncol(X1)) X1 else NULL,
                 include.mean = TRUE, method = "ML", optim.control = list(maxit = 2000)),
    error = function(e) e)
  if (inherits(fit, "error")) {
    return(list(ok = FALSE, msg = conditionMessage(fit), n = length(y), dropped = sc$dropped,
                estimator = "arima_ar1_ml"))
  }
  b <- stats::coef(fit)
  V <- fit$var.coef
  se_oim <- sqrt(pmax(diag(V), 0))
  # rename to the Stata row names: ar1 -> L.ar, intercept -> _cons
  rn <- names(b)
  rn[rn == "ar1"] <- "L.ar"; rn[rn == "ar2"] <- "L2.ar"; rn[rn == "intercept"] <- "_cons"
  names(b) <- names(se_oim) <- rn
  out <- list(ok = TRUE, n = length(y), dropped = sc$dropped, fit = fit, estimator = "arima_ar1_ml",
              loglik = fit$loglik, aic = fit$aic, sigma2 = fit$sigma2,
              converged = identical(fit$code, 0L), k = length(b))
  se_opg <- rep(NA_real_, length(b)); names(se_opg) <- rn
  ll_check <- NA_real_
  if (ar_order == 1L) {
    Z <- cbind(`_cons` = 1, X1)
    beta <- b[colnames(Z)]
    phi <- unname(b["L.ar"])
    ll_check <- sum(ar1_loglik_obs(y, Z, beta, phi, fit$sigma2)) - fit$loglik
    G <- ar1_scores(y, Z, beta, phi, fit$sigma2)
    Vopg <- tryCatch(solve(crossprod(G)), error = function(e) NULL)
    if (!is.null(Vopg)) {
      s <- sqrt(pmax(diag(Vopg), 0))
      names(s) <- c(colnames(Z), "L.ar", "s2")
      se_opg[names(s)[names(s) %in% rn]] <- s[names(s) %in% rn]
    }
  }
  se <- ifelse(is.na(se_opg), se_oim, se_opg)
  z <- b / se
  out$coef <- data.frame(term = rn, coef = unname(b), se = unname(se), se_oim = unname(se_oim),
                         se_opg = unname(se_opg), stat = unname(z), p = 2 * stats::pnorm(-abs(unname(z))),
                         stat_type = "z", df = NA_real_, stringsAsFactors = FALSE)
  out$ll_check <- ll_check
  out$resid <- as.numeric(stats::residuals(fit))
  out
}

# Estimator used for the DFER equations of a period (Plan s.3.3)
fit_dfer <- function(y, X, period) {
  if (period %in% ARIMA_PERIODS) fit_ar1_ml(y, X) else fit_ols(y, X, intercept = TRUE)
}

# ---- Model I chain (Plan s.3.1) ----------------------------------------------------------
# Stage 1 (4-1): OLS with constant, per period      -> DER_hat (one column, all periods)
# Stage 2 (4-2): no constant, lags 1..j, per period -> DER_hathat
#   primary     : DER_hat(t) on DER_hat(t-1..t-j)
#   alternative : DER(t)     on DER(t-1..t-j)       (the printed equation)
# Under "full", a lag at the start of a period reaches into the previous period (the
# stage-1 fitted values of that period); under "within" it is missing.
model_I_chain <- function(base, conv, spec, lag) {
  pid <- base$period
  der_hat <- rep(NA_real_, nrow(base))
  s1 <- list(); s1_fits <- list()
  for (p in PERIODS$period) {
    rows <- which(pid == p & stats::complete.cases(base[, c("DER", STAGE1_X)]))
    f <- fit_ols(base$DER[rows], as.matrix(base[rows, STAGE1_X]), intercept = TRUE)
    der_hat[rows] <- f$fitted
    s1_fits[[p]] <- c(f, list(rows = rows))
    s1[[p]] <- if (nrow(f$coef)) cbind(period = p, n = f$n, f$coef,
                                       dropped = paste(f$dropped, collapse = ";")) else NULL
  }
  src <- if (spec == "primary") der_hat else base$DER
  L <- sapply(seq_len(lag), function(k) lag_conv(src, k, pid, conv))
  L <- matrix(L, ncol = lag, dimnames = list(NULL, paste0("L", seq_len(lag))))
  der_hathat <- rep(NA_real_, nrow(base))
  s2 <- list()
  for (p in PERIODS$period) {
    ok <- pid == p & !is.na(src) & stats::complete.cases(L)
    rows <- which(ok)
    if (length(rows) <= lag) next
    f <- fit_ols(src[rows], L[rows, , drop = FALSE], intercept = FALSE)
    der_hathat[rows] <- f$fitted
    s2[[p]] <- data.frame(period = p, n = f$n,
                          term = if (nrow(f$coef)) f$coef$term else NA_character_,
                          coef = if (nrow(f$coef)) f$coef$coef else NA_real_,
                          se = if (nrow(f$coef)) f$coef$se else NA_real_,
                          dropped = paste(f$dropped, collapse = ";"), stringsAsFactors = FALSE)
  }
  b3 <- base
  b3$DER_hat <- der_hat
  b3$DER_hathat <- der_hathat
  list(base = b3, stage1 = do.call(rbind, s1), stage2 = do.call(rbind, s2), stage1_fits = s1_fits)
}

# Rows of one period usable for a DFER equation
eq_rows <- function(b, p, xvars) which(b$period == p & stats::complete.cases(b[, c("DFER", xvars)]))

# Tidy coefficient table for one DFER regression
tidy_fit <- function(f, keys) {
  if (!isTRUE(f$ok)) {
    return(cbind(keys, term = NA_character_, coef = NA_real_, se = NA_real_, se_oim = NA_real_,
                 se_opg = NA_real_, stat = NA_real_, p = NA_real_, stat_type = NA_character_,
                 df = NA_real_, stars = NA_character_, n = f$n, estimator = f$estimator,
                 status = paste("failed:", f$msg), stringsAsFactors = FALSE))
  }
  cf <- f$coef
  if (!"se_oim" %in% names(cf)) cf$se_oim <- NA_real_
  if (!"se_opg" %in% names(cf)) cf$se_opg <- NA_real_
  cf$stars <- sig_label(sig_level(cf$p))
  cf$n <- f$n
  cf$estimator <- f$estimator
  cf$status <- "estimated"
  if (length(f$dropped)) {
    cf <- rbind(cf, data.frame(term = f$dropped, coef = NA_real_, se = NA_real_, se_oim = NA_real_,
                               se_opg = NA_real_, stat = NA_real_, p = NA_real_, stat_type = NA_character_,
                               df = NA_real_, stars = NA_character_, n = f$n, estimator = f$estimator,
                               status = "not_identified", stringsAsFactors = FALSE))
  }
  cbind(keys[rep(1, nrow(cf)), , drop = FALSE], cf[, c("term", "coef", "se", "se_oim", "se_opg",
                                                      "stat", "p", "stat_type", "df", "stars", "n",
                                                      "estimator", "status")], row.names = NULL)
}

# ARMA order check in the ARIMA periods: AR(1) against AR(2) errors (original document s.4.2)
arma_order_row <- function(y, X, keys) {
  f1 <- fit_ar1_ml(y, X, 1L)
  f2 <- fit_ar1_ml(y, X, 2L)
  ll1 <- if (f1$ok) f1$loglik else NA_real_
  ll2 <- if (f2$ok) f2$loglik else NA_real_
  lr <- 2 * (ll2 - ll1)
  cbind(keys, n = length(y), aic_ar1 = if (f1$ok) f1$aic else NA_real_,
        aic_ar2 = if (f2$ok) f2$aic else NA_real_,
        bic_ar1 = if (f1$ok) -2 * ll1 + log(length(y)) * (f1$k + 1) else NA_real_,
        bic_ar2 = if (f2$ok) -2 * ll2 + log(length(y)) * (f2$k + 1) else NA_real_,
        lr_ar2_vs_ar1 = lr, p_lr = stats::pchisq(lr, 1, lower.tail = FALSE),
        preferred_by_aic = ifelse(is.na(ll1) | is.na(ll2), NA_character_,
                                  ifelse(f1$aic <= f2$aic, "AR(1)", "AR(2)")))
}

fit_summary_row <- function(f, keys) {
  cbind(keys, n = f$n, estimator = f$estimator, ok = isTRUE(f$ok),
        converged = if (isTRUE(f$ok) && !is.null(f$converged)) f$converged else NA,
        loglik = if (!is.null(f$loglik)) f$loglik else NA_real_,
        aic = if (!is.null(f$aic)) f$aic else NA_real_,
        sigma2 = if (!is.null(f$sigma2)) f$sigma2 else NA_real_,
        loglik_check = if (!is.null(f$ll_check)) f$ll_check else NA_real_,
        dropped = paste(f$dropped, collapse = ";"),
        message = if (!is.null(f$msg)) f$msg else "", stringsAsFactors = FALSE)
}

# ---- original targets for Tables 4.9-4.10 ------------------------------------------------
# Each numeric cell of Tables 4.9-4.10 with its period, model and term; the entry in
# parentheses below it is paired as its standard error (Plan s.5: the parentheses are SEs).
TERM_BY_NORM <- c(derhathat = "DER_hathat", dndf = "DNDF", dgdp = "DGDP", dnx = "DNX",
                  dfdb = "DFDB", dfdi = "DFDI", cons = "_cons", lar = "L.ar", n = "N")

col_period_model <- function(tg, table_id) {
  base <- if (table_id == "T4.9") 0L else 2L
  lab <- tolower(tg$col_label)
  per <- suppressWarnings(as.integer(sub(".*period\\s*([1-4]).*", "\\1", lab)))
  per[!grepl("period\\s*[1-4]", lab)] <- NA_integer_
  mod <- ifelse(grepl("model\\s*ii\\b|model\\s*2\\b", lab), "II",
                ifelse(grepl("model\\s*i\\b|model\\s*1\\b", lab), "I", NA_character_))
  # fallback when the header labels do not carry period/model: position among data columns
  cols <- sort(unique(tg$col_idx[tg$col_idx != min(tg$col_idx)]))
  pos <- match(tg$col_idx, cols)
  how <- ifelse(is.na(per) | is.na(mod), "column position", "header label")
  per[is.na(per)] <- base + ceiling(pos[is.na(per)] / 2)
  mod[is.na(mod)] <- ifelse(pos[is.na(mod)] %% 2 == 1, "I", "II")
  data.frame(period = per, model = mod, how = how, stringsAsFactors = FALSE)
}

read_estimate_targets <- function(lg) {
  tg <- read_targets(c("T4.9", "T4.10"))
  if (!nrow(tg)) stop("Tables 4.9-4.10 not found in original_targets.csv")
  tg <- tg[tg$element %in% c("value", "paren"), , drop = FALSE]
  out <- list()
  for (tid in c("T4.9", "T4.10")) {
    d <- tg[tg$table_id == tid, , drop = FALSE]
    if (!nrow(d)) { lg$warn(tid, ": no numeric cells"); next }
    pm <- col_period_model(d, tid)
    d <- cbind(d, pm)
    if (any(d$how != "header label"))
      lg$warn(tid, ": period/model taken from column position for some cells (header labels lack them)")
    d$base_label <- sub("\\s*\\(paren\\)\\s*$", "", d$row_label)
    d$term <- unname(TERM_BY_NORM[norm_label(d$base_label)])
    unk <- unique(d$base_label[is.na(d$term)])
    if (length(unk)) lg$warn(tid, ": row labels not recognised (ignored): ", paste(unk, collapse = ", "))
    d <- d[!is.na(d$term), , drop = FALSE]
    val <- d[d$element == "value", , drop = FALSE]
    par <- d[d$element == "paren", , drop = FALSE]
    key_v <- paste(val$term, val$col_idx)
    key_p <- paste(par$term, par$col_idx)
    se_i <- match(key_v, key_p)
    out[[tid]] <- data.frame(
      table_id = tid, period = val$period, model = val$model, term = val$term,
      target = to_num(val$value_text), target_text = val$cell_text,
      target_se = to_num(par$value_text[se_i]),
      target_stars = to_num(val$stars), target_dagger = to_num(val$dagger),
      labels_confirmed = val$labels_confirmed, stringsAsFactors = FALSE)
  }
  out <- do.call(rbind, out)
  out$target_level <- ifelse(out$target_stars > 0, pmin(out$target_stars, 3) + 1L,
                             ifelse(out$target_dagger > 0, 1L, 0L))
  lg$info(sprintf("Tables 4.9-4.10 targets: %d numeric cells (%d with a standard error)",
                  nrow(out), sum(!is.na(out$target_se))))
  if (any(out$labels_confirmed != "yes")) lg$warn("some Table 4.9-4.10 labels are not confirmed")
  out
}

# Model/period from a row label such as "Model II of period 3"
row_model_period <- function(lab) {
  l <- tolower(lab)
  model <- ifelse(grepl("model\\s*ii\\b", l), "II", ifelse(grepl("model\\s*i\\b", l), "I", NA_character_))
  per <- suppressWarnings(as.integer(sub(".*period\\s*([1-4]).*", "\\1", l)))
  per[!grepl("period\\s*[1-4]", l)] <- NA_integer_
  data.frame(model = model, period = per, stringsAsFactors = FALSE)
}

# ---- log summary shared by 04 and 05 ---------------------------------------------------
# Checks and a readable summary in the log (04_model_I.R, 05_model_II.R)
report_model <- function(est, fits, ll_bad, nonconv, failed, key_term, lg) {
  n_arima <- sum(fits$estimator == "arima_ar1_ml" & fits$ok)
  if (ll_bad == 0) {
    lg$info(sprintf("CHECK PASS  own AR(1) log-likelihood equals stats::arima in all %d ARIMA fits", n_arima))
  } else {
    lg$error(sprintf("own AR(1) log-likelihood differs from stats::arima in %d fit(s); OPG SEs unreliable", ll_bad))
  }
  if (nonconv) lg$warn(sprintf("%d ARIMA fit(s) report non-convergence (see the _fit.csv table)", nonconv))
  if (failed) lg$warn(sprintf("%d regression(s) not estimated (see the warnings above)", failed))
  ni <- unique(est[est$status == "not_identified", c("version", "convention", "period", "term")])
  for (i in seq_len(nrow(ni)))
    lg$info(sprintf("  not identified: %s in period %d (%s, %s): no variation in the sample",
                    ni$term[i], ni$period[i], ni$version[i], ni$convention[i]))
  has_spec <- "spec" %in% names(fits) && any(!is.na(fits$spec))
  for (conv in CONVENTIONS) {
    x <- fits[fits$convention == conv & fits$version == VERSIONS[1], ]
    if (has_spec) x <- x[x$spec == SPECS[1], ]
    grp <- if (has_spec) paste0("j=", x$lag) else rep("N", nrow(x))
    s <- tapply(seq_len(nrow(x)), grp, function(i) paste(x$n[i][order(x$period[i])], collapse = "/"))
    lg$info(sprintf("N by period P1/P2/P3/P4 (%s): %s", conv, paste(names(s), s, collapse = "; ")))
  }
  lg$info(sprintf("%s by period, primary convention (coef [se] stars):", key_term))
  d <- est[est$convention == CONVENTIONS[1] & est$term == key_term, ]
  kc <- Filter(function(v) v %in% names(d) && !all(is.na(d[[v]]) | d[[v]] == ""), c("version", "spec", "lag"))
  combos <- unique(d[, kc, drop = FALSE])
  for (i in seq_len(nrow(combos))) {
    m <- merge(d, combos[i, , drop = FALSE])
    m <- m[order(m$period), ]
    lg$info(sprintf("  %s  %s", paste(unlist(combos[i, ]), collapse = "/"),
                    paste(sprintf("P%d %s [%s]%s", m$period, fmt(m$coef, 4), fmt(m$se, 4),
                                  ifelse(is.na(m$p), "", sig_label(sig_level(m$p), ascii = TRUE))),
                          collapse = "  ")))
  }
}


# ---- tests and classification (06_diagnostics_chow.R, 07_classify.R) ---------------------
# Original study's decision rule for DW, with its own dL and dU
dw_judge <- function(d, dL, dU) {
  ifelse(is.na(d) | is.na(dL) | is.na(dU), NA_character_,
  ifelse(d < dL | d > 4 - dL, "reject",
  ifelse(d <= dU | d >= 4 - dU, "invalid", "fail to reject")))
}
norm_decision <- function(x) {
  n <- norm_label(x)
  ifelse(grepl("fail|not", n), "fail to reject", ifelse(grepl("invalid|inconclusive", n), "invalid",
         ifelse(grepl("reject", n), "reject", NA_character_)))
}

# Chow F for two adjacent samples, by hand; checked against strucchange::sctest
chow_f <- function(y1, X1, y2, X2) {
  rss <- function(y, X) sum(stats::lm.fit(cbind(1, X), y)$residuals^2)
  n1 <- length(y1); n2 <- length(y2); k <- ncol(X1) + 1L
  r1 <- rss(y1, X1); r2 <- rss(y2, X2); rp <- rss(c(y1, y2), rbind(X1, X2))
  df2 <- n1 + n2 - 2L * k
  ok <- n1 > k && n2 > k && df2 > 0
  Fs <- if (ok) ((rp - r1 - r2) / k) / ((r1 + r2) / df2) else NA_real_
  s1 <- if (n1 > k) r1 / (n1 - k) else NA_real_
  s2 <- if (n2 > k) r2 / (n2 - k) else NA_real_
  vr <- s1 / s2
  data.frame(n1 = n1, n2 = n2, k = k, rss_pooled = rp, rss_1 = r1, rss_2 = r2, F = Fs, df1 = k, df2 = df2,
             p = if (ok) stats::pf(Fs, k, df2, lower.tail = FALSE) else NA_real_,
             var_ratio = vr,
             var_ratio_p = if (ok) 2 * min(stats::pf(vr, n1 - k, n2 - k), stats::pf(vr, n1 - k, n2 - k, lower.tail = FALSE)) else NA_real_)
}

# Classification of one estimate against its original target (decision W1-07)
# Author's definition (W1-07), which supersedes the one-SE rule of Replication Plan s.4:
#   reproduced            same sign and the same star level (significant or insignificant);
#                         two insignificant estimates are reproduced even with opposite signs
#   partially reproduced  same sign, both significant, different star level
#   not reproduced        significance flips (significant to insignificant, or insignificant
#                         to significant), or the signs differ and either estimate is significant
#   significant = p < 0.10, so the dagger counts as significant; the meanings of *, ** and ***
#   come from the table's own note (cuts, from read_star_legend). Not estimated or not
#   identified = not reproduced. Magnitude (within_one_se, std_gap = gap / original SE) is
#   reported as information only.
CLASSES <- c("reproduced", "partially reproduced", "not reproduced")
classify_cell <- function(target, target_se, target_level, coef, p, status, cuts = DEFAULT_CUTS) {
  est <- !is.na(coef) & status == "estimated"
  our_level <- sig_level(p, cuts)
  sig_t <- target_level >= 1L
  sig_o <- est & our_level >= 1L
  same_sign <- est & sign(coef) == sign(target)
  same_sig <- est & our_level == target_level
  within <- est & !is.na(target_se) & abs(coef - target) <= target_se
  cls <- ifelse(!est, "not reproduced",
         ifelse(!same_sign & (sig_t | sig_o), "not reproduced",
         ifelse(sig_t != sig_o, "not reproduced",
         ifelse(!sig_t & !sig_o, "reproduced",
         ifelse(same_sig, "reproduced", "partially reproduced")))))
  reason <- ifelse(!est, ifelse(status == "not_identified", "not identified", "not estimated"),
            ifelse(!same_sign & (sig_t | sig_o), "sign differs",
            ifelse(sig_t != sig_o, ifelse(sig_t, "significant in original, insignificant here",
                                          "insignificant in original, significant here"),
            ifelse(!sig_t & !sig_o, ifelse(same_sign, "both insignificant", "both insignificant; sign differs"),
            ifelse(same_sig, "same star level", "star level differs")))))
  data.frame(our_level = our_level, same_sign = same_sign, same_significance = same_sig,
             original_significant = sig_t, ours_significant = sig_o,
             within_one_se = within, std_gap = abs(coef - target) / target_se,
             class = cls, reason = reason, stringsAsFactors = FALSE)
}

