# code/R/03_select_bop_ndf.R -- Week 2, Study A (Blueprint A.2 step A3; Replication Plan s.2.2)
#
# Chooses, BEFORE any Model I or Model II estimate, which BOP series and which NDF timing
# enter the replication. The rules are fixed in advance (decisions W2-01 to W2-03):
#
# DF-GLS: the specification in outputs/tables/dfgls_selected_spec.csv (fixed by
#         02_unit_roots.R on the original-study variables only) is applied unchanged.
#
# NDF (Replication Plan s.2.2): month-average vs month-end 12-month NDF; the variant whose
#         level DF-GLS tau is closest to the original NDF tau (Table 4.1) is chosen.
#         Tie: month-average (listed first). The DNDF tau (Table 4.2) is reported only.
#
# BOP (Replication Plan s.2.2; W2-01, W2-03, W2-12): three candidates = current account x
#         {hold, div3, ytd}; 2. 资本和金融账户 is not used (W2-12).
#   criterion 1  |tau(BOP level) - original BOP tau (Table 4.1)|
#   criterion 2  sum over Periods 1-4 of |DW - original DW (Table 4.3)|, where DW is the
#                Durbin-Watson statistic of the OLS stage-1 regression (equation 4-1)
#                  DER = b0 + b1 DBOP + b2 Di + b3 Dpi + b4 DGDP + x
#                on DEFLATED data, differences taken on the full sample (W2-05)
#   rule         rank each criterion (1 = closest); lowest rank sum wins;
#                ties -> better DF-GLS rank, then smaller DF-GLS gap, then smaller DW gap.
#
# Outputs (outputs/tables/): select_ndf.csv, select_bop.csv, select_bop_dw_detail.csv,
#          selected_inputs.csv; run log in outputs/logs/.
# Usage:   Rscript code/R/03_select_bop_ndf.R

source(file.path("code", "R", "common.R"))

NDF_VARIANTS <- c("ndf_mavg", "ndf_mend")
BOP_CANDIDATES <- paste("bop_ca", c("hold", "div3", "ytd"), sep = "_")   # W2-12

read_spec <- function(lg) {
  p <- file.path(PATHS$tables, "dfgls_selected_spec.csv")
  if (!file.exists(p)) stop(p, " not found: run 02_unit_roots.R (and commit its output) first")
  s <- read_chr_csv(p)
  lg$info(sprintf("Fixed DF-GLS spec from %s (md5 %s, fixed at %s): version=%s det=%s sample=%s rule=%s",
                  p, unname(tools::md5sum(p)), s$fixed_at, s$version, s$det, s$conv, s$rule))
  s
}

# One named target from Table 4.1 / 4.2 (row label e.g. NDF, DBOP)
tau_target <- function(tg, tid, label) {
  d <- tg[tg$table_id == tid & tg$row_norm == norm_label(label) & grepl("tau|statistic", tg$col_norm), ]
  if (!nrow(d) || is.na(d$value[1])) return(list(value = NA_real_, text = ""))
  list(value = d$value[1], text = d$value_text[1])
}

dw_targets <- function(tg, lg) {
  d <- tg[tg$table_id == "T4.3" & grepl("dstatistic|durbin", tg$col_norm) & tg$element == "value", ]
  d$period <- as.integer(sub(".*period\\s*([1-4]).*", "\\1", tolower(d$row_label)))
  d <- d[!is.na(d$period), ]
  out <- stats::setNames(rep(NA_real_, 4), paste0("p", 1:4))
  out[paste0("p", d$period)] <- d$value
  lg$info(sprintf("Table 4.3 DW targets found for %d of 4 periods", sum(!is.na(out))))
  if (any(d$labels_confirmed != "yes")) lg$warn("some Table 4.3 labels are not confirmed")
  out
}

version_series <- function(panel, col, version) {
  if (version == "deflated") panel[[col]] / (panel$pi / 100) else panel[[col]]
}

select_ndf <- function(panel, spec, tg, lg) {
  t_lev <- tau_target(tg, "T4.1", "NDF")
  t_dif <- tau_target(tg, "T4.2", "DNDF")
  if (is.na(t_lev$value)) stop("original NDF tau (Table 4.1) not found in targets")
  res <- do.call(rbind, lapply(NDF_VARIANTS, function(v) {
    y <- version_series(panel, v, spec$version)
    a <- dfgls_tau(y, spec$det, spec$conv, spec$rule)
    b <- dfgls_tau(diff(y), spec$det, spec$conv, spec$rule)
    data.frame(variant = v, version = spec$version, k = a$k, N = a$N, tau = a$tau,
               target = t_lev$value, gap = abs(a$tau - t_lev$value),
               within_print = abs(a$tau - t_lev$value) <= print_tolerance(t_lev$text),
               k_diff = b$k, tau_diff = b$tau, target_diff = t_dif$value,
               gap_diff_info_only = abs(b$tau - t_dif$value), stringsAsFactors = FALSE)
  }))
  res <- res[order(round(res$gap, 8), match(res$variant, NDF_VARIANTS)), ]
  res$selected <- seq_len(nrow(res)) == 1
  for (i in seq_len(nrow(res)))
    lg$info(sprintf("  NDF %-9s tau %s (k=%d)  target %s  gap %s%s", res$variant[i], fmt(res$tau[i]),
                    res$k[i], t_lev$text, fmt(res$gap[i]), if (res$selected[i]) "  <- selected" else ""))
  write_table(res, "select_ndf.csv", lg)
  res[res$selected, ]
}

stage1_dw <- function(panel, cand) {
  df <- deflate(panel, c(ORIGINAL_VARS, cand))            # W2-03: deflated data
  d <- data.frame(month = panel$month, DER = d1(df$ER), DBOP = d1(df[[cand]]),
                  Di = d1(df$i), Dpi = d1(df$pi), DGDP = d1(df$GDP))
  do.call(rbind, lapply(1:4, function(p) {
    x <- d[in_period(d$month, p) & stats::complete.cases(d), ]
    fit <- stats::lm(DER ~ DBOP + Di + Dpi + DGDP, data = x)
    e <- stats::residuals(fit)
    dw <- if (sum(e^2) > 0) dw_stat(e) else NA_real_
    dw_lmtest <- unname(lmtest::dwtest(fit)$statistic)
    data.frame(candidate = cand, period = p, n = nrow(x), k_regressors = 4L, dw = dw,
               dw_lmtest = dw_lmtest, first_month = min(x$month), last_month = max(x$month))
  }))
}

select_bop <- function(panel, spec, tg, lg) {
  t_lev <- tau_target(tg, "T4.1", "BOP")
  t_dif <- tau_target(tg, "T4.2", "DBOP")
  if (is.na(t_lev$value)) stop("original BOP tau (Table 4.1) not found in targets")
  dwt <- dw_targets(tg, lg)
  if (all(is.na(dwt))) stop("no Table 4.3 DW targets found")
  detail <- do.call(rbind, lapply(BOP_CANDIDATES, function(cand) stage1_dw(panel, cand)))
  detail$target <- dwt[paste0("p", detail$period)]
  detail$gap <- abs(detail$dw - detail$target)
  if (max(abs(detail$dw - detail$dw_lmtest), na.rm = TRUE) < 1e-10)
    lg$info("CHECK PASS  DW computed by hand equals lmtest::dwtest")
  else lg$error("DW by hand differs from lmtest::dwtest")
  lg$info("Stage-1 sample sizes by period: ",
          paste(sprintf("P%d n=%d", 1:4, detail$n[detail$candidate == BOP_CANDIDATES[1]]), collapse = ", "))
  write_table(detail, "select_bop_dw_detail.csv", lg)

  res <- do.call(rbind, lapply(BOP_CANDIDATES, function(cand) {
    y <- version_series(panel, cand, spec$version)
    a <- dfgls_tau(y, spec$det, spec$conv, spec$rule)
    b <- dfgls_tau(diff(y), spec$det, spec$conv, spec$rule)
    dd <- detail[detail$candidate == cand, ]
    parts <- strsplit(sub("^bop_", "", cand), "_")[[1]]
    data.frame(candidate = cand, series = parts[1], conversion = parts[2],
               k = a$k, tau = a$tau, tau_target = t_lev$value, tau_gap = abs(a$tau - t_lev$value),
               tau_diff = b$tau, tau_diff_target = t_dif$value,
               dw_p1 = dd$dw[1], dw_p2 = dd$dw[2], dw_p3 = dd$dw[3], dw_p4 = dd$dw[4],
               dw_gap_sum = sum(dd$gap, na.rm = TRUE), dw_periods_used = sum(!is.na(dd$gap)),
               stringsAsFactors = FALSE)
  }))
  if (any(is.na(res$tau)) || any(res$dw_periods_used < sum(!is.na(dwt))))
    stop("a criterion could not be computed for every candidate; see select_bop_dw_detail.csv")
  # gaps equal to 8 decimals count as ties (floating-point noise is not a difference)
  res$rank_tau <- rank(round(res$tau_gap, 8), ties.method = "min")
  res$rank_dw <- rank(round(res$dw_gap_sum, 8), ties.method = "min")
  res$rank_sum <- res$rank_tau + res$rank_dw
  res <- res[order(res$rank_sum, res$rank_tau, round(res$tau_gap, 8), round(res$dw_gap_sum, 8),
                   match(res$candidate, BOP_CANDIDATES)), ]
  # 'hold' and 'div3' differ only by the factor 3; DF-GLS tau and the stage-1 residuals are
  # invariant to rescaling a series, so the two can never be told apart by this rule.
  for (s in c("ca")) {
    h <- panel[[paste0("bop_", s, "_hold")]]; d3 <- panel[[paste0("bop_", s, "_div3")]]
    if (isTRUE(all.equal(h / 3, d3)))
      lg$info(sprintf("  note: bop_%s_hold and bop_%s_div3 are exact multiples; they tie on both criteria "
                      , s, s), "and the fixed order (hold first) decides. Downstream this only rescales the DBOP coefficient.")
  }
  res$selected <- seq_len(nrow(res)) == 1
  lg$info("BOP candidates (rank sum = DF-GLS rank + DW rank):")
  for (i in seq_len(nrow(res)))
    lg$info(sprintf("  %-14s tau %s (gap %s, rank %d)  DW gap sum %s (rank %d)  sum %d%s",
                    res$candidate[i], fmt(res$tau[i]), fmt(res$tau_gap[i]), res$rank_tau[i],
                    fmt(res$dw_gap_sum[i]), res$rank_dw[i], res$rank_sum[i],
                    if (res$selected[i]) "  <- selected" else ""))
  if (res$rank_sum[1] == res$rank_sum[min(2, nrow(res))] && nrow(res) > 1)
    lg$info("  (rank-sum tie broken by the DF-GLS rank, as fixed in W2-01)")
  if (res$rank_tau[1] != 1 || res$rank_dw[1] != 1)
    lg$warn("the selected BOP candidate is not closest on both criteria; note this in W2-10")
  write_table(res, "select_bop.csv", lg)
  res[res$selected, ]
}

main <- function(lg) {
  spec <- read_spec(lg)
  if (!file.exists(PATHS$panel)) stop(PATHS$panel, " not found: run 01_import_verify.R first")
  panel <- read_chr_csv(PATHS$panel)
  num <- setdiff(names(panel), c("month", "month_label"))
  for (v in num) panel[[v]] <- to_num(panel[[v]])
  miss <- setdiff(c(NDF_VARIANTS, BOP_CANDIDATES), names(panel))
  if (length(miss)) stop("panel lacks columns: ", paste(miss, collapse = ", "))
  tg <- read_targets(c("T4.1", "T4.2", "T4.3"))

  ndf <- select_ndf(panel, spec, tg, lg)
  bop <- select_bop(panel, spec, tg, lg)
  sel <- data.frame(
    input = c("NDF", "BOP"), selected_column = c(ndf$variant, bop$candidate),
    criterion = c(sprintf("closest level DF-GLS tau to original Table 4.1 (gap %s)", fmt(ndf$gap)),
                  sprintf("rank sum %d (DF-GLS rank %d, DW rank %d)", bop$rank_sum, bop$rank_tau, bop$rank_dw)),
    dfgls_spec = paste(spec$version, spec$det, spec$conv, spec$rule, sep = "/"),
    dw_data = c("", "deflated"), decision = c("W2-10", "W2-10"),
    selected_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), stringsAsFactors = FALSE)
  write_table(sel, "selected_inputs.csv", lg)
  lg$info(sprintf("SELECTED: NDF = %s; BOP = %s", ndf$variant, bop$candidate))
  lg$info("Notes: BOP = current account, checked equal under BPM5 and BPM6 (W2-12); Model II uses ",
          "the named Wind series and is reported 'as specified' (Replication Plan s.2.2).")
}

run_script("03_select_bop_ndf", main)
