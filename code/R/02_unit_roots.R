# code/R/02_unit_roots.R -- Week 2, Study A (Blueprint A.2 step A3; Replication Plan s.3.3)
#
# Reproduces the DF-GLS tests of the original Tables 4.1 (levels) and 4.2 (first differences)
# for the eight original-study variables that have data in Appendix I, and fixes the DF-GLS
# specification that 03_select_bop_ndf.R then applies unchanged to BOP and NDF
# (decision W2-02). BOP and NDF are NOT used here.
#
# Specification grid (every combination is computed; nothing is chosen by eye):
#   data version   raw | deflated (all variables except pi divided by pi/100)
#   deterministics constant | trend                 (ERS 1996; Stata's default is trend)
#   sample         common: all lags on the sample left by kmax lags (Stata dfgls)
#                  own:    each lag on its own longest sample (urca::ur.ers)
#   lag rule       maic (Ng-Perron modified AIC), seqt (Ng-Perron sequential t, 10%),
#                  sic (Schwarz), or a fixed lag k = 0..kmax, kmax = int(12(n/100)^0.25)
# Fixed calibration rule (decided before running; W2-02):
#   score = mean |tau - original tau| over all Table 4.1-4.2 targets of the 8 variables;
#   ties: smaller max gap, then named rules before fixed lags (maic, seqt, sic), then
#   trend before constant, then common before own sample.
#
# Outputs (outputs/tables/): dfgls_grid.csv, dfgls_calibration.csv, dfgls_selected_spec.csv,
#          dfgls_table4_1_4_2.csv, dfgls_lag_sensitivity.csv; run log in outputs/logs/.
# Usage:   Rscript code/R/02_unit_roots.R

source(file.path("code", "R", "common.R"))

CALIB_VARS <- c("FER", "GDP", "ER", "NX", "pi", "FDI", "FDB", "i")   # Table 4.1 rows with data
VERSIONS <- c("raw", "deflated")
DETS <- c("trend", "constant")
CONVS <- c("common", "own")
NAMED_RULES <- c("maic", "seqt", "sic")

# Original Table 4.1 / 4.2 targets for a set of variables (levels and first differences)
dfgls_targets <- function(vars, lg) {
  tg <- read_targets(c("T4.1", "T4.2"))
  if (!nrow(tg)) stop("Tables 4.1-4.2 not found in original_targets.csv")
  var_by_norm <- stats::setNames(vars, norm_label(vars))
  pick_col <- function(d, pattern) grepl(pattern, d$col_norm)
  out <- list()
  for (tid in c("T4.1", "T4.2")) {
    d <- tg[tg$table_id == tid, , drop = FALSE]
    rn <- d$row_norm
    if (tid == "T4.2") {
      strip <- ifelse(startsWith(rn, "d") & substring(rn, 2) %in% names(var_by_norm), substring(rn, 2), rn)
      if (any(strip == rn & rn %in% names(var_by_norm)))
        lg$warn("T4.2: a row label without the D prefix was read as a difference: ",
                paste(unique(d$row_label[strip == rn & rn %in% names(var_by_norm)]), collapse = ", "))
      rn <- strip
    }
    d$var <- unname(var_by_norm[rn])
    d <- d[!is.na(d$var), , drop = FALSE]
    for (v in unique(d$var)) {
      dv <- d[d$var == v, , drop = FALSE]
      get <- function(pattern, numeric = TRUE) {
        x <- dv[pick_col(dv, pattern), , drop = FALSE]
        if (!nrow(x)) return(if (numeric) NA_real_ else "")
        if (numeric) x$value[1] else x$cell_text[1]
      }
      out[[length(out) + 1]] <- data.frame(
        table_id = tid, var = v, series = if (tid == "T4.1") "level" else "diff",
        target = get("tau|statistic"), target_text = get("tau|statistic", FALSE),
        original_cv1 = get("^1"), original_cv5 = get("^5"),
        original_judgement = get("stationary", FALSE),
        labels_confirmed = paste(unique(dv$labels_confirmed), collapse = "/"),
        stringsAsFactors = FALSE)
    }
  }
  out <- do.call(rbind, out)
  lg$info(sprintf("Targets: %d rows for %s; %d with a numeric tau",
                  nrow(out), paste(vars, collapse = ","), sum(!is.na(out$target))))
  if (any(out$labels_confirmed != "yes"))
    lg$warn("some DF-GLS target labels are not confirmed in the label map: ",
            paste(out$var[out$labels_confirmed != "yes"], collapse = ", "))
  out
}

series_of <- function(dat, v, series) {
  x <- dat[[v]]
  if (series == "diff") diff(x) else x
}

# Full grid: every variable x series x version x deterministics x sample x rule
build_grid <- function(panel, vars, lg) {
  versions <- list(raw = panel, deflated = deflate(panel, ORIGINAL_VARS))
  rows <- list()
  lags <- list()
  for (vn in VERSIONS) for (v in vars) for (se in c("level", "diff")) for (det in DETS) {
    y <- series_of(versions[[vn]], v, se)
    tab <- dfgls_all_lags(y, det)
    kmax <- max(tab$k)
    lags[[length(lags) + 1]] <- cbind(data.frame(version = vn, var = v, series = se, det = det,
                                                 n = length(y), kmax = kmax), tab)
    rules <- c(NAMED_RULES, paste0("fixed_", 0:kmax))
    for (conv in CONVS) for (rule in rules) {
      k <- choose_lag(tab, rule)
      r <- tab[tab$k == k, ]
      rows[[length(rows) + 1]] <- data.frame(
        version = vn, var = v, series = se, det = det, conv = conv, rule = rule, k = k,
        N = if (conv == "common") r$N_common else r$N_own,
        tau = if (conv == "common") r$tau_common else r$tau_own, stringsAsFactors = FALSE)
    }
  }
  lg$info(sprintf("Computed %d DF-GLS regressions sets (%d grid rows)", length(lags), length(rows)))
  list(grid = do.call(rbind, rows), lags = do.call(rbind, lags))
}

calibrate <- function(grid, targets, lg) {
  g <- merge(grid, targets[, c("var", "series", "target", "target_text")], by = c("var", "series"))
  g$gap <- abs(g$tau - g$target)
  g$within_print <- g$gap <= print_tolerance(g$target_text)
  g <- g[!is.na(g$target), ]
  key <- c("version", "det", "conv", "rule")
  cal <- do.call(rbind, lapply(split(g, g[key], drop = TRUE), function(d) data.frame(
    version = d$version[1], det = d$det[1], conv = d$conv[1], rule = d$rule[1],
    n_targets = sum(!is.na(d$gap)), mean_gap = mean(d$gap, na.rm = TRUE),
    max_gap = max(d$gap, na.rm = TRUE),
    n_within_print = sum(d$within_print, na.rm = TRUE), stringsAsFactors = FALSE)))
  full <- max(cal$n_targets)
  dropped <- sum(cal$n_targets < full)
  if (dropped) lg$info(sprintf("%d specification(s) excluded: a fixed lag above some series' kmax", dropped))
  cal <- cal[cal$n_targets == full, ]
  rule_rank <- ifelse(cal$rule %in% NAMED_RULES, match(cal$rule, NAMED_RULES), 10L)
  cal <- cal[order(round(cal$mean_gap, 8), round(cal$max_gap, 8), rule_rank,
                   match(cal$det, DETS), match(cal$conv, CONVS)), ]
  cal$rank <- seq_len(nrow(cal))
  list(cal = cal, detail = g)
}

main <- function(lg) {
  if (!file.exists(PATHS$panel)) stop(PATHS$panel, " not found: run 01_import_verify.R first")
  panel <- read_chr_csv(PATHS$panel)
  for (v in ORIGINAL_VARS) panel[[v]] <- to_num(panel[[v]])
  lg$info("Calibration variables (original-study data only): ", paste(CALIB_VARS, collapse = ", "))

  targets <- dfgls_targets(CALIB_VARS, lg)
  gl <- build_grid(panel, CALIB_VARS, lg)
  write_table(gl$grid, "dfgls_grid.csv", lg)

  # consistency check: own-sample tau equals urca::ur.ers at the same lag
  chk <- gl$grid[gl$grid$conv == "own" & gl$grid$rule %in% c("fixed_0", "fixed_4"), ]
  versions <- list(raw = panel, deflated = deflate(panel, ORIGINAL_VARS))
  diffs <- mapply(function(vn, v, se, det, k, tau) {
    abs(urca_dfgls(series_of(versions[[vn]], v, se), det, k)$tau - tau)
  }, chk$version, chk$var, chk$series, chk$det, chk$k, chk$tau)
  if (max(diffs) < 1e-8) lg$info(sprintf("CHECK PASS  own-sample tau equals urca::ur.ers (%d cases)", length(diffs)))
  else lg$error(sprintf("own-sample tau differs from urca::ur.ers by up to %g", max(diffs)))

  cr <- calibrate(gl$grid, targets, lg)
  write_table(cr$cal, "dfgls_calibration.csv", lg)
  lg$info("Best specifications (mean |gap| over targets):")
  for (i in seq_len(min(10, nrow(cr$cal)))) {
    s <- cr$cal[i, ]
    lg$info(sprintf("  %2d. %-8s %-8s %-6s %-9s mean %s  max %s  (%d targets)", s$rank, s$version,
                    s$det, s$conv, s$rule, fmt(s$mean_gap), fmt(s$max_gap), s$n_targets))
  }
  sd_row <- cr$cal[cr$cal$det == "trend" & cr$cal$conv == "common" & cr$cal$rule == "maic", ]
  for (i in seq_len(nrow(sd_row)))
    lg$info(sprintf("  Stata default (trend, common sample, MAIC) on %s data ranks %d of %d (mean %s)",
                    sd_row$version[i], sd_row$rank[i], nrow(cr$cal), fmt(sd_row$mean_gap[i])))
  best <- cr$cal[1, ]
  spec <- data.frame(version = best$version, det = best$det, conv = best$conv, rule = best$rule,
                     mean_gap = best$mean_gap, max_gap = best$max_gap, n_targets = best$n_targets,
                     n_specs_compared = nrow(cr$cal),
                     runner_up = if (nrow(cr$cal) > 1) paste(cr$cal[2, c("version", "det", "conv", "rule")], collapse = "/") else "",
                     runner_up_mean_gap = if (nrow(cr$cal) > 1) cr$cal$mean_gap[2] else NA_real_,
                     calibration_vars = paste(CALIB_VARS, collapse = ";"),
                     fixed_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                     decision = "W2-02", stringsAsFactors = FALSE)
  write_table(spec, "dfgls_selected_spec.csv", lg)
  lg$info(sprintf("SELECTED SPEC: version=%s det=%s sample=%s rule=%s (mean gap %s)",
                  best$version, best$det, best$conv, best$rule, fmt(best$mean_gap)))
  if (grepl("^fixed_", best$rule))
    lg$warn("the best rule is a fixed lag, not a documented selection rule; record this in W2-09")

  # Table 4.1-4.2 reproduction under the selected spec, with ADF at the same lag
  sel <- cr$detail[cr$detail$version == best$version & cr$detail$det == best$det &
                     cr$detail$conv == best$conv & cr$detail$rule == best$rule, ]
  sel$within_print <- NULL
  sel <- merge(sel[, setdiff(names(sel), "target_text")],
               targets[, c("var", "series", "table_id", "target_text", "original_cv1",
                                "original_cv5", "original_judgement")], by = c("var", "series"))
  dat <- versions[[best$version]]
  extra <- do.call(rbind, lapply(seq_len(nrow(sel)), function(i) {
    y <- series_of(dat, sel$var[i], sel$series[i])
    u <- urca_dfgls(y, best$det, sel$k[i])
    adf <- urca::ur.df(y, type = if (best$det == "trend") "trend" else "drift", lags = sel$k[i])
    data.frame(urca_cv1 = u$cval[1, "1pct"], urca_cv5 = u$cval[1, "5pct"],
               adf_tau_same_k = as.numeric(adf@teststat[1]))
  }))
  sel <- cbind(sel, extra)
  sel$within_print <- sel$gap <= print_tolerance(sel$target_text)
  sel$our_judgement_5pct <- ifelse(sel$tau < sel$urca_cv5, "Yes", "No")
  sel <- sel[order(sel$table_id, match(sel$var, CALIB_VARS)), ]
  write_table(sel[, c("table_id", "var", "series", "k", "N", "tau", "target", "gap", "within_print",
                      "urca_cv1", "urca_cv5", "original_cv1", "original_cv5", "our_judgement_5pct",
                      "original_judgement", "adf_tau_same_k")], "dfgls_table4_1_4_2.csv", lg)
  lg$info(sprintf("Under the selected spec: %d of %d taus agree to the printed decimals",
                  sum(sel$within_print), nrow(sel)))
  jd <- sum(sel$our_judgement_5pct != trimws(sel$original_judgement) & nzchar(sel$original_judgement))
  if (jd) lg$warn(sprintf("%d stationarity judgement(s) differ from the original study (see the table)", jd))

  # Lag sensitivity for the selected version and deterministics
  ls <- gl$lags[gl$lags$version == best$version & gl$lags$det == best$det, ]
  ls$tau <- if (best$conv == "common") ls$tau_common else ls$tau_own
  ls$N <- if (best$conv == "common") ls$N_common else ls$N_own
  mark <- function(d, rule) d$k == choose_lag(d, rule)
  ls <- do.call(rbind, lapply(split(ls, list(ls$var, ls$series), drop = TRUE), function(d) {
    d$chosen_maic <- mark(d, "maic"); d$chosen_seqt <- mark(d, "seqt"); d$chosen_sic <- mark(d, "sic"); d
  }))
  ls <- merge(ls, targets[, c("var", "series", "target")], by = c("var", "series"), all.x = TRUE)
  ls <- ls[order(match(ls$var, CALIB_VARS), ls$series, ls$k),
           c("var", "series", "version", "det", "k", "N", "tau", "target", "maic", "sic",
             "chosen_maic", "chosen_seqt", "chosen_sic")]
  write_table(ls, "dfgls_lag_sensitivity.csv", lg)
  lg$info("Next: commit dfgls_selected_spec.csv BEFORE running 03_select_bop_ndf.R (fixed rule)")
}

run_script("02_unit_roots", main)
