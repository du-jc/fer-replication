# code/R/07_classify.R -- Week 3, Study A (Blueprint A.2 step A4; Replication Plan s.3.1, s.4)
#
# Compares every estimate of Tables 4.9-4.10 with our estimates (04, 05) and classifies it by
# decision W1-07 (the author's definition; the rule itself is classify_cell() in models.R):
#   reproduced            same sign and the same star level (significant or insignificant);
#                         two insignificant estimates are reproduced even with opposite signs
#   partially reproduced  same sign, both significant, different star level
#   not reproduced        significance flips, or the signs differ and either is significant;
#                         also when not estimated / not identified
# Significant = p < 0.10 (the dagger counts). The meanings of *, ** and *** are read from the
# note of Tables 4.9-4.10 (original_targets.csv, table_note); the script stops if the notes
# have no legible legend (W1-07). Our star level uses the same cut-offs on our p-value (z for
# ARIMA fits with OPG SEs, t for OLS). Magnitude is information only: within_one_se and
# std_gap (gap / original SE; the parentheses are SEs, Plan s.5) stay in classify_cells.csv.
#
# Which Model I specification "reproduces" Tables 4.9-4.10 (Plan s.3.1; rule W3-02):
#   among the 12 combinations (primary/alternative x lag 1-3 x raw/deflated), within each
#   convention, rank by
#     1. number of Model I cells reproduced (more is better)
#     2. mean |ours - original| / original SE over the Model I cells we estimated (smaller)
#     3. candidate order: primary before alternative, lower lag, raw before deflated
# The headline is the primary convention, CONVENTIONS[1] (within since W4-27; full before, W3-01); the other is reported alongside.
#   Model II has no spec choice; its cells are classified per version and convention.
#
# Outputs (outputs/tables/): classify_cells.csv, classify_N.csv, classify_summary.csv,
#   classify_rank_model_I.csv
# Usage:   Rscript code/R/07_classify.R

source(file.path("code", "R", "common.R"))
source(file.path("code", "R", "models.R"))

read_est <- function(name, lg) {
  p <- file.path(PATHS$tables, name)
  if (!file.exists(p)) return(NULL)
  e <- read_chr_csv(p)
  for (v in c("lag", "period", "coef", "se", "se_oim", "se_opg", "p", "n")) e[[v]] <- to_num(e[[v]])
  lg$info(sprintf("Read %s (%d rows)", p, nrow(e)))
  e
}

main <- function(lg) {
  e1 <- read_est("model_I_estimates.csv", lg)
  if (is.null(e1)) stop("outputs/tables/model_I_estimates.csv not found: run 04_model_I.R first")
  e2 <- read_est("model_II_estimates.csv", lg)
  if (is.null(e2)) lg$warn("model_II_estimates.csv not found (NDF absent or 05 not run): Model II cells not classified")
  est <- rbind(e1, e2)
  tg <- read_estimate_targets(lg)
  cuts <- read_star_legend(c("T4.9", "T4.10"), lg)
  cells_t <- tg[tg$term != "N", ]
  n_t <- tg[tg$term == "N" & tg$model %in% unique(est$model), ]
  if (any(is.na(cells_t$target_se)))
    lg$warn(sprintf("%d target cell(s) have no paired SE; they can be at most partially reproduced",
                    sum(is.na(cells_t$target_se))))

  # ---- every target cell x every matching estimate ----
  m <- merge(cells_t, est, by = c("model", "period", "term"))
  cells_t <- cells_t[cells_t$model %in% unique(est$model), ]        # Model II only if 05 ran
  miss <- cells_t[!paste(cells_t$model, cells_t$period, cells_t$term) %in% paste(est$model, est$period, est$term), ]
  if (nrow(miss)) lg$warn(sprintf("%d target cell(s) have no estimate in any specification: %s", nrow(miss),
                                  paste(unique(sprintf("%s P%d %s", miss$model, miss$period, miss$term)), collapse = ", ")))
  m <- cbind(m, classify_cell(m$target, m$target_se, m$target_level, m$coef, m$p, m$status, cuts))
  m$target_stars_text <- sig_label(m$target_level)
  m$our_stars_text <- ifelse(is.na(m$our_level), "", sig_label(m$our_level))
  keep <- c("table_id", "model", "period", "term", "version", "convention", "spec", "lag",
            "target_text", "target", "target_se", "target_stars_text", "coef", "se", "se_oim", "se_opg",
            "p", "our_stars_text", "n", "estimator", "status", "same_sign", "same_significance",
            "original_significant", "ours_significant",
            "within_one_se", "std_gap", "class", "reason", "labels_confirmed")
  m <- m[order(m$model, m$version, m$convention, m$spec, m$lag, m$period, m$term), keep]
  write_table(m, "classify_cells.csv", lg)

  # ---- N ----
  nn <- unique(est[, c("model", "version", "convention", "spec", "lag", "period", "n")])
  nv <- merge(n_t[, c("model", "period", "target")], nn, by = c("model", "period"))
  nv$n_match <- nv$n == nv$target
  write_table(nv, "classify_N.csv", lg)
  for (conv in CONVENTIONS) {
    x <- nv[nv$convention == conv, ]
    lg$info(sprintf("N (%s): %d of %d comparisons equal the original N", conv, sum(x$n_match), nrow(x)))
  }

  # ---- summary per model x version x convention x spec x lag ----
  grp <- c("model", "version", "convention", "spec", "lag")
  key <- do.call(paste, c(m[grp], sep = "|"))
  summ <- do.call(rbind, lapply(split(m, key), function(d) {
    data.frame(d[1, grp], n_cells = nrow(d),
               reproduced = sum(d$class == CLASSES[1]), partially = sum(d$class == CLASSES[2]),
               not_reproduced = sum(d$class == CLASSES[3]),
               # two views of the reproduced cells (README s.2.8): how many only because both
               # are insignificant, and how many also reproduce the number (within one original SE)
               reproduced_both_insignificant = sum(d$class == CLASSES[1] & !d$original_significant & !d$ours_significant),
               reproduced_within_one_se = sum(d$class == CLASSES[1] & d$within_one_se),
               n_estimated = sum(d$status == "estimated"),
               mean_std_gap = mean(d$std_gap[d$status == "estimated"], na.rm = TRUE),
               stringsAsFactors = FALSE)
  }))
  rownames(summ) <- NULL
  write_table(summ, "classify_summary.csv", lg)

  # ---- rank of Model I combinations (W3-02) ----
  r <- summ[summ$model == "I", ]
  r$lag <- as.integer(r$lag)
  r <- r[order(match(r$convention, CONVENTIONS), -r$reproduced, round(r$mean_std_gap, 8),
               match(r$spec, SPECS), r$lag, match(r$version, VERSIONS)), ]
  r$rank <- stats::ave(seq_len(nrow(r)), r$convention, FUN = seq_along)
  r$tie_broken_by <- NA_character_
  for (conv in CONVENTIONS) {
    x <- r[r$convention == conv, ]
    if (nrow(x) < 2) next
    tb <- if (x$reproduced[1] != x$reproduced[2]) "count" else
      if (!isTRUE(all.equal(x$mean_std_gap[1], x$mean_std_gap[2]))) "mean gap" else "candidate order"
    r$tie_broken_by[r$convention == conv & r$rank == 1] <- tb
  }
  write_table(r, "classify_rank_model_I.csv", lg)
  lg$info("Model I combinations ranked (W3-02): reproduced count, then mean |gap|/original SE, then order")
  for (conv in CONVENTIONS) {
    x <- r[r$convention == conv, ]
    lg$info(sprintf("  convention %s%s:", conv, if (conv == CONVENTIONS[1]) " (primary)" else " (sensitivity)"))
    for (i in seq_len(nrow(x)))
      lg$info(sprintf("   %2d. %-11s j=%d %-8s reproduced %d, partial %d, not %d of %d; mean gap/SE %s",
                      x$rank[i], x$spec[i], x$lag[i], x$version[i], x$reproduced[i], x$partially[i],
                      x$not_reproduced[i], x$n_cells[i], fmt(x$mean_std_gap[i], 2)))
  }
  top <- r[r$convention == CONVENTIONS[1] & r$rank == 1, ]
  if (nrow(top)) {
    lg$info(sprintf("HEADLINE (Model I, %s convention): %s, lag %d, %s data; %d of %d cells reproduced (rank 1 decided by %s)",
                  CONVENTIONS[1], top$spec, top$lag, top$version, top$reproduced, top$n_cells,
                  top$tie_broken_by))
    lg$info(sprintf("  of the %d reproduced: %d because both are insignificant; %d also within one original SE",
                    top$reproduced, top$reproduced_both_insignificant, top$reproduced_within_one_se))
    if (top$reproduced < top$n_cells)
      lg$info("  Not every cell is reproduced: the note reports which ones (classify_cells.csv).")
  }
  s2 <- summ[summ$model == "II", ]
  for (i in seq_len(nrow(s2)))
    lg$info(sprintf("Model II %-8s %-6s: reproduced %d, partial %d, not %d of %d; mean gap/SE %s",
                    s2$version[i], s2$convention[i], s2$reproduced[i], s2$partially[i],
                    s2$not_reproduced[i], s2$n_cells[i], fmt(s2$mean_std_gap[i], 2)))
  for (i in seq_len(nrow(s2)))
    lg$info(sprintf("  Model II %-8s %-6s: of the %d reproduced, %d both insignificant, %d also within one original SE",
                    s2$version[i], s2$convention[i], s2$reproduced[i], s2$reproduced_both_insignificant[i],
                    s2$reproduced_within_one_se[i]))
  lg$info("Record the headline and the Model II result in the W3 result decisions (README_week3 s.7).")
}

run_script("07_classify", main)
