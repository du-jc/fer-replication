# code/R/01_import_verify.R -- Week 2, Study A (Blueprint A.2 step A3; Replication Plan s.1, s.2)
#
# 1. Imports data/interim/original_appendix.csv with explicit types and checks its structure.
# 2. Rebuilds Tables 3.2-3.9 of the original document (period statistics and correlations) from the extracted
#    data, on raw and on deflated data, and compares every cell with original_targets.csv.
#    A cell matches when it agrees to the decimals printed in the original document (decision W2-07).
# 3. Imports the monthly NDF (register_wind.py) and the six BOP candidates (collect_bop.py),
#    checks months and completeness, and writes the monthly analysis file
#    data/interim/study_a_monthly.csv (git-ignored: it holds Wind-derived values; W2-08).
#
# Outputs: outputs/tables/verify_T3_cells.csv (every cell), verify_T3_summary.csv,
#          data/interim/study_a_monthly.csv, outputs/logs/01_import_verify_<ts>.log
# Usage:   Rscript code/R/01_import_verify.R        (from the project root)

source(file.path("code", "R", "common.R"))

SUMMARY_STATS <- c(obs = "n", mean = "mean", stddev = "sd", min = "min", max = "max")
PERIOD_WORDS <- c("first", "second", "third", "fourth")

import_appendix <- function(lg) {
  a <- read_chr_csv(PATHS$appendix)
  miss <- setdiff(c("month_label", "month", ORIGINAL_VARS), names(a))
  if (length(miss)) stop("appendix columns missing: ", paste(miss, collapse = ", "),
                         " (found: ", paste(names(a), collapse = ", "), ")")
  if (nrow(a) != length(SAMPLE_MONTHS)) stop(sprintf("appendix has %d rows, expected %d",
                                                     nrow(a), length(SAMPLE_MONTHS)))
  if (!identical(a$month, SAMPLE_MONTHS)) stop("appendix months are not 2000-01..2011-12 in order")
  lg$info(sprintf("CHECK PASS  appendix: %d rows, months %s..%s consecutive",
                  nrow(a), a$month[1], a$month[nrow(a)]))
  for (v in ORIGINAL_VARS) {
    x <- to_num(a[[v]])
    bad <- which(is.na(x))
    if (length(bad)) stop(sprintf("appendix column %s: %d value(s) not numeric, first at %s",
                                  v, length(bad), a$month[bad[1]]))
    a[[v]] <- x
  }
  lg$info(sprintf("CHECK PASS  all %d variables numeric, no missing values", length(ORIGINAL_VARS)))
  # the raw ER in Period 1 is constant (Replication Plan s.2.1); report, do not assume
  sd_er1 <- stats::sd(a$ER[in_period(a$month, 1)])
  lg$info(sprintf("Raw ER standard deviation in Period 1: %s", fmt(sd_er1, 6)))
  a
}

label_map_status <- function(lg) {
  if (!file.exists(PATHS$label_map)) { lg$warn("label map not found"); return(invisible()) }
  m <- read_chr_csv(PATHS$label_map)
  conf <- sum(tolower(trimws(m$confirmed)) == "yes" & nzchar(trimws(m$confirmed_label)))
  msg <- sprintf("label map: %d/%d labels confirmed", conf, nrow(m))
  if (conf < nrow(m)) lg$warn(msg, " -- confirm them (decision W1-04) before trusting pi-labelled cells")
  else lg$info("CHECK PASS  ", msg)
}

table_period <- function(tid, caption) {
  w <- regmatches(tolower(caption), regexpr("first|second|third|fourth", tolower(caption)))
  if (length(w)) return(list(p = match(w, PERIOD_WORDS), how = "caption"))
  num <- as.integer(sub("^T3\\.", "", tid))
  list(p = ceiling((num - 1) / 2), how = "table order (caption had no period word)")
}

cell_value <- function(dat, kind, v1, v2, stat, rows) {
  if (is.na(v1)) return(NA_real_)
  x <- dat[[v1]][rows]
  if (kind == "summary") {
    return(switch(stat, n = sum(!is.na(x)), mean = mean(x), sd = stats::sd(x),
                  min = min(x), max = max(x), NA_real_))
  }
  if (is.na(v2)) return(NA_real_)
  suppressWarnings(stats::cor(x, dat[[v2]][rows]))
}

verify_tables3 <- function(a, lg) {
  tg_all <- read_targets(sprintf("T3.%d", 2:9))
  if (!nrow(tg_all)) stop("no Table 3.2-3.9 targets in original_targets.csv")
  versions <- list(raw = a, deflated = deflate(a, ORIGINAL_VARS))
  var_by_norm <- stats::setNames(ORIGINAL_VARS, norm_label(ORIGINAL_VARS))
  cells <- list()
  for (tid in sprintf("T3.%d", 2:9)) {
    tg <- tg_all[tg_all$table_id == tid & tg_all$element == "value", , drop = FALSE]
    if (!nrow(tg)) { lg$warn(tid, ": no numeric target cells"); next }
    tp <- table_period(tid, tg$caption[1])
    if (tp$how != "caption") lg$warn(tid, ": period taken from ", tp$how)
    kind <- if (any(tg$col_norm %in% names(SUMMARY_STATS))) "summary" else "correlation"
    rows <- in_period(a$month, tp$p)
    for (r in seq_len(nrow(tg))) {
      v1 <- unname(var_by_norm[tg$row_norm[r]])
      v2 <- if (kind == "correlation") unname(var_by_norm[tg$col_norm[r]]) else NA_character_
      stat <- if (kind == "summary") unname(SUMMARY_STATS[tg$col_norm[r]]) else NA_character_
      tol <- print_tolerance(tg$value_text[r])
      rec <- data.frame(table_id = tid, period = tp$p, kind = kind,
                        row_label = tg$row_label[r], col_label = tg$col_label[r],
                        variable = v1, variable2 = v2, statistic = stat,
                        target_text = tg$value_text[r], target = tg$value[r], tolerance = tol,
                        labels_confirmed = tg$labels_confirmed[r], stringsAsFactors = FALSE)
      for (vn in names(versions)) {
        val <- cell_value(versions[[vn]], kind, v1, v2, stat, rows)
        rec[[paste0(vn, "_value")]] <- val
        rec[[paste0(vn, "_diff")]] <- val - tg$value[r]
        rec[[paste0(vn, "_match")]] <- !is.na(val) && abs(val - tg$value[r]) <= tol
      }
      cells[[length(cells) + 1]] <- rec
    }
  }
  cells <- do.call(rbind, cells)
  unmatched <- is.na(cells$variable) | (cells$kind == "summary" & is.na(cells$statistic)) |
    (cells$kind == "correlation" & is.na(cells$variable2))
  if (any(unmatched)) {
    lg$warn(sprintf("%d target cell(s) whose labels do not name a variable/statistic, e.g. '%s' x '%s'",
                    sum(unmatched), cells$row_label[unmatched][1], cells$col_label[unmatched][1]))
  }
  summ <- do.call(rbind, lapply(split(cells, cells$table_id), function(d) data.frame(
    table_id = d$table_id[1], period = d$period[1], kind = d$kind[1], n_cells = nrow(d),
    n_labels_unconfirmed = sum(d$labels_confirmed != "yes"),
    raw_matches = sum(d$raw_match), deflated_matches = sum(d$deflated_match),
    raw_max_abs_diff = suppressWarnings(max(abs(d$raw_diff), na.rm = TRUE)),
    deflated_max_abs_diff = suppressWarnings(max(abs(d$deflated_diff), na.rm = TRUE)))))
  for (i in seq_len(nrow(summ))) {
    s <- summ[i, ]
    best <- max(s$raw_matches, s$deflated_matches)
    msg <- sprintf("%s (period %d, %s): %d cells; matches raw %d, deflated %d",
                   s$table_id, s$period, s$kind, s$n_cells, s$raw_matches, s$deflated_matches)
    if (best == s$n_cells) lg$info("CHECK PASS  ", msg) else lg$warn(msg, " -- mismatches listed in verify_T3_cells.csv")
  }
  better <- ifelse(summ$raw_matches >= summ$deflated_matches, "raw", "deflated")
  lg$info("Version matching best, per table: ",
          paste(sprintf("%s=%s", summ$table_id, better), collapse = ", "))
  mm <- cells[!cells$raw_match & !cells$deflated_match, , drop = FALSE]
  if (nrow(mm)) {
    lg$info(sprintf("%d cell(s) match in neither version; first ones:", nrow(mm)))
    for (i in utils::head(seq_len(nrow(mm)), 10)) {
      lg$info(sprintf("  %s  %-6s x %-10s target %s  raw %s  deflated %s", mm$table_id[i],
                      mm$row_label[i], mm$col_label[i], mm$target_text[i],
                      fmt(mm$raw_value[i], 4), fmt(mm$deflated_value[i], 4)))
    }
  }
  write_table(cells, "verify_T3_cells.csv", lg)
  write_table(summ, "verify_T3_summary.csv", lg)
}

import_month_file <- function(path, cols, what, lg) {
  if (!file.exists(path)) {
    lg$error(sprintf("%s not found (%s). Run the Python step first.", path, what))
    return(NULL)
  }
  d <- read_chr_csv(path)
  miss <- setdiff(c("month", cols), names(d))
  if (length(miss)) { lg$error(sprintf("%s: columns missing: %s", path, paste(miss, collapse = ", "))); return(NULL) }
  d <- d[d$month %in% SAMPLE_MONTHS, , drop = FALSE]
  if (!identical(d$month, SAMPLE_MONTHS)) {
    lg$error(sprintf("%s: sample months incomplete or out of order (%d of %d)", path,
                     nrow(d), length(SAMPLE_MONTHS)))
    return(NULL)
  }
  for (v in cols) {
    d[[v]] <- to_num(d[[v]])
    if (anyNA(d[[v]])) { lg$error(sprintf("%s: %s has missing values in the sample", path, v)); return(NULL) }
  }
  lg$info(sprintf("CHECK PASS  %s: %d sample months, columns %s complete", what,
                  nrow(d), paste(cols, collapse = ", ")))
  d[, c("month", cols)]
}

build_panel <- function(a, lg) {
  ndf <- import_month_file(PATHS$ndf_monthly, c("ndf_mavg", "ndf_mend", "n_days"), "monthly NDF", lg)
  bop_cols <- paste("ca", c("hold", "div3", "ytd"), sep = "_")      # W2-12: current account only
  bop <- import_month_file(PATHS$bop_monthly, bop_cols, "BOP candidates", lg)
  if (is.null(ndf) || is.null(bop)) {
    lg$error("study_a_monthly.csv not written: NDF or BOP input missing")
    return(invisible())
  }
  names(ndf)[names(ndf) == "n_days"] <- "ndf_n_days"
  names(bop)[-1] <- paste0("bop_", names(bop)[-1])
  panel <- merge(merge(a[, c("month", "month_label", ORIGINAL_VARS)], ndf, by = "month"), bop, by = "month")
  panel <- panel[order(panel$month), ]
  if (nrow(panel) != length(SAMPLE_MONTHS)) stop("merge lost months")
  utils::write.csv(panel, PATHS$panel, row.names = FALSE, fileEncoding = "UTF-8")
  lg$info(sprintf("Wrote %s: %d months x %d columns (git-ignored; decision W2-08)",
                  PATHS$panel, nrow(panel), ncol(panel)))
}

main <- function(lg) {
  a <- import_appendix(lg)
  label_map_status(lg)
  verify_tables3(a, lg)
  build_panel(a, lg)
}

run_script("01_import_verify", main)
