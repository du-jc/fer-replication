# code/R/check_deflate_scope.R -- Week 4 rerun, open item "deflate() scope" (Study A)
#
# Which columns does the 'deflated' data version divide by pi/100? The article's list (p. 8) is to be
# compared by hand with this output (README_week4, rerun step R6). The script builds the deflated panel
# exactly as 08_corrections.R does (corr_version_panel -> version_panel -> deflate) and, as a check, the
# way the Week 3 scripts call it, then reports for every column whether it changed and whether the change
# equals division by pi/100. It estimates nothing and types no value.
#
# Output: outputs/tables/check_deflate_scope.csv; run log in outputs/logs/.
# Usage:  Rscript code/R/check_deflate_scope.R

source(file.path("code", "R", "common.R"))
source(file.path("code", "R", "models.R"))
source(file.path("code", "R", "corrections.R"))

main <- function(lg) {
  sel <- read_selected_inputs(lg)
  panel <- read_panel(lg)
  raw <- corr_version_panel(panel, "raw", sel)
  dfl <- corr_version_panel(panel, "deflated", sel)
  cols <- setdiff(names(panel), c("month", "month_label", "period"))
  f <- panel$pi / 100
  out <- do.call(rbind, lapply(cols, function(v) {
    a <- to_num(raw[[v]]); b <- to_num(dfl[[v]])
    ok <- is.finite(a) & is.finite(b)
    changed <- any(abs(a[ok] - b[ok]) > 1e-12 * pmax(1, abs(a[ok])))
    divided <- changed && isTRUE(all.equal(b[ok], a[ok] / f[ok], tolerance = 1e-10))
    role <- if (identical(v, sel$bop)) "BOP (selected, W2-10)" else if (identical(v, sel$ndf)) "NDF (selected, W2-10)" else ""
    data.frame(column = v, role = role, used_in_models = v %in% c(ORIGINAL_VARS, sel$bop, sel$ndf),
               changed = changed, divided_by_pi_over_100 = divided, stringsAsFactors = FALSE)
  }))
  write_table(out, "check_deflate_scope.csv", lg)
  lg$info(sprintf("Deflated version divides by pi/100: %s", paste(out$column[out$divided_by_pi_over_100], collapse = ", ")))
  lg$info(sprintf("Not divided: %s", paste(out$column[!out$divided_by_pi_over_100], collapse = ", ")))
  if (any(out$changed & !out$divided_by_pi_over_100))
    lg$warn("some columns changed in a way other than division by pi/100: see check_deflate_scope.csv")
  lg$info("Compare with the article's list of deflated variables (p. 8) and record which list 'deflated' follows (README_week4 R6).")
}

run_script("check_deflate_scope", main)
