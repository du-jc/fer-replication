# 00_packages.R -- Study A, Week 1 (Blueprint A.3, Week 1 setup)
#
# Installs (if missing) and records the R packages used in Study A, inside the project's
# renv library. Run from the project root (fer-replication), after renv::init():
#
#   In R:           source("code/R/00_packages.R")      then  renv::snapshot()
#   Or in cmd:      Rscript code/R/00_packages.R
#
# Writes a run log to outputs/logs/00_packages_<timestamp>.log
# R never calls an API or a web page in this project (Blueprint 0.2); installing packages
# from CRAN is the only network use, and it happens here.

pkgs <- c("tidyverse", "readxl", "lmtest", "sandwich", "urca", "tseries",
          "forecast", "strucchange", "car", "boot", "testthat")

# renv finds dependencies by reading library() calls in the project's R files.
# This block never runs, but it lets renv::snapshot() see every package above.
if (FALSE) {
  library(tidyverse); library(readxl); library(lmtest); library(sandwich)
  library(urca); library(tseries); library(forecast); library(strucchange)
  library(car); library(boot); library(testthat)
}

# ---- run log ----------------------------------------------------------------------------
if (!file.exists(file.path("code", "R", "00_packages.R"))) {
  stop("Run this from the project root (the folder that contains code/ and data/).")
}
dir.create(file.path("outputs", "logs"), recursive = TRUE, showWarnings = FALSE)
log_path <- file.path("outputs", "logs",
                      sprintf("00_packages_%s.log", format(Sys.time(), "%Y%m%d-%H%M%S")))
log_con <- file(log_path, open = "wt", encoding = "UTF-8")
n_warn <- 0L
n_err <- 0L
log_line <- function(level, msg) {
  line <- sprintf("%s %-5s %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), level, msg)
  cat(line, "\n", sep = "")
  writeLines(line, log_con)
  flush(log_con)
  if (level == "WARN") n_warn <<- n_warn + 1L
  if (level == "ERROR") n_err <<- n_err + 1L
}

# ---- main -------------------------------------------------------------------------------
tryCatch({
  log_line("INFO", sprintf("%s on %s", R.version.string, R.version$platform))
  renv_active <- requireNamespace("renv", quietly = TRUE) && !is.null(renv::project())
  if (renv_active) {
    log_line("INFO", sprintf("renv active; library: %s", .libPaths()[1]))
  } else {
    log_line("WARN", "renv is not active: packages go to your user library. Run renv::init() first.")
  }
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    log_line("INFO", sprintf("Installing: %s", paste(missing, collapse = ", ")))
    if (renv_active) renv::install(missing) else install.packages(missing)
  } else {
    log_line("INFO", "All packages already installed")
  }
  for (p in pkgs) {
    if (requireNamespace(p, quietly = TRUE)) {
      log_line("INFO", sprintf("  %-12s %s", p, as.character(utils::packageVersion(p))))
    } else {
      log_line("ERROR", sprintf("  %-12s NOT INSTALLED", p))
    }
  }
  if (renv_active) log_line("INFO", "Next: renv::snapshot() to write renv.lock")
}, error = function(e) {
  log_line("ERROR", conditionMessage(e))
})

status <- if (n_err > 0) "FAIL" else if (n_warn > 0) "WARN" else "OK"
log_line("INFO", sprintf("STATUS: %s (warnings=%d, errors=%d)", status, n_warn, n_err))
close(log_con)
cat("Run log:", log_path, "\n")
