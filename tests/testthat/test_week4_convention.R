# tests/testthat/test_week4_convention.R -- Week 4 rerun 2: primary convention 'within' (W4-27), Study A
# Toy tables only; no original, SAFE, Wind, IMF or FRED values.
# Run from the project root:   Rscript -e "testthat::test_dir('tests/testthat')"

root <- normalizePath(file.path("..", ".."))
owd <- setwd(root)
source(file.path("code", "R", "common.R"))
source(file.path("code", "R", "models.R"))
source(file.path("code", "R", "corrections.R"))
# the function definitions of a script, without running it (its last line calls run_script)
script_functions <- function(path, env) {
  for (e in as.list(parse(path))) {
    if (is.call(e) && identical(e[[1]], as.name("<-")) && is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function")))
      eval(e, env)
  }
  env
}
f08 <- script_functions(file.path("code", "R", "08_corrections.R"), new.env(parent = globalenv()))
r_files <- normalizePath(list.files(file.path("code", "R"), pattern = "\\.R$", full.names = TRUE))
setwd(owd)

test_that("the primary convention is within (W4-27)", {
  expect_identical(CONVENTIONS[1], "within")
  expect_setequal(CONVENTIONS, c("within", "full"))
})

test_that("read_headline() of 08 takes the rank-1 row of the primary convention", {
  td <- tempfile(); dir.create(td)
  old <- PATHS$tables; PATHS$tables <<- td; on.exit(PATHS$tables <<- old)
  utils::write.csv(data.frame(model = "I", convention = c("full", "within", "within"), spec = c("primary", "alternative", "primary"),
                              lag = c(2, 1, 3), version = c("deflated", "deflated", "raw"), rank = c(1, 1, 2)),
                   file.path(td, "classify_rank_model_I.csv"), row.names = FALSE)
  lg <- list(info = function(...) NULL, warn = function(...) NULL)
  h <- f08$read_headline(lg)
  expect_identical(h$spec, "alternative"); expect_identical(h$lag, 1L); expect_identical(h$version, "deflated")
})

test_that("no script selects the 'full' convention by a literal", {
  bad <- "convention\\s*==\\s*[\"']full[\"']|conv\\s*=\\s*[\"']full[\"']|[\"']diff_full[\"']|CONVENTIONS\\s*<-\\s*c\\([\"']full|\"[^\"]*full convention[^\"]*\""   # last part: a log or table label naming the convention (07, W4-27 rerun 2)
  hits <- unlist(lapply(r_files, function(f) {
    x <- readLines(f, warn = FALSE)
    i <- grep(bad, x)
    if (length(i)) sprintf("%s:%d: %s", basename(f), i, trimws(x[i])) else character(0)
  }))
  expect(length(hits) == 0, paste(c("hard-coded 'full' convention found (W4-27):", hits), collapse = "\n"))
})
