# docs/.Rprofile -- Week 4, Study A
# Quarto starts R inside docs/, where the project's .Rprofile (which activates renv) and the
# project's .Renviron are not read. This file does what those two do, from the project root:
# it switches renv's sandbox off (slow or hanging at startup on this Windows machine; see
# README_week4, Step 1 and W4-13) unless already set, activates renv, then returns to docs/.
local({
  root <- normalizePath("..", winslash = "/", mustWork = FALSE)
  act <- file.path(root, "renv", "activate.R")
  if (!nzchar(Sys.getenv("RENV_CONFIG_SANDBOX_ENABLED")))
    Sys.setenv(RENV_CONFIG_SANDBOX_ENABLED = "FALSE")
  if (file.exists(act)) {
    owd <- setwd(root)
    on.exit(setwd(owd))
    source(act)
  }
})
