library(testthat)
library(ss3diags)
library(r4ss)

test_example_path <- system.file("extdata", "simple_small", package = "r4ss")

tmp_path <- file.path(tempdir(check = TRUE), "test-retro")
dir.create(tmp_path, showWarnings = FALSE)
runs_path <- file.path(tmp_path, "simple_small")
file.copy(test_example_path, tmp_path, recursive = TRUE)
file.copy(file.path("tests", "testthat", "ss"), runs_path)
retro_years <- 0:-3
# clean up
on.exit(unlink(tmp_path, recursive = TRUE))

## Run retrospectives
r4ss::retro(dir = runs_path, oldsubdir = "", newsubdir = "retrospectives", years = retro_years, show_in_console = FALSE)
         # unlink(file.path(runs_path, "retrospectives", "retro0", "ss"))
         # unlink(file.path(runs_path, "retrospectives", "retro-1", "ss"))
         # unlink(file.path(runs_path, "retrospectives", "retro-2", "ss"))
         # unlink(file.path(runs_path, "retrospectives", "retro-3", "ss"))
        #  unlink(file.path(runs_path, "ss"))

# Creating retrospective object here so that multiple test files can access it without having to re-run retrospective for each test
retroModels <- r4ss::SSgetoutput(
    dirvec = file.path(
      runs_path, "retrospectives",
      paste0("retro", retro_years)
    )
  )
retrosum.simple <- r4ss::SSsummarize(retroModels)

test_check("ss3diags")
