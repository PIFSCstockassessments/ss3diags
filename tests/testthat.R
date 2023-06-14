library(testthat)
library(ss3diags)
library(r4ss)

test_example_path <- system.file("extdata", "simple_small", package = "r4ss")
test_fixtures <- system.file("fixtures", package = "ss3diags")

list.files(system.file("tests", "testthat", package = "ss3diags"))
list.files(system.file("tests", "testthat", "fixtures", package = "ss3diags"))


# runs_path avoids repeated use of "extdata" that would have to be added
# if using tmp_path directly
runs_path <- file.path(tmp_path, "extdata")
path_simple_small <- file.path(runs_path, "simple_small")
retro_years <- 0:-2

#testthat::test_path("fixtures")
file.path(
      test_fixtures, "retrospectives",
      paste0("retro", 0:-3)
    )
# Creating retrospective object here so that multiple test files can access it without having to re-run retrospective for each test
retroModels <- r4ss::SSgetoutput(
    dirvec = file.path(
      test_fixtures, "retrospectives",
      paste0("retro", 0:-3)
    )
  )
retrosum.simple <- r4ss::SSsummarize(retroModels)

test_check("ss3diags")
