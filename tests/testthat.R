library(testthat)
library(ss3diags)
library(r4ss)

test_example_path <- system.file("extdata", "simple_small", package = "r4ss")
test_fixtures <- system.file("tests", "testthat", "fixtures", package = "ss3diags")
#testthat::test_path("fixtures")

# Creating retrospective object here so that multiple test files can access it without having to re-run retrospective for each test
retroModels <- r4ss::SSgetoutput(
    dirvec = file.path(
      test_fixtures, "retrospectives",
      paste0("retro", 0:-3)
    )
  )
retrosum.simple <- r4ss::SSsummarize(retroModels)

test_check("ss3diags")
