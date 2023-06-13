library(testthat)
library(ss3diags)

test_example_path <- system.file("extdata", "simple_small", package = "r4ss")

# Creating retrospective object here so that multiple test files can access it without having to re-run retrospective for each test
retroModels <- SSgetoutput(
    dirvec = file.path(
      test_example_path, "retrospectives",
      paste0("retro", 0:-3)
    )
  )
retrosum.simple <- r4ss::SSsummarize(retroModels)

test_check("ss3diags")
