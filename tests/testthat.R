library(testthat)
library(ss3diags)
library(r4ss)

test_example_path <- system.file("extdata", "simple_small", package = "r4ss")
files_path <- system.file("extdata", package = "ss3diags")
run_tmp <- file.path(tempdir(check = TRUE), "test-runs")
dir.create(run_tmp, showWarnings = FALSE)
file.copy(from = list.files(files_path, full.names = TRUE), to = run_tmp)
#r4ss::get_ss3_exe(dir = run_tmp, version = "v3.30.21")
ss3diags::get_ss3_exe(dir = run_tmp, version = "v3.30.21")
## Run retrospectives
if (file.exists(file.path(run_tmp, "ss3")) | file.exists(file.path(run_tmp, "ss3.exe"))) {
  r4ss::retro(dir = run_tmp, oldsubdir = "", newsubdir = "retrospectives", years = 0:-3, show_in_console = FALSE)
  # Creating retrospective object here so that multiple test files can access it without having to re-run retrospective for each test
  retroModels <- r4ss::SSgetoutput(
    dirvec = file.path(
      run_tmp, "retrospectives",
      paste0("retro", 0:-3)
    )
  )
  retrosum.simple <- r4ss::SSsummarize(retroModels)
}
on.exit(unlink(tmp_path, recursive = TRUE))

test_check("ss3diags")
