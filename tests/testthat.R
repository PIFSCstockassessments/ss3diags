library(testthat)
library(ss3diags)
library(r4ss)

# Check if we're in CI
is_ci <- Sys.getenv("CI") != "" || Sys.getenv("GITHUB_ACTIONS") != ""

if (is_ci) {
  message("Running in CI environment - skipping SS3 executable download")
  .retro_available <<- FALSE
} else {
  # Set up test environment (only for local testing)
  test_example_path <- system.file("extdata", "simple_small", package = "r4ss")
  files_path <- system.file("extdata", package = "ss3diags")
  run_tmp <- file.path(tempdir(check = TRUE), "test-runs")
  dir.create(run_tmp, showWarnings = FALSE)
  file.copy(from = list.files(files_path, full.names = TRUE), to = run_tmp)

  # Try to get SS3 executable with timeout
  tryCatch({
    # Set shorter timeout for local testing
    old_timeout <- getOption("timeout")
    options(timeout = 60)  # 60 seconds
    on.exit(options(timeout = old_timeout), add = TRUE)
    
    r4ss::get_ss3_exe(dir = run_tmp)
  }, error = function(e) {
    message("Could not download SS3 executable: ", e$message)
  })

  ## Run retrospectives if SS3 is available
  if (file.exists(file.path(run_tmp, "ss3")) || file.exists(file.path(run_tmp, "ss3.exe"))) {
    tryCatch({
      r4ss::retro(dir = run_tmp, oldsubdir = "", newsubdir = "retrospectives", 
                  years = 0:-3, show_in_console = FALSE)
      
      # Create retrospective object and make it globally available
      retroModels <- r4ss::SSgetoutput(
        dirvec = file.path(run_tmp, "retrospectives", paste0("retro", 0:-3))
      )
      retrosum.simple <<- r4ss::SSsummarize(retroModels)
      .retro_available <<- TRUE
      
    }, error = function(e) {
      message("Error running retrospectives: ", e$message)
      .retro_available <<- FALSE
    })
  } else {
    message("SS3 executable not found")
    .retro_available <<- FALSE
  }

  # Clean up on exit
  on.exit(unlink(run_tmp, recursive = TRUE), add = TRUE)
}

test_check("ss3diags")