## Test script for hindcast cross validation and MASE

path <- file.path(tempdir(), "test_runs")
dir.create(path, showWarnings = FALSE)

## Hindcast plotting of indices

test_that("Hindcast plot is created for simple model indices", {
  skip_if(
    !exists("retrosum.simple"),
    message = "skipping test that requires SS3 executable"
  )
  SSplotHCxval(retrosum.simple,
    add = T,
    verbose = F,
    legendcex = 0.7,
    use_png = TRUE,
    print_plot = TRUE,
    plotdir = path
  )

  fleets <- unique(retrosum.simple$indices$Fleet)
  plot_names <- paste0(
    rep(path, length(fleets)),
    "/",
    rep("hcxval", length(fleets)),
    "_",
    fleets,
    ".png"
  )

  ## Check that each plot exists individually
  expect_true(file.exists(plot_names[1]))
})


## Test that MASE table is correct
#### note: function was too complicated to replicate in the test script so used values directly from running the function. If code for calculations changes, the values will change and it will error or if the SS3 input files change, the values will be different and it will error.

test_that("MASE table gives expected values for simple model", {
  skip_if(
    !exists("retrosum.simple"),
    message = "skipping test that requires SS3 executable"
  )

  mase <- SSplotHCxval(retrosum.simple, add = T, verbose = F)

  expect_match(mase$Index[1], "SURVEY1")
  expect_equal(round(mase$MASE[1], 7), 0.494371)
  expect_equal(round(mase$MAE.PR[1], 7), 0.0873324)
})


test_that("SSretroComps returns the correct comp data for simple model", {
  skip_if(
    !exists("retrosum.simple"),
    message = "skipping test that requires SS3 executable"
  )
  retro_comps <- SSretroComps(retroModels)

  expect_equal(retro_comps$n, 4)
  expect_equal(retro_comps$startyrs, rep(2011, 4))
  expect_equal(retro_comps$endyrs, rep(2022, 4))
  # expect_gt(nrow(retro_comps$con), 1)
  expect_gt(nrow(retro_comps$len), 1)
  expect_gt(nrow(retro_comps$age), 1)
})


test_that("SSmase base.adj changes", {
  skip_if(
    !exists("retrosum.simple"),
    message = "skipping test that requires SS3 executable"
  )
  ssmase <- SSmase(retrosum.simple, MAE.base.adj = 0.15)

  expect_equal(round(ssmase$MASE.adj[1], 7), 0.4943710)
})
