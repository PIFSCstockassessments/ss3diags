## Test script for JABBA residual plot
simple <- r4ss::SS_output(dir = test_example_path, verbose = FALSE, printstats = FALSE)

path <- file.path(tempdir(), "test_runs")
dir.create(path, showWarnings = FALSE)

## Simple
test_that("file of simple_cpue_jabbaresiduals plot exists", {
  SSplotJABBAres(simple,
    use_png = TRUE,
    print_plot = TRUE,
    subplots = "cpue",
    plotdir = path,
    filenameprefix = "simple_cpue"
  )

  expect_true(file.exists(file.path(path, "simple_cpuejabbaresidual.png")))
})

test_that("file of simple_len_jabbaresiduals plot exists", {
  SSplotJABBAres(simple,
    use_png = TRUE,
    print_plot = TRUE,
    subplots = "len",
    plotdir = path,
    filenameprefix = "simple_len2_"
  )

  expect_true(file.exists(file.path(path, "simple_len2_jabbaresidual.png")))
})

## CAAL uncomment when con option is finished in function
# test_that("file of simple_con_jabbaresiduals plot exists", {
#
# SSplotJABBAres(simple,
# use_png = TRUE,
# print_plot = TRUE,
# subplots = "con",
# plotdir = path,
# filenameprefix = "simple_con_"
# )

# expect_true(file.exists(file.path(path, "simple_con_jabbaresidual.png")))
#
# })

test_that("calculate RMSE for CPUE index", {
  rmse <- SSrmse(simple, quants = "cpue")

  expect_equal(rmse$RMSE$RMSE.perc[1], 21.6)
})


test_that("Calculate combined RMSE for length comp data", {
  rmse <- SSrmse(simple, quants = "len")$RMSE

  expect_equal(rmse$RMSE.perc[3], 2.5)
})
