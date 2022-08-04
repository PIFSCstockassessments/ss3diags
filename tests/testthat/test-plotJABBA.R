## Test script for JABBA residual plot

simple <- ss3diags::simple

path <- file.path(tempdir(), "test_runs")
dir.create(path, showWarnings = FALSE)


## Simple
test_that("file of simple_cpue_jabbaresiduals plot exists", {
  SSplotJABBAres(simple,
    use_png = TRUE,
    print_plot = T,
    subplots = "cpue",
    plotdir = path,
    filenameprefix = "simple_cpue"
  )

  expect_true(file.exists(file.path(path, "simple_cpuejabbaresidual.png")))
})

test_that("file of simple_len_jabbaresiduals plot exists", {
  SSplotJABBAres(simple,
    use_png = TRUE,
    print_plot = T,
    subplots = "len",
    # indexselect = 2,
    plotdir = path,
    filenameprefix = "simple_len2_"
  )

  expect_true(file.exists(file.path(path, "simple_len2_jabbaresidual.png")))
})

## CAAL uncomment when con option is finished in function
test_that("file of simple_con_jabbaresiduals plot exists", {
#
SSplotJABBAres(simple,
  use_png = TRUE,
  print_plot = T,
  subplots = "con",
  plotdir = path,
  filenameprefix = "simple_con_"
)

  expect_true(file.exists(file.path(path, "simple_con_jabbaresidual.png")))
#
  })

test_that("calculate RMSE for CPUE index", {
  
  rmse <- SSrmse(simple, quants = "cpue")
  
  expect_equal(rmse$RMSE$RMSE.perc[1], 20.4)
  
})
  

test_that("Calculate combined RMSE for length comp data", {
  
  rmse <- SSrmse(simple, quants = "len")$RMSE
  
  expect_equal(rmse$RMSE.perc[3], 4.3)
  
})