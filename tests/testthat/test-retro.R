## Test script for retrospective and forecast bias functions

path <- file.path(tempdir(), "test_runs")
dir.create(path, showWarnings = FALSE)


## SSB
test_that("Retrospective plot is created for SSB (w/ deprecated parameters)", {
  skip_if(
    !exists("retrosum.simple"),
    message = "skipping test that requires SS3 executable"
  )
  SSplotRetro(retrosum.simple,
    subplots = "SSB",
    use_png = TRUE,
    print_plot = TRUE,
    plot = TRUE,
    plotdir = path
  )

  expect_true(file.exists(file.path(path, "retro_SSB.png")))
})

## F
test_that("Retrospective plot is created for F (w/ deprecated parameters)", {
  skip_if(
    !exists("retrosum.simple"),
    message = "skipping test that requires SS3 executable"
  )
  SSplotRetro(retrosum.simple,
    subplots = "F",
    use_png = TRUE,
    print_plot = TRUE,
    plot = TRUE,
    plotdir = path
  )

  expect_true(file.exists(file.path(path, "retro_F.png")))
})

## Mohn's Rho table using SShcbias function. It's the same code used in SSplotRetro to make the table.
## SSB
test_that("Mohn's Rho table is created for SSB", {
  skip_if(
    !exists("retrosum.simple"),
    message = "skipping test that requires SS3 executable"
  )
  n <- retrosum.simple$n
  endyrs <- retrosum.simple$endyrs - seq(0, n - 1, 1)
  exp <- retrosum.simple$SpawnBio
  y.ref <- exp[, 1]
  subset <- exp$Yr <= endyrs[2]
  subsetfc <- exp$Yr <= endyrs[2] + 1
  x <- exp$Yr[subset]
  y <- exp[subset, 2]
  xfc <- exp$Yr[subsetfc]
  yfc <- exp[subsetfc, 2]

  rho.i <- (y[length(y)] - y.ref[length(y)]) /
    y.ref[length(y)]
  fcrho.i <- (yfc[length(yfc)] - y.ref[length(yfc)]) /
    y.ref[length(yfc)]

  mohn.rho <- SShcbias(retrosum.simple, quants = "SSB")

  expect_equal(round(mohn.rho$Rho[1], 9), rho.i)
  expect_equal(round(mohn.rho$ForcastRho[1], 9), fcrho.i)
  expect_match(mohn.rho$type, "SSB")
})

## F
test_that("Mohn's Rho table is created for F", {
  skip_if(
    !exists("retrosum.simple"),
    message = "skipping test that requires SS3 executable"
  )
  n <- retrosum.simple$n
  endyrs <- retrosum.simple$endyrs - seq(0, n - 1, 1)
  exp <- retrosum.simple$Fvalue
  y.ref <- exp[, 1]
  subset <- exp$Yr <= endyrs[2]
  subsetfc <- exp$Yr <= endyrs[2] + 1
  x <- exp$Yr[subset]
  y <- exp[subset, 2]
  xfc <- exp$Yr[subsetfc]
  yfc <- exp[subsetfc, 2]

  rho.i <- (y[length(y)] - y.ref[length(y)]) /
    y.ref[length(y)]
  fcrho.i <- (yfc[length(yfc)] - y.ref[length(yfc)]) /
    y.ref[length(yfc)]

  mohn.rho <- SShcbias(retrosum.simple, quants = "F")

  expect_equal(round(mohn.rho$Rho[1], 8), rho.i)
  expect_equal(round(mohn.rho$ForcastRho[1], 9), fcrho.i)
  expect_match(mohn.rho$type, "F")
})
