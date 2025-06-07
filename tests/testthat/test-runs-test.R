simple <- r4ss::SS_output(dir = test_example_path, verbose = FALSE, printstats = FALSE)

#### Simple Model ###############################################################

########################################################################################


## SSrunstest function
test_that("runs test works with simple model", {
  test.resids <- simple$cpue[which(simple$cpue$Fleet_name == "SURVEY2"), c("Fleet_name", "Yr", "Obs", "Exp")]
  test.resids$residuals <- log(test.resids$Obs) - log(test.resids$Exp)

  mu <- 0
  mr <- abs(diff(test.resids$residuals - mu))
  amr <- mean(mr, na.rm = TRUE)
  ulmr <- 3.267 * amr
  mr <- mr[mr < ulmr]
  amr <- mean(mr, na.rm = TRUE)
  stdev <- amr / 1.128
  lcl <- mu - 3 * stdev
  ucl <- mu + 3 * stdev
  runstest <- randtests::runs.test(test.resids$residuals,
    threshold = 0,
    alternative = "left.sided"
  )
  test.p <- round(runstest$p.value, 3)

  ## for cpue
  n.cpue <- length(unique(simple$cpue$Fleet))
  run_cpue <- SSrunstest(simple, quants = "cpue")

  ## testing structure of dataframe
  expect_match(run_cpue$Index[1], "SURVEY1")
  expect_equal(nrow(run_cpue), n.cpue)
  ## testing values in the first row
  expect_match(run_cpue$test[2], "Passed")
  expect_equal(run_cpue$runs.p[2], test.p)
  expect_equal(run_cpue$sigma3.lo[2], lcl)
  expect_equal(run_cpue$sigma3.hi[2], ucl)

  ## for length comp
  len.test.resids <- simple$lendbase[which(simple$lendbase$Fleet == 1), ]
  len.test.resids$indx <- paste(len.test.resids$Fleet, len.test.resids$Yr, len.test.resids$Seas)

  uind <- unique(len.test.resids$indx)
  pldat <- matrix(0, length(uind), 13,
    dimnames = list(
      uind,
      c(
        "Obsmn", "Obslo", "Obshi", "semn", "Expmn", "Like", "Std.res",
        "ObsloAdj", "ObshiAdj", "Fleet", "Yr", "Time", "Seas"
      )
    )
  )

  for (i in 1:length(uind)) {
    subdbase <- len.test.resids[which(len.test.resids$indx == uind[i]), ]

    if (is.null(subdbase$Nsamp_adj)) subdbase$Nsamp_adj <- subdbase$N
    xvar <- subdbase$Bin
    pldat[i, "Obsmn"] <- sum(subdbase$Obs * xvar) / sum(subdbase$Obs)
    pldat[i, "Expmn"] <- sum(subdbase$Exp * xvar) / sum(subdbase$Exp)
    pldat[i, "semn"] <- sqrt((sum(subdbase$Exp * xvar^2) / sum(subdbase$Exp) -
      pldat[i, "Expmn"]^2) / mean(subdbase$Nsamp_adj))
    pldat[i, "Obslo"] <- pldat[i, "Obsmn"] - 2 * pldat[i, "semn"]
    pldat[i, "Obshi"] <- pldat[i, "Obsmn"] + 2 * pldat[i, "semn"]
    pldat[i, "Std.res"] <- (pldat[i, "Obsmn"] - pldat[i, "Expmn"]) / pldat[i, "semn"]
    pldat[i, "Fleet"] <- mean(subdbase$Fleet)
    pldat[i, "Yr"] <- mean(subdbase$Yr)
    pldat[i, "Time"] <- mean(subdbase$Time)
    pldat[i, "Seas"] <- mean(subdbase$Seas)
    pldat[i, "Like"] <- mean(subdbase$Like)
  }

  Nmult <- 1 / var(pldat[, "Std.res"], na.rm = TRUE)

  for (i in 1:length(uind)) {
    pldat[i, "ObsloAdj"] <- pldat[i, "Obsmn"] - 2 * pldat[i, "semn"] / sqrt(Nmult)
    pldat[i, "ObshiAdj"] <- pldat[i, "Obsmn"] + 2 * pldat[i, "semn"] / sqrt(Nmult)
  }

  pldat <- data.frame(pldat)
  yrs <- pldat$Yr

  runs_dat <- data.frame(
    Fleet = pldat$Fleet,
    Fleet_name = simple$FleetNames[pldat$Fleet],
    Yr = yrs,
    Time = pldat$Time,
    Seas = pldat$Seas,
    Obs = pldat$Obsmn,
    Exp = pldat$Expmn,
    SE = ((pldat$Obsmn - pldat$ObsloAdj) / 1.96) / pldat$ObsloAdj,
    Like = pldat$Like
  )

  runs_dat$residuals <- log(runs_dat$Obs) - log(runs_dat$Exp)
  runstest <- randtests::runs.test(runs_dat$residuals,
    threshold = 0,
    alternative = "left.sided"
  )
  test.p <- round(runstest$p.value, 3)

  mu <- 0
  mr <- abs(diff(runs_dat$residuals - mu))
  amr <- mean(mr, na.rm = TRUE)
  ulmr <- 3.267 * amr
  mr <- mr[mr < ulmr]
  amr <- mean(mr, na.rm = TRUE)
  stdev <- amr / 1.128
  lcl <- mu - 3 * stdev
  ucl <- mu + 3 * stdev


  n.fish <- nrow(simple$Length_Comp_Fit_Summary)
  run_fish <- SSrunstest(simple, quants = "len")

  ## testing structure of dataframe
  expect_match(run_fish$Index[1], "FISHERY")
  expect_equal(nrow(run_fish), n.fish)
  ## testing values in first row
  expect_equal(run_fish$runs.p[1], test.p)
  expect_equal(run_fish$sigma3.lo[1], lcl)
  expect_equal(run_fish$sigma3.hi[1], ucl)

  ## specifying cpue index
  run_cpue <- SSrunstest(simple, quants = "len", indexselect = 2)
  expect_match(run_cpue$Index, "SURVEY1")

  # CAAL since switching to simple_small, there is no conditional-age-at-length data.
  # run_con <- SSrunstest(simple, quants = "con")

  # expect_match(run_con$Index, "Fishery")
  # expect_match(run_con$test, "Passed")
})


#### Snapshot plots for simple model ###################################################
########################################################################################


## Testing plotting function
#### right now it only checks if a plot file is saved in the temp directory, would like to use expect_snapshot_file to actually compare new plots to the original but need to be able to save the plot object not just the dataframe from SSplotRunstest() ####

path <- file.path(tempdir(), "test_runs")
dir.create(path, showWarnings = FALSE)

## Simple
# test_that("file of simple_cpue_residruns plot exists", {
#   SSplotRunstest(simple,
#     use_png = TRUE,
#     print_plot = T,
#     subplots = "cpue",
#     indexselect = 1,
#     plotdir = path,
#     filenameprefix = "simple_"
#   )

#   expect_true(file.exists(file.path(path, "simple_residruns_SURVEY1.png")))
# })


# test_that("file of simple_len_residruns plot exists", {
#   SSplotRunstest(simple,
#     use_png = TRUE,
#     print_plot = T,
#     subplots = "len",
#     plotdir = path,
#     filenameprefix = "simple_len_"
#   )

#   expect_true(file.exists(file.path(path, "simple_len_residruns_FISHERY.png")))
# })
