#' Hindcasting Cross-Validations of Multiple Models
#'
#' Plots one-step ahead hindcasting cross-validations and computes MASE from
#' prediction residuals.`MASE` is calculated as the average ratio of mean
#' absolute error (`MAE`) of prediction residuals (`MAE.PR`) and naive
#' predictions (`MAE.base`). `MASE.adj` sets the `MAE.base` to a minimum
#' `MAE.base.adj` (`default=0.1`). `MASE.adj` allow passing (`MASE<1`) if
#' `MAE.PE < 0.1` and thus accurate if obs show very little annual variation
#'
#' @param retroSummary List created by [r4ss::SSsummarize()] or
#' [ss3diags::SSretroComps()]
#' @param subplots optional use of the following:
#' \itemize{
#'  \item `"cpue"` Index of abundance data
#'  \item `"len"` Length-composition data
#'  \item `"age"` Age-composition data
#' }
#' @param Season option to specify Season as an integer of value 1-4. Default
#' uses first available, i.e. usual Seas = 1
#' @param models Optional subset of the models of `summaryoutput` (or a similar
#' field with a different name): a list created by the function
#' [r4ss::SSsummarize]. Either `"all"` or a vector of numbers indicating
#' columns in summary tables.
#' @param endyrvec Optional single year or vector of years representing the
#' final year of values to show for each model. By default it is set to the
#' ending year specified in each model.
#' @param MAE.base.adj minimum `MASE` denominator (naive predictions) for
#' `MASE.adj` (default = 0.1)
#' @param show.mase.adj if TRUE, it show `mase.adj` in plot
#' @param indexfleets Fleet numbers for each model to compare
#' indices of abundance. Can take different forms:
#' \itemize{
#'   \item integer: (default) create a single comparison plot for the chosen
#' index
#'   \item NULL: create a separate plot for each index as long as the fleet
#' numbering is the same across all models.
#'   \item vector of length equal to number of models: a single fleet number
#' for each model to be compared in a single plot
#'   \item list: list of fleet numbers associated with indices within each
#' model to be compared, where the list elements are each a vector of the
#' same length but the names of the list elements don't matter and can be
#' absent.
#' }
#' @param xmin optional number first year shown in plot (if available)
#' @param ylim will over-write `ylimAdj` if specified
#' @param shadecol uncertainty shading of hcxval horizon
#' @param shadecol2 color for uncertainty in early years not affected by
#' hindcast
#' @param shadealpha Transparency adjustment used to make default `shadecol`.
#' (currently not used)
#' @param indexselect Vector of fleet numbers for each model for which to
#' compare
#' @param indexQlabel [Logical][base::logical] flag to add catchability to
#' legend in plot of index fits.
#' @param indexQdigits Number of significant digits for catchability in legend
#' @param indexUncertainty Show fixed uncertainty intervals on index
#' (not estimated)
#' @param mcmcVec
#' Logical vector of TRUE/FALSE values (or single value) indicating
#' whether input values are from MCMC or to use normal distribution around
#' MLE.
#'
#'
#' @inheritParams SSplotGeneric
#' @inheritParams SSplotGenericLegend
#' @inheritParams SSplotGenericPar
#'
#' @author Henning Winker (JRC-EC) and Laurence Kell (Sea++)
#'
#' @keywords ssplot hindcasting
#'
#' @importFrom grDevices grey
#' @importFrom stats qnorm qlnorm
#' @importFrom lifecycle deprecated
#'
#' @export
SSplotHCxval <- function(retroSummary,
                         subplots = c("cpue", "len", "age"),
                         Season = "default",
                         print = deprecated(),
                         print_plot = FALSE,
                         png = deprecated(),
                         use_png = print_plot,
                         pdf = deprecated(),
                         use_pdf = FALSE,
                         models = "all",
                         endyrvec = "default",
                         xmin = NULL,
                         indexselect = NULL,
                         MAE.base.adj = 0.1,
                         show.mase.adj = TRUE,
                         indexUncertainty = TRUE,
                         col = NULL,
                         pch = NULL,
                         lty = 1,
                         lwd = 5,
                         pt.cex = 3,
                         tickEndYr = TRUE,
                         xlim = "default",
                         ylimAdj = 1.05,
                         ylim = NULL,
                         xaxs = "i",
                         yaxs = "i",
                         xylabs = TRUE,
                         type = "o",
                         legend = TRUE,
                         legendlabels = "default",
                         legendloc = "topright",
                         legendorder = "default",
                         legendncol = 1,
                         legendcex = 1,
                         legendsp = 1,
                         legendindex = NULL,
                         pwidth = 6.5,
                         pheight = 5.0,
                         punits = "in",
                         res = 300,
                         ptsize = 12,
                         cex.main = 1,
                         plotdir = NULL,
                         filenameprefix = "",
                         par = list(mar = c(5, 4, 1, 1) + .1, family = "sans"),
                         verbose = TRUE,
                         shadecol = grey(0.5, 0.4),
                         shadecol2 = grey(0.5, 0.8),
                         shadealpha = 0.3,
                         new = TRUE,
                         add = TRUE,
                         mcmcVec = FALSE,
                         indexQlabel = TRUE,
                         indexQdigits = 4,
                         indexfleets = 1,
                         plot = TRUE) { # plot different fits to a single index of abundance

  # Parameter DEPRECATION checks
  if (lifecycle::is_present(print)) {
    lifecycle::deprecate_warn("2.0.0", "SSplotHCxval(print)", "SSplotHCxcval(print_plot)")
    print_plot <- print
  }

  if (lifecycle::is_present(png)) {
    lifecycle::deprecate_warn("2.0.0", "SSplotHCxval(png)", "SSplotHCxcval(use_png)")
    use_png <- png
  }

  if (lifecycle::is_present(pdf)) {
    lifecycle::deprecate_warn("2.0.0", "SSplotHCxval(pdf)", "SSplotHCxval(use_pdf)")
    use_pdf <- pdf
  }

  if (!isTRUE(plot)) {
    lifecycle::deprecate_warn(
      when = "2.0.0",
      what = "SSplotHCxval(plot)",
      details = "The ability to explictly disable plot windows or plot subplots is unused and will be defunct in a future version"
    )
  }

  if (!isTRUE(new)) {
    lifecycle::deprecate_warn(
      when = "2.0.0",
      what = "SSplotHCxval(new)",
      details = "The ability to explicitly disable new plot windows is unused and will be defunct in a future version"
    )
  }



  #------------------------------------------
  # r4ss plotting functions
  #------------------------------------------
  # subfunction to write png files
  if (!add) graphics.off()
  plot <- TRUE
  # save_png <- function(file) {
  # if extra text requested, add it before extention in file name
  # file <- paste0(filenameprefix, file)
  # open png file
  #  png(
  #   filename = file.path(plotdir, file),
  #   width = pwidth, height = pheight, units = punits, res = res, pointsize = ptsize
  # )
  # change graphics parameters to input value
  # par(par)
  # }



  if (is.null(retroSummary[["indices"]]) & subplots[1] == "cpue") {
    stop("Require input object from r4ss::SSsummarize()")
  }

  if (subplots[1] %in% c("len", "age")) {
    if (is.null(retroSummary[["age"]]) & is.null(retroSummary[["len"]])) {
      stop("Require input object from ss3diags::SSdiagsComps")
    }
  }

  if (subplots[1] == "len") {
    if (is.null(retroSummary[["len"]])) stop("No Length Comps found")
    retroSummary[["indices"]] <- retroSummary[["len"]]
  }

  if (subplots[1] == "age") {
    if (is.null(retroSummary[["age"]])) stop("No Age Comps found")
    retroSummary[["indices"]] <- retroSummary[["age"]]
  }

  # subset if indexselect is specified
  if (is.null(indexselect) == F & is.numeric(indexselect)) {
    iname <- unique(retroSummary[["indices"]][["Fleet_name"]])[indexselect]
    if (TRUE %in% is.na(iname)) stop("One or more index numbers exceed number of available indices")
    retroSummary[["indices"]] <- retroSummary[["indices"]][retroSummary[["indices"]][["Fleet_name"]] %in% iname, ]
  }



  if (is.null(legendindex)) legendindex <- 1:retroSummary[["n"]]
  if (!legend) legendindex <- 10000


  plot_hcxval <- function(indexfleets = 1) {
    if (use_png) print_plot <- TRUE
    if (use_png & is.null(plotdir)) {
      stop("to print PNG files, you must supply a directory as 'plotdir'")
    }

    # check for internal consistency
    if (use_pdf & use_png) {
      stop("To use 'use_pdf', set 'print_plot' or 'use_png' to FALSE.")
    }
    if (use_pdf) {
      if (is.null(plotdir)) {
        stop("to write to a PDF, you must supply a directory as 'plotdir'")
      }
      pdffile <- file.path(
        plotdir,
        paste0(
          filenameprefix, "SSplotComparisons_",
          format(Sys.time(), "%d-%b-%Y_%H.%M"), ".pdf"
        )
      )
      pdf(file = pdffile, width = pwidth, height = pheight)
      if (verbose) cat("PDF file with plots will be:", pdffile, "\n")
      par(par)
    }

    labels <- c(
      "Year", # 1
      "Index", # 2
      "Log index"
    ) # 3

    if (subplots[1] == "len") labels[2] <- "Mean length"
    if (subplots[1] == "age") labels[2] <- "Mean age"


    #-------------------------------------------------------------
    # plot_hcxal function
    #-------------------------------------------------------------
    # get stuff from summary output (minimized)
    n <- retroSummary[["n"]]
    startyrs <- retroSummary[["startyrs"]]
    endyrs <- retroSummary[["endyrs"]]
    indices <- retroSummary[["indices"]]

    if (models[1] == "all") models <- 1:n
    nlines <- length(models)

    if (endyrvec[1] == "default") {
      endyrvec <- endyrs - seq(0, n - 1, 1)
    }
    if (length(endyrvec) == 1) {
      stop("SSplotHCxval requires a minimum of one reference and one retro peel")
    }
    # check length of indexfleets
    if (!is.null(indexfleets) && length(indexfleets) < n) {
      if (length(indexfleets) == 1) {
        indexfleets <- rep(indexfleets, n)
      } else {
        warning(
          "'indexfleets' needs to have length either 1 or n=", n, "\n",
          "with each value a fleet number for the index to compare.\n"
        )
        indexfleets <- NULL
      }
    }
    # setup colors, points, and line types
    if (is.null(col) & nlines > 3) col <- r4ss::rich.colors.short(nlines + 1)[-1]
    if (is.null(col) & nlines < 3) col <- r4ss::rich.colors.short(nlines)
    if (is.null(col) & nlines == 3) col <- c("blue", "red", "green3")
    if (is.null(shadecol)) {
      # new approach thanks to Trevor Branch
      shadecol <- adjustcolor(col, alpha.f = shadealpha)
    }
    # set pch values if no input
    if (is.null(pch)) {
      pch <- rep(1:25, 10)[1:nlines]
    } else {
      pch <- rep(pch[1], 1000)[1:nlines]
    }

    # if line stuff is shorter than number of lines, recycle as needed
    if (length(col) < nlines) col <- rep(col, nlines)[1:nlines]
    if (length(pch) < nlines) pch <- rep(pch, nlines)[1:nlines]
    if (length(lty) < nlines) lty <- rep(lty, nlines)[1:nlines]
    if (length(lwd) < nlines) lwd <- rep(lwd, nlines)[1:nlines]

    if (!is.expression(legendlabels[1]) &&
      legendlabels[1] == "default") {
      legendlabels <- c("Reference", paste(endyrvec)[-1])
    }
    if (legendorder[1] == "default") legendorder <- 1:nlines

    # open new window if requested
    if (plot & use_png == FALSE) {
      if (!add) dev.new(width = pwidth, height = pheight, pointsize = ptsize, record = TRUE)
    } else {
      if (!add) par(par)
    }

    # Exclude all Time steps not use in reference run replist1
    if (subplots[1] %in% c("len", "age")) {
      indices[["Use"]] <- ifelse(is.na(indices[["Like"]]), -1, 1)
    }
    RefUse <- indices[indices[["imodel"]] == 1 & indices[["Use"]] == 1, ]
    RefUse <- paste0(RefUse[["Fleet_name"]], ".", RefUse[["Time"]])
    indices <- indices[paste0(indices[["Fleet_name"]], ".", indices[["Time"]]) %in% RefUse, ]

    indices2 <- NULL
    for (iline in 1:nlines) {
      imodel <- models[iline]
      subset1 <- indices[["imodel"]] == imodel & !is.na(indices[["Like"]]) & indices[["Use"]] == 1
      subset2 <- indices[["imodel"]] == imodel # & indices[["Use"]] == 1 #><>
      if (length(unique(indices[["Fleet"]][subset1])) > 1) {
        if (!is.null(indexfleets[imodel])) {
          ifleet <- indexfleets[imodel]
          indices2 <- rbind(indices2, indices[subset2 & indices[["Fleet"]] == ifleet, ])
        } else {
          if (verbose) {
            cat(
              "some models have multiple indices, 'indexfleets' required\n",
              "to compare fits to indices.\n"
            )
          }
          return()
        }
      } else {
        indices2 <- rbind(indices2, indices[subset2, ])
      }
    }


    # Subset by month
    if (Season == "default") {
      Season <- unique(indices2[["Seas"]])[1]
      if (verbose & length(unique(indices2[["Seas"]])) > 1) {
        cat("Taking Season", Season, "by default for Index", unique(indices2[["Fleet_name"]]))
      }
    } else {
      if (as.integer(Season) < 1 | as.integer(Season) > 4) stop("Season must be an integer between 1 and 4")
      if (is.na(Season)) stop("Season must be 'default' or an integer value of a season included in the index data (ie 1,2,3,4).")
      Season <- as.numeric(Season)[1]
    }

    indices <- indices[indices[["Seas"]] == Season, ]
    indices2 <- indices2[indices2[["Seas"]] == Season, ]


    # get quantities for plot
    yr <- indices2[["Yr"]]
    obs <- indices2[["Obs"]]
    exp <- indices2[["Exp"]]
    imodel <- indices2[["imodel"]]
    Q <- indices2[["Calc_Q"]]

    ylab <- labels[2]

    # get uncertainty intervals if requested
    if (indexUncertainty) {
      subset <- indices2[["imodel"]] == models[1] & indices2[["Use"]] == 1
      indexSEvec <- indices2[["SE"]][subset]
      y <- obs[subset]
      upper <- qlnorm(.975, meanlog = log(y), sdlog = indexSEvec)
      lower <- qlnorm(.025, meanlog = log(y), sdlog = indexSEvec)
    } else {
      upper <- NULL
      lower <- NULL
    }


    if (is.null(xmin)) {
      xmin <- min(endyrvec) - 5
    } else {
      xmin <- min(xmin, min(endyrvec) - 3)
    }

    meanQ <- rep(NA, nlines)
    imodel <- models[which(endyrvec == max(endyrvec))[1]]
    subset <- indices2[["imodel"]] == imodel & !is.na(indices2[["Like"]]) & yr >= xmin


    ### make plot of index fits
    # calculate ylim (excluding dummy observations from observed but not expected)
    sub <- !is.na(indices2[["Like"]]) & yr >= xmin

    if (is.null(ylim)) {
      +1

      ylim <- ylimAdj * range(c(exp[sub], obs[sub], lower[sub], upper[sub]), na.rm = TRUE)
      # if no values included in subset, then set ylim based on all values
      if (!any(sub)) {
        ylim <- ylimAdj * range(exp, obs, lower, upper, na.rm = TRUE)
      }

      # 0 included if not in log space
      ylim <- range(0, ylim * 1.1, na.rm = T)
    }


    # hcxval section
    yr.eval <- c(endyrvec)
    yr.eval <- (sort(yr.eval))
    yr.obs <- yr.eval %in% yr
    pe.eval <- which(yr.eval %in% yr)[-1]

    if (length(which(yr.eval %in% yr)) - length(pe.eval) < 1) {
      pe.eval <- pe.eval[-1]
    }
    npe <- length(pe.eval) # number of prection errors
    obs.eval <- rep(NA, length(yr.eval))
    obs.eval[yr.eval %in% yr] <- obs[subset][yr[subset] %in% yr.eval]
    nhc <- length(endyrvec) - 1
    # naive.eval = log(obs.eval[1:nhc])-log(obs.eval[2:(nhc+1)]) # add log for v1.1
    # npe <- length(naive.eval[is.na(naive.eval)==F])  # number of prection errors
    # scaler = mean(abs(naive.eval[is.na(naive.eval)==F]))


    if (length(endyrvec[yr %in% endyrvec]) > 0 & length(which(yr.eval %in% yr)) > 1) { # ><>
      if (verbose) {
        cat(paste(
          "\n", "Computing MASE with", ifelse(npe < (length(endyrvec) - 1), "only", "all"),
          npe, "of", length(endyrvec) - 1, " prediction residuals for Index", indices2[["Fleet_name"]][1]
        ), "\n")
      }
      if (verbose & npe < (length(endyrvec) - 1)) cat(paste("\n", "Warning:  Unequal spacing of naive predictions residuals may influence the interpretation of MASE", "\n"))

      plot(0,
        type = "n", xlim = c(max(min(yr), xmin), min(c(max(yr), max(endyrvec)))), yaxs = yaxs,
        ylim = ylim, xlab = ifelse(xylabs, "Year", ""), ylab = ifelse(xylabs, ylab, ""), axes = FALSE
      )

      if (yaxs != "i") {
        abline(h = 0, col = "grey")
      }
      Qtext <- rep("(Q =", nlines)



      if (indexUncertainty) {
        polygon(c(yr[subset], rev(yr[subset])), c(lower[subset], rev(upper[subset])), col = shadecol, border = shadecol)
        polygon(c(yr[subset & yr <= min(endyrvec)], rev(yr[subset & yr <= min(endyrvec)])), c(lower[subset & yr <= min(endyrvec)], rev(upper[subset & yr <= min(endyrvec)])), col = shadecol2, border = shadecol2)
      }

      lines(yr[subset], obs[subset], pch = 21, lty = 2, col = "grey15", lwd = lwd)
      points(yr[subset], obs[subset], pch = 24, cex = pt.cex, bg = "white") 
      points(yr.eval[pe.eval], obs.eval[pe.eval], pch = 24, cex = pt.cex, bg = (rev(col))[pe.eval - 1])

      # Plot Reference
      index.i <- unique(indices2[["Fleet_name"]])
      x.ref <- indices[indices[["imodel"]] == imodel & indices[["Yr"]] >= xmin & indices[["Fleet_name"]] == index.i, ][["Yr"]]
      y.ref <- indices[indices[["imodel"]] == imodel & indices[["Yr"]] >= xmin & indices[["Fleet_name"]] == index.i, ][["Exp"]]
      lines(x.ref, y.ref, col = col[1], lwd = lwd, lty = 1, type = type, pch = 16)
      pred.resid <- NULL # Note Prediction Residuals
      for (iline in (2:nlines)[!mcmcVec]) {
        imodel <- models[iline]
        subset <- indices2[["imodel"]] == imodel & yr <= endyrvec[iline] + 1 & yr >= xmin
        subset.ref <- indices2[["imodel"]] == imodel

        if (endyrvec[iline - 1] %in% yr) {
          x <- yr[subset]
          y <- exp[subset]
          yobs <- obs[subset]
          pred.resid <- c(pred.resid, log(y[length(x)]) - log(yobs[length(x)])) # add log() for v1.1


          lines(x[1:(length(x) - 1)], y[1:(length(x) - 1)], 
            lwd = lwd[iline], 
            lty = lty[iline], col = col[iline], type = "l", cex = 0.9
          )

          lines(x[(length(x) - 1):(length(x))], y[(length(x) - 1):(length(x))],
            lwd = lwd[iline],
            col = col[iline], lty = 2
          )

          points(x[length(x)], y[length(y)],
            pch = 21,
            bg = col[iline], col = 1, type = "p", cex = pt.cex
          )
        }
      } # end for

      maepr <- mean(abs(pred.resid))
      # nhc = length(endyrvec)-1
      # naive.eval = log(obs.eval[1:nhc])-log(obs.eval[2:(nhc+1)]) # add log for v1.1
      # npe <- length(naive.eval[is.na(naive.eval)==F])  # number of prection errors
      naive.eval <- log(obs.eval[pe.eval]) - log(obs.eval[is.na(obs.eval) == F][-(npe + 1)])
      scaler <- mean(abs(naive.eval))

      mase <- maepr / scaler
      mase.adj <- maepr / (max(scaler, MAE.base.adj))

      MASE.i <- NULL
      MASE.i <- data.frame(Index = unique(indices2[["Fleet_name"]])[1], Season = Season, MASE = mase, MAE.PR = maepr, MAE.base = scaler, MASE.adj = mase.adj, n.eval = npe)

      # legendlabels <- c("Ref",rev(yr.eval))
      if (indexQlabel) {
        legendlabels2 <- paste(
          legendlabels, Qtext,
          format(meanQ, digits = indexQdigits), ")"
        )
      }
      if (legend) {
        # add legend if requested

        r4ss::add_legend(legendlabels,
          legendloc = legendloc,
          legendcex = legendcex,
          legendsp = legendsp,
          legendncol = legendncol,
          legendorder = legendorder,
          pch = pch, col = col, lty = lty,
          lwd = lwd,
          type = type
        )
        legend_info <- r4ss::add_legend(legendlabels,
          legendloc = legendloc,
          legendcex = legendcex,
          legendsp = legendsp,
          legendncol = legendncol,
          legendorder = legendorder,
          pch = pch, col = col, lty = lty,
          lwd = lwd,
          type = type
        )
        # Add a standard R legend just for the CI box
        legend_coords <- legend_info$rect
        ci_legend_x <- legend_coords$left
        ci_legend_y <- legend_coords$top - legend_coords$h

        if(uncertainty == TRUE){
          legend(x = ci_legend_x, y = ci_legend_y, 
            legend = c("Observed", "Expected", "95% CI of years \nwith all data", "95% CI of years \nwith data removed"),
            pch = c(24, 16, 15, 15), 
            bg = c("white", NA, shadecol2, shadecol), 
            col = c("black", "black", shadecol2, shadecol), 
            pt.cex = 2,
            bty = "n")
        }else{
          legend(x = ci_legend_x, y = ci_legend_y, 
            legend = c("Observed", "Expected"),
            pch = c(24, 16), 
            bg = c("white", NA), 
            col = c("black", "black"), 
            pt.cex = 2,
            bty = "n")          
        }

      }
      if (mase == mase.adj | show.mase.adj == FALSE) legend("top", paste0(unique(indices2[["Fleet_name"]])[1], ifelse(length(unique(retroSummary[["indices"]][["Seas"]])) > 1, paste0(".S", Season), ""), ": MASE = ", round(mase, 2)), bty = "n", y.intersp = -0.2, cex = legendcex + 0.1)
      if (mase.adj < mase & show.mase.adj == TRUE) legend("top", paste0(unique(indices2[["Fleet_name"]])[1], ifelse(length(unique(retroSummary[["indices"]][["Seas"]])) > 1, paste0(".S", Season), ""), ": MASE = ", round(mase, 2), " (", round(mase.adj, 2), ")"), bty = "n", y.intersp = -0.2, cex = legendcex + 0.1)


      axis(1, at = c(max(xmin, min(yr)):max(endyrvec)))
      if (tickEndYr) axis(1, at = max(endyrvec))

      axis(2)
      box()
    } else {
      if (verbose) cat(paste0("\n", "No observations in evaluation years to compute prediction residuals for Index ", indices2[["Fleet_name"]][1]), "\n")
      MASE.i <- NULL
      MASE.i <- data.frame(Index = unique(indices2[["Fleet_name"]])[1], Season = Season, MASE = NA, MAE.PR = NA, MAE.base = NA, MASE.adj = NA, n.eval = 0)
      if (use_png == FALSE & add == FALSE) dev.off()
    }
    return(list(MASE = MASE.i))
  } # End of plot_hcxval function
  #------------------------------------------------------------

  if (verbose) cat("Plotting Hindcast Cross-Validation (one-step-ahead) \n")
  if (plot) {
    # LOOP through fleets
    nfleets <- length(unique(retroSummary[["indices"]][["Fleet"]]))
    if (print_plot) {
      MASE <- NULL
      for (fi in 1:nfleets) {
        legend <- F
        if (fi %in% legendindex) legend <- TRUE
        indexfleets <- unique(retroSummary[["indices"]][["Fleet"]])[fi]
        # save_png(paste0("hcxval_", unique(retroSummary[["indices"]][["Fleet"]])[fi], ".png", sep = ""))

        plotinfo <- NULL
        r4ss::save_png(
          plotinfo = plotinfo,
          file = paste0("hcxval_", unique(retroSummary[["indices"]][["Fleet"]])[fi], ".png", sep = ""),
          plotdir = plotdir,
          pwidth = pwidth,
          pheight = pheight,
          punits = punits,
          res = res,
          ptsize = ptsize,
          filenameprefix = filenameprefix
        )
        par(par)
        get_mase <- plot_hcxval(indexfleets)[["MASE"]]
        dev.off()
        MASE <- rbind(MASE, get_mase)
      } # End of Fleet Loop
    }


    MASE <- NULL
    for (fi in 1:nfleets) {
      legend <- F
      if (fi %in% legendindex) legend <- TRUE
      indexfleets <- unique(retroSummary[["indices"]][["Fleet"]])[fi]
      if (!add) (par)
      get_mase <- plot_hcxval(indexfleets)[["MASE"]]
      MASE <- rbind(MASE, get_mase)
    } # End of Fleet Loop
  }


  if (verbose) cat(paste0("\n", "MASE stats by Index:", "\n"))
  return(MASE)
} # end of SSplotHCxal()
#-----------------------------------------------------------------------------------------
