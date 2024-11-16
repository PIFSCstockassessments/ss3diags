#' Model ensemble plots
#'
#' Plots model ensembles and forecasts with uncertainty represented by MVLN or
#' MCMC posteriors
#'
#' @param kb `kb` type output created by `SSdeltaMVLN()`
#' @param subplots vector to create subplots with these options:
#' \itemize{
#'  \item `"stock"` Fish Population
#'  \item `"harvest"` Harvest Rate
#'  \item `"SSB"` Spawning Stock Biomass
#'  \item `"F"` Fishing Mortality
#'  \item `"Recr"` Recruitment
#'  \item `"Catch"` Total Catch
#' }
#' @param models option to manually subset the models in `kb[["run"]]`
#' @param quantiles quantiles for uncertainty in plots. Input as a list,
#' default is the 95TH percentile: `list(c(0.025, 0.975))`
#' @param ylabs y-axis labels for quants
#' @param shadealpha Transparency adjustment used to make uncertainty regions,
#' default is 0.3
#' @param indexQlabel TRUE/FALSE include labels for indices. Default is TRUE
#' (currently not used)
#' @param indexQdigits Number of significant digits for catchability in legend.
#' Default is 4
#' @param endyrvec Optional single year or vector of years representing the
#' final year of values to show for each model. By `"default"` it is set to the
#' ending year specified in each model.
#'
#' @importFrom grDevices graphics.off rgb adjustcolor dev.new dev.off
#' @importFrom graphics polygon abline axis box
#' @importFrom stats dnorm
#' @importFrom lifecycle deprecated
#'
#' @inheritParams SSplotGeneric
#' @inheritParams SSplotGenericLegend
#' @inheritParams SSplotGenericPar
#' @inheritParams SSplotGenericUncertainty
#'
#' @author Mostly adopted from r4ss::SSplotComparisons by Taylor et al
#'
#' @examples
#' \dontrun{
#'
#' mvln <- SSdeltaMVLN(simple, run = "Simple")
#' sspar(mfrow = c(3, 2), plot.cex = 0.7)
#' SSplotEnsemble(mvln[["kb"]], ylabs = mvln[["labels"]], add = T, verbose = F)
#' }
#' @keywords ssplot ensemble
#'
#' @export
SSplotEnsemble <- function(kb,
                           subplots = c("stock", "harvest", "SSB", "F", "Recr", "Catch"),
                           models = "all",
                           quantiles = list(c(0.025, 0.975)),
                           ylabs = NULL,
                           endyrvec = "default",
                           plot = TRUE,
                           print = deprecated(),
                           print_plot = FALSE,
                           png = deprecated(),
                           use_png = print_plot,
                           pdf = FALSE,
                           use_pdf = FALSE,
                           col = NULL,
                           pch = NULL,
                           lty = 1,
                           lwd = 2,
                           tickEndYr = FALSE,
                           xlim = NULL,
                           ylimAdj = 1.05,
                           xaxs = "i",
                           yaxs = "i",
                           xylabs = TRUE,
                           type = "l",
                           uncertainty = TRUE,
                           legend = TRUE,
                           legendlabels = "default",
                           legendloc = "topright",
                           legendorder = "default",
                           legendncol = 1,
                           legendcex = 1,
                           legendsp = 0.9,
                           pwidth = 6.5,
                           pheight = 5.0,
                           punits = "in",
                           res = 300,
                           ptsize = 10,
                           cex.main = 1,
                           plotdir = NULL,
                           filenameprefix = "",
                           par = list(mar = c(5, 4, 1, 1) + .1),
                           verbose = TRUE,
                           shadealpha = 0.3,
                           new = TRUE,
                           add = FALSE,
                           mcmcVec = FALSE,
                           indexQlabel = TRUE,
                           indexQdigits = 4,
                           legendindex = NULL) {
  # plot different fits to a single index of abundance

  # Parameter DEPRECATION checks
  if (lifecycle::is_present(print)) {
    lifecycle::deprecate_warn("2.0.0", "SSplotEnsemble(print)", "SSplotEnsemble(print_plot)")
    print_plot <- print
  }

  if (lifecycle::is_present(png)) {
    lifecycle::deprecate_warn("2.0.0", "SSplotEnsemble(png)", "SSplotEnsemble(use_png)")
    use_png <- png
  }

  if (lifecycle::is_present(pdf)) {
    lifecycle::deprecate_warn("2.0.0", "SSplotEnsemble(pdf)", "SSplotEnsemble(use_pdf)")
    use_pdf <- pdf
  }

  if (!isTRUE(plot)) {
    lifecycle::deprecate_warn(
      when = "2.0.0",
      what = "SSplotEnsemble(plot)",
      details = "The ability to explictly disable plot windows or plot subplots is unused and will be defunct in a future version"
    )
  }

  if (!isTRUE(new)) {
    lifecycle::deprecate_warn(
      when = "2.0.0",
      what = "SSplotEnsemble(new)",
      details = "The ability to explicitly disable new plot windows is unused and will be defunct in a future version"
    )
  }

  #------------------------------------------
  # r4ss plotting functions
  #------------------------------------------
  # subfunction to write png files
  if (!add) graphics.off()
  if (add) {
    print_plot <- F
    use_png <- F
  }

  if (is.null(ylabs)) {
    ylab.default <- TRUE
    ylabs <- c(expression(SSB / SSB[MSY]), expression(F / F[MSY]), "SSB (t)", "Fishing Mortality (F)", "Recruits ('000s)", "Catch (t)")
  } else {
    ylab.default <- FALSE
  }


  refquants <- c("stock", "harvest", "SSB", "F", "Recr", "Catch")

  # Check time line
  minyr <- max(aggregate(year ~ run, kb, min)[, 2])
  maxyr <- min(aggregate(year ~ run, kb, max)[, 2])
  kb <- kb[kb[["year"]] >= minyr & kb[["year"]] <= maxyr, ]

  quants <- subplots



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
    if (verbose) {
      message("PDF file with plots will be:", pdffile)
    }
    par(par)
  }


  plot_quants <- function(quant = "SSB") {
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
      if (verbose) {
        message("PDF file with plots will be:", pdffile, "\n")
      }
      par(par)
    }



    #-------------------------------------------------------------
    # plot function

    # get stuff from summary output (minimized)
    n <- length(unique(kb[["run"]]))
    startyrs <- min(kb[["year"]])
    endyrs <- max(kb[["year"]])
    years <- unique(kb[["year"]])
    y <- kb[, quant]
    run <- kb[["run"]]
    year <- kb[["year"]]


    exp <- list()
    n.quantiles <- length(quantiles)
    for (i in 1:n.quantiles) {
      exp[[i]] <- aggregate(y ~ year + run, kb, mean)
      exp[[i]][["lower"]] <- aggregate(y ~ year + run, kb, quantile, quantiles[[i]][1])[["y"]]
      exp[[i]][["upper"]] <- aggregate(y ~ year + run, kb, quantile, quantiles[[i]][2])[["y"]]
    }


    # exp[["Yr"]] <- exp[["year"]]
    # lower[["Yr"]] <- lower[["year"]]
    # upper[["Yr"]] <- upper[["year"]]

    if (models[1] == "all") models <- 1:n
    nlines <- length(models)
    runs <- unique(kb[["run"]])[models]


    # setup colors, transparency, points, and line types
    if (n.quantiles == 1) shadealpha <- 0.3
    if (n.quantiles > 1) shadealpha <- seq(0.3, 0.05, by = -0.05) # maxes out at 6 quantiles

    if (is.null(col)) col <- r4ss::rich.colors.short(nlines, alpha = shadealpha) # [,-1]
    #-1 removes first column bc that color is black


    # if (is.null(col) & nlines < 3) col <- c("blue", "green4")
    # if (is.null(col) & nlines == 3) col <- c("blue", "red", "green4")


    ## This section removed bc transparency is done with rich colors short function now
    # if (is.null(shadecol)) {
    #   # new approach thanks to Trevor Branch
    #     shadecol[i] <- adjustcolor(col, alpha.f = shadealpha[i])
    # }

    # if line stuff is shorter than number of lines, recycle as needed
    if (length(col) < nlines) col <- rep(col, nlines)[1:nlines]
    if (length(pch) < nlines) pch <- rep(pch, nlines)[1:nlines]
    if (length(lty) < nlines) lty <- rep(lty, nlines)[1:nlines]
    if (length(lwd) < nlines) lwd <- rep(lwd, nlines)[1:nlines]

    if (!is.expression(legendlabels[1]) &&
      legendlabels[1] == "default") {
      legendlabels <- runs
    }
    if (legendorder[1] == "default") legendorder <- 1:(nlines)

    # open new window if requested
    if (plot & use_png == FALSE) {
      if (!add) dev.new(width = pwidth, height = pheight, pointsize = ptsize, record = TRUE)
    } else {
      if (!add) par(par)
    }

    # yr <- years
    full.exp <- do.call(rbind, exp)
    if (is.null(xlim)) xlim <- c(max(min(full.exp[["year"]])), max(years))
    xmin <- min(xlim)

    ylim <- c(0, max(ifelse(uncertainty,
      max(full.exp[["upper"]][full.exp[["year"]] >= xmin]) * ylimAdj,
      ylimAdj * max(full.exp[["upper"]][full.exp[["year"]] >= xmin]) * 1.05
    )))


    if (ylab.default) {
      ylab <- ylabs[which(refquants %in% quant)]
    } else {
      ylab <- ylabs[which(subplots %in% quant)]
    }


    plot(0,
      type = "n", xlim = xlim, yaxs = yaxs,
      ylim = ylim, xlab = ifelse(xylabs, "Year", ""), ylab = ifelse(xylabs, ylab, ""), axes = FALSE
    )

    if (uncertainty & quant != "Catch") {
      for (iline in nlines:1) {
        for (q in 1:n.quantiles) {
          if (quant %in% c("SSB", "stock", "harvest", "F")) {
            polygon(
              x = c(years, rev(years)),
              y = c(exp[[q]][["lower"]][exp[[q]][["run"]] == runs[iline]], rev(exp[[q]][["upper"]][exp[[q]][["run"]] == runs[iline]])),
              col = col[iline], border = col[iline]
            )
          } else {
            adj <- 0.2 * iline / nlines - 0.1
            arrows(
              x0 = exp[[q]][["year"]] + adj, y0 = exp[[q]][["lower"]][exp[[q]][["run"]] == runs[iline]],
              x1 = exp[[q]][["year"]] + adj, y1 = exp[[q]][["upper"]][exp[[q]][["run"]] == runs[iline]],
              length = 0.02, angle = 90, code = 3, col = col[iline]
            )
          }
        }
      }
    }

    for (iline in 1:nlines) {
      for (q in 1:n.quantiles) {
        if (quant %in% c("SSB", "stock", "harvest", "F", "Catch")) {
          lines(years, exp[[q]][["y"]][exp[[q]][["run"]] == runs[iline]], col = col[iline], pch = pch[iline], lty = lty[iline], lwd = lwd[iline], type = "l")
        } else {
          points(years, exp[[q]][["y"]][exp[[q]][["run"]] == runs[iline]], col = col[iline], pch = 16, cex = 0.8)
        }
      }
    }
    if (quant == "stock") abline(h = 1, lty = 2)
    if (quant == "harvest") abline(h = 1, lty = 2)

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
    }

    # axis(1, at=c(min(xmin,min(yr)):max(endyrvec)))
    axis(1)

    if (tickEndYr) axis(1, at = max(endyrvec))

    axis(2)
    box()
  } # End of plot_quant function

  legend.temp <- legend

  # Do plotting
  if (plot) {
    # subplots
    for (s in 1:length(subplots)) {
      if (print_plot) {
        quant <- subplots[s]
        par(par)




        plotinfo <- NULL
        r4ss::save_png(
          plotinfo = plotinfo,
          file = paste0("ModelComp_", quant, ".png", sep = ""),
          plotdir = plotdir,
          pwidth = pwidth,
          pheight = pheight,
          punits = punits,
          res = res,
          ptsize = ptsize,
          filenameprefix = filenameprefix
        )
        plot_quants(quant)
        dev.off()
      }
    }
    # subplots
    for (s in 1:length(subplots)) {
      if (verbose) {
        message("Plot Comparison of ", subplots[s])
      }
      if (!add) par(par)
      quant <- subplots[s]
      plot_quants(quant)
    }
  } # endplot
} # end of SSplotEnsemble()
#-----------------------------------------------------------------------------------------

#' Plot Indices
#'
#' function to plot different fits to a single index of abundance
#'
#' @param summaryoutput summaryoutput
#' @param varlist variable list
#' @param indexfleets Fleet vector index
#' @param verbose Option to output messages to Rconsole
#' @param legendloc Location of legend. Either a string like "topleft" or a vector
#' of two numeric values representing the fraction of the maximum in the x and y
#' dimensions, respectively. See ?legend for more info on the string options.
#' @param legendcex Allows to adjust legend cex
#' @param legendsp Space between legend labels
#' @param legendncol Number of columns for the legend.
#' @param type Type parameter passed to points (default 'o' overplots points on
#' top of lines)
#'
#' @keywords internal ssplot
#'
#' @importFrom grDevices png
#'
ensemble_plot_index <- function(summaryoutput,
                                varlist,
                                indexfleets = 1,
                                verbose = TRUE,
                                legendloc = "topright",
                                legendcex = 1,
                                legendsp = 0.9,
                                legendncol = 1,
                                type = "l") {
  # subfunction to add legend
  # add_legend <- function(legendlabels, cumulative = FALSE) {
  # if (cumulative) {
  # legendloc <- "topleft"
  # }
  # if (is.numeric(legendloc)) {
  # Usr <- par()[["usr"]]
  # legendloc <- list(
  #   x = Usr[1] + legendloc[1] * (Usr[2] - Usr[1]),
  #   y = Usr[3] + legendloc[2] * (Usr[4] - Usr[3])
  # )
  # }

  # if type input is "l" then turn off points on top of lines in legend
  # legend.pch <- pch
  # if (type == "l") {
  #  legend.pch <- rep(NA, length(pch))
  # }
  # legend(legendloc,
  #  legend = legendlabels[legendorder],
  #  col = col[legendorder], lty = lty[legendorder], seg.len = 2,
  #  lwd = lwd[legendorder], pch = legend.pch[legendorder], bty = "n", ncol = legendncol, pt.cex = 0.7, cex = legendcex, y.intersp = legendsp
  # )
  # }

  # r4ss Colors
  # rc <- function(n, alpha = 1) {
  # a subset of rich.colors by Arni Magnusson from the gregmisc package
  # a.k.a. rich.colors.short, but put directly in this function
  # to try to diagnose problem with transparency on one computer
  # x <- seq(0, 1, length = n)
  # r <- 1 / (1 + exp(20 - 35 * x))
  # g <- pmin(pmax(0, -0.8 + 6 * x - 5 * x^2), 1)
  # b <- dnorm(x, 0.25, 0.15) / max(dnorm(x, 0.25, 0.15))
  # rgb.m <- matrix(c(r, g, b), ncol = 3)
  # rich.vector <- apply(rgb.m, 1, function(v) rgb(v[1], v[2], v[3], alpha = alpha))
  # }

  labels <- c(
    "Year", # 1
    "Index", # 2
    "Log index"
  ) # 3

  #-------------------------------------------------------------
  # plot_index function
  #-------------------------------------------------------------
  # get stuff from summary output (minimized)
  n <- summaryoutput[["n"]]
  nsexes <- summaryoutput[["nsexes"]]
  startyrs <- summaryoutput[["startyrs"]]
  endyrs <- summaryoutput[["endyrs"]]
  indices <- summaryoutput[["indices"]]


  if (models[1] == "all") models <- 1:n
  nlines <- length(models)

  if (endyrvec[1] == "default") {
    endyrvec <- endyrs
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
  if (is.null(col) & nlines < 3) col <- c("blue", "green4")
  if (is.null(col) & nlines == 3) col <- c("blue", "red", "green4")
  if (is.null(shadecol)) {
    # new approach thanks to Trevor Branch
    shadecol <- adjustcolor(col, alpha.f = varlist[["shadealpha"]])
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
    legendlabels <- paste("model", 1:nlines)
  }
  if (legendorder[1] == "default") legendorder <- 1:nlines

  # open new window if requested
  if (varlist[["show_plot_window"]] & varlist[["use_png"]] == FALSE) {
    # "Add" param does not pass through this function, negating its check.
    dev.new(
      width = varlist[["pwidth"]],
      height = varlist[["pheight"]],
      pointsize = varlist[["ptsize"]],
      record = TRUE
    )
  } else {
    par(par) # "Add" param does not pass through this function, negating its check.
  }


  indices2 <- NULL
  for (iline in 1:nlines) {
    imodel <- models[iline]
    subset1 <- indices[["imodel"]] == imodel & !is.na(indices[["Like"]])
    subset2 <- indices[["imodel"]] == imodel
    if (length(unique(indices[["Fleet"]][subset1])) > 1) {
      if (!is.null(indexfleets[imodel])) {
        ifleet <- indexfleets[imodel]
        indices2 <- rbind(indices2, indices[subset2 & indices[["Fleet"]] == ifleet, ])
      } else {
        if (verbose) {
          # TODO: Catch as exception
          message(
            "Some models have multiple indices, 'indexfleets' required\n",
            "to compare fits to indices"
          )
        }
        return()
      }
    } else {
      indices2 <- rbind(indices2, indices[subset2, ])
    }
  }
  # get quantities for plot
  yr <- indices2[["Yr"]]
  obs <- indices2[["Obs"]]
  exp <- indices2[["Exp"]]
  imodel <- indices2[["imodel"]]
  se <- indices2[["SE"]]
  Q <- indices2[["Calc_Q"]]

  # "log" param does not pass through this function, negating its check.
  ylab <- labels[2]

  # get uncertainty intervals if requested
  # Note: Not used for Ensemble Plots -ef
  upper <- NULL
  lower <- NULL


  ### make plot of index fits
  # calculate ylim (excluding dummy observations from observed but not expected)
  sub <- !is.na(indices2[["Like"]])
  ylim <- varlist[["ylimAdj"]] * range(exp, obs[sub], lower[sub], upper[sub], na.rm = TRUE)
  # if no values included in subset, then set ylim based on all values


  if (!any(sub)) {
    ylim <- varlist[["ylimAdj"]] * range(exp, obs, lower, upper, na.rm = TRUE)
  }
  ylim <- range(0, ylim * 1.1)


  if (is.null(xmin)) {
    xmin <- min(startyrs)
  }

  meanQ <- rep(NA, nlines)


  plot(0,
    type = "n",
    xlim = c(max(min(yr), xmin), min(c(max(yr), max(endyrvec)))),
    yaxs = varlist[["yaxs"]],
    ylim = ylim,
    xlab = ifelse(varlist[["xylabs"]], "Year", ""),
    ylab = ifelse(varlist[["xylabs"]], ylab, ""),
    axes = FALSE
  )

  # "log" param does not pass through this function, negating its check.
  if (varlist[["yaxs"]] != "i") {
    abline(h = 0, col = "grey")
  }
  Qtext <- rep("(Q =", nlines)

  # Note: IndexUncertainty not used for Ensemble Plots
  for (iline in 1:nlines) {
    adj <- 0.2 * iline / nlines - 0.1
    imodel <- models[iline]
    subset <- indices2[["imodel"]] == imodel & !is.na(indices2[["Like"]]) & yr >= xmin
    subexp <- indices2[["imodel"]] == imodel & yr >= xmin
    if (iline == 1) {
      points(yr[subset], obs[subset], pch = 21, cex = 1, bg = "white")
    }
    lines(yr[subexp], exp[subexp], lwd = lwd, col = col[iline])
  }
  if (varlist[["quant_s"]] == "Bratio") abline(h = 1, lty = 2)
  # Plot Reference


  # legendlabels <- c("Ref",rev(yr.eval))
  if (varlist[["indexQlabel"]]) {
    legendlabels2 <- paste(
      legendlabels, Qtext,
      format(meanQ, digits = varlist[["indexQdigits"]]), ")"
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
  }
  legend("top", paste0(unique(indices2[["Fleet_name"]])[1]), bty = "n", y.intersp = -0.2, cex = varlist[["legendcex"]] + 0.1)




  axis(1, at = c(max(xmin, min(yr)):max(endyrvec)))
  if (varlist[["tickEndYr"]]) axis(1, at = max(endyrvec))

  axis(2)
  box()
} # End of plot_index function
