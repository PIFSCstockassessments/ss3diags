#' Compare Multiple SS Model Estimates
#'
#' A function to plot SSB, B-ratio, F, Recruits, and/or Index of Abundance fits
#' from multiple SS models. This function uses an object of multiple SS models
#' summarized with [r4ss::SSsummarize()].
#'
#' @param summaryoutput List created by [r4ss::SSsummarize()]
#' @param subplots option to "SSB","Bratio","Fvalue","Recruits","Index"
#' \itemize{
#'  \item `"SSB"` Spawning Stock Biomass
#'  \item `"Bratio"` Stock Biomass relative to biomass reference point
#'  \item `"Fvalue"` Fishing Mortality
#'  \item `"Recruits"` Age-0 Recruits
#'  \item `"Index"` Index of abundance
#'  \item `"RecDev"` Recruitment Deviations
#' }
#' @param brp option to set reference point `c("msy","btargs")`
#' @param fmsy to specify `Fvalue` as `F/Fmsy` if so in starter file setting
#' @param ylabs yaxis labels for quants
#' final year of values to show for each model. By default it is set to the
#' @param xmin NULL optional number first year shown in plot (if available)
#' @param indexselect Vector of fleet numbers for each model for which to
#' compare
#' @param indexfleets Fleet numbers for each model to compare
#' indices of abundance. Can take different forms:
#' \itemize{
#'   \item integer (default): create a single comparison plot for the chosen
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
#' @param indexUncertainty Show fixed uncertainty intervals on index
#' (not estimated)
#' @param shadecol uncertainty shading of hcxval horizon
#' @param shadealpha Transparency adjustment used to make default shadecol
#' @param indexQlabel [Logical][base::logical]. If TRUE add catchability to
#' legend in plot of index fits (currently not used)
#' @param indexQdigits Number of significant digits for catchability in legend
#' @param endyrvec Optional single year or vector of years representing the
#' final year of values to show for each model. By `"default"` it is set to the
#' ending year specified in each model.
#'
#' @author Mostly adopted from r4ss::SSplotComparisons by Taylor et al
#' @export
#'
#' @importFrom grDevices pdf
#' @importFrom lifecycle deprecated
#'
#' @inheritParams SSplotGeneric
#' @inheritParams SSplotGenericLegend
#' @inheritParams SSplotGenericPar
#' @inheritParams SSplotGenericUncertainty
#'
#' @keywords ssplot hindcasting
#'
SSplotModelcomp <- function(summaryoutput,
                            plot = TRUE,
                            print = deprecated(),
                            print_plot = FALSE,
                            png = deprecated(),
                            use_png = print_plot,
                            pdf = deprecated(),
                            use_pdf = FALSE,
                            models = "all",
                            subplots = c("SSB", "Bratio", "Fvalue", "Recruits", "Index", "RecDevs"),
                            brp = c("msy", "btargs"),
                            fmsy = TRUE,
                            ylabs = c("SSB (t)", expression(SSB / SSB[MSY]), "Fishing mortality F", "Recruits ('000s)", "Index", "Recruitment Deviations"),
                            endyrvec = "default",
                            xmin = NULL,
                            indexselect = NULL,
                            indexUncertainty = TRUE,
                            col = NULL,
                            pch = NULL,
                            lty = 1,
                            lwd = 2,
                            tickEndYr = FALSE,
                            xlim = "default", ylimAdj = 1.05,
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
                            shadecol = NULL,
                            shadealpha = 0.3,
                            new = TRUE,
                            add = FALSE,
                            mcmcVec = FALSE,
                            indexQlabel = TRUE,
                            indexQdigits = 4,
                            indexfleets = 1) {
  # plot different fits to a single index of abundance

  # Parameter DEPRECATION checks
  if (lifecycle::is_present(print)) {
    lifecycle::deprecate_warn("2.0.0", "SSplotModelcomp(print)", "SSplotModelcomp(print_plot)")
    print_plot <- print
  }

  if (lifecycle::is_present(png)) {
    lifecycle::deprecate_warn("2.0.0", "SSplotModelcomp(png)", "SSplotModelcomp(use_png)")
    use_png <- png
  }

  if (lifecycle::is_present(pdf)) {
    lifecycle::deprecate_warn("2.0.0", "SSplotModelcomp(pdf)", "SSplotModelcomp(use_pdf)")
    use_pdf <- pdf
  }

  if (!isTRUE(plot)) {
    lifecycle::deprecate_warn(
      when = "2.0.0",
      what = "SSplotModelcomp(plot)",
      details = "The ability to explictly disable plot windows or plot subplots is unused and will be defunct in a future version"
    )
  }

  if (!isTRUE(new)) {
    lifecycle::deprecate_warn(
      when = "2.0.0",
      what = "SSplotModelcomp(new)",
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
  quant <- subplots[1]
  refplots <- c("SSB", "Bratio", "Fvalue", "Recruits", "Index", "RecDevs")
  refline <- refline2 <- 1
  if (brp[1] != "msy") ylabs[2] <- expression(SSB / SSB[0])
  if (brp[1] != "msy") refline <- summaryoutput[["btargs"]][1]
  # if(fmsy) refline2  = 1
  if (fmsy) ylabs[3] <- expression(F / F[MSY])

  # save_png <- function(file) {
  # if extra text requested, add it before extention in file name
  # file <- paste0(filenameprefix, file)
  # open png file
  # png(
  # filename = file.path(plotdir, file),
  # width = pwidth,
  # height = pheight,
  # units = punits,
  # res = res,
  # pointsize = ptsize
  # )
  # change graphics parameters to input value
  # par(par)
  # }

  # subset if indexselect is specified
  if (is.null(indexselect) == F & is.numeric(indexselect)) {
    iname <- unique(summaryoutput[["indices"]][["Fleet_name"]])[indexselect]
    if (TRUE %in% is.na(iname)) {
      stop("One or more index numbers exceed number of available indices")
    }
    summaryoutput[["indices"]] <- summaryoutput[["indices"]][summaryoutput[["indices"]][["Fleet_name"]] %in% iname, ]
  }


  # NOTE: 'log scale' option is not used -ef
  if (is.null(legendindex)) legendindex <- 1:summaryoutput[["n"]]
  if (!legend) legendindex <- 10000


  if (use_png) print_plot <- TRUE
  if (use_png & is.null(plotdir)) {
    stop("to print PNG files, you must supply a directory as 'plotdir'")
  }

  # check for internal consistency
  if (use_pdf & use_png) {
    stop("To use 'pdf', set 'print_plot' or 'use_png' to FALSE.")
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


  #-------------
  # plot Index
  #-------------
  plot_index <- function(indexfleets = 1) {
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
      legendlabels <- paste("model", 1:nlines)
    }
    if (legendorder[1] == "default") legendorder <- 1:nlines

    # open new window if requested
    if (plot & use_png == FALSE) {
      if (!add) {
        dev.new(width = pwidth, height = pheight, pointsize = ptsize, record = TRUE)
      }
    } else {
      if (!add) par(par)
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
    # get quantities for plot
    yr <- indices2[["Yr"]]
    obs <- indices2[["Obs"]]
    exp <- indices2[["Exp"]]
    imodel <- indices2[["imodel"]]
    se <- indices2[["SE"]]
    Q <- indices2[["Calc_Q"]]
    ylab <- labels[2]


    # get uncertainty intervals if requested
    if (indexUncertainty) {
      indexSEvec <- indices2[["SE"]]
      upper <- qlnorm(.975, meanlog = log(obs), sdlog = indexSEvec)
      lower <- qlnorm(.025, meanlog = log(obs), sdlog = indexSEvec)
    } else {
      upper <- NULL
      lower <- NULL
    } # end indexUncertainty

    ### make plot of index fits
    # calculate ylim (excluding dummy observations from observed but not expected)
    sub <- !is.na(indices2[["Like"]])
    ylim <- ylimAdj * range(exp, obs[sub], lower[sub], upper[sub], na.rm = TRUE)
    # if no values included in subset, then set ylim based on all values


    if (!any(sub)) {
      ylim <- ylimAdj * range(exp, obs, lower, upper, na.rm = TRUE)
    }
    # 0 included if not in log space
    # NOTE: 'log scale' option is not used -ef
    ylim <- range(0, ylim * 1.1)


    if (is.null(xmin)) {
      xmin <- min(startyrs)
    }

    meanQ <- rep(NA, nlines)

    plot(0,
      type = "n",
      xlim = c(max(min(yr), xmin), min(c(max(yr), max(endyrvec)))),
      yaxs = yaxs,
      ylim = ylim,
      xlab = ifelse(xylabs, "Year", ""),
      ylab = ifelse(xylabs, ylab, ""),
      axes = FALSE
    )

    # NOTE: 'log scale' option is not used -ef
    if (yaxs != "i") {
      abline(h = 0, col = "grey")
    }
    Qtext <- rep("(Q =", nlines)


    for (iline in 1:nlines) {
      adj <- 0.2 * iline / nlines - 0.1
      imodel <- models[iline]
      subset <- indices2[["imodel"]] == imodel & !is.na(indices2[["Like"]]) & yr >= xmin
      subexp <- indices2[["imodel"]] == imodel & yr >= xmin
      if (iline == 1) {
        if (indexUncertainty) {
          arrows(
            x0 = yr[subset],
            y0 = lower[subset],
            x1 = yr[subset],
            y1 = upper[subset],
            length = 0.02,
            angle = 90,
            code = 3,
            col = 1
          )
        }
        points(yr[subset], obs[subset], pch = 21, cex = 1, bg = "white")
      }
      lines(yr[subexp], exp[subexp], lwd = lwd, col = col[iline])
    }
    if (quant == "Bratio") abline(h = 1, lty = 2)

    # Plot Reference


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
    }
    legend("top",
      paste0(unique(indices2[["Fleet_name"]])[1]),
      bty = "n",
      y.intersp = -0.2,
      cex = legendcex + 0.1
    )

    axis(1, at = c(max(xmin, min(yr)):max(endyrvec)))

    if (tickEndYr) axis(1, at = max(endyrvec))

    axis(2)
    box()
  } # End of plot_index function
  #------------------------------------------------------------

  plot_quants <- function(quant = "SSB") {
    if (use_png) print_plot <- TRUE
    if (use_png & is.null(plotdir)) {
      stop("to print PNG files, you must supply a directory as 'plotdir'")
    }

    # check for internal consistency
    if (use_pdf & use_png) {
      stop("To use 'pdf', set 'print_plot' or 'use_png' to FALSE.")
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



    #-------------------------------------------------------------
    # plot function

    # get stuff from summary output (minimized)
    n <- summaryoutput[["n"]]
    startyrs <- summaryoutput[["startyrs"]]
    endyrs <- summaryoutput[["endyrs"]]
    years <- min(startyrs):max(endyrs)
    if (quant == "SSB") {
      exp <- summaryoutput[["SpawnBio"]]
      lower <- summaryoutput[["SpawnBioLower"]]
      upper <- summaryoutput[["SpawnBioUpper"]]
    }
    if (quant == "Bratio") {
      exp <- summaryoutput[["Bratio"]]
      lower <- summaryoutput[["BratioLower"]]
      upper <- summaryoutput[["BratioUpper"]]
    }
    if (quant == "Fvalue") {
      exp <- summaryoutput[["Fvalue"]]
      lower <- summaryoutput[["FvalueLower"]]
      upper <- summaryoutput[["FvalueUpper"]]
    }
    if (quant == "Recruits") {
      exp <- summaryoutput[["recruits"]]
      lower <- summaryoutput[["recruitsLower"]]
      upper <- summaryoutput[["recruitsUpper"]]
    }
    if (quant == "RecDevs") {
      exp <- summaryoutput[["recdevs"]]
      lower <- summaryoutput[["recdevsLower"]]
      upper <- summaryoutput[["recdevsUpper"]]
      for (r in 1:(ncol(exp) - 2)) {
        exp[, r] <- ifelse(is.na(exp[, r]), 0, exp[, r])
        lower[, r] <- ifelse(is.na(lower[, r]) & is.na(exp[, r]) == F, 0.01, lower[, r])
        upper[, r] <- ifelse(is.na(upper[, r]) & is.na(exp[, r]) == F, -0.01, upper[, r])
      }

      base <- summaryoutput[["recruits"]]
      if (min(base[["Yr"]]) < min(exp[["Yr"]])) {
        base <- base[base[["Yr"]] %in% exp[["Yr"]] == FALSE, ]
        base[, 1:(ncol(base) - 2)] <- 0
        exp <- rbind(base, exp)
        lower <- rbind(base, lower)
        upper <- rbind(base, upper)
      } else {
        exp <- exp[exp[["Yr"]] >= min(base[["Yr"]]), ]
        lower <- lower[exp[["Yr"]] >= min(base[["Yr"]]), ]
        upper <- upper[exp[["Yr"]] >= min(base[["Yr"]]), ]
      }
    }
    if (models[1] == "all") models <- 1:n
    nlines <- length(models)


    if (endyrvec[1] == "default") {
      endyrvec <- endyrs
    }

    exp <- exp[exp[["Yr"]] <= max(endyrvec) & exp[["Yr"]] >= max(startyrs), ]
    lower <- lower[lower[["Yr"]] <= max(endyrvec) & lower[["Yr"]] >= max(startyrs), ]
    upper <- upper[upper[["Yr"]] <= max(endyrvec) & upper[["Yr"]] >= max(startyrs), ]


    # setup colors, points, and line types
    if (is.null(col) & nlines > 3) col <- r4ss::rich.colors.short(nlines + 1)[-1]
    if (is.null(col) & nlines < 3) col <- c("blue", "green4")
    if (is.null(col) & nlines == 3) col <- c("blue", "red", "green4")
    if (is.null(shadecol)) {
      # new approach thanks to Trevor Branch
      shadecol <- adjustcolor(col, alpha.f = shadealpha)
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
    if (legendorder[1] == "default") legendorder <- 1:(nlines)

    # open new window if requested
    if (plot & use_png == FALSE) {
      if (!add) dev.new(width = pwidth, height = pheight, pointsize = ptsize, record = TRUE)
    } else {
      if (!add) par(par)
    }


    # Check if uncertainty is measured
    if (uncertainty == TRUE & sum(exp[, 1] - lower[, 1]) == 0) {
      if (verbose) message("No uncertainty estimates available from the provided")
      uncertainty <- FALSE
    }


    ### make plot of index fits
    # calculate ylim (excluding dummy observations from observed but not expected)
    # ylim <- c(0,max(ifelse(uncertainty,unlist(upper[,1:nlines])*ylimAdj, ylimAdj*unlist(exp[,1:nlines])*1.05)))
    # if no values included in subset, then set ylim based on all values

    yr <- exp[["Yr"]]


    if (is.null(xmin)) {
      xmin <- min(yr)
    } else {
      xmin <- min(xmin, min(endyrvec) - 3)
    }

    if (quant != "RecDevs") {
      ylim <- c(0, max(ifelse(uncertainty, max(unlist(upper[upper[["Yr"]] >= xmin, 1:nlines])) * ylimAdj, ylimAdj * max(unlist(exp[exp[["Yr"]] >= xmin, 1:nlines])) * 1.05)))
    } else {
      ylims <- max(ifelse(uncertainty, max(abs(unlist(upper[upper[["Yr"]] >= xmin, 1:nlines]))) * ylimAdj, ylimAdj * max(abs(unlist(exp[exp[["Yr"]] >= xmin, 1:nlines]))) * 1.05))
      ylim <- c(-ylims, ylims)
    }

    plot(0,
      type = "n",
      xlim = c(max(min(yr), xmin), max(yr)),
      yaxs = yaxs,
      ylim = ylim,
      xlab = ifelse(xylabs, "Year", ""),
      ylab = ifelse(xylabs, ylabs[which(refplots %in% quant)], ""),
      axes = FALSE
    )

    if (uncertainty) {
      for (iline in 1:nlines) {
        if (quant %in% c("SSB", "Fvalue", "Bratio")) {
          polygon(c(yr, rev(yr)),
            c(lower[, iline], rev(upper[, iline])),
            col = shadecol[iline], border = shadecol[iline]
          )
        } else {
          adj <- 0.2 * iline / nlines - 0.1
          arrows(
            x0 = yr + adj,
            y0 = lower[, iline],
            x1 = yr + adj,
            y1 = upper[, iline],
            length = 0.02,
            angle = 90,
            code = 3,
            col = col[iline]
          )
        }
      }
    }

    for (iline in 1:nlines) {
      if (quant %in% c("SSB", "Fvalue", "Bratio")) {
        lines(yr, exp[, iline], col = col[iline], pch = pch[iline], lty = lty[iline], lwd = lwd[iline], type = "l")
      } else {
        points(yr, exp[, iline], col = col[iline], pch = 16, cex = 0.8)
      }
    }
    if (quant == "Bratio") abline(h = refline, lty = 2)
    if (quant == "Fvalue" & fmsy) abline(h = refline2, lty = 2)

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
        if (subplots[s] != "index") {
          quant <- subplots[s]
          par(par)
          # save_png(paste0("ModelComp_", quant, ".png", sep = ""))

          plotinfo <- NULL
          r4ss::save_png(
            plotinfo = plotinfo,
            file = paste0("jabbaresidual.png", sep = ""),
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
        } else {
          if (subplots[s] == "index") {
            nfleets <- length(unique(summaryoutput[["indices"]][["Fleet"]]))
          }
          for (fi in 1:nfleets) {
            legend <- F
            if (fi %in% legendindex) legend <- TRUE
            indexfleets <- unique(summaryoutput[["indices"]][["Fleet"]])[fi]
            # save_png(paste0("FitsIndex_", unique(summaryoutput[["indices"]][["Fleet"]])[fi], ".png", sep = ""))

            plotinfo <- NULL
            r4ss::save_png(
              plotinfo = plotinfo,
              file = paste0("jabbaresidual.png", sep = ""),
              plotdir = plotdir,
              pwidth = pwidth,
              pheight = pheight,
              punits = punits,
              res = res,
              ptsize = ptsize,
              filenameprefix = filenameprefix
            )
            par(par)
            plot_index(indexfleets)
            dev.off()
            legend <- legend.temp
          } # End of Fleet Loop
        } # End index ifelse
      } # End print_plot
    }

    # subplots
    for (s in 1:length(subplots)) {
      if (verbose) message(paste0("Plot Comparison of ", subplots[s]))
      if (subplots[s] != "Index") {
        if (!add) par(par)
        quant <- subplots[s]
        plot_quants(quant)
      } else {
        nfleets <- length(unique(summaryoutput[["indices"]][["Fleet"]]))

        for (fi in 1:nfleets) {
          legend <- F
          if (fi %in% legendindex) legend <- TRUE
          indexfleets <- unique(summaryoutput[["indices"]][["Fleet"]])[fi]
          if (!add) par(par)
          plot_index(indexfleets)
          legend <- legend.temp
        } # End of Fleet Loop
      }
    }
  } # endplot
} # end of SSplotModelcomp()
#-----------------------------------------------------------------------------------------
