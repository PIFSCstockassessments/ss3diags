#' Retrospective-Forecast with One-Step Ahead Hindcasting
#'
#' Plots retrospective pattern, including (optional) one-step ahead forecast
#' and computes Mohn's Rho
#'
#' @param summaryoutput List created by [r4ss::SSsummarize()]
#' @param endyrvec Optional single year or vector of years representing the
#' final year of values to show for each model. By default it is set to the
#' ending year specified in each model.
#' @param subplots Optional vector of subplots to be created:
#' \itemize{
#'  \item `"SSB"` Spawning Stock Biomass
#'  \item `"F"` Fishing Mortality
#' }
#' @param xmin optional minimum year shown in plot (default first yr)
#' @param labels `yaxis` label for biomass (bony fish and sharks)
#' @param ylim option to specify `ylim` range
#' @param forecast [Logical][base::logical]. If TRUE, one-step ahead forecasts
#' are shown in plot
#' @param forecastrho [Logical][base::logical]. If TRUE, one-step ahead
#' forecast rho value is denoted in plot
#' @param shadecol uncertainty shading of hcxval horizon
#' @param shadecol1 uncertainty shading of early years not affected by hindcast
#' @param shadealpha set the transparency level (alpha) of the area of
#' uncertainty. Defaults to 0.3 (currently not used)
#' @param indexQlabel [Logical][base::logical]. If TRUE, add catchability to
#' legend in plot of index fits (currently not used)
#' @param indexQdigits Number of significant digits for catchability in legend
#' @param showrho [Logical][base::logical] flag to include Mohn's rho value.
#' Defaults to TRUE
#'
#' @inheritParams SSplotGeneric
#' @inheritParams SSplotGenericLegend
#' @inheritParams SSplotGenericPar
#' @inheritParams SSplotGenericUncertainty
#'
#'
#' @author Henning Winker (JRC-EC) and Laurance Kell (Sea++)
#'
#' @keywords ssplot retro
#'
#' @importFrom lifecycle deprecated
#'
#' @export
SSplotRetro <- function(summaryoutput,
                        subplots = c("SSB", "F"),
                        plot = TRUE,
                        print = deprecated(),
                        print_plot = FALSE,
                        png = deprecated(),
                        use_png = print_plot,
                        pdf = deprecated(),
                        use_pdf = FALSE,
                        models = "all",
                        endyrvec = "default",
                        xlim = NULL,
                        xmin = NULL,
                        labels = NULL,
                        ylim = NULL,
                        forecast = TRUE,
                        forecastrho = TRUE,
                        showrho = TRUE,
                        col = NULL,
                        pch = NA,
                        lwd = 5,
                        tickEndYr = TRUE,
                        ylimAdj = 1.05,
                        xaxs = "i",
                        yaxs = "i",
                        xylabs = TRUE,
                        type = "o",
                        uncertainty = TRUE,
                        legend = TRUE,
                        legendlabels = "default",
                        legendloc = "topright",
                        legendorder = "default",
                        legendncol = 1,
                        legendcex = 1,
                        legendsp = 1,
                        legendindex = NULL,
                        pt.cex = 3,
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
                        shadecol = "#404040", # grey(0.4, 0.6),
                        new = TRUE,
                        add = TRUE,
                        mcmcVec = FALSE,
                        shadecol1 = "#c8d0d9", # grey(0.5, 0.4),
                        indexQlabel = TRUE,
                        indexQdigits = 4,
                        shadealpha = 0.3) {
  # Parameter Deprecation Checks
  if (lifecycle::is_present(print)) {
    lifecycle::deprecate_warn("2.0.0", "SSplotRetro(print)", "SSplotRetro(print_plot)")
    print_plot <- print
  }

  if (lifecycle::is_present(pdf)) {
    lifecycle::deprecate_warn("2.0.0", "SSplotRetro(pdf)", "SSplorRetro(use_pdf)")
    use_pdf <- pdf
  }

  if (lifecycle::is_present(png)) {
    lifecycle::deprecate_warn("2.0.0", "SSplotRetro(png)", "SSplotRetro(use_png)")
    use_png <- png
  }

  if (!isTRUE(plot)) {
    lifecycle::deprecate_warn(
      when = "2.0.0",
      what = "SSplotRetro(plot)",
      details = "The ability to explictly disable plot windows or plot subplots is unused and will be defunct in a future version"
    )
  }

  if (!isTRUE(new)) {
    lifecycle::deprecate_warn(
      when = "2.0.0",
      what = "SSplotRetro(new)",
      details = "The ability to explicitly disable new plot windows is unused and will be defunct in a future version"
    )
  }

  #------------------------------------------
  # r4ss plotting functions
  #------------------------------------------
  # subfunction to write png files
  if (!add) graphics.off()


  # save_png <- function(file) {
  # if extra text requested, add it before extention in file name
  # file <- paste0(filenameprefix, file)
  # open png file
  # png(
  # filename = file.path(plotdir, file),
  # width = pwidth, height = pheight, units = punits, res = res, pointsize = ptsize
  # )
  # change graphics parameters to input value
  # par(par)
  # }


  if (is.null(legendindex)) legendindex <- 1:summaryoutput[["n"]]
  if (!legend) legendindex <- 10000

  quant <- subplots[1]

  #------------------------------------------------------------------
  plot_retro <- function(quant = quant) {
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
      if (verbose) message("PDF file with plots will be:", pdffile)
      par(par)
    }

    #-------------------------------------------------------------
    # plot function

    # get stuff from summary output (minimized)
    n <- summaryoutput[["n"]]
    startyrs <- summaryoutput[["startyrs"]]
    endyrs <- summaryoutput[["endyrs"]]

    if (endyrvec[1] == "default") {
      endyrvec <- endyrs - seq(0, n - 1, 1)
    }

    years <- min(startyrs):max(endyrvec)

    if (quant == "SSB") {
      mu <- summaryoutput[["SpawnBio"]]
      Lower <- summaryoutput[["SpawnBioLower"]]
      Upper <- summaryoutput[["SpawnBioUpper"]]
      if (is.null(labels)) {
        if (summaryoutput[["SpawnOutputUnits"]][1] == "numbers") {
          labels <- "Stock fecundity"
        } else {
          labels <- "Spawning biomass (t)"
        }
      }
    }

    if (quant == "F") {
      mu <- summaryoutput[["Fvalue"]]
      Lower <- summaryoutput[["FvalueLower"]]
      Upper <- summaryoutput[["FvalueUpper"]]
      if (is.null(labels)) {
        if (strsplit(summaryoutput[["FvalueLabels"]][1], ";")[[1]][1] == "_abs_F") {
          labels <- "Fishing mortality F"
        } else if (strsplit(summaryoutput[["FvalueLabels"]][1], ";")[[1]][1] == "(F)/(Fmsy)") {
          labels <- expression(F / F[MSY])
        } else {
          labels <- "F ratio"
        }
      }
    }


    ylab <- labels

    if (models[1] == "all") {
      models <- 1:n
    }


    nlines <- length(models)



    if (length(endyrvec) == 1) {
      stop("SSplotRequires requires a minimum of one reference and one retro peel")
    }

    # tableau inspired color palette
    # tableau10.pal <- c("#6D93BA", "#F28E2B", "#E46264", "#76B7B2",
    # "#3AA363", "#edc948", "#b07aa1", "#FF7278", "#9c755f", "#bab0ac")
    # if (is.null(col)) col <- tableau10.pal[1:nlines]

    if (is.null(col) & nlines > 3) col <- r4ss::rich.colors.short(nlines + 1)[-1]
    if (is.null(col) & nlines < 3) col <- r4ss::rich.colors.short(nlines)
    if (is.null(col) & nlines == 3) col <- c("blue", "red", "green3")

    if (is.null(shadecol)) {
      # new approach thanks to Trevor Branch
      shadecol <- adjustcolor(col, alpha.f = shadealpha)
    }

    # if line stuff is shorter than number of lines, recycle as needed
    if (length(pch) < nlines) pch <- rep(pch, nlines)[1:nlines]
    if (length(lwd) < nlines) lwd <- rep(lwd, nlines)[1:nlines]
    if (length(pt.cex) < nlines) pt.cex <- rep(pt.cex, nlines)[1:nlines]
    lty <- 1:nlines

    if (!is.expression(legendlabels[1]) &&
      legendlabels[1] == "default") {
      legendlabels <- c("Reference", paste(endyrvec[-1]))
    }
    if (legendorder[1] == "default") legendorder <- 1:(nlines)

    # open new window if requested
    if (plot & use_png == FALSE) {
      if (!add) dev.new(width = pwidth, height = pheight, pointsize = ptsize, record = TRUE)
    } else {
      if (!add) par(par)
    }

    # get quantities for plot



    # get exp
    exp <- mu[mu[["Yr"]] %in% years, ]

    # get uncertainty intervals if requested
    lower <- Lower[Lower[["Yr"]] %in% years, ]
    upper <- Upper[Upper[["Yr"]] %in% years, ]


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


    if (is.null(ylim)) ylim <- c(0, max(ifelse(uncertainty, max(c(unlist(exp[exp[["Yr"]] >= xmin, 1:nlines]), unlist(upper[upper[["Yr"]] >= xmin, 1]))) * ylimAdj, ylimAdj * max(unlist(exp[exp[["Yr"]] >= xmin, 1:nlines])) * 1.05)))
    if (is.null(xlim)) xlim <- c(max(min(yr), xmin), min(c(max(yr), max(endyrvec + 0.5))))

    # hindcast section
    yr.eval <- c(endyrvec)
    yr.eval <- (sort(yr.eval))
    nhc <- length(endyrvec) - 1


    plot(0,
      type = "n", xlim = xlim, yaxs = yaxs,
      ylim = ylim, xlab = ifelse(xylabs, "Year", ""), ylab = ifelse(xylabs, ylab, ""), axes = FALSE
    )

    imodel <- models[which(endyrvec == max(endyrvec))[1]]

    if (uncertainty) {
      polygon(c(seq(xlim[1], xlim[2]), rev(seq(xlim[1], xlim[2]))),
        c(
          lower[which(lower[["Yr"]] == xlim[1]):which(lower[["Yr"]] == xlim[2]), imodel],
          rev(upper[which(upper[["Yr"]] == xlim[1]):which(upper[["Yr"]] == xlim[2]), imodel])
        ),
        col = shadecol,
        border = shadecol
      )
    }


    # Plot Reference
    x.ref <- exp[["Yr"]][which(exp[["Yr"]] == xlim[1]):which(exp[["Yr"]] == xlim[2])]
    y.ref <- exp[which(exp[["Yr"]] == xlim[1]):which(exp[["Yr"]] == xlim[2]), imodel]
    lines(x.ref, y.ref, col = col[1], lwd = lwd, lty = 1, pch = 16)
    rho.i <- fcrho.i <- NULL
    for (iline in (2:nlines)[!mcmcVec]) {
      imodel <- models[iline]
      subset <- yr <= endyrvec[iline] & yr >= xlim[1]
      subsetfc <- yr <= endyrvec[iline] + 1 & yr >= xlim[1]
      x <- yr[subset]
      y <- exp[subset, imodel]
      xfc <- yr[subsetfc]
      yfc <- exp[subsetfc, imodel]
      lines(x, y, lwd = lwd[iline], col = col[iline], type = "l", cex = 1, lty = lty[iline])
      if (forecast) {
        lines(xfc[(length(xfc) - 1):length(xfc)],
          yfc[(length(xfc) - 1):length(xfc)],
          lwd = lwd,
          col = col[iline],
          type = "l",
          cex = 1,
          lty = 1
        )
        points(xfc[length(xfc)], yfc[length(yfc)],
          pch = 21,
          bg = col[iline], col = 1, type = "p", cex = pt.cex
        )
      }
      rho.i[iline - 1] <- (y[length(y)] - y.ref[length(y)]) /
        y.ref[length(y)]
      fcrho.i[iline - 1] <- (yfc[length(yfc)] - y.ref[length(yfc)]) /
        y.ref[length(yfc)]
    }


    rho <- mean(rho.i)
    fcrho <- mean(fcrho.i)
    rho.table <- data.frame(type = quant, peel = c(endyrvec[-1], "Combined"), Rho = c(rho.i, rho), ForecastRho = c(fcrho.i, fcrho))


    if (legend) {
      r4ss::add_legend(legendlabels,
        legendloc = legendloc,
        legendcex = legendcex,
        legendsp = legendsp,
        legendncol = legendncol,
        legendorder = legendorder,
        pch = pch,
        col = col,
        lty = lty,
        lwd = lwd,
        pt.cex = pt.cex,
        type = type
      )
      if (uncertainty) {
        # get position of legend
        legend_info <- r4ss::add_legend(legendlabels,
          legendloc = legendloc,
          legendcex = legendcex,
          legendsp = legendsp,
          legendncol = legendncol,
          legendorder = legendorder,
          pch = pch,
          col = col,
          lty = lty,
          lwd = lwd,
          pt.cex = pt.cex,
          type = type
        )
        # Add a standard R legend just for the CI box
        legend_coords <- legend_info[["rect"]]
        ci_legend_x <- legend_coords[["left"]]
        ci_legend_y <- legend_coords[["top"]] - legend_coords[["h"]]
        legend(
          x = ci_legend_x, y = ci_legend_y,
          legend = "95% CI",
          pch = 15,
          col = shadecol,
          pt.cex = 2,
          bty = "n"
        )
      }
    }
    if (showrho) {
      legend("top", paste0(
        "Mohn's rho = ", round(rho, 2),
        ifelse(forecast & forecastrho, paste0("\nForecast Mohn's rho = ", round(fcrho, 2)), "")
      ),
      bty = "n", y.intersp = -0.2, cex = legendcex + 0.1
      )
    }

    # axis(1, at=c(max(xmin,min(yr)):max(endyrvec)))
    axis(1)
    if (tickEndYr) axis(1, at = max(endyrvec))

    axis(2)
    box()

    return(rho.table)
  } # End of plot_retro function
  #------------------------------------------------------------

  if (verbose) message("Plotting Retrospective pattern")
  if (plot) {
    if (print_plot) {
      # save_png(paste0("retro_", quant, ".png", sep = ""))
      plotinfo <- NULL
      r4ss::save_png(
        plotinfo = plotinfo,
        file = paste0("retro_", quant, ".png", sep = ""),
        plotdir = plotdir,
        pwidth = pwidth,
        pheight = pheight,
        punits = punits,
        res = res,
        ptsize = ptsize,
        filenameprefix = filenameprefix
      )
      par(par)
      get_rho <- plot_retro(quant)
      dev.off()
    }


    if (!add) (par)
    get_rho <- plot_retro(quant)
  }


  if (verbose) cat(paste0("Mohn's Rho stats, including one step ahead forecasts:", "\n"))
  return(get_rho)
} # end of SSplotRetro()
#-----------------------------------------------------------------------------------------
