#' Residual plot
#'
#' plots residuals for all indices as boxplot (color coded by fleet) with a loess showing systematic trends over time. This function is from the package JABBA.
#'
#' @param ss3rep Stock Synthesis output as read by r4SS function SS_output
#' @param subplots string of type of data to plot, 'cpue' for index of abundance data, 'len' or 'age' for length- or age-composition, 'size' for generalized size composition, or 'con' for conditional age-at-length data.
#' @param seas string indicating how to treat data from multiple seasons
#' 'comb' - combine seasonal data for each year and plot against Yr
#' 'sep' - treat season separately, plotting against Yr.S.
#' If is.null(seas), it is assumed that there is only one season and option 'comb' is used.
#' @param plot Deprecated. Plots (and subplots) are drawn to the active plot device
#' by default (TRUE), and the option to disable this, via FALSE, is unused.
#' @param print Deprecated. Please use 'print_plot'.
#' @param print_plot Option to print to PNG files
#' @param png Deprecated. please use 'use_png'.
#' @param use_png Draw plots in PNG format
#' @param pdf Deprecated. Please use 'use_pdf'.
#' @param use_pdf option for pdf plots (not tested for TRUE)
#' @param indexselect Vector of fleet numbers for each model for which to compare
#' @param miny  minimum abs values of ylim
#' @param col Optional vector of colors to be used for lines. Input NULL
#' @param pch Optional vector of plot character values
#' @param lty Optional vector of line types
#' @param lwd Optional vector of line widths
#' @param tickEndYr TRUE/FALSE switch to turn on/off extra axis mark at final
#' year in time series plots.
#' @param ylimAdj Multiplier for ylim parameter. Allows additional white space
#' @param xaxs Choice of x-axis parameter (see ?par for more info)
#' @param yaxs Choice of y-axis parameter (see ?par for more info)
#' @param type Type parameter passed to points (default 'o' overplots points on
#' top of lines)
#' @param legend Option to add a legend. TRUE by default.
#' @param legendlabels Optional vector of labels to include in legend.
#' @param legendloc Location of legend. Either a string like "topleft" or a vector
#' of two numeric values representing the fraction of the maximum in the x and y
#' dimensions, respectively. See ?legend for more info on the string options.
#' @param legendorder Optional vector of model numbers that can be used to have
#' the legend display the model names in an order that is different than that
#' which is represented in the summary input object.
#' @param legendncol Number of columns for the legend.
#' @param legendcex Allows to adjust legend cex. Defaults to 1.
#' @param legendsp Space between legend labels
#' @param legendindex Allows to add legend for selected indices (plots)
#' @param pwidth Width of plot
#' @param pheight Height of plot
#' @param punits Units for PNG file
#' @param res Resolution for PNG file
#' @param ptsize Point size for PNG file
#' @param cex.main Character expansion for plot titles
#' @param plotdir Directory where PNG or PDF files will be written. By default
#' it will be the directory where the model was run.
#' @param filenameprefix Additional text to append to PNG or PDF file names.
#' It will be separated from default name by an underscore.
#' @param par list of graphics parameter values passed to par() function
#' @param verbose Report progress to R GUI?
#' @param boxcol color boxes
#' @param new Deprecated. New plot windows are created by default (TRUE), and the
#' option to disable this, via FALSE, is unused.
#' @param add supresses par() to create multiplot figs
#' @param xlim Optional, values for x-axis range of years to display on plot.
#' Default = "default" displays all years of available data. (currently not used)
#' @param ylim Optional, min and max values for the ylim to override the "default" value (-0.7, 0.5)
#' @param xylabs TRUE or FALSE, include x- and y-axis labels
#'
#' @author Henning Winker (JRC-EC)
#'
#' @keywords ssplot
#'
#'
#' @importFrom grDevices grey
#' @importFrom graphics boxplot
#' @importFrom stats predict loess runif residuals
#' @importFrom lifecycle deprecated
#' @importFrom rlang .data
#' @importFrom dplyr group_by arrange mutate summarise ungroup
#' @importFrom r4ss save_png
#'
#' @export
SSplotJABBAres <- function(ss3rep = ss3diags::simple,
                           subplots = c("cpue", "len", "age", "size", "con")[1],
                           seas = NULL,
                           plot = TRUE,
                           print = lifecycle::deprecated(),
                           print_plot = FALSE,
                           png = lifecycle::deprecated(),
                           use_png = print_plot,
                           pdf = lifecycle::deprecated(),
                           use_pdf = FALSE,
                           indexselect = NULL,
                           miny = 3,
                           col = NULL,
                           pch = 21,
                           lty = 1,
                           lwd = 2,
                           tickEndYr = TRUE,
                           xlim = "default",
                           ylim = "default",
                           ylimAdj = 1.1,
                           xaxs = "i",
                           yaxs = "i",
                           xylabs = TRUE,
                           type = "o",
                           legend = TRUE,
                           legendlabels = "default",
                           legendloc = "bottomleft",
                           legendorder = "default",
                           legendncol = 1,
                           legendcex = 1,
                           legendsp = 0.9,
                           legendindex = NULL,
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
                           boxcol = grey(0.8, 0.5),
                           new = TRUE,
                           add = FALSE) {


  # Parameter DEPRECATION checks
  if (lifecycle::is_present(print)) {
    lifecycle::deprecate_warn("2.0.0", "SSplotJABBAres(print)", "SSplotJABBAres(print_plot)")
    print_plot <- print
  }

  if (lifecycle::is_present(png)) {
    lifecycle::deprecate_warn("2.0.0", "SSplotJABBAres(png)", "SSplotJABBAres(use_png)")
    use_png <- png
  }

  if (lifecycle::is_present(pdf)) {
    lifecycle::deprecate_warn("2.0.0", "SSplotJABBAres(pdf)", "SSplotJABBAres(use_pdf)")
    use_pdf <- pdf
  }

  if (!isTRUE(plot)) {
    lifecycle::deprecate_warn(
      when = "2.0.0",
      what = "SSplotJABBAres(plot)",
      details = "The ability to explictly disable plot windows or plot subplots is unused and will be defunct in a future version"
    )
  }

  if (!isTRUE(new)) {
    lifecycle::deprecate_warn(
      when = "2.0.0",
      what = "SSplotJABBAres(new)",
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

  subplots <- subplots[1]
  datatypes <- c("Index", "Mean length", "Mean age", "Conditional Age")
  ylabel <- datatypes[which(c("cpue", "len", "age", "con") %in% subplots)]

  # log <- FALSE # (no option to plot on log scale) #removed this line, not sure why it is necessary - MO 7/14/22
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


  resids_list <- SSrmse(ss3rep, quants = subplots, seas = seas, indexselect = indexselect)
  #-----------------
  # start plot
  #----------------
  jabbaresiduals <- function(resids_list) {
    
    Res <- resids_list[["residuals"]] %>% 
      dplyr::group_by(.data[["Fleet"]]) %>% 
      dplyr::arrange(.data[["Yr"]], .by_group = TRUE)
    positions <- runif(nrow(Res), -0.2, 0.2)
    
    series <- 1:length(unique(Res[["Fleet"]]))

    labels <- c(
      "Year", # 1
      "Residuals"
    ) # 2

    if (is.null(legendindex)) legendindex <- series
    if (!legend) legendindex <- 10000

    # open new window if requested
    if (plot & use_png == FALSE) {
      if (!add) dev.new(width = pwidth, height = pheight, pointsize = ptsize, record = TRUE)
    } else {
      if (!add) par(par)
    }


    # get quantities for plot
    indices <- unique(Res[["Fleet_name"]])
    yr <- unique(Res[["Time"]])
    n.years <- length(yr)
    ylab <- paste(ylabel, "residuals")


    # setup colors, points, and line types
    n.indices <- length(unique(Res[["Fleet"]]))
    if (is.null(col) & n.indices > 3) col <- r4ss::rich.colors.short(n.indices + 1)[-1]
    if (is.null(col) & n.indices < 3) col <- r4ss::rich.colors.short(n.indices)
    if (is.null(col) & n.indices == 3) col <- c("blue", "red", "green3")
    # set pch values if no input

    # if line stuff is shorter than number of lines, recycle as needed
    if (!is.expression(legendlabels[1]) &&
      legendlabels[1] == "default") {
      legendlabels <- c(paste(indices), "Loess")
    }
    if (legendorder[1] == "default") legendorder <- 1:(n.indices + 1)

    # open new window if requested
    if (plot & png == FALSE) {
      if (!add) dev.new(width = pwidth, height = pheight, pointsize = ptsize, record = TRUE)
    } else {
      if (!add) par(par)
    }

    ### make plot of index fits
    yrange <- ifelse(rep(max(ifelse(abs(Res[["residuals"]]) > miny, 0, Res[["residuals"]]), na.rm = T), 2) > 0.5, 
                     range(ylimAdj * ifelse(abs(Res[["residuals"]]) > miny, 0, Res[["residuals"]]), na.rm = T), 
                     range(c(-0.7, 0.5)))
    
    if(ylim[1] == "default"){
      ylim <- c(-max(abs(yrange)), max(abs(yrange)))
    } else {
      ylim <- ylim
    }

    if (xlim[1] == "default") xlim <- range(yr)
   
    Res[["residuals"]] <- ifelse(abs(Res[["residuals"]]) > 3, NA, Res[["residuals"]])
      

    Resids <- reshape2::dcast(Res, Time ~ Fleet, value.var = "residuals")
    Resids <- t(Resids[,-1])
      
    plot(0, 
         type = "n", xlim = xlim, ylim = ylim, yaxs = yaxs, 
         xlab = ifelse(xylabs, "Year", ""), ylab = ifelse(xylabs, ylab, ""), axes = FALSE) 
    boxplot(as.matrix(Resids), add = TRUE, at = yr, xaxt = "n", 
            col = grey(0.8,.5), notch = FALSE, outline = F, axes = F)
    abline(h = 0, lty = 2)
    
    for(i in 1:n.indices){
      for(t in 1:n.years){
        lines(rep((yr + positions[i])[t], 2), c(0, Resids[i,t]), col = col[i])
      }
      points(yr + positions[i], Resids[i,], col = 1, pch = pch, bg = col[i])
    }

    mean.res <-  Res %>% 
      dplyr::group_by(.data[["Yr"]]) %>%
      dplyr::summarise(mean.res = mean(.data[["residuals"]], na.rm = TRUE)) %>% 
      dplyr::mutate(Yr = as.numeric(.data[["Yr"]]))
    
    mean.res[["smooth.res"]] <- predict(loess(mean.res[["mean.res"]] ~ mean.res[["Yr"]]), 
                                        data.frame(unique(Res[["Yr"]])))
    #mean.res[["Yr"]] <- as.factor(mean.res[["Yr"]])
    lines(mean.res[["Yr"]], mean.res[["smooth.res"]], lwd = 2)

    legend("topright", 
           c(paste0("RMSE = ", resids_list[["RMSE"]][resids_list[["RMSE"]][["Fleet"]] == "Combined", "RMSE.perc"], "%")), 
           bty = "n", cex = legendcex + 0.1, y.intersp = 0.2, x.intersp = 0)
    
    if (legend) legend(legendloc, legendlabels, bty = "n", col = 1, pt.cex = 1.1, 
                       cex = legendcex, pch = c(rep(21, n.indices), -1), pt.bg = c(col, 1), 
                       lwd = c(rep(-1, n.indices), 2))
    axis(1, at =  mean.res$Yr, labels = unique(Res[["Yr"]])) 
    if (tickEndYr) axis(1, at = max(floor(yr)))
    axis(2)
    box()
    return(resids_list[["RMSE"]])

  } # end jabba residual plot
  #------------------------------------------------------------

  if (verbose) message("Plotting JABBA residual plot.")
  if (verbose) message("is plot TRUE? ", plot)
  if (plot) {
    if (verbose) message("drawing plot at ", plotdir)
    if (print_plot) {
      # save_png(paste0("jabbaresidual.png", sep = ""))
      plotinfo <- NULL
      save_png(
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
      rmse <- jabbaresiduals(resids_list)
      dev.off()
    }
    if (verbose) {
      message(
        "Plot exists: ",
        file.exists(file.path(plotdir, paste0(filenameprefix, "jabbaresidual.png")))
      )
    }

    if (!add) (par)
    rmse <- jabbaresiduals(resids_list) # End of Fleet Loop
  }


  if (verbose) cat(paste0("RMSE stats by Index:", "\n"))
  return(rmse)
} # end of SSplotJABBAresids()
#-----------------------------------------------------------------------------------------
