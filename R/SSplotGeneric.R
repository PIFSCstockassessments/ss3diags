

#' Generic SSplot function
#' 
#' Generalized version of ss3diags SSplots functions. Not intended for 
#' production
#' 
#' @param pwidth 
#' Default width of plot printed to plot in units of `punits`
#' 
#' @param pheight 
#' Height of plot printed to plot in units of `punits `
#' 
#' @param punits 
#' Measurement units for `pwidth` and `pheight`. Default is `"in"`.
#' \itemize{
#'  \item `"px"` (pixels)
#'  \item `"in"` (inches)
#'  \item `"cm"` (centimeters)
#'  \item `"mm"` (millimeters)
#' }
#' 
#' @param res 
#' Resolution for plots printed to files.
#' 
#' @param ptsize 
#' Point size for plotted text in plots printed in files. See `help("png")`
#' for more details
#' 
#' @param cex.main 
#' Character expansion for plot titles.
#' 
#' @param plotdir 
#' Directory where output plot file will be written. By default, 
#' it will be the directory where the model was run.
#' 
#' @param filenameprefix 
#' Additional text to append to output plot file name.
#' It will be separated from default name by an underscore.
#' 
#' @param par 
#' A numerical vector of the form c(bottom, left, top, right) which 
#' gives the number of lines of margin to be specified on the four sides of 
#' the plot,which is passed to `par()`. Entering `NULL` passes plot's default 
#' `par()` values (which depends on whether the main title is included or not)
#' 
#' @param verbose 
#' Flag to print additional diagnostic messages to R console
#' 
#' @param plot 
#' DEPRECATED. By default, TRUE, Plots (and subplots) are drawn to the 
#' plot device. The option to explicitly disable this option (FALSE), is not 
#' implemented. This option flag will be defunct in a future version
#' 
#' @param print 
#' DEPRECATED, please use `print_plot`.
#' 
#' @param print_plot 
#' Flag to enable plot graphic device to print to PNG or PNG files.
#' 
#' @param png 
#' DEPRECATED. Please use `use_png`.
#' 
#' @param use_png 
#' Enables plots to be generated to PNG files. Defaults to print value
#' 
#' @param pdf 
#' DEPRECATED. Please use `use_pdf`.
#' 
#' @param use_pdf 
#' Enables plots to be generated to pdf file.
#' 
#' @param new 
#' Deprecated. New plot windows are created by default (TRUE), and the
#' option to disable this, via FALSE, is unused.
#' 
#' @param add 
#' suppresses `par()` to create multiplot figs
#' 
SSplotGeneric <- function(pwidth = 6.5,
                          pheight = 5.0,
                          punits = "in",
                          res = 300,
                          ptsize = 10,
                          cex.main = 1,
                          plotdir = NULL,
                          filenameprefix = "",
                          par = list(mar = c(5, 4, 1, 1) + .1),
                          verbose = TRUE,
                          plot = TRUE,
                          print = deprecated(),
                          print_plot = FALSE,
                          png = deprecated(),
                          use_png = print_plot,
                          pdf = deprecated(),
                          use_pdf = FALSE,
                          new = TRUE,
                          add = FALSE) {
  
  stop("SSplotGeneric not implemented")
}


#' Legend parameters for Generic SSplot 
#'
#' Generalized version of ss3diags SSplots functions specfic to plot legend 
#' parameters. Not intended for production.
#'
#' @param legend 
#' Flag to enable legend to plot. TRUE by default.
#' 
#' @param legendlabels 
#' Optional vector of labels to include in legend.
#' 
#' @param legendloc 
#' Location of legend. Either a string like "topleft" or a vector of two 
#' numeric values representing the fraction of the maximum in the x and y
#' dimensions, respectively. See `help("legend")` for more info on the 
#' string options.
#' 
#' @param legendorder 
#' Optional vector of model numbers that can be used to have the legend 
#' display the model names in an order that is different than that
#' which is represented in the summary input object.
#' 
#' @param legendncol 
#' Number of columns for the legend.
#' 
#' @param legendcex 
#' Allows to adjust legend cex
#' 
#' @param legendsp 
#' Space between legend labels
#' 
#' @param legendindex 
#' Allows to add legend for selected indices (plots)
#'
SSplotGenericLegend <- function(legend = TRUE,
                                legendlabels = "default",
                                legendloc = "topright",
                                legendorder = "default",
                                legendncol = 1,
                                legendcex = 1,
                                legendsp = 0.9,
                                legendindex = NULL) {
  
  stop("SSplotGenericLegend not implemented")
}


#' Graphical Parameters for Stock Synthesis Generic plots.
#'
#' Generalized version of ss3diags SSplots functions to specify to `par()` or
#' to pass plot attributes. Not intended for production.
#'
#' @param col 
#' Optional vector of colors to be used for lines. Input NULL
#' 
#' @param pch 
#' Optional vector of plot character values
#' 
#' @param lty 
#' Optional vector of line types
#' 
#' @param lwd 
#' Optional vector of line widths
#' 
#' @param tickEndYr 
#' [Logical][base::logical] flag: set TRUE or FALSE to switch to turn 
#' on/off extra axis mark at final year in timeseries plots.
#' 
#' @param ylimAdj 
#' Multiplier for ylim parameter. Allows additional white space.
#' 
#' @param xlim 
#' Optional, years to use for x-axis. Default value NULL (or "default"), 
#' uses all years available.
#' 
#' @param xaxs 
#' Choice of xaxs parameter See `?par` for more info.
#' 
#' @param yaxs 
#' Choice of yaxs parameter. See `?par` for more info.
#' 
#' @param xylabs 
#' [Logical][base::logical] flag: set TRUE or FALSE to include x- and 
#' y-axis labels to the plot. Defaults to TRUE
#' 
#' @param type 
#' The type of plot to be drawn. For more details, see [`plot`][base::plot].
#' 
SSplotGenericPar <- function(col = NULL,
                             pch = NULL,
                             lty = 1,
                             lwd = 2,
                             tickEndYr = FALSE,
                             xlim = NULL,
                             ylimAdj = 1.05,
                             xaxs = "i",
                             yaxs = "i",
                             xylabs = TRUE,
                             type = "l") {
  stop("SSplotGenericPar not implemented")
}


#' Uncertainty parameters for Stock Synthesis Generic plots.
#'
#' Generalized version of ss3diags SSplots functions to specify uncertainty.
#' Not intended for production.
#'
#'
#' @param uncertainty 
#' Logical flag to enable plots with uncertainty intervals. Either a single
#' TRUE/FALSE value, or a vector of TRUE/FALSE values around SSB or F 
#' estimated timeseries, or a set of integers corresponding to 
#' the choice of models.
#' 
#' @param mcmcVec 
#' Logical vector of TRUE/FALSE values (or single value) indicating
#' whether input values are from MCMC or to use normal distribution around
#' MLE.
#' 
#' @param endyrvec 
#' Optional single year or vector of years representing the
#' final year of values to show for each model. By `"default"` it is set to the
#' ending year specified in each model.
#' 
#' @param models Optional subset of the models of `summaryoutput` (or a similar
#' field with a different name): a list created by the function 
#' [r4ss::SSsummarize]. Either `"all"` or a vector of numbers indicating 
#' columns in summary tables.
#' 
#'
SSplotGenericUncertainty <- function (uncertainty = TRUE,
                                      mcmcVec = FALSE,
                                      endyrvec = "default",
                                      models = "all") {
  
  stop("SSplotGenericUncertainty not implemented")
}