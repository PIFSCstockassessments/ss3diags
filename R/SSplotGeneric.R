

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
SSplotGeneric <- function(pwidth = 6.5,
                           pheight = 5.0,
                           punits = "in",
                           res = 300,
                           ptsize = 10,
                           cex.main = 1,
                           plotdir = NULL,
                           filenameprefix = "",
                           par = list(mar = c(5, 4, 1, 1) + .1),
                           verbose = TRUE) {
  
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

