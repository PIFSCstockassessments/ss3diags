

#' Generic SSplot function
#' 
#' Generalized version of ss3diags SSplots functions. Not intended for 
#' production
#'
#' @param pwidth 
#' Width of plot
#' 
#' @param pheight 
#' Height of plot
#' 
#' @param punits 
#' Measurement units for PNG file
#' 
#' @param res 
#' Resolution for PNG file
#' 
#' @param ptsize 
#' Point size for PNG file
#' 
#' @param cex.main 
#' Character expansion for plot titles
#' 
#' @param plotdir 
#' Directory where PNG or PDF files will be written. By default, 
#' it will be the directory where the model was run.
#' 
#' @param filenameprefix 
#' Additional text to append to PNG or PDF file names.
#' It will be separated from default name by an underscore.
#' 
#' @param par 
#' list of graphics parameter values passed to `par()` function
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
