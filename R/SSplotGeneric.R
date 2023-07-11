

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

