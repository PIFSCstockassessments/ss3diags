#' @param pwidth
#' Width of the plot
#' 
#' @param pheight
#' Height of the plot
#' 
#' @param punits
#' Measurement units for output PNG image file 
#' 
#' @param res
#' Resolution for output PNG image file
#' 
#' @param ptsize
#' Point size for output PNG image file
#' 
#' @param cex.main
#' Character Expansion for plot titles. See ['par'][graphics::par()] for more info.
#' 
#' @param filenameprefix
#' Additional text to append to output image file name. It will be separated from the 
#' default name by an underscore.
#' 
#' @param plotdir
#' Directory where output image files will be written. By default,
#' it will be the directory where the model was run.
#' 
#' @param par
#' list of graphics parameter values passed to ['par'][graphics::par()] function
#' 
#' @param verbose
#' Option to report progress to R console. Default is TRUE.
#' 
#' @param plot 
#' Deprecated. Plots (and subplots) are drawn to the active plot device
#' by default (TRUE), and the option to disable this, via FALSE, is unused.
#' 
#' @param print
#' Deprecated. Please use `print_plot`
#' 
#' @param print_plot
#' Option to output plot as output image file. 
#' 
#' @param pdf 
#' Deprecated. Please use 'use_pdf'.
#' 
#' @param use_pdf 
#' Option to use the [`pdf`][grDevices::pdf()] graphical device to generate pdf plots. (Note: not tested  TRUE)
#' 
#' @param png 
#' Deprecated. Please use `use_png`.
#' 
#' @param use_png 
#' Option to use the [`png`][grDevices::png()] graphical device to draw plot
#' 
#' @param new 
#' Deprecated. New plot windows are created by default (`TRUE`), and the
#' option to disable this, via FALSE, is unused.
#' 
#' @param add 
#' suppresses [`par`][graphics::par()] to create multiplot figs
#' 
#' @param pch
#' Optional [`par`][graphics::par()] vector of plot character values
#' 
#' @param lty
#' Optional [`par`][graphics::par()] vector of line types
#' 
#' @param lwd
#' Optional [`par`][graphics::par()] vector of line widths
#' 
#' @param tickEndYr 
#' TRUE/FALSE switch to turn on/off extra axis marks at final
#' 
#' @param ylimAdj 
#' Multiplier for ylim parameter. Allows additional white space.
#' 
#' @param xaxs
#' Choice of [`par`][graphics::par()] xaxs parameter (see [`par`][graphics::par()] for more info)
#' 
#' @param yaxs 
#' Choice of [`par`][graphics::par()] yaxs parameter (see [`par`][graphics::par()] for more info)
#' 
#' @param xylabs
#' Option to include x- and y-axis labels. Defaults to TRUE
#' 
#' @param type
#' Type parameter passed to points. If type is `o`, it overplots points on
#' top of lines. if type input is `l`, then turn off points on top of 
#' lines in legend. For more detail see [`plot.default`][graphics::plot.default()]
#' 
#' 