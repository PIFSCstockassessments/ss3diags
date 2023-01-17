#'
#' @param mcmcVec
#' UNTESTED. Vector of TRUE/FALSE values (or single value) indicating mcmc 
#' values are used.
#'
#' @param uncertainty
#' [Logical vector][base::logical] (TRUE/FALSE) to include uncertainty intervals 
#' around SSB or F estimated time series. Defaults to TRUE.
#' 
#' @param endyrvec
#' Ending year specified in each model.
#' 
#' @param models
#' Optional subset of the r4ss summary output object models described in r4ss function [`SSsummarize`][r4ss::SSsummarize]().  Either "all" or a vector of numbers indicating
#' columns in summary tables.
#' 
