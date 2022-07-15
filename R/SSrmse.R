#' Function for calculating RMSE
#' used for `SSplotJABBAres()`
#' 
#' @param ss3rep output from `SS_output()`
#' @param quants the dataset to calculate RMSE for. "cpue" for index of abundance, "len" for length comp, "age" for age composition, and "con" for conditional age-at-length. 
#' @return returns a list that includes the RMSE table output (by fleet and combined) and the dataframe of residuals which can be used for creating the `SSplotJABBAres()` plot
#' @importFrom magrittr "%>%"
#' 
#' @export


SSrmse <- function(ss3rep, quants = c("cpue", "len", "age", "con")){
  
  if(length(quants) > 1) 
    warning("RMSE can only be calculated for one object at a time, calculating RMSE for ", quants[1], " only." )
  
  quants <- quants[1]
  datatypes <- c("Index", "Mean length", "Mean age", "Conditional Age")
  
  if (quants == "cpue") {
    cpue <- ss3rep[["cpue"]]
    cpue[["residuals"]] <- ifelse(is.na(cpue[["Obs"]]), NA, log(cpue[["Obs"]]) - log(cpue[["Exp"]]))
    if (is.null(cpue[["Fleet_name"]])) { # Deal with Version control
      cpue[["Fleet_name"]] <- cpue[["Name"]]
    }
    Res <- cpue
  }
  
  if (quants == "len" | quants == "age" | quants == "size") {
    comps <- SScompsTA1.8(ss3rep, fleet = NULL, type = quants, plotit = FALSE)$runs_dat
    comps[["residuals"]] <- ifelse(is.na(comps[["Obs"]]), NA, log(comps[["Obs"]]) - log(comps[["Exp"]]))
    if (is.null(comps[["Fleet_name"]])) { # Deal with Version control
      comps[["Fleet_name"]] <- comps[["Name"]]
    }
    Res <- comps
  }
  
  if (quants == "con") {
    cond <- SScompsTA1.8(ss3rep, fleet = NULL, type = quants, plotit = FALSE)$runs_dat
    cond[["residuals"]] <- ifelse(is.na(cond[["Obs"]]), NA, log(cond[["Obs"]]) - log(cond[["Exp"]]))
    if (is.null(cond[["Fleet_name"]])) { # Deal with Version control
      cond[["Fleet_name"]] <- cond[["Name"]]
    }
    Res <- cond
  }
  
  if (is.null(seas)) {
    seas <- "comb"
    if (length(unique(Res[["Seas"]])) > 1) {
      cat("Warning: combining data from multiple seasons\n")
    }
  }
  
  
  # subset if indexselect is specified
  if (is.null(indexselect) == F & is.numeric(indexselect)) {
    iname <- unique(Res[["Fleet_name"]])[indexselect]
    if (TRUE %in% is.na(iname)) stop("One or more index numbers exceed number of available indices")
    Res <- Res[Res[["Fleet_name"]] %in% iname, ]
  }
  
  RMSE <- Res %>% dplyr::summarise(RMSE.perc = round(100 * sqrt(mean(residuals^2, na.rm = TRUE)), 1),
                                   Nobs = length(!is.na(residuals))) %>% 
    dplyr::mutate(Fleet = "Combined") %>% 
    dplyr::select(Fleet, RMSE.perc, Nobs)

  rmse_table <- Res %>% 
    dplyr::group_by(Fleet_name) %>% 
    dplyr::summarise(resi = sum(residuals^2, na.rm = TRUE),
                     ni = length(!is.na(residuals))) %>% 
    dplyr::mutate(rmse = round(100* sqrt(resi/ni), 1)) %>% 
    dplyr::select(Fleet_name, rmse, ni) %>% 
    dplyr::rename(Fleet = "Fleet_name",
           RMSE.perc = "rmse",
           Nobs = "ni") %>% 
    dplyr::bind_rows(RMSE)
  
  output <- list(RMSE = rmse_table, residuals = Res)
    
  return(output)
  
  
}
