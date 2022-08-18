library(tidyverse)
library(ggplot2)
library(patchwork)


SSrecRMSE <- function(retroModels = ss3diags::retroSimple,
    rec_fun = "bh"
                      ){
  
  ## Function to calculate expected recruitment based on BH 
  if(rec_fun == "bh"){
    
    # make BH function
    bh <- function(x, retroOut, ...){
      R0   <- retroOut[["parameters"]] %>% dplyr::filter(.data[["Label"]] %in% "SR_LN") %>% .$Value %>% exp()
      h  <- retroOut[["parameters"]] %>% dplyr::filter(.data[["Label"]] %in% "SR_BH_steep") %>% .$Value
      sigmaR <- retroOut[["parameters"]] %>% dplyr::filter(.data[["Label"]] %in% "SR_sigmaR") %>% .$Value
      B0     <- retroOut[["SBzero"]]
      b.corr <- 1 # bias correction is zero in the forecasts (=1)
      Rhat <- (4*h*R0*x / (B0*(1-h) + x*(5*h-1))) * exp(-0.5*b.corr*sigmaR)
      return(Rhat)
    }
    
  }
 
  pred.err.ens <- NULL
  out <- NULL
  pred.err <- NULL
  # extract and organise R and SSB from retro
  for(pl in 1:length(retroModels)){
    tmp <- retroModels[[pl]]$timeseries %>% 
      dplyr::filter(Era=="TIME") %>% 
      dplyr::group_by(Yr) %>%
      dplyr::summarise(Recruit_0 = sum(Recruit_0),
                       SpawnBio = mean(SpawnBio, na.rm=T)) %>%
      dplyr::mutate(TmY = retroModels[[pl]]$Retro_year)
    out <- dplyr::bind_rows(out,tmp)
  }
  out <- out %>%
    dplyr::filter(.data[["Yr"]] <= .data[["TmY"]])
  
  # retain 5 peels from the retro 
  out <- out %>% 
    dplyr::filter(.data[["TmY"]] >= (max(out$TmY) - 5))
  
  # Calculate prediction error from geometric mean
  for(time.window in 2:10){
    for(i in min(out[["TmY"]]):(max(out[["TmY"]])-1)){
      tmp <- out %>%
        dplyr::filter(.data[["TmY"]] %in% i) %>%
        tail(time.window) %>%
        dplyr::summarise(gm=prod(.data[["Recruit_0"]])^(1/time.window),
                         TmY=unique(.data[["TmY"]]))
      tmp <- out %>%
        dplyr::filter(.data[["TmY"]] %in% max(.data[["TmY"]])) %>%
        dplyr::filter(.data[["Yr"]] %in% (tmp[["TmY"]]+1)) %>%
        dplyr::select(.data[["Yr"]], .data[["Recruit_0"]]) %>%
        dplyr::bind_cols(tmp) %>%
        dplyr::mutate(err=log(.data[["Recruit_0"]])-log(.data[["gm"]])) %>%
        dplyr::mutate(wn=ifelse(nchar(time.window)==1, paste0("gm0",time.window), paste0("gm",time.window)))
      pred.err <- tmp %>%
        dplyr::select(.data[["err"]], .data[["wn"]]) %>%
        dplyr::bind_rows(pred.err)
    }
  }
  pred.err.gm <- pred.err
  
  # Calculate prediction error from S-R
  pred.err <- NULL
  for(pl in 2:length(retroModels)){
    yri <- retroModels[[pl]][["Retro_year"]]
    tmp <- out %>%
      dplyr::filter(.data[["TmY"]] %in% yri) %>%
      dplyr::filter(.data[["Yr"]] %in% yri) %>%
      dplyr::summarise(rec=bh(x=SpawnBio,retroModels[[pl]]),
                       TmY=.data[["TmY"]])
    tmp <- out %>%
      dplyr::filter(.data[["TmY"]] %in% max(out[["TmY"]])) %>%
      dplyr::filter(.data[["Yr"]] %in% (tmp$TmY+1)) %>%
      dplyr::select(.data[["Yr"]], .data[["Recruit_0"]]) %>%
      dplyr::bind_cols(tmp) %>%
      dplyr::mutate(err=log(.data[["Recruit_0"]])-log(.data[["rec"]])) %>%
      dplyr::mutate(wn="bh")
    pred.err <- tmp %>%
      dplyr::select(.data[["err"]],.data[["wn"]]) %>%
      dplyr::bind_rows(pred.err)
  }
  pred.err.bh <- pred.err
  # pull outcomes
  pred.err <- dplyr::bind_rows(pred.err.gm, pred.err.bh)
  
  pred.err.ens <- pred.err %>% dplyr::mutate(run=rr) %>% dplyr::bind_rows(pred.err.ens)
  
# plot
ggplot2::ggplot(pred.err.ens %>%
                  dplyr::mutate(sq.err = err^2) %>%
                  dplyr::group_by(wn,run) %>%
                  dplyr::summarise(rmse=sqrt(mean(sq.err)))) +
  ## geom_boxplot(aes(wn,rmse)) +
  ggplot2::geom_point(ggplot2::aes(wn,rmse)) +
  ggplot2::geom_point(data=pred.err.ens %>%
                        dplyr::mutate(sq.err = err^2) %>%
                        dplyr::group_by(wn,run) %>%
                        dplyr::summarise(rmse=sqrt(mean(sq.err))) %>%
                        dplyr::group_by(wn) %>%
                        dplyr::summarise(md.rmse=median(rmse)), ggplot2::aes(wn,md.rmse), col=2) +
  ggplot2::ylab("RMSE") + ggplot2::xlab("Recruitment model") +
  ggplot2::theme_bw()


  
}





  
 
  
  
 
  
  


