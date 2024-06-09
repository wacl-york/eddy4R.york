#' UoY Lag definition wrapper
#'
#' Wrapper for def.lag to use in UoY workflows. Returns data lag times and acf information
#'
#' @param eddy.data eddy.data object
#' @param para parameter list from \code{def.uoy.para}
#' @param agg_count # to use when implementing drifting lags - to do
#'
#' @export

wrap.uoy.lag = function(eddy.data,para,agg_count){

  dum_run <- 0
  for(var in  para$cross_correlation_vars) {
    dum_run <- dum_run + 1
    if(para$determine_lag) {
        lagged <- eddy4R.base::def.lag(refe=eddy.data$veloXaxs,
                                       meas=eddy.data[,var],
                                       dataRefe=eddy.data,
                                       lagMax=40*para$freqIN,
                                       lagCnst=TRUE,
                                       lagNgtvPstv=para$lagNgtvPstv,
                                       lagAll=TRUE,
                                       freq=para$freqIN,
                                       hpf=TRUE,
                                       plot = para$plot_acf,
                                       DirPlot = paste(para$DirOut,"/",para$analysis,sep=""))

        if(para$restrict_lag_range){
          if(lagged$lag/para$freqIN < min(para$lag_boundary[[dum_run]]) | lagged$lag/para$freqIN > max(para$lag_boundary[[dum_run]])){
            lagged$lag <- as.numeric(para$absolute_lag[dum_run])*para$freqIN}
        }

    } else {
      lagged <- list()
      lagged$lag <- para$absolute_lag[dum_run] * para$freqIN
      lagged$corrCros <- NA # set corrCros to NA as def.lag has not been used. prevent error when writing lag times
    }

    # peform lag operation
    if(!is.na(lagged$lag)){
      eddy.data[[var]] <- DataCombine::shift(VarVect = eddy.data[[var]],shiftBy = -lagged$lag, reminder = FALSE)
    }

    #create outputs
    if(dum_run == 1) {
      dum_lag <- lagged$lag / para$freqIN
      dum_corrCros <- lagged$corrCros
      if (para$determine_lag) {
        if(para$lag_type == "ccf" & class(lagged$corr) == "acf"){
          ACF = data.frame(date = eddy.data$date[1],lag = lagged$corr$lag,acf = lagged$corr$acf)
        }

      }
    } else {
      dum_lag <- c(dum_lag, lagged$lag / para$freqIN)
      dum_corrCros <- c(dum_corrCros, lagged$corrCros)
      if(para$determine_lag) {
        if(para$lag_type == "ccf" & class(lagged$corr) == "acf"){
          ACF = cbind(ACF,lagged$corr$acf)
        }
      }
    }
  }

  lag_time <- data.frame(dum_corrCros,dum_lag) %>%
    cbind(para$cross_correlation_vars)

  #handle lagging no and noc channels of nox data separatly before combining into NO and NO2
  if(para$noc_lag & sum(c("ratioMoleDryNO","ratioMoleDryNO2") %in% para$speciesRatioName) == 2){
    eddy.data$ratioMoleDryNO2 = (eddy.data$ratioMoleDryNO2 - eddy.data$ratioMoleDryNO)/eddy.data$ce

  }

  if(para$determine_lag & class(lagged$corr) == "acf"){
    names(ACF) = c("date","lag",para$cross_correlation_vars)
  } else {
    ACF = NULL
  }

  ret = list(eddy.data = eddy.data,
             lag_time = lag_time,
             ACF = ACF,
             para = para)

  #return
  ret
}

