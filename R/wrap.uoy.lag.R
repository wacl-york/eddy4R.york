#' UoY Lag definition wrapper
#'
#' Wrapper for def.lag to use in UoY workflows. Returns data lag times and acf information
#'
#' @param eddy.data eddy.data object
#' @param para parameter list from \code{def.uoy.para}
#' @param agg_count # to use when implementing drifting lags - to do

wrap.uoy.lag = function(eddy.data,para,agg_count){

  dum_run <- 0
  for(var in  para$cross_correlation_vars) {
    dum_run <- dum_run + 1
    if(para$determine_lag) {
      if(para$lag_type == "ccf"){
        lagged <- eddy4R.base::def.lag(refe=eddy.data$w_met,
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
      }
      if(para$lag_type == "fft"){
        lag_data = stats::na.omit(eddy.data[,c("w_met",var)])
        lagged = def.lag.fft(lag_data[,"w_met"],lag_data[,var],trim = 40*para$freqIN,lagNgtvPstv = para$lagNgtvPstv)
      }

      if(para$restrict_lag_range){
        if(class(para$lag_boundary) == "numeric"){# lag_boundary only contains one pair of values
          if(lagged$lag/para$freqIN < min(para$lag_boundary) | lagged$lag/para$freqIN > max(para$lag_boundary))
            lagged$lag <- as.numeric(para$absolute_lag[dum_run])*para$freqIN
        }else{# if longer, use the flux aggregation period counter - dum_run - to select the value
          if(lagged$lag/para$freqIN < min(para$lag_boundary[[dum_run]]) | lagged$lag/para$freqIN > max(para$lag_boundary[[dum_run]]))
            lagged$lag <- as.numeric(para$absolute_lag[dum_run])*para$freqIN
        }
      }

    } else {
      lagged <- list()
      lagged$lag <- para$absolute_lag[dum_run] * para$freqIN
      lagged$corrCros <- NA # set corrCros to NA as def.lag has not been used. prevent error when writing lag times
    }


    # peform lag operation
    if(!is.na(lagged$lag))
      eddy.data[[var]] <- DataCombine::shift(VarVect = eddy.data[[var]],shiftBy = -lagged$lag, reminder = FALSE)

    #create outputs
    if(dum_run == 1) {
      dum_lag <- lagged$lag / para$freqIN
      dum_corrCros <- lagged$corrCros
      if (para$determine_lag) {
        if(para$lag_type == "ccf" & class(lagged$corr) == "acf"){
          ACF = data.frame(date = eddy.data$date[1],lag = lagged$corr$lag,acf = lagged$corr$acf)
        }
        if(para$lag_type == "fft"){
          ACF = data.frame(date = eddy.data$date[1],lagged$corr)
        }
      }
    } else {
      dum_lag <- c(dum_lag, lagged$lag / para$freqIN)
      dum_corrCros <- c(dum_corrCros, lagged$corrCros)
      if(para$determine_lag) {
        if(para$lag_type == "ccf" & class(lagged$corr) == "acf"){
          ACF = cbind(ACF,lagged$corr$acf)
        }
        if(para$lag_type == "fft"){
          ACF = dplyr::left_join(ACF,lagged$corr,"index")
        }
      }
    }
  }

  lag_time <- data.frame(dum_corrCros,dum_lag) %>%
    cbind(para$cross_correlation_vars)

  #handle lagging no and noc channels of nox data separatly before combining into NO and NO2
  if(para$noc_lag & sum(c("FD_mole_NO","FD_mole_NO2") %in% para$flux_species_mole) == 2){
    eddy.data$FD_mole_NO2 = (eddy.data$FD_mole_NO2 - eddy.data$FD_mole_NO)/eddy.data$ce
    eddy.data$ce = NULL

    # Change behaviour of rowSums from
    # n + NA = NA
    # to
    # n + NA = n
    myrowSums = function(df,missing = -99999,...){
      df$new = Mod(missing)*-1 # guarentee no data flag is negative
      df$new = rowSums(df,...)
      df$new[df$new == missing] = NA
      # Return
      df$new+Mod(missing)
    }

    # Create NOXASNO2 - this is not valid for flux, but used for stationarity statistics that apply to the NOx concentrations
    eddy.data$FD_mole_NOXASNO2 = myrowSums(eddy.data[,c("FD_mole_NO","FD_mole_NO2")],na.rm = T)

    para$species = c(para$species,"NOXASNO2")
    para$flux_species_mole = def.spcs.name(para$species,"mole")
    para$flux_species_mass = def.spcs.name(para$species,"mass")
    para$flux_species_kin = def.spcs.name(para$species,"kin")
    para$species_RMM[[(length(para$species_RMM)+1)]] = eddy4R.base::IntlNatu$MolmNO2
    names(para$species_RMM)[length(para$species_RMM)] = "NOXASNO2"
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

