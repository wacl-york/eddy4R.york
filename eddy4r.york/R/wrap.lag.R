#' UoY Lag definition wrapper
#'
#' Wrapper for def.lag to use in UoY workflows. Returns data lag times and acf information
#'
#' @param eddy.data eddy.data object
#'
#' @inheritParams def.para
#'
#' @export

wrap.lag = function(eddy.data,
                    lagVars,
                    lagApplyRangeLimit,
                    lagRangeLimit,
                    lagDefaults,
                    lagNOc,
                    freq,
                    speciesRatioName,
                    lagNgtvPstv){

  lagged <- purrr::pmap(list(a = lagVars,
                             b = lagRangeLimit,
                             c = lagDefaults),
                        function(a,b,c){
                          lagged = eddy4R.base::def.lag(refe=eddy.data$veloZaxs,
                                                        meas=eddy.data[,a],
                                                        dataRefe=eddy.data,
                                                        lagMax=40*freq,
                                                        lagCnst=TRUE,
                                                        lagNgtvPstv=lagNgtvPstv,
                                                        lagAll=TRUE,
                                                        freq=freq,
                                                        hpf=TRUE)

                          if(lagApplyRangeLimit){
                            # if the determined lag has fallen outside of the range
                            if(lagged$lag/freq < min(b) | lagged$lag/freq > max(b)){

                              # If a lag default has been supplied, then use that
                              if(!is.na(c)){
                                lagged$lag <- as.numeric(c)*freq
                              }else{ # otherwise, lets pick the best lag in the given window
                                # filter the ACF between the supplied lag ranges
                                tempDat = data.frame(
                                  lag = lagged$corr$lag[,,1],
                                  acf = lagged$corr$acf[,,1]
                                ) |>
                                  dplyr::filter(
                                    dplyr::between(.data$lag/freq, min(b), max(b))
                                    )
                                # find the maximum acf in that range
                                lagged$lag = tempDat$lag[which(abs(tempDat$acf) == max(abs(tempDat$acf)))]
                              }
                            }
                          }

                          list(lag = lagged$lag,
                               corrCross = lagged$corrCros,
                               corr = lagged$corr)

                        })

  lagTimes = tibble::tibble(name = lagVars,
                            lagTime = purrr::map_dbl(lagged, purrr::pluck("lag")),
                            corr = purrr::map_dbl(lagged, purrr::pluck("corrCross")))

  ACF = purrr::map2_df(lagged,
                       lagVars,
                       ~{
                         dat = purrr::pluck(.x, "corr")

                         tibble::tibble(date = eddy.data$date[1],
                                        lag = dat$lag[ , 1, 1],
                                        acf = dat$acf[ , 1, 1],
                                        name = .y)
                       }
  )

  # peform lag operation
  for(i in 1:nrow(lagTimes)){
    if(!is.na(lagTimes$lagTime[i])){
      eddy.data[[lagTimes$name[i]]] <- DataCombine::shift(VarVect = eddy.data[[lagTimes$name[i]]],shiftBy = -lagTimes$lagTime[i], reminder = FALSE)
    }
  }

  #handle lagging no and noc channels of nox data separatly before combining into NO and NO2
  if(lagNOc & sum(c("rtioMoleDryNO","rtioMoleDryNO2") %in% speciesRatioName) == 2){
    eddy.data$rtioMoleDryNO2 = (eddy.data$rtioMoleDryNO2 - eddy.data$rtioMoleDryNO)/eddy.data$ce
    eddy.data$ce = NULL # remove CE after calculation so we don't into unit errors in def.stat.sta.diff()
  }

  ret = list(eddy.data = eddy.data,
             lagTimes = lagTimes,
             ACF = ACF)

  #return
  ret
}

