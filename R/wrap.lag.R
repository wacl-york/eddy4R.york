#' UoY Lag definition wrapper
#'
#' Wrapper for def.lag to use in UoY workflows. Returns data lag times and acf information
#'
#' @param eddy.data eddy.data object
#' @param para parameter list from \code{def.uoy.para}
#'
#' @export

wrap.lag = function(eddy.data,para){

  lagged <- purrr::pmap(list(a = para$lagVars,
                             b = para$lagRangeLimit,
                             c = para$lagDefaults),
                        function(a,b,c){
                          lagged = eddy4R.base::def.lag(refe=eddy.data$veloXaxs,
                                                        meas=eddy.data[,a],
                                                        dataRefe=eddy.data,
                                                        lagMax=40*para$freq,
                                                        lagCnst=TRUE,
                                                        lagNgtvPstv=para$lagNgtvPstv,
                                                        lagAll=TRUE,
                                                        freq=para$freq,
                                                        hpf=TRUE)

                          if(para$lagApplyRangeLimit){
                            if(lagged$lag/para$freq < min(b) | lagged$lag/para$freq > max(b)){
                              lagged$lag <- as.numeric(c)*para$freq
                            }
                          }

                          list(lag = lagged$lag,
                               corrCross =lagged$corrCros,
                               corr = lagged$corr)

                        })

  lagTimes = tibble::tibble(name = para$lagVars,
                            lagTime = purrr::map_dbl(lagged, purrr::pluck("lag")),
                            corr = purrr::map_dbl(lagged, purrr::pluck("corrCross")))

  ACF = purrr::map2_df(lagged,
                       para$lagVars,
                       ~{
                         dat = purrr::pluck(.x, "corr")

                         tibble::tibble(date = eddy.data$date[1],
                                        lag = dat$lag,
                                        acf = dat$acf,
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
  if(para$lagNOc & sum(c("rtioMoleDryNO","rtioMoleDryNO2") %in% para$speciesRatioName) == 2){
    eddy.data$rtioMoleDryNO2 = (eddy.data$rtioMoleDryNO2 - eddy.data$rtioMoleDryNO)/eddy.data$ce

  }

  ret = list(eddy.data = eddy.data,
             lagTimes = lagTimes,
             ACF = ACF)

  #return
  ret
}

