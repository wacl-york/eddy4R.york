#' Flux Limit of Detection
#' 
#' Langford et al., 2015 Eddy-covariance data with low signal-to-noise ratio method
#' of flux Limit of detection. Based on original implementation by Adam Vaughan
#' 
#' @param REYN eddy4R REYN list - requires mean and diff
#' @param measCol columns to calculate LOD for
#' @param freq measurement frequency
#' @param refCol column to lag against, default veloZaxs
#' 
#' @export

def.lod = function(REYN,
                   measCol,
                   freq,
                   refCol = "veloXaxs"){
  
  # Satisfy CMD Check
  lag = conf = NULL
  
  
  lods = vector(mode = "numeric", length = length(measCol)+1) %>% 
    stats::setNames(c(measCol, "conf"))
  
  for(var in names(measCol)){
    
    #Langford et al., (2015), Eddy-covariance data with low signal-to-noise ratio, page 4201
    spec <- stats::ccf(x = REYN$diff[[refCol]],
                       y = REYN$diff[[var]],
                       type="covariance",
                       lag.max=180*freq,
                       plot=F)
    
    specDf <- data.frame(lag=spec$lag,
                         acf=spec$acf) %>% 
      dplyr::filter(lag > 150*freq | lag < -150*freq)
    
    er <- stats::sd(specDf$acf,na.rm=T)
 
    if(var %in% c("rtioMoleDryH2o", "tempAir")){
      # Latent Heat
      if(var == "rtioMoleDryH2o"){
        lods[var] <- REYN$mean$densMoleAirDry * er
      }
      # Sensible Heat
      if(var == "tempAir"){
        lods[var] <- er
        
      }
    }
    
    if(length(measCol) > 2){
      lods[var] =  REYN$mean$densMoleAirDry * er
    }
    
  }
  
  output = as.data.frame(
    t(cbind(
      lods*1.96,
      lods*3
    )))
  
  output$conf = c(95, 99)
  
  #
  
  tidyr::pivot_longer(output, -conf)
  
}

