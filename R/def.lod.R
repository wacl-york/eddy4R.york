#' Function definition. Flux limit of detection.
#' @author
#' Adam R Vaughan \email{adam.vaughan@york.ac.uk}
#' @description Function definition. Warpper for Wavelet Eddy-covariance averaging.
#' @param ref Reference instantaneous data, e.g. w_hor.
#' @param vars Data.frame of instantaneous data for LOD calculation.
#' @param spcs Chemical species.
#' @param rmm Relative molecular masses of chemical species in kg mol-1.
#' @param rho_dry Dry air density in mol m-3.
#' @param rho_H2O Partial density of H2O.
#' @param Lv latent heat of vaporization.
#' @param freq Data frequency in Hz.
#' @param conf Confidence interval, either 95 or 99%.
#' @export

def.lod <- function(ref,vars,spcs,rmm,rho_dry,rho_H2O,Lv,freq,conf=95){

  # Satisfy CMD Check
  . = lag = sd = NULL

  #LOD export list
  export <- data.frame(matrix(ncol = ncol(vars), nrow = 1))
  colnames(export) <- names(vars)

  for(dd in names(vars)){

    #Langford et al., (2015), Eddy-covariance data with low signal-to-noise ratio, page 4201
    spec <- stats::ccf(ref,vars[[dd]],type="covariance",lag.max=180*freq,plot=F)
    spec <- data.frame(lag=spec$lag,acf=spec$acf)
    spec2 <- spec %>%
      dplyr::filter(lag > (150*freq) & lag < (180*freq))

    spec3 <- spec %>%
      dplyr::filter(lag > ((-180)*freq) & lag < ((-150)*freq))

    spec4 <- spec2 %>%
      rbind(spec3)

    er <- sd(spec4$acf,na.rm=T)

    if(conf==95){lod <- 1.96*er}
    if(conf==99){lod <- 3*er}

    #CHEMICAL FLUX LODs
    if(!dd=="FD_mole_H2O")
      if(!dd=="T_air"){
        export[,paste0("F_",dd %>% gsub("FD_mole_","",.),"_kin")] <- rho_dry * lod
        export[,paste0("F_",dd %>% gsub("FD_mole_","",.),"_mass")] <- export[,paste0("F_",dd %>%
                                                                                       gsub("FD_mole_","",.),"_kin")] *
          1e6 * 3600 * rmm[which(spcs==(dd %>% gsub("FD_mole_","",.)))]

        #attach correct units
        attr(export[,paste0("F_",dd %>% gsub("FD_mole_","",.),"_kin")],"unit") <- "mol m-2 s-1"
        attr(export[,paste0("F_",dd %>% gsub("FD_mole_","",.),"_mass")],"unit") <- "mg m-2 h-1"}

    #SENSIBLE HEAT FLUX LODs
    if(dd=="FD_mole_H2O"){
      export$F_LE_kin <- rho_dry * lod
      export$F_LE_en <- Lv * eddy4R.base::IntlNatu$MolmH2o * export$F_LE_kin

      #attach correct units
      attr(export$F_LE_kin,"unit") <- "mol m-2 s-1"
      attr(export$F_LE_en,"unit") <- "W m-2"}



    #LATENT HEAT FLUX LODs
    if(dd=="T_air"){
      export$F_H_kin <- lod
      export$F_H_en <- (eddy4R.base::IntlNatu$CpDry * rho_dry * eddy4R.base::IntlNatu$MolmDry +
                          eddy4R.base::IntlNatu$CpH2o * rho_H2O * eddy4R.base::IntlNatu$MolmH2o) * export$F_H_kin

      #attach correct units
      attr(export$F_H_kin,"unit") <- "K m s-1"
      attr(export$F_H_en,"unit") <- "W m-2"}
  }

  #clean up data.frame
  export <- export[,-c(1:ncol(vars))] %>% data.frame()

  #reutn data
  return(export)
}
