#' Wrapper Integral Length Scales
#'
#' Wraps \code{def.dist.isca()}
#'
#' @param REYN requires REYN$data and REYN$diff
#' @param species species names
#' @param speciesRatioName species mixing ration column names
#' @param PltfEc platform - defined scalEddy for \code{def.dist.isca()} - currently only supports "towr"
#'
#' @export
#'

wrap.isca = function(REYN,
                     species,
                     speciesRatioName,
                     PltfEc){

  whr_scal = c("veloXaxs","veloYaxs",
               "veloZaxs","tempAir",
               "tempVirt","tempPot00",
               "rtioMoleDryH2o")

  whr_flux = c("veloFricXaxsSq","veloFricYaxsSq",
               "veloFric","fluxTempEngy",
               "fluxH2oEngy")

  if(!is.null(species)){

    whr_scal = c(whr_scal, speciesRatioName)
    whr_flux = c(whr_flux, paste0("flux",species))

  }

  if(PltfEc == "towr"){
    scalEddy = REYN$base$unixTime-min(REYN$data$unixTime)
  }else{
    stop("wrap.isca only supports PltfEc == 'towr' for now")
  }

  isca_vari <- data.frame(
    t(
      sapply(
        whr_scal,
        function(x) eddy4R.turb::def.dist.isca(scalEddy,
                                               data=REYN$diff[,x]^2,
                                               veloXaxs = REYN$data$veloXaxs))))

  isca_scal <- data.frame(
    t(
      sapply(
        whr_scal,
        function(x) eddy4R.turb::def.dist.isca(scalEddy,
                                               data=REYN$diff[,x],
                                               veloXaxs = REYN$data$veloXaxs))))


  whr_not <- sapply(whr_flux, function(x) length(which(is.na(REYN$diff[,x]))) / nrow(REYN$diff))
  whr_not <- which(whr_not > 0.1)

  if(length(whr_not) == 0){
    whr_flux <-  whr_flux
  }else{
    whr_flux <-  whr_flux[-whr_not]
  }

  isca_flux <- data.frame(
    t(
      sapply(
        whr_flux,
        function(x) eddy4R.turb::def.dist.isca(scalEddy,
                                               data=REYN$diff[,x],
                                               veloXaxs = REYN$data$veloXaxs))))

  list(scal=isca_scal,
       vari=isca_vari,
       flux=isca_flux)
}
