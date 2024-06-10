#' Wrap Tower
#'
#' University of York Tower Eddy-Covariance turbulence processing.
#' Based on earlier workflows with contributions from: \cr
#' Adam R Vaughan \cr
#' Stefan Metzger \cr
#' Ke Xu \cr
#' Andrei Serafimovich \cr
#'
#'
#' @param eddy.data input data
#' @param para params list
#' @param verbose boolean, suppresses some chatty functions
#' @param progress_bar progress bar object
#' @param thshFile The file directory where the threshold table are being saved. Default as NULL.
#' @param diagSens Logical to state if the sensor diqgnostic flags are calculated. Default as FALSE.
#' @param agg_period from \code{def.avg} containing all the aggregation period defintions.
#' TODO is this actually needed or can we just get this from para?
#'
#' @author W. S. Drysdale
#'
#' @export


wrap.towr <- function(
  eddy.data,
  para,
  verbose = FALSE,
  progress_bar,
  agg_period,
  thshFile = NULL,
  diagSens = FALSE){

  # satisfy CMD Check
  . = NULL

  # Setup
  REYN = list()
  lag_out = list()

  #--------------------------------------------------------------------------------------------
  # Apply Anemometer Corrections
  eddy.data = eddy4R.york::wrap.anem.cor(eddy.data,para)

  #--------------------------------------------------------------------------------------------
  # Despike data before lag correction
  if(para$despike){
    eddy.data = eddy4R.york::wrap.despike(eddy.data = eddy.data,
                                          despike_vars = para$despike_vars,
                                          despike_threshold = para$despike_threshold,
                                          verbose)

  }

  #--------------------------------------------------------------------------------------------
  # Maximize cross correlation
  if(para$lag_correction){
    lag_out = eddy4R.york::wrap.lag(eddy.data,para)
    eddy.data = lag_out$eddy.data

  }

  #--------------------------------------------------------------------------------------------
  # Handle missing values
  eddy.data = eddy4R.york::def.miss.hndl(eddy.data,para)

  #--------------------------------------------------------------------------------------------
  # Rotation of wind vectors
  eddy.data = eddy4R.york::wrap.rot(data = eddy.data,
                                    MethRot = para$MethRot,
                                    plnrFitCoef = para$plnrFitCoef,
                                    plnrFitType = para$plnrFitType)




  # sort units
  eddy.data = eddy4R.york::assign_input_units(eddy.data, para)

  #--------------------------------------------------------------------------------------------
  # calculate time-domain fluxes (classical EC)
  REYN = eddy4R.turb::wrap.flux(data = eddy.data,
                                AlgBase = para$AlgBase,
                                ListGasSclr = para$ListGasSclr)


  #--------------------------------------------------------------------------------------------
  # stationarity testing
  REYN$stat = eddy4R.turb::def.stna(data=REYN$data,
                                    MethStna=c(1, 2, 3)[3],
                                    NumSubSamp=para$agg_period/300,
                                    corTempPot=FALSE,
                                    whrVar = para$stnaVar,
                                    presTempPot=eddy4R.base::IntlNatu$Pres00,
                                    PltfEc=para$PltfEc,
                                    vrbs = F,
                                    ListGasSclr = para$ListGasSclr)


  # ITC ---------------------------------------------------------------------
  REYN$itc <- eddy4R.turb::def.itc(stblObkv = REYN$mean$paraStbl,
                                   lat = para$Lat,
                                   VarInp = "all",
                                   sd = data.frame(
                                     u_hor = REYN$sd$veloXaxsHor,
                                     w_hor = REYN$sd$veloZaxsHor,
                                     T_air = REYN$sd$tempAir),
                                   varScal = data.frame(
                                     u_star = REYN$mean$veloFric,
                                     T_star_SL = REYN$mean$tempScalAtmSurf),
                                   CorTemp = FALSE)


  # Length Scales -----------------------------------------------------------
  REYN$isca = eddy4R.york::wrap.isca(REYN,
                                     species = para$species,
                                     speciesRatioName = para$speciesRatioName,
                                     PltfEc = para$PltfEc)

  #--------------------------------------------------------------------------------------------
  # flux error calculations
  REYN$error = eddy4R.turb::def.ucrt.samp(data = NULL,
                                          distIsca=REYN$isca,
                                          valuMean=REYN$mean,
                                          coefCorr=REYN$corr,
                                          distMean=mean(REYN$data$unixTime-min(REYN$data$unixTime)),
                                          timeFold = 0,
                                          spcsNameRatio = para$speciesRatioName,
                                          spcsNameFlux = paste0("flux", para$species)
  )

  #--------------------------------------------------------------------------------------------
  # flux limit of detection calculations

  REYN$lod  <-  def.lod(REYN,
                        measCol = para$cross_correlation_vars,
                        freq = para$freq)

  REYN$lod$date <- REYN$mn$date


  #--------------------------------------------------------------------------------------------
  ## Write output

  # Tidy file name
  fn = agg_period[file_count,1] %>%
    as.character() %>%
    stringr::str_replace_all(":","") %>%
    stringr::str_replace_all("-","") %>%
    stringr::str_replace_all(" ","_")

  if(nchar(fn) <= 8){
    fn = paste0(fn,"_000000")
  }

  # write classical
  write.REYN(REYN = REYN,
             lag_time = lag_out$lagTimes,
             ACF = lag_out$ACF,
             para = para,
             file_name = fn)


}
