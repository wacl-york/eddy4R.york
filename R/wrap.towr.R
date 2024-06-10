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
  REYN$stat = eddy4R.turb::def.stna(data=eddy.data,
                                    MethStna=c(1, 2, 3)[3],
                                    NumSubSamp=para$agg_period/300,
                                    corTempPot=FALSE,
                                    whrVar = para$stnaVar,
                                    presTempPot=eddy4R.base::IntlNatu$Pres00,
                                    PltfEc=para$PltfEc,
                                    vrbs = F,
                                    flagCh4 = F, # Pass these to ... for REYNflux_FD_mole_dry
                                    spcs = para$species,
                                    rmm = para$species_RMM,
                                    tempHead = para$tempHead)


  #--------------------------------------------------------------------------------------------
  # integral turbulence scales, lengths and characteristics
  # called from global enviroment until pacakged
  REYN$isca = def.scales(REYN = REYN,
                         lat = para$Lat,
                         VarInp=c("veloXaxs", "veloZaxs", "temp", "all")[4],
                         sd = data.frame(u_hor=REYN$sd$u_hor,w_hor=REYN$sd$w_hor,T_air=REYN$sd$T_air),
                         varScal = data.frame(u_star=REYN$mn$u_star,T_star_SL=REYN$mn$T_star_SL),
                         species = para$species)

  #--------------------------------------------------------------------------------------------
  # flux error calculations
  REYN$error = eddy4R.turb::def.ucrt.samp(data = NULL,
                                          distIsca=REYN$isca,
                                          valuMean=REYN$mn,
                                          coefCorr=REYN$cor,
                                          distMean=REYN$max$d_xy_flow,
                                          timeFold = 0,
                                          spcs = para$species)

  #--------------------------------------------------------------------------------------------
  # flux limit of detection calculations

  REYN$lod  <-  def.lod(ref=REYN$imfl$w_hor,
                        vars=REYN$imfl %>% dplyr::select(.,c(tidyselect::contains("FD_mole_"),"T_air")),
                        spcs=para$species,
                        rmm=para$species_RMM,
                        rho_dry=REYN$mn$rho_dry,
                        rho_H2O=REYN$mn$rho_H2O,
                        Lv=REYN$mn$Lv,
                        freq=para$freq,
                        conf=95)

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
