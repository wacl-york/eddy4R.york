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
#' @param file_count index of the loop in the above function (e.g uoy.towr.ec) for use in the progress bar
#' @param skip_scalar character vector of any scalars to be skipped for this file only
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
  file_count,
  skip_scalar,
  verbose = FALSE,
  progress_bar,
  agg_period,
  thshFile = NULL,
  diagSens = FALSE){

  # satisfy CMD Check
  . = NULL

  # Setup
  error_list = list()
  REYN = list()
  lag_out = list()

  #add missing H2O
  if(!"ratioMoleDryH2o" %in% names(eddy.data))
    eddy.data$ratioMoleDryH2o <- 1e-12

  #assign eddy.data to be used in qfqm processing (not skip any missing scalar)
  eddyInp <- eddy.data
  #assign species name before skipping (will be used in qfqm processing)
  qfPara <- list()
  qfPara$species <- para$species
  progress_bar$pb$tick(tokens = list(file = file_count,
                                     tfile = progress_bar$total_file,
                                     praise = progress_bar$some_praise))
  # if optional scalars have been deemed invalid by def.vaild.input, recreate the relevent para entires to skip them this time
  if(!is.null(skip_scalar)){
    if(length(skip_scalar) > 0){
      keepInt <- which(!para$cross_correlation_vars %in% def.spcs.name(skip_scalar, "mole")) #changed to make sure absolute lags match up with correct species

      para$absolute_lag = para$absolute_lag[keepInt]
      para$despike_threshold = para$despike_threshold[!para$despike_vars %in% def.spcs.name(skip_scalar,"mole")]
      para$despike_vars = para$despike_vars[!para$despike_vars %in% def.spcs.name(skip_scalar,"mole")]
      para$cross_correlation_vars = para$cross_correlation_vars[!para$cross_correlation_vars %in% def.spcs.name(skip_scalar,"mole")]

      all_species = para$species
      para$species = para$species[!para$species %in% skip_scalar]
      if(length(para$species) == 0){
        para$flux_species_mole = NULL
        para$flux_species_mass = NULL
        para$flux_species_kin = NULL
        para$species_RMM = NULL
      }else{
        para$flux_species_mole = def.spcs.name(para$species,"mole")
        para$flux_species_mass = def.spcs.name(para$species,"mass")
        para$flux_species_kin = def.spcs.name(para$species,"kin")
        para$species_RMM = para$species_RMM[!all_species %in% skip_scalar]
      }

      for(var in def.spcs.name(skip_scalar,"mole"))
        eddy.data[,var] = NULL
    }
  }

  #Apply plausibility function
  #Plausibility testing function
  if (!is.null(thshFile)){

    qfInp <- wrap.qf.voc(data = eddyInp, thshFile = thshFile)

    #remove bad data
    #List of variables to check for flags to remove bad data
    listVar <- names(qfInp)[names(qfInp) %in% names(eddy.data)]
    #temporary place for qfInp will be use in bad data remove
    qfTmp <- qfInp
    if (length(listVar) > 0){
      #Replace the flagged data with NaN
      # TODO What happens if a critical value has all of its data removed?
      # Maybe another call to def.valid.input is required, and another stage of removing flagged skip_scalars
      # Should also include the skip_scalar block being converted to a function.
      for (idxVar in listVar){
        setBad <-  base::which(base::rowSums(qfTmp[[idxVar]] == 1) > 0)
        #Remove the data for each variable according to the position vector identified
        eddy.data[[idxVar]][setBad] <- NA
      }
    } else {
      eddy.data <- eddy.data
    }
  }
  rm(qfTmp)

  #--------------------------------------------------------------------------------------------
  # Apply Anemometer Corrections
  eddy.data = wrap.anem.cor(eddy.data,para)

  #--------------------------------------------------------------------------------------------
  # Despike data before lag correction
  if(para$despike){
    eddy.data = wrap.uoy.despike(eddy.data = eddy.data,
                                 despike_vars = para$despike_vars,
                                 despike_threshold = para$despike_threshold,
                                 verbose)

  }

  #--------------------------------------------------------------------------------------------
  # Maximize cross correlation
  if(para$lag_correction){

    lag_out = wrap.uoy.lag(eddy.data,para,file_count)
    eddy.data = lag_out$eddy.data
    para = lag_out$para

  }


  #--------------------------------------------------------------------------------------------
  # Handle missing values
  eddy.data = def.miss.hndl(eddy.data,para)

  #--------------------------------------------------------------------------------------------
  # Rotation of wind vectors
  eddy.data = wrap.rot(eddy.data,
                       MethRot = para$MethRot,
                       plnrFitCoef = para$plnrFitCoef,
                       plnrFitType = para$plnrFitType)



  #--------------------------------------------------------------------------------------------
  # calculate time-domain fluxes (classical EC)
  REYN = eddy4R.turb::wrap.flux(data = eddy.data,
                                AlgBase = para$AlgBase,
                                ListGasSclr = para$ListGasSclr)

  #--------------------------------------------------------------------------------------------
  #determine qfqm
  if (!is.null(thshFile)) {
    #initial define name of variable will detemine the flag
    dp01TmpName <- c(names(eddyInp), "u_hor", "v_hor", "w_hor", "p_H2O", "rho_H2O", "rho_air",
                     "rho_dry", "T_v", "Lv", "cph", "cvh", "Rh", "Kah", "T_air_0", "T_v_0", "PSI_uv",
                     "u_star2_x", "u_star2_y", "u_star", "F_H_kin",
                     "F_H_en", "F_H_kin_v_0", "F_H_en_v_0", "F_LE_kin", "F_LE_en",
                     paste0("F_", qfPara$species, "_kin"), paste0("F_", qfPara$species, "_mass"),
                     "I", "d_L_v_0", "sigma", "w_star", "t_star", "T_star_SL", "T_star_ML",
                     "ratioMoleDryH2o_star_SL", "ratioMoleDryH2o_star_ML")

    qfOut <- eddy4R.qaqc::wrap.dp01.qfqm.eddy(qfInp = qfInp, MethMeas = "voc", RptExpd = TRUE, dp01 = dp01TmpName, qfSens = NULL)

    #adding timestamp
    for (idxDf in names(qfOut)){
      if (idxDf == "qm"){
        lapply(names(qfOut[[idxDf]]), function(x) {
          qfOut[[idxDf]][[x]] <<- cbind(date = REYN$mn$date, qfOut[[idxDf]][[x]])
        })#end lapply
      } else {
        qfOut[[idxDf]] <- cbind(date = REYN$mn$date, qfOut[[idxDf]])
      }
    }
  }

  #--------------------------------------------------------------------------------------------
  # stationarity testing
  # What should have stationarity tests applied?
  whr_flux = c("u_star2_x", "u_star2_y", "u_star", "F_H_en")
  if("FD_mole_H2O" %in% names(eddy.data))
    whr_flux = c(whr_flux,"F_LE_en")

  if(!is.null(para$species))
    whr_flux = c(whr_flux,para$flux_species_mass)


  REYN$stat = eddy4R.turb::def.stna(data=eddy.data,
                                    MethStna=c(1, 2, 3)[3],
                                    NumSubSamp=para$agg_period/300,
                                    corTempPot=FALSE,
                                    whrVar = whr_flux,
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
  #combine stationarity flags and ITCs flags into qfFinl
  if (!is.null(thshFile)) {

    # Save the current qfFinl values separatley
    qfOut$qfFinl_raw = qfOut$qfFinl


    # qf stat flag
    lapply(names(qfOut$qfFinl), function(x)  {
      if (x %in% names(REYN$stat$qf)) {
        qfOut$qfFinl[[x]] <<- ifelse(REYN$stat$qf[[x]] == 0 & qfOut$qfFinl[[x]] == 0, 0, 1)}


    })

    # create sans flag before applying ITC rules
    qfOut$qfFinl_sansITC = qfOut$qfFinl

    # add ITC to qfFinl
    lapply(names(qfOut$qfFinl), function(x)  {
      if (x %in% names(REYN$isca$qfItc)) {
        qfOut$qfFinl[[x]] <<- ifelse(REYN$isca$qfItc[[x]] == 0 & qfOut$qfFinl[[x]] == 0, 0, 1)}

    })

  }

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
                        vars=REYN$imfl %>% dplyr::select(.,c(dplyr::contains("FD_mole_"),"T_air")),
                        spcs=para$species,
                        rmm=para$species_RMM,
                        rho_dry=REYN$mn$rho_dry,
                        rho_H2O=REYN$mn$rho_H2O,
                        Lv=REYN$mn$Lv,
                        freq=para$freqOUT,
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

  if(nchar(fn) <= 8)
    fn = paste0(fn,"_000000")

  # determine if qf will write out
  if (is.null(thshFile))
    qfOut <- NULL
  else
    qfOut <- qfOut

  # write classical
  write.REYN(REYN = REYN,
             lag_time = lag_out$lag_time,
             ACF = lag_out$ACF,
             para = para,
             file_name = fn,
             qfOut = qfOut)


}
