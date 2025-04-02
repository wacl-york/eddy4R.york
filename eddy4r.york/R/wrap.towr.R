#' Tower Wrapper
#'
#' Handles file reading, progress and error logging.
#'
#' @param paraMain parameters list made by \code{def.para()}
#' @param resume loop iterator to restart at.
#' @param thshFile The file directory where the threshold table are being saved. Default as NULL.
#' @param diagSens Logical to state if the sensor diqgnostic flags are calculated. Default as FALSE.
#'
#' @author W. S. Drysdale
#'
#' @export

wrap.towr = function(paraMain,
                     resume = NULL,
                     thshFile = NULL,
                     diagSens = FALSE){

  Logger = get("Logger.Singleton", getNamespace("eddy4R.base"))

  if(!dir.exists(paraMain$DirOut)){
    dir.create(paraMain$DirOut, recursive = T)
  }

  wrap_tower_log = Logger$new()
  wrap_tower_log$set_log_file(file.path(paraMain$DirOut, "wrap_tower_logfile.txt"))
  wrap_tower_log$log_message(level = "info", paste("Begining run: ",
                                                   paraMain$siteName,
                                                   paraMain$analysis,
                                                   paraMain$runID,
                                                   sep = " - "))

  if(is.null(resume)){
    start = 1
  }else{
    start = resume
  }

  saveRDS(paraMain,file = file.path(paraMain$DirOut, paste0(paraMain$analysis,"_para.RDS")))

  #determine flux aggregation
  det_avg = eddy4R.york::def.avg(files = paraMain$files,
                                 fileMask = paraMain$fileMask,
                                 fileDuration = paraMain$fileDuration,
                                 aggregationDuration = paraMain$aggregationDuration,
                                 freq = paraMain$freq,
                                 tz = paraMain$tz,
                                 fileFirstStart = paraMain$fileFirstStart,
                                 fileLastStart = paraMain$fileLastStart)

  agg_files = det_avg$agg_files
  aggregationPeriod = det_avg$aggregationPeriod

  for(i in start:length(agg_files)){

    # if there are no files for this aggregationg period, skip
    if(is.na(agg_files[i])){

      eddy4R.york::log_message(wrap_tower_log, "warn", "File Aggregation - no files", aggregationPeriod[i,])

      next
    }

    # Read data
    eddy.data = tryCatch({
      eddy4R.york::read_input(DirInp = paraMain$DirInp,
                              dateFormat = paraMain$dateFormat,
                              agg_f = agg_files[[i]],
                              agg_p = aggregationPeriod[i,],
                              tz = paraMain$tz,
                              freq = paraMain$freq,
                              idepVar = paraMain$idepVar,
                              PltfEc=paraMain$PltfEc)
    },
    error = function(e){
      eddy4R.york::log_message(wrap_tower_log, "error", "Read Files", aggregationPeriod[i,], e)
      return(NULL)
    })

    if(is.null(eddy.data)){next}

    # Check input file
    skip_scalar = tryCatch({
      eddy4R.york::def.valid.input(eddy.data,
                                   varsRequired = paraMain$varsRequired,
                                   varsCritical = paraMain$varsCritical,
                                   species = paraMain$species,
                                   speciesRatioName = paraMain$speciesRatioName,
                                   aggregationDuration = paraMain$aggregationDuration,
                                   aggregationPeriod = aggregationPeriod[i,],
                                   missingThreshold  = paraMain$missingThreshold,
                                   freq = paraMain$freq,
                                   logger = wrap_tower_log
      )},
      error = function(e){
        eddy4R.york::log_message(wrap_tower_log, "error", "Valid Input", aggregationPeriod[i,], e)
        return("valid_error")
      })

    if(!is.null(skip_scalar)){
      if("valid_error" %in% skip_scalar){
        next
      }
    }

    if(length(skip_scalar) > 0){

      # need to actually remove the missing columns from eddy.data, otherwise we fall foul of unit checking in def.stat.sta.diff()
      skipRtio = paraMain$speciesRatioName[which(paraMain$species %in% skip_scalar)]
      eddy.data = eddy.data[!names(eddy.data) %in% skipRtio]
      # eddy.data = eddy.data |>
      #   dplyr::select(-dplyr::any_of(skipRtio))

      para = eddy4R.york::def.para.tmp(paraMain, skip_scalar)

    }else{
      para = paraMain
    }

    # Apply Anemometer Corrections --------------------------------------------
    eddy.data = tryCatch({
      eddy4R.york::wrap.anem.cor(eddy.data,
                                 anemometerOffset = para$anemometerOffset,
                                 wBoost = para$wBoost)},
      error = function(e){
        eddy4R.york::log_message(wrap_tower_log, "error", "Anemometer Correction", aggregationPeriod[i,], e)
        return(NULL)
      })

    if(is.null(eddy.data)){next}

    # Despike data before lag correction --------------------------------------
    if(para$despike){
      eddy.data = tryCatch({
        eddy4R.york::wrap.despike(eddy.data = eddy.data,
                                  despikeVars = para$despikeVars,
                                  despikeThreshold = para$despikeThreshold,
                                  verbose = FALSE)},
        error = function(e){
          eddy4R.york::log_message(wrap_tower_log, "error", "Despiking", aggregationPeriod[i,], e)
          return(NULL)
        })

      if(is.null(eddy.data)){next}
    }

    # Maximize cross correlation ----------------------------------------------
    if(para$lagApplyCorrection){
      lag_out = tryCatch({
        eddy4R.york::wrap.lag(eddy.data,
                              lagVars = para$lagVars,
                              lagApplyRangeLimit = para$lagApplyRangeLimit,
                              lagRangeLimit = para$lagRangeLimit,
                              lagDefaults = para$lagDefaults,
                              lagNOc = para$lagNOc,
                              freq = para$freq,
                              speciesRatioName = para$speciesRatioName,
                              lagNgtvPstv = para$lagNgtvPstv)},
        error = function(e){
          eddy4R.york::log_message(wrap_tower_log, "error", "Lag Correction", aggregationPeriod[i,], e)
          return(NULL)
        })

      if(is.null(lag_out)){next}

      eddy.data = lag_out$eddy.data
    }else{
      lag_out = NULL
    }

    # Handle missing values ---------------------------------------------------
    eddy.data = tryCatch({
      eddy4R.york::def.miss.hndl(eddy.data,
                                 missingMethod = para$missingMethod,
                                 missingThreshold = para$missingThreshold,
                                 aggregationDuration = para$aggregationDuration,
                                 freq = para$freq)},
      error = function(e){
        eddy4R.york::log_message(wrap_tower_log, "error", "Handel Missing Values", aggregationPeriod[i,], e)
        return(NULL)
      })

    if(is.null(eddy.data)){next}

    # Rotation of wind vectors ------------------------------------------------
    eddy.data = tryCatch({
      eddy4R.york::wrap.rot(data = eddy.data,
                            MethRot = para$MethRot,
                            plnrFitCoef = para$plnrFitCoef,
                            plnrFitType = para$plnrFitType)},
      error = function(e){
        eddy4R.york::log_message(wrap_tower_log, "error", "Wind Vector Rotation", aggregationPeriod[i,], e)
        return(NULL)
      })

    if(is.null(eddy.data)){next}

    # Units -------------------------------------------------------------------
    eddy.data = tryCatch({
      eddy4R.york::assign_input_units(eddy.data,
                                      unitList = para$unitList,
                                      idepVar = para$idepVar,
                                      speciesRatioName = para$speciesRatioName)},
      error = function(e){
        eddy4R.york::log_message(wrap_tower_log, "error", "Units", aggregationPeriod[i,], e)
        return(NULL)
      })

    if(is.null(eddy.data)){next}

    # calculate time-domain fluxes (classical EC) -----------------------------
    REYN = tryCatch({
      eddy4R.turb::wrap.flux(data = eddy.data,
                             AlgBase = para$AlgBase,
                             ListGasSclr = para$ListGasSclr)},
      error = function(e){
        eddy4R.york::log_message(wrap_tower_log, "error", "Flux Wrapper", aggregationPeriod[i,], e)
        return(NULL)
      })

    if(is.null(REYN)){next}

    # stationarity testing ----------------------------------------------------
    REYN$stna = tryCatch({
      eddy4R.turb::def.stna(data=REYN$data,
                            MethStna=c(1, 2, 3)[3],
                            NumSubSamp=para$aggregationDuration/300,
                            corTempPot=FALSE,
                            whrVar = para$stnaVar,
                            presTempPot=eddy4R.base::IntlNatu$Pres00,
                            vrbs = F,
                            ListGasSclr = para$ListGasSclr)},
      error = function(e){
        eddy4R.york::log_message(wrap_tower_log, "error", "Stationarity", aggregationPeriod[i,], e)
        return(NULL)
      })


    # ITC ---------------------------------------------------------------------
    REYN$itc <- tryCatch({
      eddy4R.turb::def.itc(stblObkv = REYN$mean$paraStbl,
                           lat = para$lat,
                           VarInp = "all",
                           sd = data.frame(
                             u_hor = REYN$sd$veloXaxsHor,
                             w_hor = REYN$sd$veloZaxsHor,
                             T_air = REYN$sd$tempAir),
                           varScal = data.frame(
                             u_star = REYN$mean$veloFric,
                             T_star_SL = REYN$mean$tempScalAtmSurf),
                           CorTemp = FALSE)},
      error = function(e){
        eddy4R.york::log_message(wrap_tower_log, "error", "ITC", aggregationPeriod[i,], e)
        return(NULL)
      })


    # Length Scales -----------------------------------------------------------
    REYN$isca = tryCatch({
      eddy4R.york::wrap.isca(REYN,
                             species = para$species,
                             speciesRatioName = para$speciesRatioName,
                             PltfEc = para$PltfEc)},
      error = function(e){
        eddy4R.york::log_message(wrap_tower_log, "error", "Length Scales", aggregationPeriod[i,], e)
        return(NULL)
      })



    # flux error calculations -------------------------------------------------
    REYN$error = tryCatch({

      if(!is.null(para$speciesRatioName)){
        speciesFluxName = paste0("flux", para$species)
      }else{
        speciesFluxName = NULL
      }

      eddy4R.turb::def.ucrt.samp(data = NULL,
                                 distIsca=REYN$isca,
                                 valuMean=REYN$mean,
                                 coefCorr=REYN$corr,
                                 distMean=mean(REYN$data$unixTime-min(REYN$data$unixTime)),
                                 timeFold = 0,
                                 spcsNameRtio = para$speciesRatioName,
                                 spcsNameFlux = speciesFluxName)},
      error = function(e){
        eddy4R.york::log_message(wrap_tower_log, "error", "def.ucrt.samp", aggregationPeriod[i,], e)
        return(NULL)
      })

    # flux limit of detection calculations ------------------------------------
    REYN$lod  <-  tryCatch({
      eddy4R.york::def.lod(REYN,
                           measCol = para$lagVars,
                           freq = para$freq)},
      error = function(e) {
        eddy4R.york::log_message(wrap_tower_log, "error", "def.ucrt.samp", aggregationPeriod[i,], e)
        return(NULL)
      })



    # Write -------------------------------------------------------------------
    tryCatch({
      eddy4R.york::write.REYN(REYN,
                              lag_out,
                              DirOut = para$DirOut,
                              analysis = para$analysis,
                              tz = para$tz,
                              writeFastData = para$writeFastData,
                              subDir = para$subDir)},
      error = function(e){
        eddy4R.york::log_message(wrap_tower_log, "error", "File Writing", aggregationPeriod[i,], e)
        return(NULL)
      })

  }
  print("Run Complete!")
}
