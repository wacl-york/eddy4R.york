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

  if(is.null(resume)){
    start = 1
  }else{
    start = resume
  }

  if(!dir.exists(paraMain$DirOut)){
    dir.create(paraMain$DirOut, recursive = T)
  }

  saveRDS(paraMain,file = file.path(paraMain$DirOut, paste0(paraMain$analysis,"_para.RDS")))

  #determine flux aggregation
  det_avg = eddy4R.york::def.avg(files = paraMain$files,
                                 mask = paraMain$file_mask,
                                 file_duration = paraMain$file_duration,
                                 aggr_dur = paraMain$agg_period,
                                 freq = paraMain$freq,
                                 tz = paraMain$tz,
                                 first_file_begin = paraMain$first_file_begin,
                                 final_file_begin = paraMain$final_file_begin)

  agg_files = det_avg$agg_files
  agg_period = det_avg$agg_period

  for(i in start:length(agg_files)){

    # if there are no files for this aggregationg period, skip
    if(is.na(agg_files[i])){
      next
    }

    # Read data
    eddy.data = eddy4R.york::read_input(DirInp = paraMain$DirInp,
                                        dateFormat = paraMain$dateFormat,
                                        agg_f = agg_files[[i]],
                                        agg_p = agg_period[i,],
                                        tz = paraMain$tz,
                                        freq = paraMain$freq,
                                        idepVar = paraMain$idepVar,
                                        PltfEc=paraMain$PltfEc)

    # Check input file
    valid = eddy4R.york::def.valid.input(eddy.data, paraMain, i)

    if(length(valid$skip_scalar) > 0){
      para = eddy4R.york::def.para.tmp(paraMain, valid$skip_scalar)
    }else{
      para = paraMain
    }

    # If the input file has not been flagged to skip
    if(!eddy4R.york::err_skip(valid$error_list)){

      # Apply Anemometer Corrections --------------------------------------------
      eddy.data = eddy4R.york::wrap.anem.cor(eddy.data,para)

      # Despike data before lag correction --------------------------------------
      if(para$despike){
        eddy.data = eddy4R.york::wrap.despike(eddy.data = eddy.data,
                                              despike_vars = para$despike_vars,
                                              despike_threshold = para$despike_threshold,
                                              verbose = FALSE)

      }


      # Maximize cross correlation ----------------------------------------------
      if(para$lag_correction){
        lag_out = eddy4R.york::wrap.lag(eddy.data,para)
        eddy.data = lag_out$eddy.data

      }


      # Handle missing values ---------------------------------------------------
      eddy.data = eddy4R.york::def.miss.hndl(eddy.data,para)

      # Rotation of wind vectors ------------------------------------------------
      eddy.data = eddy4R.york::wrap.rot(data = eddy.data,
                                        MethRot = para$MethRot,
                                        plnrFitCoef = para$plnrFitCoef,
                                        plnrFitType = para$plnrFitType)




      # sort units
      eddy.data = eddy4R.york::assign_input_units(eddy.data, para)


      # calculate time-domain fluxes (classical EC) -----------------------------
      REYN = eddy4R.turb::wrap.flux(data = eddy.data,
                                    AlgBase = para$AlgBase,
                                    ListGasSclr = para$ListGasSclr)



      # stationarity testing ----------------------------------------------------
      REYN$stna = eddy4R.turb::def.stna(data=REYN$data,
                                        MethStna=c(1, 2, 3)[3],
                                        NumSubSamp=para$agg_period/300,
                                        corTempPot=FALSE,
                                        whrVar = para$stnaVar,
                                        presTempPot=eddy4R.base::IntlNatu$Pres00,
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



      # flux error calculations -------------------------------------------------
      REYN$error = eddy4R.turb::def.ucrt.samp(data = NULL,
                                              distIsca=REYN$isca,
                                              valuMean=REYN$mean,
                                              coefCorr=REYN$corr,
                                              distMean=mean(REYN$data$unixTime-min(REYN$data$unixTime)),
                                              timeFold = 0,
                                              spcsNameRtio = para$speciesRatioName,
                                              spcsNameFlux = paste0("flux", para$species)
      )

      # flux limit of detection calculations ------------------------------------
      REYN$lod  <-  eddy4R.york::def.lod(REYN,
                                         measCol = para$cross_correlation_vars,
                                         freq = para$freq)


      # Write -------------------------------------------------------------------
      eddy4R.york::write.REYN(REYN,
                              lag_out,
                              DirOut = para$DirOut,
                              analysis = para$analysis,
                              tz = para$tz,
                              write_fast_data = para$write_fast_data,
                              subDirMonthly = para$subDirMonthly
                              )

    }

  }
  print("Run Complete!")
}
