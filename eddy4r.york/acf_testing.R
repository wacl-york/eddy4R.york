acf_test_defaults = function(i){
  para = eddy4R.york::def.para(
    DirWrk = "/data_dir/",
    DirInp = "in",
    siteName = "th",
    analysis = "lag_test",
    runID = "standard_run",
    fileMask = "input_%Y%m%d_%H.csv",
    species = c("O3"),
    aggregationPeriod = 3600,
    fileDuration = 3600,
    lat = 32.264,
    writeFastData = TRUE,
    AlgBase = "trnd",
    idepVar = "unixTime",
    MethRot = "double",
    missingMethod = "mean",
    lagApplyCorrection = TRUE,
    lagApplyRangeLimit = TRUE,
    lagRangeLimit = list(c(0,0),
                         c(0,0),
                         c(0,-10)
    ),
    lagDefaults = c(0,0,-6))

  # eddy4R.york::wrap.towr(para)


  paraMain = para
  resume = NULL
  thshFile = NULL
  diagSens = FALSE


  # wrap.towr ---------------------------------------------------------------

  if(is.null(resume)){
    start = 1
  }else{
    start = resume
  }

  saveRDS(paraMain,file = file.path(paraMain$DirOut, paste0(paraMain$analysis,"_para.RDS")))

  #determine flux aggregation
  det_avg = eddy4R.york::def.avg(filePaths = paraMain$filePaths,
                                 fileNames = paraMain$fileNames,
                                 fileMask = paraMain$fileMask,
                                 fileDuration = paraMain$fileDuration,
                                 aggregationDuration = paraMain$aggregationDuration)

  # Read data
  eddy.data = eddy4R.york::read_input(DirInp = paraMain$DirInp,
                                      dateFormat = paraMain$dateFormat,
                                      filePaths = det_avg$filePaths[[i]],
                                      periodStartDate = det_avg$periodStartDate[i],
                                      periodEndDate = det_avg$periodEndDate[i],
                                      tz = paraMain$tz,
                                      freq = paraMain$freq,
                                      idepVar = paraMain$idepVar,
                                      PltfEc=paraMain$PltfEc)

  # Check input file
  skip_scalar = eddy4R.york::def.valid.input(eddy.data,
                                             varsRequired = paraMain$varsRequired,
                                             varsCritical = paraMain$varsCritical,
                                             species = paraMain$species,
                                             speciesRatioName = paraMain$speciesRatioName,
                                             aggregationDuration = paraMain$aggregationDuration,
                                             periodStartDate = det_avg$periodStartDate[i],
                                             periodEndDate = det_avg$periodEndDate[i],
                                             missingThreshold  = paraMain$missingThreshold,
                                             freq = paraMain$freq,
                                             logger = wrap_tower_log
  )

  if(length(skip_scalar) > 0){

    # need to actually remove the missing columns from eddy.data, otherwise we fall foul of unit checking in def.stat.sta.diff()
    skipRtio = paraMain$speciesRatioName[which(paraMain$species %in% skip_scalar)]
    eddy.data = eddy.data[!names(eddy.data) %in% skipRtio]
    # eddy.data = eddy.data |>
    #   dplyr::select(-dplyr::any_of(skipRtio))

    para = def.para.tmp(paraMain, skip_scalar)

  }else{
    para = paraMain
  }

  # Apply Anemometer Corrections --------------------------------------------
  eddy.data = eddy4R.york::wrap.anem.cor(eddy.data,
                                         anemometerOffset = para$anemometerOffset,
                                         wBoost = para$wBoost)

  # Despike data before lag correction --------------------------------------
  if(para$despike){
    eddy.data = eddy4R.york::wrap.despike(eddy.data = eddy.data,
                                          despikeVars = para$despikeVars,
                                          despikeThreshold = para$despikeThreshold,
                                          verbose = FALSE)

  }

  # wrap.lag ----------------------------------------------------------------
  lagVars = para$lagVars
  lagApplyRangeLimit = para$lagApplyRangeLimit
  lagRangeLimit = para$lagRangeLimit
  lagDefaults = para$lagDefaults
  lagNOc = para$lagNOc
  freq = para$freq
  speciesRatioName = para$speciesRatioName
  lagNgtvPstv = para$lagNgtvPstv

  a = lagVars[3]
  b = lagRangeLimit[3]
  c = lagDefaults[3]

  # def.lag -----------------------------------------------------------------

  args = list(
    refe=eddy.data$veloZaxs,
    meas=eddy.data[,a],
    dataRefe=eddy.data,
    lagCnst=TRUE,
    lagNgtvPstv=lagNgtvPstv,
    lagAll=TRUE,
    freq=freq,
    hpf=TRUE,
    measVar = NULL,
    fracMin = 0.1
  )

  args$dataRefe = args$refe
  args$dataMeas = args$meas

  args

}

extract_acf_df = function(lag_out){

  dplyr::tibble(
    acf = lag_out$corr$acf[,1,1],
    lag = lag_out$corr$lag[,1,1]
  )

}
