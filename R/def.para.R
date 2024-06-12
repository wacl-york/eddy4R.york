#' Define eddy4R.york Parameters
#'
#' Create para list, use arguments to overide defaults
#'
#' @param file_duration expected duration of a complete input file (Hz)
#' @param file_mask string mask that can be passed to \code{as.POSIXct(format = mask)} to decode the date in the file name
#' @param species vector of species names. Match with IntlNatu naming
#' @param speciesRatioName created automatically from species. name of gas species following the rtioMoleDry<spc> format
#' @param unitList defaults to \code{default_unit_list()} plus species field based off of species argument
#' speices is a list with names that match speciesRatioName describing the units per species. Defaults to mol<spc> mol-1Dry
#' @param freq data aquisition frequency of input data
#' @param files list of file names
#' @param tz default "UTC".
#' @param dateFormat default %Y-%m-%d %H:%M:%OS"
#' @param file_pattern pattern to help filter input directory - default .csv
#' @param required_para character vector of columns that must be nominally present to pass def.valid.input()
#' @param critical_variable these must have greater than the missing_thresh to pass def.valid.input()
#' @param AlgBase detrending method for def.base.ec "mean","trend","ord03"
#' @param idepVar required if Algbase is trend or ord03. Name of column
#' containing independant variable for determining the base function. Column
#' gets duplicated to a column called "idep" use use in
#' \code{eddy4R.turb::def.stat.sta.diff}. Default unixTime.
#' @param agg_period flux aggregation period (s)
#' @param missing_thresh decimal percentage of missing data threshold per file
#' @param missing_method how should missing data be handeled if it is less than the threshold. "drop","mean"
#' @param noc_lag when processing NOx, is NO2 actually NOc and therfore CE applied after lagging. T/F
#' @param anem_type "other" or "CSAT3". In the case of CSAT 3, use the def.met.body() function. Otherwise use def.vect.orie(). \cr
#'                  For CSAT 3, anemometer offset should be positve from north. For other, anemomet offset should be +ve for clockwise correction, \cr
#'                  negative for anticlockwise
#' @param anemometer_offset rotation angle for uv plane of wind vectors (degrees)
#' @param veloXaxs wind vector definition for wrap.anem.cor
#' @param veloYaxs wind vector definition for wrap.anem.cor
#' @param veloZaxs wind vector definition for wrap.anem.cor
#' @param w_boost should w boost correction for certain Gill Anemometers be applied? Default false
#' @param lag_correction Should lag correction be undertaken? T/F
#' @param lagNgtvPstv what "direction" can lag occur? "n","p","np"
#' @param absolute_lag numeric value or vector to apply to lag if ccf falls outside of range or is not used
#' @param restrict_lag_range Should a window be applied to the ccf lag?
#' @param lag_boundary vector or list of vectors containing boundary for acceptable lags
#' @param despike Should despiking be undertaken? T/F
#' @param despike_threshold numeric vector of standard deviations for temperature and water vapour, plus all species
#' @param MethRot type of coordinate rotation of wind vectors to be performed. "single","double","planarFit"
#' @param plnrFitCoef coefficients for planar fit calculation. \cr
#'                    either numeric vector for plnrFitType = "simple" or data.frame for plnrFitType = "wind" or "time"
#' @param plnrFitType type of planar fit, "simple", "date" or "wind". (character vector) \itemize{
#'                    \item simple - numeric vector constant of coefficeients, or coefficeients that are controlled from the workflow. c(al,be,b0)
#'                    \item time - data.frame with columns date, al, be, b0. values with date nearest to mn$date are used
#'                    \item time - data.frame with columns PSI_uv, al, be, b0. values with date nearest to mn$PSI_uv are used}
#' @param wlp_cor Should fast water vapour be used to correct NO and NO2 concentrations.
#' @param highfreq_cor Should high frequency corrections be performed
#' @param file_id Used in file nameing
#' @param run_id Used in file nameing
#' @param file_type_out output file type, currently only supports .csv
#' @param site_name Used in file nameing
#' @param analysis Used in file nameing
#' @param write_fast_data should high frequency intermediates be writen (diff,data, base, conv outputs from REYN)
#' @param PltfEc switch for eddy4r functions between tower and aircraft. towr,airc
#' @param TimeDiffUtcLt offset of timezone from UTC
#' @param ZoneUtm data.frame containing UTM info e.g data.frame(Zone=30, Estg=698478, Nthg=5711690)
#' @param ElevAslTow Tower elevation above sealevel
#' @param ElevAglSens Tower elevation above ground
#' @param ElevAglDisp Displacement Height
#' @param Lat latitude of tower location
#' @param first_file_begin used to aid def.avg - should be created internally
#' @param final_file_begin used to aid def.avg - should be created internally
#' @param DirWrk root of the data directory
#' @param DirInp must be supplied - relative to DirWrk
#' @param DirOut by default is created as \code{file.path(DirWrk,"out",site_name, run_id, analysis)}, can be overridden here.
#' @param subDirMonthly should the outputs be split into monthly subdirectories. Default FALSE
#' @param cross_correlation_vars created from species plus temperature and water vapour
#' @param despike_vars created from species plus temperature and water vapour
#' @param wavelet_ec supply argument true or false for conducting wavelet eddy-covariance
#' @param wavelet_av supply argument "mean" or "band" for conducting wavelet eddy-covariance averaging.
#' @param wavelet_win supply value for data window to use for wavelet averaging. Tower setup give value in minutes, e.g. 5. Aircraft give distance in meters.
#' @param wavelet_agg supply value for data jump to use for wavelet averaging. Tower setup give value in minutes, e.g. 1. Aircraft gives distance in meters.
#' @param wavelet_maxscale maximum wavlet scale to use for averaging. Default is 60 minutes. Give value in in minutes, e.g. 60.
#' @param wavelet_cores specify number of CPU cores to use for parallel wavelet averaging.
#' @param footprint Footprint modelling.
#' @param ... supply arguments that have not been given an explicit definition yet here
#'
#' @author W. S. Drysdale
#'
#' @export

def.para = function(file_duration = 3600,# Input Data information
                    file_mask,
                    species = NULL,
                    speciesRatioName = NULL,
                    unitList = NULL,
                    freq = 5,
                    files = NULL,
                    file_pattern = ".csv",
                    tz = "UTC",
                    dateFormat = "%Y-%m-%d %H:%M:%OS",
                    # columns must be nominally present to pass def.valid.input()
                    required_para = c("unixTime","distZaxsMeas","presAtm","distZaxsAbl"),
                    # these must have greater than the missing_thresh to pass def.valid.input()
                    critical_variable = c("veloXaxs","veloYaxs","veloZaxs","tempAir"),
                    # Eddy Covariance Settings
                    AlgBase = "trnd",
                    idepVar = "unixTime",
                    agg_period = 3600,
                    missing_thresh = 0.1,
                    missing_method = c("drop","mean")[1],
                    noc_lag = F,

                    ## Anemometer
                    anem_type = c("other","CSAT3"),
                    anemometer_offset = NULL,
                    veloXaxs = "+veloXaxs",
                    veloYaxs = "+veloYaxs",
                    veloZaxs = "+veloZaxs",
                    w_boost = F,

                    ## Lag
                    lag_correction = T,
                    lagNgtvPstv = c("n","p","np")[1],
                    absolute_lag = -10,
                    restrict_lag_range = F,
                    lag_boundary = NULL,

                    ## Despike
                    despike = T,
                    despike_threshold = c(5,8,8),

                    ## Rotation
                    MethRot = c("single","double","planarFit")[2],
                    plnrFitCoef = NULL,
                    plnrFitType = c("simple","time","wind")[1],

                    ## Flow control
                    wlp_cor = F,
                    highfreq_cor = T,

                    # File Management/Housekeeping
                    file_id = "NOx_5Hz",
                    run_id = "BT_Tower",
                    file_type_out = c("csv"),
                    site_name = NULL,
                    analysis,
                    write_fast_data = TRUE,
                    DirInp,
                    DirWrk,
                    DirOut = NULL,
                    subDirMonthly = FALSE,

                    # Site Info
                    PltfEc = "towr",
                    TimeDiffUtcLt = 0,
                    ZoneUtm = data.frame(Zone=30, Estg=698478, Nthg=5711690),
                    ElevAslTow = 35,
                    ElevAglSens = 177,
                    ElevAglDisp = 0,
                    Lat = 51.521381,
                    # Should be auto-generated in many cases
                    first_file_begin = NULL,
                    final_file_begin = NULL,
                    cross_correlation_vars = NULL,
                    despike_vars = NULL,

                    # Wavelet settings
                    wavelet_ec = FALSE,
                    wavelet_av = "mean",
                    wavelet_win = 5,
                    wavelet_agg = 1,
                    wavelet_cores = 2,
                    wavelet_maxscale = 60,
                    footprint = F,

                    #Ellipsis
                    ...){

  # Get all arguments into a list
  para = c(as.list(environment()), list(...))

  if(!dir.exists(para$DirWrk)){
    stop(paste0("Could not find DirWrk: ", para$DirWrk))
  }

  para$DirInp = file.path(DirWrk, DirInp)
  if(!dir.exists(para$DirInp)){
    stop(paste0("Could not find DirInp: ", para$DirInp))
  }

  # Then make adjustments as necessary
  # directory is created in write.REYN so subDirMonthly can be respected
  if(is.null(DirOut)){
    para$DirOut = file.path(DirWrk,"out",site_name, run_id, analysis)
  }

  if(is.null(unitList)){
    para$unitList = eddy4R.york::default_unit_list()
  }

  if(!is.null(species)){
    if(is.null(speciesRatioName)){
      para$speciesRatioName = paste0("rtioMoleDry", species)
    }

    if(is.null(para$unitList$species)){
      para$unitList$species = stats::setNames(as.list(paste0("mol", species, " mol-1Dry")), para$speciesRatioName)
    }

    para$ListGasSclr = purrr::map2(para$species,
                                   para$unitList$species,
                                   ~{
                                     list(Conv = "densMoleAirDry",
                                          Unit = base::data.frame(InpVect = "m s-1",
                                                                  InpSclr = .y,
                                                                  Conv = "mol m-3",
                                                                  Out = "mol m-2 s-1"),
                                          NameOut = paste0("flux",.x))
                                   }) %>%
      stats::setNames(para$speciesRatioName)
  }



  if(is.null(cross_correlation_vars)){
    para$cross_correlation_vars = c("tempAir", "rtioMoleDryH2o",para$speciesRatioName)
  }else{
    para$cross_correlation_vars = cross_correlation_vars
  }

  if(is.null(despike_vars)){
    para$despike_vars = c("veloZaxs",para$speciesRatioName)
  }else{
    para$despike_vars = despike_vars
  }

  if(is.null(files)){
    para$files = list.files(paste0(para$DirInp),pattern = para$file_pattern)
  }else{
    para$files = files
  }

  if(length(para$files) < 1){
    warning("No files found")
  }

  if(PltfEc=="towr"){
    para$cwt_win = lubridate::minutes(wavelet_win)
    para$cwt_agr = lubridate::minutes(wavelet_agg)
  }
  if(PltfEc=="airc"){
    para$cwt_win = wavelet_win
    para$cwt_agr = wavelet_agg
  }

  if(length(absolute_lag) != length(para$cross_correlation_vars)){
    if(length(absolute_lag) == 1){
      message("Only one absolute lag value supplied for multiple cross correlation variables - this value have been recycled")
      absolute_lag = rep(absolute_lag,length(para$cross_correlation_vars))
    }else
      stop("Absolute lag must be of length one or equal to length of cross correlation vars")
  }

  if(restrict_lag_range){
    if(is.null(lag_boundary)){
      para$lag_boundary = rep(list(-10,0),length(para$cross_correlation_vars))
    }

    if(!"list" %in% class(lag_boundary)){
      stop("lag_boundary is not of class list")
    }else{
      if(length(lag_boundary) != length(para$cross_correlation_vars)){
        stop("lag boundary must be a list with a length the same as cross_correlation_vars")
      }
    }
  }

  if(para$AlgBase != "mean" & is.null(para$idepVar)){
    stop("When AlgBase is trend or ord03 idepVar needs to be assigned to a column e.g. unixTime")
  }


  # What should have stationarity tests applied?
  para$stnaVar = c("veloFricXaxsSq", "veloFricYaxsSq", "veloFric", "fluxTempEngy", "fluxH2oEngy", para$speciesRatioName)

  para$call = match.call()

  para
}
