#' Define eddy4R.york Parameters
#'
#' Create para list, use arguments to overide defaults
#'
#' @param fileDuration expected duration of a complete input file (Hz)
#' @param fileMask string mask that can be passed to \code{as.POSIXct(format = mask)} to decode the date in the file name
#' @param species vector of species names. Match with IntlNatu naming
#' @param speciesRatioName created automatically from species. name of gas species following the rtioMoleDry<spc> format
#' @param speciesFluxName created automatically from species. name of gas species following the flux<spc> format
#' @param unitList defaults to \code{default_unit_list()} plus species field based off of species argument
#' speices is a list with names that match speciesRatioName describing the units per species. Defaults to mol<spc> mol-1Dry
#' @param freq data aquisition frequency of input data
#' @param files list of file names
#' @param tz default "UTC".
#' @param dateFormat default %Y-%m-%d %H:%M:%OS"
#' @param filePattern pattern to help filter input directory - default .csv
#' @param varsRequired character vector of columns that must be nominally present to pass def.valid.input()
#' @param varsCritical these must have greater than the missingThreshold to pass def.valid.input()
#' @param AlgBase detrending method for def.base.ec "mean","trend","ord03"
#' @param idepVar required if Algbase is trend or ord03. Name of column
#' containing independant variable for determining the base function. Column
#' gets duplicated to a column called "idep" use use in
#' \code{eddy4R.turb::def.stat.sta.diff}. Default unixTime.
#' @param aggregationDuration flux aggregation period (s)
#' @param missingThreshold decimal percentage of missing data threshold per file
#' @param missingMethod how should missing data be handeled if it is less than the threshold. "drop","mean"
#' @param lagNOc when processing NOx, is NO2 actually NOc and therfore CE applied after lagging. T/F
#' @param anemometerType "other" or "CSAT3". In the case of CSAT 3, use the def.met.body() function. Otherwise use def.vect.orie(). \cr
#'                  For CSAT 3, anemometer offset should be positve from north. For other, anemomet offset should be +ve for clockwise correction, \cr
#'                  negative for anticlockwise
#' @param anemometerOffset rotation angle for uv plane of wind vectors (degrees)
#' @param veloXaxs wind vector definition for wrap.anem.cor
#' @param veloYaxs wind vector definition for wrap.anem.cor
#' @param veloZaxs wind vector definition for wrap.anem.cor
#' @param wBoost should w boost correction for certain Gill Anemometers be applied? Default false
#' @param lagApplyCorrection Should lag correction be undertaken? T/F
#' @param lagNgtvPstv what "direction" can lag occur? "n","p","np"
#' @param lagDefaults numeric value or vector to apply to lag if ccf falls outside of range or is not used
#' @param lagApplyRangeLimit Should a window be applied to the ccf lag?
#' @param lagRangeLimit vector or list of vectors containing boundary for acceptable lags
#' @param despike Should despiking be undertaken? T/F
#' @param despikeThreshold numeric vector of standard deviations for temperature and water vapour, plus all species
#' @param MethRot type of coordinate rotation of wind vectors to be performed. "single","double","planarFit"
#' @param plnrFitCoef coefficients for planar fit calculation. \cr
#'                    either numeric vector for plnrFitType = "simple" or data.frame for plnrFitType = "wind" or "time"
#' @param plnrFitType type of planar fit, "simple", "date" or "wind". (character vector) \itemize{
#'                    \item simple - numeric vector constant of coefficeients, or coefficeients that are controlled from the workflow. c(al,be,b0)
#'                    \item time - data.frame with columns date, al, be, b0. values with date nearest to mn$date are used
#'                    \item time - data.frame with columns PSI_uv, al, be, b0. values with date nearest to mn$PSI_uv are used}
#' @param runID Used in file nameing
#' @param siteName Used in file nameing
#' @param analysis Used in file nameing
#' @param writeFastData should high frequency intermediates be writen (diff,data, base, conv outputs from REYN)
#' @param PltfEc switch for eddy4r functions between tower and aircraft. towr,airc
#' @param ZoneUtm data.frame containing UTM info e.g data.frame(Zone=30, Estg=698478, Nthg=5711690)
#' @param lat latitude of tower location
#' @param fileFirstStart used to aid def.avg - should be created internally
#' @param fileLastStart used to aid def.avg - should be created internally
#' @param DirWrk root of the data directory
#' @param DirInp must be supplied - relative to DirWrk
#' @param DirOut by default is created as \code{file.path(DirWrk,"out",siteName, runID, analysis)}, can be overridden here.
#' @param subDir one of c("none", "monthly", "daily") - default none. Should the outputs be split into monthly or daily subdirectories
#' @param lagVars created from species plus temperature and water vapour
#' @param despikeVars created from species plus temperature and water vapour
#' @param wavelet_ec supply argument true or false for conducting wavelet eddy-covariance
#' @param wavelet_av supply argument "mean" or "band" for conducting wavelet eddy-covariance averaging.
#' @param wavelet_win supply value for data window to use for wavelet averaging. Tower setup give value in minutes, e.g. 5. Aircraft give distance in meters.
#' @param wavelet_agg supply value for data jump to use for wavelet averaging. Tower setup give value in minutes, e.g. 1. Aircraft gives distance in meters.
#' @param wavelet_maxscale maximum wavlet scale to use for averaging. Default is 60 minutes. Give value in in minutes, e.g. 60.
#' @param wavelet_cores specify number of CPU cores to use for parallel wavelet averaging.
#' @param ... supply arguments that have not been given an explicit definition yet here
#'
#' @author W. S. Drysdale
#'
#' @export

def.para = function(
  # File Management/Housekeeping
  DirInp,
  DirWrk,
  runID,
  analysis,
  fileMask,
  fileDuration,
  DirOut = NULL,
  siteName = NULL,
  writeFastData = TRUE,
  subDir = c("none", "monthly", "daily")[1],
  species = NULL,
  speciesRatioName = NULL,
  speciesFluxName = NULL,
  unitList = NULL,
  freq = 5,
  files = NULL,
  filePattern = ".csv",
  tz = "UTC",
  dateFormat = "%Y-%m-%d %H:%M:%OS",
  # columns must be nominally present to pass def.valid.input()
  varsRequired = c("unixTime","distZaxsMeas","presAtm","distZaxsAbl"),
  # these must have greater than the missingThreshold to pass def.valid.input()
  varsCritical = c("veloXaxs","veloYaxs","veloZaxs","tempAir"),
  # Eddy Covariance Settings
  AlgBase = "trnd",
  idepVar = "unixTime",
  aggregationDuration = 3600,
  missingThreshold = 0.1,
  missingMethod = c("drop","mean")[1],
  lagNOc = F,

  ## Anemometer
  anemometerType = c("other","CSAT3"),
  anemometerOffset = NULL,
  veloXaxs = "+veloXaxs",
  veloYaxs = "+veloYaxs",
  veloZaxs = "+veloZaxs",
  wBoost = F,

  ## Lag
  lagApplyCorrection = T,
  lagNgtvPstv = c("n","p","np")[1],
  lagDefaults = -10,
  lagApplyRangeLimit = F,
  lagRangeLimit = NULL,
  lagVars = NULL,

  ## Despike
  despike = T,
  despikeThreshold = c(5,8,8),
  despikeVars = NULL,

  ## Rotation
  MethRot = c("single","double","planarFit")[2],
  plnrFitCoef = NULL,
  plnrFitType = c("simple","time","wind")[1],

  # Site Info
  lat,
  PltfEc = "towr",
  ZoneUtm = data.frame(Zone=30, Estg=698478, Nthg=5711690),

  # Should be auto-generated in many cases
  fileFirstStart = NULL,
  fileLastStart = NULL,

  # Wavelet settings
  wavelet_ec = FALSE,
  wavelet_av = "mean",
  wavelet_win = 5,
  wavelet_agg = 1,
  wavelet_cores = 2,
  wavelet_maxscale = 60,

  #Ellipsis
  ...){

  # This throws an error if an argument that doesn't have a default AND isn't used in the function has not had a value supplied.
  # e.g x = function(y){print("hello")};x() doesn't error but the below does.
  # x = function(y){sapply(ls(environment()), get, envir = environment(), inherits = FALSE);print("hello")};x()
  # This means we can get away with using c(as.list(environment()), list(...)) to capture the args
  # while being sure all the defaults have been supplied
  sapply(ls(environment()), get, envir = environment(), inherits = FALSE)

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
    para$DirOut = file.path(DirWrk,"out",siteName, runID, analysis)
  }

  if(is.null(unitList)){
    para$unitList = eddy4R.york::default_unit_list()
  }

  if(!is.null(species)){
    if(is.null(speciesRatioName)){
      para$speciesRatioName = paste0("rtioMoleDry", species)
    }

    if(is.null(speciesFluxName)){
      para$speciesFluxName = paste0("flux", species)
    }

    if(is.null(para$unitList$species)){
      para$unitList$species = stats::setNames(as.list(paste0("mol", species, " mol-1Dry")), para$speciesRatioName)
    }

    para$ListGasSclr = purrr::map2(para$speciesFluxName,
                                   para$unitList$species,
                                   ~{
                                     list(Conv = "densMoleAirDry",
                                          Unit = base::data.frame(InpVect = "m s-1",
                                                                  InpSclr = .y,
                                                                  Conv = "mol m-3",
                                                                  Out = "mol m-2 s-1"),
                                          NameOut = .x)
                                   }) %>%
      stats::setNames(para$speciesRatioName)
  }



  if(is.null(lagVars)){
    para$lagVars = c("tempAir", "rtioMoleDryH2o",para$speciesRatioName)
  }else{
    para$lagVars = lagVars
  }

  if(is.null(despikeVars)){
    para$despikeVars = c("veloZaxs",para$speciesRatioName)
  }else{
    para$despikeVars = despikeVars
  }

  if(is.null(files)){
    para$files = list.files(paste0(para$DirInp),pattern = para$filePattern)
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

  if(length(lagDefaults) != length(para$lagVars)){
    if(length(lagDefaults) == 1){
      message("Only one absolute lag value supplied for multiple cross correlation variables - this value have been recycled")
      lagDefaults = rep(lagDefaults,length(para$lagVars))
    }else
      stop("Absolute lag must be of length one or equal to length of cross correlation vars")
  }

  if(lagApplyRangeLimit){
    if(is.null(lagRangeLimit)){
      para$lagRangeLimit = rep(list(-10,0),length(para$lagVars))
    }

    if(!"list" %in% class(lagRangeLimit)){
      stop("lagRangeLimit is not of class list")
    }else{
      if(length(lagRangeLimit) != length(para$lagVars)){
        stop("lag boundary must be a list with a length the same as lagVars")
      }
    }
  }

  if(para$AlgBase != "mean" & is.null(para$idepVar)){
    stop("When AlgBase is trend or ord03 idepVar needs to be assigned to a column e.g. unixTime")
  }


  # What should have stationarity tests applied?
  para$stnaVar = c("veloFricXaxsSq", "veloFricYaxsSq", "veloFric", "fluxTempEngy", "fluxH2oEngy", para$speciesFluxName)

  para$call = match.call()

  para
}
