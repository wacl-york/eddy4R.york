#' Valid Input
#'
#' Checks input file for several key requirements \cr
#' \itemize{
#' \item Number of rows !=0
#' \item Are required variables present? Skips the file if critcal vals are missing
#' otherwise flags the optional variables that are missing
#' \item Are these columns the correct class
#' \item Are any of the stanard deviations of variables 0? Skip/flag as above
#' \item Check against the missing data threshold set in para
#' \item If there is no H20 data, set it to 1e-12.
#' }
#'
#' @param eddy.data input data
#' @param logger logger object
#' @param periodStartDate periodStartDate
#' @param periodEndDate periodEndDate
#'
#'
#' @inheritParams def.para
#'
#' @export

def.valid.input = function(eddy.data,
                           varsRequired,
                           varsCritical,
                           species,
                           speciesRatioName,
                           aggregationDuration,
                           periodStartDate,
                           periodEndDate,
                           missingThreshold,
                           freq,
                           logger){

  skip_scalar = c()

  # Is there anything in the file at all?

  if(nrow(eddy.data) == 0){
    stop("No data in file")
  }

  # Are all sets of data nominally present?
  req_names = c(varsRequired,
                varsCritical,
                speciesRatioName)

  name_test = !(req_names %in% names(eddy.data))

  if(sum(name_test) != 0){
    err_names = req_names[name_test] %>%
      stats::na.omit() %>%
      paste0(collapse = ", ")

    if(err_names != ""){
      if(length(req_names[name_test]) > 1){
        verb = " are"
      }else{
        verb = " is"}

      stop(paste0(err_names,verb, " missing"))
      # if the correct columns are not present, do not continue other tests, just skip file
    }
  }

  # All columns are the right class generally?
  classes = apply(eddy.data,2, class)


  classes = classes[classes != "numeric"]

  if(length(classes) != 0){
    stop("Input files can only contain, numeric columns")
  }

  ## Test missing data
  #  Skip file
  for(var in varsCritical){
    missing_data = eddy.data[,var][is.na(eddy.data[,var])] %>% length
    sd_data = stats::sd(eddy.data[,var],na.rm = T)

    if(missing_data/nrow(eddy.data) >= missingThreshold){
      stop(paste0("Critical variable ",var," is missing greater than ",missingThreshold*100,"% of data"))
    }

    if(sd_data == 0 | is.na(sd_data)){
      stop(paste0("Critical variable ",var," has a standard deviation of 0, or has no data"))
    }

  }
  #  Continue but missing scalars must be handled in the workflow

  for(i in 1:length(speciesRatioName)){

    var = speciesRatioName[i]

    missing_data = eddy.data[,var][is.na(eddy.data[,var])] %>%
      length()

    sd_data = stats::sd(eddy.data[,var],na.rm = T)


    skipFlag = 0
    if(missing_data/nrow(eddy.data) >= missingThreshold){

      eddy4R.york::log_message(logger = logger,
                               logLevel = "warn",
                               header = paste0("Optional scalar ",var," is missing greater than ",missingThreshold*100,"% of data"),
                               periodStartDate = periodStartDate,
                               periodEndDate = periodEndDate)
      skipFlag = skipFlag+1
    }

    if(sd_data == 0 | is.na(sd_data)){
      eddy4R.york::log_message(logger = logger,
                               logLevel = "warn",
                               header = paste0("Optional scalar ",var," has a standard deviation of 0, or has no data"),
                               periodStartDate = periodStartDate,
                               periodEndDate = periodEndDate)


      skipFlag = skipFlag+1

    }

    if(skipFlag > 0){
      eddy.data = dplyr::select(eddy.data, -tidyselect::all_of(var))
      skip_scalar = c(skip_scalar,species[i])
    }
  }

  # TODO probably should move this to the preprocessing
  if("rtioMoleDryH2o" %in% names(eddy.data)){
    missing_h2o = eddy.data$rtioMoleDryH2o[is.na(eddy.data$rtioMoleDryH2o)] %>% length()

    if(missing_h2o/nrow(eddy.data) > missingThreshold)
      eddy.data$rtioMoleDryH2o = 1e-12
  }

  if(!nrow(eddy.data)>((1-missingThreshold) * aggregationDuration * freq)){
    stop("Not enough data in file, < 90%")
  }

  unique(skip_scalar)
}
