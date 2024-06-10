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
#' @param para params list
#' @param file_count current iteration of aggregation period loop
#'
#' @export

def.valid.input = function(eddy.data,
                           para,
                           file_count){

  error_list = list()
  skip_scalar = c()

  # Is there anything in the file at all?

  if(nrow(eddy.data) == 0){
    error_list = add_err("No data in file",error_list,location = "input")
    valid = list(error_list = error_list,
                 skip_scalar = skip_scalar)
    return(valid) # if the file is empty, do not continue other tests, just skip file
  }

  # Are all sets of data nominally present?
  req_names = c(para$required_para,
                para$critical_variable,
                para$speciesRatioName)

  name_test = !(req_names %in% names(eddy.data))

  if(sum(name_test) != 0){
    err_names = req_names[name_test] %>%
      stats::na.omit() %>%
      paste0(collapse = ", ")

    if(err_names != ""){
      if(length(req_names[name_test]) > 1)
        verb = " are"
      else
        verb = " is"

      error_list = add_err((paste0(err_names,verb, " missing from ",para$files[file_count])),error_list,location = "input")
      valid = list(error_list = error_list,
                   skip_scalar = skip_scalar)
      return(valid) # if the correct columns are not present, do not continue other tests, just skip file
    }
  }

  # All columns are the right class generally?
  classes = vector("character",ncol(eddy.data))
  for(i in 1:ncol(eddy.data)){
    classes[i] = paste0(class(eddy.data[,i]),collapse = "")
  }

  if(length(classes[classes == "POSIXctPOSIXt"]) != 1)
    error_list = add_err("Input files should contain exactly one POSIXct POSIXt column",error_list,location = "input")

  classes = classes[!(classes %in% c("integer","numeric","POSIXctPOSIXt","logical"))]

  if(length(classes) != 0)
    error_list = add_err("Input files can only contain POSIXct POSIXt, numeric and integer classes",error_list,location = "input")

  # Is the date in the date column?
  if(paste0(class(eddy.data$date),collapse = "") != "POSIXctPOSIXt"){
    error_list = add_err("Date column is not of class POSIXct POSIXt",error_list,location = "input")
  }
  #else{
  #   # Only test date if it is a POSIXct POSIXt
  #   # Is the date what we expect to find from the file name?
  #   if(lubridate::floor_date(eddy.data$date[1],"1 hour") != mask_extract_date(para$files[file_count],para$file_mask,tz = para$Tz))
  #     error_list = add_err("Date does not match file name",error_list,location = "input")
  # }
  ## Test missing data
  #  Skip file
  for(var in para$critical_variable){
    missing_data = eddy.data[,var][is.na(eddy.data[,var])] %>% length
    sd_data = stats::sd(eddy.data[,var],na.rm = T)

    if(missing_data/nrow(eddy.data) >= para$missing_thresh){
      error_list = add_err(paste0("Critical variable ",var," is missing greater than ",para$missing_thresh*100,"% of data"),
                           error_list,location = "input")
    }
    if(sd_data == 0 | is.na(sd_data))
      error_list = add_err(paste0("Critical variable ",var," has a standard deviation of 0, or has no data"),
                           error_list,location = "input")

  }
  #  Continue but missing scalars must be handled in the workflow

  for(i in 1:length(para$speciesRatioName)){

    var = para$speciesRatioName[i]

    missing_data = eddy.data[,var][is.na(eddy.data[,var])] %>%
      length()

    sd_data = stats::sd(eddy.data[,var],na.rm = T)

    if(missing_data/nrow(eddy.data) >= para$missing_thresh){
      error_list = add_err(paste0(" Optional scalar ",var," is missing greater than ",para$missing_thresh*100,"% of data"),
                           error_list,location = "input",condition = "continue")
      skip_scalar = c(skip_scalar,para$species[i])

      eddy.data = dplyr::select(eddy.data, -tidyselect::all_of(var))
    }
    if(sd_data == 0 | is.na(sd_data)){
      error_list = add_err(paste0("Optional scalar ",var," has a standard deviation of 0, or has no data"),
                           error_list,location = "input",condition = "continue")
      skip_scalar = c(skip_scalar,para$species[i])
      eddy.data = dplyr::select(eddy.data, -tidyselect::all_of(var))
    }
  }

  # TODO probably should move this to the preprocessing
  if("ratioMoleDryH2o" %in% names(eddy.data)){
    missing_h2o = eddy.data$ratioMoleDryH2o[is.na(eddy.data$ratioMoleDryH2o)] %>% length()

    if(missing_h2o/nrow(eddy.data) > para$missing_thresh)
      eddy.data$ratioMoleDryH2o = 1e-12
  }

  if(!nrow(eddy.data)>((1-para$missing_thresh) * para$agg_period * para$freq)){
    error_list = add_err(paste0("Not enough data in file, < 90%"),
                         error_list,location = "input")}


  skip_scalar = stringr::str_replace_all(skip_scalar,"FD_mole_","") %>%
    unique()

  list(error_list = error_list,
       skip_scalar = skip_scalar)
}
