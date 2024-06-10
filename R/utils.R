#' Default Unit List
#'
#' Returns a list containing the default units for temperature pressure and
#' velocity. Useful as a base if the user needs to change the defaults in
#' \code{def.para()$unitList}
#'
#' @export

default_unit_list = function(){

  list(
    temp = eddy4R.base::IntlUnit$Intl$Temp,
    pres = eddy4R.base::IntlUnit$Intl$Pres,
    velo = paste0(eddy4R.base::IntlUnit$Intl$Dist," ",
                  eddy4R.base::IntlUnit$Intl$Time, "-1"),
    h2o = paste0(eddy4R.base::IntlUnit$Intl$Num,"H2o ",
                 eddy4R.base::IntlUnit$Intl$Num,"-1Dry"),
    dist = eddy4R.base::IntlUnit$Intl$Dist,
    species = NULL
  )

}

#' Assign Unit Inputs
#'
#' Takes the input list from \code{def.para()} or \code{default_unit_list()}
#' and sets the corresponding columns in eddy.data.
#'
#' @param eddy.data input data
#' @param para parameter list
#'
#' @export

assign_input_units = function(eddy.data,
                              para){
  # temperature
  attr(x = eddy.data$tempAir, which = "unit") = para$unitList$temp

  # pressure
  attr(x = eddy.data$presAtm, which = "unit") = para$unitList$pres

  # wind vectors
  for(var in c("veloXaxs", "veloYaxs", "veloZaxs")){
    attr(x = eddy.data[,var], which = "unit") = para$unitList$velo
  }

  # distances
  for(var in c("distZaxsAbl", "distZaxsMeas")){
    attr(x = eddy.data[,var], which = "unit") = para$unitList$dist
  }

  # H2O
  attr(x = eddy.data$rtioMoleDryH2o, which = "unit") = para$unitList$h2o

  # unix time
  attr(x = eddy.data$unixTime, which = "unit") = eddy4R.base::IntlUnit$Intl$Time

  # idep - for airc probably will want to define a different idep...
  if(para$idepVar == "unixTime"){
    attr(x = eddy.data$idep, which = "unit") = eddy4R.base::IntlUnit$Intl$Time
  }


  # species
  if(!is.null(para$speciesRatioName)){
    for(var in para$speciesRatioName){
      attr(x = eddy.data[,var], which = "unit") = para$unitList$species[[var]]
    }
  }

  eddy.data

}
