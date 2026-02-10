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
#'
#' @inheritParams def.para
#'
#' @export

assign_input_units = function(eddy.data,
                              unitList,
                              idepVar,
                              speciesRatioName){
  # temperature
  attr(x = eddy.data$tempAir, which = "unit") = unitList$temp

  # pressure
  attr(x = eddy.data$presAtm, which = "unit") = unitList$pres

  # wind vectors
  for(var in c("veloXaxs", "veloYaxs", "veloZaxs", "veloXaxsInp", "veloYaxsInp", "veloZaxsInp")){
    attr(x = eddy.data[,var], which = "unit") = unitList$velo
  }

  # distances
  for(var in c("distZaxsAbl", "distZaxsMeas")){
    attr(x = eddy.data[,var], which = "unit") = unitList$dist
  }

  # H2O
  attr(x = eddy.data$rtioMoleDryH2o, which = "unit") = unitList$h2o

  # unix time
  attr(x = eddy.data$unixTime, which = "unit") = eddy4R.base::IntlUnit$Intl$Time

  # idep - for airc probably will want to define a different idep...
  if(idepVar == "unixTime"){
    attr(x = eddy.data$idep, which = "unit") = eddy4R.base::IntlUnit$Intl$Time
  }


  # species
  if(!is.null(speciesRatioName)){
    for(var in speciesRatioName){
      attr(x = eddy.data[,var], which = "unit") = unitList$species[[var]]
    }
  }

  eddy.data

}

#' Define Rotation Matrix
#'
#' Defines a rotation matrix in degrees, where a positive value rotates in the clockwise direction
#'
#' @param degrees rotation in degrees - cw == +ve
#'
#' @return rotation matrix
#'
#' @author W. S. Drysdale
#'
#' @export


def.rot.mat = function(degrees){
  rad = degrees*(pi/180)
  matrix(
    c(cos(rad),sin(rad),-sin(rad),cos(rad)),
    ncol = 2)
}

#' Log eddy4.york
#'
#' Uses a logger object created by eddy4R.base::Logger.Singleton and writes a log message of the format:
#' timestamp log_level: errorType | period: aggregationPeriodText | errorSimple
#'
#' @param logger the logger object
#' @param logLevel one of warn, error, info or debug
#' @param header character string describing the succinctly describing where in the workflow this is being logged from.
#' @param periodStartDate periodStartDate
#' @param periodEndDate description
#' @param error the simpleError caught by tryCatch
#'
#' @export

log_message = function(logger, logLevel, header, periodStartDate, periodEndDate, error = NULL){

  quiet <- function(x) {
    sink(tempfile())
    on.exit(sink())
    invisible(force(x))
  }

  aggregationPeriodText = paste(periodStartDate,periodEndDate, sep = " - ")

  if(is.null(error)){
    quiet(
      logger$log_message(level = logLevel,
                         message = paste0(header," | period: ",aggregationPeriodText))
    )
  }else{

    if("simpleError" %in% class(error)){
      if(!is.null(error$parent)){
        errorString = paste(error$message,
                            as.character(error$parent))
      }else{
        errorString = error$message
      }
    }else{
      errorString = error
    }

    quiet(
    logger$log_message(level = logLevel,
                       message = paste0(header," | period: ",aggregationPeriodText,
                                        " | Error: ",errorString))
    )
  }

}





