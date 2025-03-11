#' Define how missing values are handled
#'
#' @description
#' One of:
#' * drop rows containing missing values
#' * fill missing values with column mean
#'
#' @param eddy.data eddy.data data frame
#'
#' @inheritParams def.para
#'
#' @author W. S. Drysdale
#'
#' @return eddy.data with missing values handled
#'
#' @export

def.miss.hndl = function(eddy.data,
                         missingMethod,
                         missingThreshold,
                         aggregationPeriod,
                         freq){

  if(missingMethod == "drop"){
    row.has.na <- apply(eddy.data, 1, function(x){any(is.na(x))})
    eddy.data <- eddy.data[!row.has.na,]

    if(1-(nrow(eddy.data)/(aggregationPeriod*freq)) > missingThreshold)
      stop("using missingMethod drop has caused avaliable data to fall belowing missingThreshold")
    return(eddy.data)
  }

  if(missingMethod == "mean"){
    for(i in 1:ncol(eddy.data))
      eddy.data[,i][is.na(eddy.data[,i])] = mean(eddy.data[,i],na.rm = T)

    return(eddy.data)
  }

}
