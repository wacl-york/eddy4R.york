#' Define how missing values are handled
#'
#' @description
#' One of:
#' * drop rows containing missing values
#' * fill missing values with column mean
#'
#' @param eddy.data eddy.data data frame
#' @param para parameters created by def.uoy.para
#'
#' @author W. S. Drysdale
#'
#' @return eddy.data with missing values handled
#'
#' @export

def.miss.hndl = function(eddy.data,para){

  if(para$missing_method == "drop"){
    row.has.na <- apply(eddy.data, 1, function(x){any(is.na(x))})
    eddy.data <- eddy.data[!row.has.na,]

    if(1-(nrow(eddy.data)/(para$agg_period*para$freq)) > para$missing_thresh)
      stop("using missing_method drop has caused avaliable data to fall belowing missing_thresh")
    return(eddy.data)
  }

  if(para$missing_method == "mean"){
    for(i in 1:ncol(eddy.data))
      eddy.data[,i][is.na(eddy.data[,i])] = mean(eddy.data[,i],na.rm = T)

    return(eddy.data)
  }

}
