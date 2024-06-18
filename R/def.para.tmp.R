#' Remove scalars from para
#'
#' Removes the scalars from para that can be skipped this itteration
#'
#' @param paraMain the main para list
#' @param skip_scalar names of species
#'
#'
#' @author W. S. Drysdale
#'
#' @export

def.para.tmp = function(paraMain, skip_scalar){

  paraCall = paraMain$call

  skipRtio = paraMain$speciesRatioName[which(paraMain$species %in% c(skip_scalar))]
  skipCcvPos = which(paraMain$cross_correlation_vars %in% skipRtio)

  # edit the original call to only the species we have
  paraCall$species = paraCall$species[paraCall$species != skip_scalar]
  paraCall$absolute_lag = eval(paraCall$absolute_lag)[-skipCcvPos]
  paraCall$lag_boundary = eval(paraCall$lag_boundary)[-skipCcvPos]

  # If there are no species left, make it NULL
  if(length(paraCall$species) == 0){
    paraCall$species = NULL
  }

  # rebuild para list with new species argument
  do.call(utils::getFromNamespace("def.para","eddy4R.york"), as.list(paraCall))

}

