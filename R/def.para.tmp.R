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
  skipCcvPos = which(paraMain$lagVars %in% skipRtio)

  # edit the original call to only the species we have
  paraCall$species = paraCall$species[paraCall$species != skip_scalar]
  paraCall$lagDefaults = eval(paraCall$lagDefaults)[-skipCcvPos]
  paraCall$lagRangeLimit = eval(paraCall$lagRangeLimit)[-skipCcvPos]

  # If there are no species left, make it NULL
  if(length(paraCall$species) == 0){
    paraCall$species = NULL
  }

  # rebuild para list with new species argument
  do.call(get("def.para", getNamespace("eddy4R.york")), as.list(paraCall))

}

