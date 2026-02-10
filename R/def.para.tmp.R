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

  skipSpeciesPosition = which(paraMain$species %in% skip_scalar)
  skipRtio = paraMain$speciesRatioName[which(paraMain$species %in% skip_scalar)]
  skipLagVarPos = which(paraMain$lagVars %in% skipRtio)

  # edit the original call to only the species we have
  paraCall$species = eval(paraCall$species)[-skipSpeciesPosition]
  paraCall$lagDefaults = eval(paraCall$lagDefaults)[-skipLagVarPos]
  paraCall$lagRangeLimit = eval(paraCall$lagRangeLimit)[-skipLagVarPos]
  paraCall$lagVars = eval(paraCall$lagVars)[-skipLagVarPos]

  # If there are no species left, make it NULL
  if(length(paraCall$species) == 0){
    paraCall$species = NULL
  }

  # rebuild para list with new species argument
  eval(paraCall)

}

