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

  # edit the original call to only the species we have
  paraCall$species = paraCall$species[paraCall$species != skip_scalar]

  # If there are no species left, make it NULL
  if(length(paraCall$species) == 0){
    paraCall$species = NULL
  }

  # rebuild para list with new species argument
  do.call(utils::getFromNamespace("def.para","eddy4R.york"), as.list(paraCall))

}

