#' Wrap UoY Despiking
#'
#' Contains the despiking loop
#'
#' @param eddy.data eddy.data object
#' @param despikeVars character vector of variables to be despiked
#' @param despikeThreshold number of standard deviations to qualify as a spike
#' @param verbose boolean - when false the messages from \code{def.dspk.br86} are suppressed
#'
#' @author W. S. Drysdale
#'
#' @export

wrap.despike = function(eddy.data,despikeVars,despikeThreshold,verbose){

  quiet <- function(x) {
    sink(tempfile())
    on.exit(sink())
    invisible(force(x))
  }

  for (i in 1:length(despikeVars)){
    if(verbose){
      despiked = eddy4R.qaqc::def.dspk.br86(dataIn = eddy.data[,despikeVars[i]],
                                            ThshReso = despikeThreshold[i])
    }else{
      despiked = quiet(eddy4R.qaqc::def.dspk.br86(dataIn = eddy.data[,despikeVars[i]],
                                                  ThshReso = despikeThreshold[i]))
    }

    eddy.data[,despikeVars[i]] = despiked$dataOut
  }
  #Return
  eddy.data
}
