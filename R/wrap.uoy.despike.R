#' Wrap UoY Despiking
#'
#' Contains the despiking loop
#'
#' @param eddy.data eddy.data object
#' @param despike_vars character vector of variables to be despiked
#' @param despike_threshold number of standard deviations to qualify as a spike
#' @param verbose boolean - when false the messages from \code{def.dspk.br86} are suppressed
#'
#' @author W. S. Drysdale
#'
#' @export

wrap.uoy.despike = function(eddy.data,despike_vars,despike_threshold,verbose){

  quiet <- function(x) {
    sink(tempfile())
    on.exit(sink())
    invisible(force(x))
  }

  for (i in 1:length(despike_vars)){
    if(verbose){
      despiked = eddy4R.qaqc::def.dspk.br86(dataIn = eddy.data[,despike_vars[i]],
                                            ThshReso = despike_threshold[i])
    }else{
      despiked = quiet(eddy4R.qaqc::def.dspk.br86(dataIn = eddy.data[,despike_vars[i]],
                                                  ThshReso = despike_threshold[i]))
    }

    eddy.data[,despike_vars[i]] = despiked$dataOut
  }
  #Return
  eddy.data
}
