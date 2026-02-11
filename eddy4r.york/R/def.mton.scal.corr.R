#' Definition Function: Motion Scale Correction
#'
#' Apply motion scale correction to the vertical wind vector (veloZaxs).
#' Correction is outlined in Prytherch et al. (2015) equation 2.
#' C. G. Stapleton tested to check there was no difference if applied to raw data vs applied to instantaneous deviations
#'
#' @param eddy.data eddy.data input data.frame containing veloZaxs, veloZpltf and accZpltf
#'
#' @author C. G. Stapleton
#'
#' @export

def.mton.scal.corr  <- function(eddy.data){

  # Compute linear coefficients
  alpha1 <- stats::lm(eddy.data$veloZaxs ~ eddy.data$accZpltf)$coefficients[2]

  alpha2 <- stats::lm(eddy.data$veloZaxs ~ eddy.data$veloZpltf)$coefficients[2]

  # Apply correction
  eddy.data$veloZaxs <- eddy.data$veloZaxs -
    (alpha1 * eddy.data$accZpltf) -
    (alpha2 * eddy.data$veloZpltf)

  return(eddy.data)
}
