#' Motion Scale Correction Wrapper
#'
#' Apply motion scale correction to the vertical wind vector (veloZaxs).
#' Correction is outlined in Prytherch et al. (2015) equation 2.
#' C. G. Stapleton tested to check there was no difference if applied to raw data vs applied to instantaneous deviations
#'
#' @param eddy.data eddy.data data.frame
#'
#' @author C. G. Stapleton
#'
#' @export

wrap.mton.scale.corr <- function(eddy.data){

  # Compute linear coefficients
  alpha1 <- (lm(eddy.data$veloZaxs ~ eddy.data$accZplat)$coefficients[2])

  alpha2 <- (lm(eddy.data$veloZaxs ~ eddy.data$veloZplat)$coefficients[2])

  # Apply correction
  eddy.data$veloZaxs <- eddy.data$veloZaxs -
    (alpha1 * eddy.data$accZplat) -
    (alpha2 * eddy.data$veloZplat)

  return(eddy.data)
}
