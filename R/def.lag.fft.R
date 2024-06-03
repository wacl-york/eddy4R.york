#' Define lag Time via fft
#'
#' Adapted from code from link below. Utilise fft to quickly determine the lag time of a scalar vs reference. \cr
#' Scalar is expected to be behind the reference data
#'
#' https://stats.stackexchange.com/questions/147105/r-own-implementation-of-cross-correlation-using-convolution
#'
#' @param scal vector of scalar
#' @param ref vector of reference data
#' @param trim +/- should the corr output be trimmed to in time steps
#' @param lagNgtvPstv "n", "p" or "np" - are we searching for negative or positive lags, or should we search in both directions
#'
#' @author W. S. Drysdale
#'
#' @export

def.lag.fft <- function(ref,scal,trim = NULL,lagNgtvPstv) {
  x = scal
  y = ref
  n <- length(x)
  if(is.null(trim))
    trim = n
  # Normalize
  x <- as.numeric(scale(x))
  y <- as.numeric(scale(y))
  # Enlarge with 0's to size 2*n-1 to account for periodicity
  x <- c(x, rep(0, length(x) - 1))
  y <- c(y, rep(0, length(y) - 1))
  # FFT
  xfft <- stats::fft(x)
  yfft <- stats::fft(y)
  # Cross-correlation via convolution
  crosscor <- stats::fft(Conj(xfft) * yfft, inverse=T) / length(x)
  crosscor <- Re(crosscor) / (n - 1)
  # Slice it up to make it for lags -n:n not 0:(2n-1)
  crosscor <- c(crosscor[(n+1):length(crosscor)], crosscor[1:n])
  # Store lag as names attribute of vector

  corr = data.frame(index = (1-n):(n-1))
  corr$crosscor = crosscor
  corr = corr[corr$index <= trim & corr$index >= -trim,]

  if(lagNgtvPstv == "n")
    corr = corr[corr$index <=0,]
  if(lagNgtvPstv == "p")
    corr = corr[corr$index >= 0,]
  lag = corr$index[abs(corr$crosscor) == max(abs(corr$crosscor))]

  corr = corr[corr$index <= trim & corr$index >= -trim,]
  ret = list(
    lag = lag,
    corrCros = max(abs(corr$crosscor)),
    corr = corr
  )

}
