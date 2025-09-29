#' Wrap Spectral Analysis
#'
#' Computes the power and co-spectra using \code{eddy4R.turb::def.spec.fft.fwd()}
#' and \code{eddy4R.turb::def.spec.cosp()}. Output is then tidied into data.frame
#' containing measured, relative (0-1) and normalised (f*(U/(z-d))) frequency,
#' as well as the power and cospectra, which are marked by the "spectrumType" column
#'
#' @param eddy.data time domain data including "veloZaxs" and "tempAir".
#'                  It is expected that lag corrections and other QA/QC has been
#'                  performed on these data already.
#' @param speciesRatioName chemical species for flux calculations, can be NULL if
#'                         no species are present.
#'
#' @inheritParams def.para
#'
#' @author W. S. Drysdale
#'
#' @export

wrap.spec = function(
    eddy.data,
    speciesRatioName,
    freq,
    spectralTaperingWeight = 0.05
    ){

  fftOut = eddy4R.turb::def.spec.fft.fwd(
    time = eddy.data$unixTime,
    data = eddy.data[, c("veloZaxs", "tempAir",speciesRatioName)] |>
      as.matrix(),
    veloRltv = sqrt(eddy.data$veloXaxs^2 + eddy.data$veloZaxs^2),
    distZaxsMeas = eddy.data$distZaxsMeas,
    FreqSamp = freq,
    WghtTape = spectralTaperingWeight
  )

  cospec = eddy4R.turb::def.spec.cosp(
    dataSpec = fftOut$specFft,
    dataTimeDomn = fftOut$dataSpecInp
  )

  cospec$dataCosp |>
    as.data.frame() |>
    tibble::tibble() |>
    dplyr::bind_cols(as.data.frame(fftOut$specFftEngy)) |>
    dplyr::mutate(
      freqMeas = fftOut$freqMeas,
      freqRltv = fftOut$freqRltv,
      freqNorm = fftOut$freqNorm
    ) |>
    dplyr::filter(dplyr::row_number() < max(dplyr::row_number())/2) |>
    tidyr::pivot_longer(-tidyselect::contains("freq")) |>
    dplyr::mutate(
      spectrumType = ifelse(stringr::str_detect(.data$name, "specFft"), "power", "co")
    )
}


