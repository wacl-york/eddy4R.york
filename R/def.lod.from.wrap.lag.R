#' Limit of Detection from wrap.lag output
#'
#' Uses the output of \code{wrap.lag} to calculate the LOD following Langford et al 2015 and \code{def.lod}.
#'
#' @param lag_out output of wrap.lag
#'
#' @inheritParams def.lod
#'
#' @export



def.lod.from.wrap.lag = function(
    lag_out,
    REYN,
    freq){

  specDf = lag_out$ACF |>
    dplyr::filter(.data$lag > 150*freq | .data$lag < -150*freq)

  output = specDf |>
    dplyr::group_by(.data$name) |>
    dplyr::summarise(er = stats::sd(.data$acf)) |>
    dplyr::mutate(
      er = ifelse(.data$name == "tempAir", .data$er, .data$er*REYN$mean$densMoleAirDry),# This is the same as "conv" that is passed to def.flux.sclr. This is nothing for sens heat, but is the desity of air for latent heat and gas scalars. Definied in listGasSclr as densMoleAirDry
      `95` = .data$er*1.96,
      `99` = .data$er*3
      ) |>
    dplyr::select(-"er") |>
    tidyr::pivot_longer(-tidyselect::all_of("name"), names_to = "conf") |>
    dplyr::mutate(conf = as.numeric(.data$conf)) |>
    dplyr::select("conf", "name", "value")

  #
  output
}
