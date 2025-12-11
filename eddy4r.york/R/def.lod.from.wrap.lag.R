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
    dplyr::filter(lag > 150*freq | lag < -150*freq)

  output = specDf |>
    dplyr::group_by(name) |>
    dplyr::summarise(er = stats::sd(acf)) |>
    dplyr::mutate(
      er = ifelse(name == "tempAir", er, er*REYN$mean$densMoleAirDry),# This is the same as "conv" that is passed to def.flux.sclr. This is nothing for sens heat, but is the desity of air for latent heat and gas scalars. Definied in listGasSclr as densMoleAirDry
      `95` = er*1.96,
      `99` = er*3
      ) |>
    dplyr::select(-er) |>
    tidyr::pivot_longer(-name, names_to = "conf") |>
    dplyr::mutate(conf = as.numeric(conf)) |>
    dplyr::select(conf, name, value)

  #
  output
}
