#' Wrap K04 Footprint model
#'
#' Wraps the K04 (\code{eddy4R.turb::def.foot.k04}) footprint model to pass
#' params and tidy output into data.frame. Outputs data.frame with x, y and value columns
#' x and y are in m (based off of footprintResolutionM).
#'
#' @inheritParams def.para
#  I think the \code{} in the eddy4r package documentation prevents using @inheritParams here
#' @param angZaxsErth wind direction to rotate the inertial footprint matrix
#' @param veloYaxsHorSd crosswind fluctuations
#' @param veloZaxsHorSd vertical wind fluctuations
#' @param veloFric friction velocity
#' @param distZaxsMeasDisp height of measurement - displacement
#' @param distZaxsAbl	boundary layer height
#' @param distZaxsRgh Roughness length
#' @param univFunc integral over the stability-dependent universal function to make the log-wind-profile applicable to different atmospheric stratifications; from def.func.univ()
#'
#' @export
#'
#' @author W. S. Drysdale

wrap.foot.k04 = function(
    angZaxsErth,
    footprintResolutionM,
    veloYaxsHorSd,
    veloZaxsHorSd,
    veloFric,
    distZaxsMeasDisp,
    distZaxsAbl,
    distZaxsRgh,
    footprintCumulativeThreshold,
    univFunc
){

  foot = eddy4R.turb::def.foot.k04(
    angZaxsErth = angZaxsErth,
    distReso = footprintResolutionM,
    veloYaxsHorSd = veloYaxsHorSd,
    veloZaxsHorSd = veloZaxsHorSd,
    veloFric = veloFric,
    distZaxsMeasDisp = distZaxsMeasDisp,
    distZaxsAbl = distZaxsAbl,
    distZaxsRgh = distZaxsRgh,
    thsh = footprintCumulativeThreshold,
    univFunc = univFunc
  )


  foot$wghtFootXaxsYaxsItgr |>
    apply(2, rev) |> # rotate the matrix so when we melt x and y are correct.
    t() |>
    as.data.frame() |>
    dplyr::mutate(x = dplyr::row_number()*footprintResolutionM) |>
    tidyr::pivot_longer(-"x", names_to = "y") |>
    dplyr::mutate(y = as.numeric(stringr::str_remove(.data$y, "V"))*footprintResolutionM)

}
