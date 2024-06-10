#' Definition function. Create uv_met and d_xy_flow columns
#'
#' Used to create/recreate the wind vector dependant uv_met and d_xy_flow columns. \cr
#' d_xy_flow needs to be recreated for each aggregation period, used during eddy.data subsetting in read.e4r_input() \cr
#' uv_met and d_xy_flow also need recreating if anemometer definitions are altered. used in wrap.anem.cor()
#'
#' @param eddy.data data.frame containing veloXaxs and veloYaxs
#' @param freq time resolution of data in Hz
#'
#' @author W. S. Drysdale
#'
#' @export

def.wind.dir.flow = function(eddy.data,freq){
  veloXaxs = eddy.data$veloXaxs
  veloYaxs = eddy.data$veloYaxs

  veloXaxs[is.na(veloXaxs)] = 0
  veloYaxs[is.na(veloYaxs)] = 0

  # eddy.data$uv_met = sqrt(veloXaxs^2 + veloYaxs^2)
  #
  # eddy.data$d_xy_flow = cumsum(eddy.data$uv_met)/freq

  eddy.data

}
