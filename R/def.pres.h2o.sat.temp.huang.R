#' Calculate Saturated Water Vapour Pressure (Huang)
#'
#' Calculate Saturated Water Vapour Pressure from Temperature using Huang Formulation
#' See [Huang 2018 JAMC](https://doi.org/10.1175/JAMC-D-17-0334.1)
#'
#' @param temp temperature in K
#'
#' @author W. S. Drysdale
#'
#' @export

def.pres.h2o.sat.temp.huang = function(temp){
  # derrive water saturaton pressure (i.e vapour pressure at 100% RH)
  # based on emperical equation from https://doi.org/10.1175/JAMC-D-17-0334.1

  temp = temp-273.15

  presH2oSat = exp(34.494 - (4924.99 / (temp + 237.1))) / ((temp + 105)^1.57)

  presH2oSat
}



