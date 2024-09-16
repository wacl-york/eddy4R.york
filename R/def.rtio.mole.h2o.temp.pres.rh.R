#' Calculate dry mole fraction of H2O from temperature, pressure and Relative Humidity
#'
#' Uses the saturation water vapour pressure determined via [def.pres.h2o.sat.temp.huang]
#' along with air temperature (K) and pressure (Pa) to calculate the dry mole fraction
#' of H2O.
#'
#' @param temp air temperature in K
#' @param pres air pressure in Pa
#' @param rh relative humidity as a percentage (e.g 50 for 50%)

def.rtio.mole.h2o.temp.pres.rh = function(temp, pres, rh){

  # water saturation vapour pressure in Pa
  presH2oSat = def.pres.h2o.sat.temp.huang(temp)

  # water vapour pressure
  presH2o = rh * (presH2oSat / 100)

  #
  rtioMolmH2oAir = eddy4R.base::IntlNatu$MolmH2o/eddy4R.base::IntlNatu$MolmDry
  rtioMolmAirH2o = eddy4R.base::IntlNatu$MolmDry/eddy4R.base::IntlNatu$MolmH2o

  # specific humidity in dry air (kg kg-1)
  rtioMassDryH2o = (rtioMolmH2oAir*presH2oSat)/(pres - ((1-rtioMolmH2oAir)*presH2oSat))

  # specific humidity in dry air (mol mol-1)
  rtioMoleDryH2o = rtioMassDryH2o*rtioMolmAirH2o

  attributes(rtioMoleDryH2o) = NULL

  rtioMoleDryH2o
}

