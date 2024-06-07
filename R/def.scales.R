#' Definition function: Wrapper function for calculating integral scale lengths and turbulence characteristics.
#'
#' Function defintion. Wrapper function for calculating integral scale lengths and turbulence characteristics.
#'
#' @author
#' Adam Vaughan \email{adam.vaughan@york.ac.uk}
#'
#' @param REYN Output list from eddy4R.turb REYNflux function.
#' @param species names of scalars to be passed to \code{def.spcs.name}
#' @param lat Latitude and of class "numeric".
#' @param VarInp A vector of class "character" containing the name of variables
#' to be performed integral turbulence characteristics test. VarInp =
#' c("veloXaxs","veloZaxs","temp","all"), where "veloXaxs" is along-axis horizontal
#' wind speed, "veloZaxs" is vertical-axis wind speed, "temp" is air temperature,
#' and "all" is all three variables. Defaults to "all".
#' @param sd A vector or data frame containing standard deviation of VarInp and
#' of class "numeric". If VarInp = "all", sd shall contain in the follwing orders,
#' standard deviation of along-axis horizontal wind speed, standard deviation of
#' vertical-axis wind speed, and standard deviation of air temperature.
#' @param varScal A vector or data frame containing the scaling variables of VarInp
#' and of class "numeric". If VarInp = "all", varScal shall contain in the follwing
#' orders, scaling variable of wind speed (friction velocity will be used for both
#' "veloXaxs" and "veloZaxs") and scaling variable of air temperature.
#'
#' @export
#'
#' Changelog
#'  Will Drysdale (2019-01-29)
#'    Removed SiteInfo dependance, added switches as explicit variables
#'  Natchaya Pingintha-Durden (2019-09-09)
#'    added determination of qfItc for each flux
#'  Natchaya Pingintha-Durden (2019-10-24)
#'    update to use itc from w_hor to represent itc of F_H_en

def.scales <- function(REYN,
                       lat,
                       VarInp=c("veloXaxs", "veloZaxs", "temp", "all")[4],
                       sd,
                       varScal,
                       species = NULL
){

  REYN$itcs <- eddy4R.turb::def.itc(stblObkv = REYN$mn$sigma,
                                    lat = lat,
                                    VarInp = VarInp,
                                    sd = sd,
                                    varScal = varScal)

  attributes(REYN$data$d_xy_flow)$unit <- "m"

  whr_scal <- c("u_hor", "v_hor", "w_hor", "T_air", "T_air_0", "T_v_0")
  if("ratioMoleDryH2o" %in% names(REYN$imfl))
    if(sd(REYN$imfl$ratioMoleDryH2o)>0)
      whr_scal <- c(whr_scal,"ratioMoleDryH2o")
  if(!is.null(species))
    whr_scal <- c(whr_scal,def.spcs.name(species,"mole"))

  whr_flux=c("u_star2_x", "u_star2_y", "u_star", "F_H_en")
  if("F_LE_en" %in% names(REYN$imfl))
    if(sd(REYN$imfl$ratioMoleDryH2o)>0)
      whr_flux <- c(whr_flux,"F_LE_en")
  if(!is.null(species))
    whr_flux <- c(whr_flux, def.spcs.name(species,"mass"))

  #save whr_flux name before delete by whr_not; will use later in qfItc
  qfColName <- whr_flux

  isca_vari <- data.frame(t(sapply(whr_scal,function(x) eddy4R.turb::def.dist.isca(scalEddy=REYN$data$d_xy_flow,data=REYN$imfl[,x]^2))))

  isca_scal <- data.frame(t(sapply(whr_scal, function(x) eddy4R.turb::def.dist.isca(
    scalEddy=REYN$data$d_xy_flow,
    data=REYN$imfl[,x]))))


  whr_not <- sapply(whr_flux, function(x) length(which(is.na(REYN$imfl[,x]))) / nrow(REYN$imfl))
  whr_not <- which(whr_not > 0.1)
  if(length(whr_not) == 0)
    whr_flux <-  whr_flux
  else
    whr_flux <-  whr_flux[-whr_not]

  isca_flux <- data.frame(t(sapply(whr_flux, function(x) eddy4R.turb::def.dist.isca(
    scalEddy=REYN$data$d_xy_flow,
    data=REYN$imfl[,x]))))

  #determine ITC, model ITC and qfItc for each flux
  #create empty datafrme
  itc <- data.frame(matrix(ncol = length(qfColName), nrow = 1))
  colnames(itc) <- qfColName
  lapply(names(itc), function(x)  {
    if (!(x %in% c("u_star2_x", "u_star2_y", "u_star", "F_H_en"))) {itc[[x]] <<- REYN$itcs$itc$w_hor}
    if (x %in% c("u_star2_x", "u_star")) {itc[[x]] <<-  REYN$itcs$itc$u_star}
    if (x %in% "u_star2_y") {itc[[x]] <<-  NA}
    #if (x %in% "F_H_en") {itc[[x]] <<-  REYN$itcs$itc$F_H_kin}
    if (x %in% "F_H_en") {itc[[x]] <<-  REYN$itcs$itc$w_hor}
  })

  # Model ITC output
  ModlItc <- data.frame(matrix(ncol = length(qfColName), nrow = 1))
  colnames(ModlItc) <- qfColName
  lapply(names(ModlItc), function(x)  {
    if (!(x %in% c("u_star2_x", "u_star2_y", "u_star", "F_H_en"))) {ModlItc[[x]] <<- REYN$itcs$ModlItc$w_hor}
    if (x %in% c("u_star2_x", "u_star")) {ModlItc[[x]] <<-  REYN$itcs$ModlItc$u_star}
    if (x %in% "u_star2_y") {ModlItc[[x]] <<-  0}
    if (x %in% "F_H_en") {ModlItc[[x]] <<-  REYN$itcs$ModlItc$F_H_kin}
  })

  #create empty datafrme
  qfItc <- data.frame(matrix(ncol = length(qfColName), nrow = 1))
  colnames(qfItc) <- qfColName
  lapply(names(qfItc), function(x)  {
    if (!(x %in% c("u_star2_x", "u_star2_y", "u_star", "F_H_en"))) {qfItc[[x]] <<- REYN$itcs$qfItc$w_hor}
    if (x %in% c("u_star2_x", "u_star")) {qfItc[[x]] <<-  REYN$itcs$qfItc$u_star}
    if (x %in% "u_star2_y") {qfItc[[x]] <<-  0}
    #if (x %in% "F_H_en") {qfItc[[x]] <<-  REYN$itcs$qfItc$F_H_kin}
    if (x %in% "F_H_en") {qfItc[[x]] <<-  REYN$itcs$qfItc$w_hor}
  })
  isca <- list(scal=isca_scal,
               vari=isca_vari,
               flux=isca_flux,
               itc = itc,
               ModlItc = ModlItc,
               qfItc=qfItc)

  return(isca)
}
