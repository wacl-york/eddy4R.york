#' Define Rotation of Wind Vectors
#'
#' Perform various types of wind vector rotations incl. single, double, planar fit.
#'
#' @param data data.frame containing veloXaxs, veloYaxs and veloZaxs \cr
#'             vectors should be defined correspondingly as u/v/w == E/N/U == x/y/z (data.frame)
#' @param MethRot method of rotation to be used, one of: \itemize{
#'                \item "single" - rotate into the mean wind
#'                \item "double" - apply single rotation and additionally rotate to align w and z (minimise w)
#'                \item "planarFit" - apply the coefficeints for planat fit, supplied to plnrFitCoef. \cr
#'                      Note that these coefficients should be determined in the coordinate system they are applied to.
#'                \item "autoPlanrFit" - determine and apply the planar fit coefficeints from data.
#'                \item "none" - perform no rotation
#' }
#' @param plnrFitCoef coefficients for planar fit. (numeric vector or data.frame) \cr
#' Depending on plnrFitType, plnrFitCoef should be supplied as the following structure: \itemize{
#'                    \item simple - numeric vector constant of coefficeients, or coefficeients
#'                          that are controlled from the workflow. c(al,be,b0)
#'                    \item time - data.frame with columns date, al, be, b0. values with date nearest to mean(data$date) will be used
#'                    \item wind - data.frame with columns PSI_uv, al, be, b0. values with PSI_uv nearest to average PSI_uv will be used
#'                    }
#' @param plnrFitType type of planar fit, "simple", "date" or "wind". (character vector)
#'
#' @return Data object with rotated wind vectors names as u/v/veloZaxs
#'
#' @references Code adapted from REYNFlux_P5 (35ceda9) and flow.turb.tow.neon.dp04.r (914a9e9) \cr
#'  Stefan Metzger / Dave Durden / Natchaya P-Durden / Cove Sturtevant / Ke Xu
#'
#' @author W. S. Drysdale
#'
#' @export

wrap.rot = function(data,
                    MethRot = c("single","double","plnrFit","autoPlnrFit","none")[1],
                    plnrFitCoef = NULL,
                    plnrFitType = c("simple","time","wind")[1]){

  # rotation angle
  mnPSI_uv = eddy4R.base::def.pol.cart(matrix(c(mean(data$veloYaxs, na.rm = TRUE),
                                                mean(data$veloXaxs, na.rm = TRUE)),
                                              ncol=2))

  if(MethRot %in% c("single","double")){
    rotang <- (eddy4R.base::def.unit.conv(data=(mnPSI_uv+180),unitFrom="deg",unitTo="rad")) %% (2*pi)

    B <- matrix(nrow=3, ncol=3)
    B[1,1] <- cos(rotang)
    B[1,2] <- sin(rotang)
    B[1,3] <- 0.
    B[2,1] <- -sin(rotang)
    B[2,2] <- cos(rotang)
    B[2,3] <- 0.
    B[3,1] <- 0.
    B[3,2] <- 0.
    B[3,3] <- 1.
    BT <- t(B)
    U <- rbind(data$veloYaxs, data$veloXaxs, data$veloZaxs)
    Urot <- B %*% U

    if(MethRot == "double"){
      #second rotation
      rotang_v = atan2(mean(Urot[3,],na.rm = TRUE),mean(Urot[1,],na.r = TRUE))
      B2 <- matrix(nrow=3, ncol=3)
      B2[1,1] <- cos(rotang_v)
      B2[1,2] <- 0
      B2[1,3] <- sin(rotang_v)
      B2[2,1] <- 0
      B2[2,2] <- 1
      B2[2,3] <- 0
      B2[3,1] <- -sin(rotang_v)
      B2[3,2] <- 0
      B2[3,3] <- cos(rotang_v)
      B = B %*% B2
      BT = t(B)
      Urot = B2 %*% Urot
    }

    data$veloXaxs <- Urot[1,]
    data$veloYaxs <- -Urot[2,]
    data$veloZaxs <- Urot[3,]

  }

  if(MethRot == "none"){
    return(data)
  }

  if(stringr::str_detect(tolower(MethRot),"plnrfit") == T){

    if(stringr::str_detect(tolower(MethRot),"auto") == T){

      # Determine code adpted from flow.turb.tow.neon.dp04.r - 914a9e9
      # failsafe: test that greater than 2 non-NA data entries (required by lm.fit function) must exists in all of
      # veloXaxs, veloYaxs, veloZaxs, and that length(idx) > 0
      if(length(intersect(intersect(which(!is.na(data$veloXaxs)),which(!is.na(data$veloYaxs))),which(!is.na(data$veloZaxs)))) > 2) {

        # determine planar fit coefficients in units radians and m s-1
        coefPf <- eddy4R.turb::def.pf.derv.coef(u = data$veloXaxs,
                                                v = data$veloYaxs,
                                                w = data$veloZaxs)

        plnrFitCoef = c(coefPf$al,coefPf$be,coefPf$b0)

      }else{
        plnrFitCoef <- c(0,0,0)
      }
    }

    if(is.null(plnrFitCoef)){
      stop("plnrFitCoef is NULL")
    }

    if(plnrFitType == "simple"){

      # Expect a vector for plnrFitCoef
      if(!"numeric" %in% class(plnrFitCoef))
        stop("When plnrFitType == simple, plnrFitCoef must be a numeric vector")

      # Apply planar fit
      plnrFitData = eddy4R.turb::def.pf.rot(
        veloWind = data.frame(
          veloXaxs = data$veloXaxs,
          veloYaxs = data$veloYaxs,
          veloZaxs = data$veloZaxs
        ),
        AngEnuYaxs = plnrFitCoef[1],
        AngEnuXaxs = plnrFitCoef[2],
        Ofst = plnrFitCoef[3]
      )
    }

    if(plnrFitType %in% c("time","wind")){
      if(!"data.frame" %in%  class(plnrFitCoef))
        stop("When plnrFitType == time or wind, plnrFitCoef must be a data.frame")

      # Filter plnrFitCoef
      if(plnrFitType == "time")
        plnrFitCoef = plnrFitCoef[which.min(plnrFitCoef$date-mean(data$date,na.rm = TRUE)),]
      # for time, the nearest plnrFitCoef to the mean date is selected
      if(plnrFitType == "wind"){
        min_dir = which.min(abs(plnrFitCoef$PSI_uv-mnPSI_uv))
        plnrFitCoef = plnrFitCoef[min_dir,]
      } # for wind, the nearest plnrFitCoef to the mean PSI_uv is selected

      # Apply planar fit
      plnrFitData = eddy4R.turb::def.pf.rot(
        veloWind = data.frame(
          veloXaxs = data$veloXaxs,
          veloYaxs = data$veloYaxs,
          veloZaxs = data$veloZaxs
        ),
        AngEnuYaxs = plnrFitCoef$al,
        AngEnuXaxs = plnrFitCoef$be,
        Ofst = plnrFitCoef$b0
      )

    }

    # reassign vectors
    data$veloXaxs = plnrFitData$xaxs
    data$veloYaxs = plnrFitData$yaxs
    data$veloZaxs = plnrFitData$zaxs
  }

  # Return
  data

}
