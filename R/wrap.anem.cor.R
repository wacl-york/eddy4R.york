#' Anemometer Corrections Wrapper
#'
#' Apply transformations to wind vectors easily. Steps in order: \cr
#' Use def.rot.mat to rotate around the uv plane - anemometerOffset \cr
#' Apply W-boost correction to Gill Anemometers - wBoost
#'
#' @param eddy.data eddy.data data.frame
#'
#' @inheritParams def.para
#'
#' @author W. S. Drysdale
#'
#' @export

wrap.anem.cor = function(eddy.data,
                         anemometerOffset,
                         wBoost){

  orignal_data = eddy.data

  # Rotate vectors
  if(!is.null(anemometerOffset)){
    rotMat = def.rot.mat(anemometerOffset)
    unrotVec = as.matrix(eddy.data[,c("veloXaxs","veloYaxs")])
    rotVec = unrotVec %*% rotMat
    eddy.data$veloXaxs = rotVec[,1]
    eddy.data$veloYaxs = rotVec[,2]
  }

  # W-boost
  # option to apply corrections to Gill Anemometers detailed here:
  # http://gillinstruments.com/data/manuals/KN1509_WindMaster_WBug_info.pdf
  if(wBoost){
    eddy.data$veloZaxs[eddy.data$veloZaxs > 0 ] = eddy.data$veloZaxs[eddy.data$veloZaxs > 0 ]*1.166
    eddy.data$veloZaxs[eddy.data$veloZaxs < 0 ] = eddy.data$veloZaxs[eddy.data$veloZaxs < 0 ]*1.289
  }


  # Preserve Input Vectors for Wind Direction Calculation -------------------

  eddy.data$veloXaxsInp = eddy.data$veloXaxs
  eddy.data$veloYaxsInp = eddy.data$veloYaxs
  eddy.data$veloZaxsInp = eddy.data$veloZaxs

  # Return
  eddy.data
}
