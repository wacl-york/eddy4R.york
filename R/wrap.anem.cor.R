#' Anemometer Corrections Wrapper
#'
#' Apply transformations to wind vectors easily. Steps in order: \cr
#' Use def.rot.mat to rotate around the uv plane - para$anemometer_offset \cr
#' Apply W-boost correction to Gill Anemometers - para$w_boost
#'
#' @param eddy.data eddy.data data.frame
#' @param para parameters list
#'
#' @author W. S. Drysdale
#'
#' @export

wrap.anem.cor = function(eddy.data,para){

  orignal_data = eddy.data

  # Rotate vectors
  if(!is.null(para$anemometer_offset)){
    rotMat = def.rot.mat(para$anemometer_offset)
    unrotVec = as.matrix(eddy.data[,c("veloXaxs","veloYaxs")])
    rotVec = unrotVec %*% rotMat
    eddy.data$veloXaxs = rotVec[,1]
    eddy.data$veloYaxs = rotVec[,2]
  }

  # W-boost
  # option to apply corrections to Gill Anemometers detailed here:
  # http://gillinstruments.com/data/manuals/KN1509_WindMaster_WBug_info.pdf
  if(para$w_boost){
    eddy.data$veloZaxs[eddy.data$veloZaxs > 0 ] = eddy.data$veloZaxs[eddy.data$veloZaxs > 0 ]*1.166
    eddy.data$veloZaxs[eddy.data$veloZaxs < 0 ] = eddy.data$veloZaxs[eddy.data$veloZaxs < 0 ]*1.289
  }


  # re calculate uv_met and d_xy_flow
  eddy.data = def.wind.dir.flow(eddy.data,para$freq)
  # Return
  eddy.data
}
