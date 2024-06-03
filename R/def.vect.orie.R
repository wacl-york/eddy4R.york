#' Define Vector Orientation
#'
#' Apply a transformation to a wind vector based on particular codes: \cr
#' e.g +u_met will return the positive u_met column, -u_met will return u_met*-1 \cr
#' Used in wrapper to apply user defined wind vector corrections \cr
#' Function returns the column only so that the wrapper can handle reasssignment to data.frame \cr
#' this is to avoid issues surrounding the order in which the function is applied
#'
#' @param eddy.data eddy.data data.frame
#' @param vec_id character string of format (+/-)(u/v/w)_met
#'
#' @return wind vector column with transformation applied
#'
#' @author W. S. Drysdale
#'
#' @export

def.vect.orie = function(eddy.data,vec_id){
  if(nchar(vec_id) != 6)
    stop("vec_id must be exactly 6 characters in length e.g '+u_met'")

  sign = stringr::str_split_fixed(vec_id,pattern = "",n = 2)[1]
  col  = stringr::str_split_fixed(vec_id,pattern = "",n = 2)[2]

  if(sign == "+")
    ret = eddy.data[,col]

  if(sign == "-")
    ret = eddy.data[,col]*-1

  #return
  ret
}
