#' Define Rotation Matrix
#'
#' Defines a rotation matrix in degrees, where a positive value rotates in the clockwise direction
#'
#' @param degrees rotation in degrees - cw == +ve
#'
#' @return rotation matrix
#'
#' @author W. S. Drysdale
#'
#' @export


def.rot.mat = function(degrees){
  rad = degrees*(pi/180)
  matrix(
    c(cos(rad),sin(rad),-sin(rad),cos(rad)),
    ncol = 2)
}
