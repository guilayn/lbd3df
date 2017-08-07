#' Function for internal use
#'
#' It simply detects and replace values in the spectrum based on x and y conditions
#' @param x Emission wavelenght
#' @param y Excitation wavelenght
#' @param z Intensity
#' @param span Excitation "distance" from the x=y diffusion line that will also be eliminated
#' @param replace_by Which value will replace eliminated values, default is 0, NA might be useful
#' @keywords 3D fluorescence
#' @export
#' @examples diffu_detect(350,350,999)returns 0 because it is within the diffusion zone.

diffu_detect=function(x,y,z,span=16,replace_by=0) {if((y+span-x)>0) {return(replace_by)} else {return(z)}}
