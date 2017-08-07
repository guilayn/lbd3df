#' Function for internal use
#'
#' It simply detects and replace values in the spectrum based on x and y conditions
#' @param x Emission wavelenght
#' @param y Excitation wavelenght
#' @param z Intensity
#' @param replace_by Which value will replace eliminated values, default is 0, NA might be useful
#' @keywords 3D fluorescence
#' @export
#' @examples diffra_detect(500,250,999) returns 0 because it is within the diffraction zone.

diffra_detect=function(x,y,z,replace_by=0) {
  if (x<390) {return(z)}
  else if (x<490) {
    if(((y+34)-(0.6*x))<0)
    {return(replace_by)} else {return(z)}
  }
  else {
    if(((y-37.23)-(0.4545*x))<0)
    {return(replace_by)} else {return(z)}
  }
}
