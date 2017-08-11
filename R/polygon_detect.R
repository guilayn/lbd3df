#' Function for internal use
#'
#' It simply detects if a given coordinate is inside a polygon with a previous established function.
#'
#'
#' @param coordinates Polygon coordinate (MATRIX object)
#' @param x Emission wavelenght
#' @param y Excitation wavelenght
#' @param z Intensity
#' @param replace_by Which value will replace eliminated values, default is 0, NA might be useful
#' @keywords 3D fluorescence
#' @export

polygon_detect=function(coordinates, x, y, z, replace_by=0) {
  point=matrix(c(x,y),nrow=1)
  if(!in.out(coordinates,point))
    {
    return(replace_by)}
  else {return(z)}
  }
#end of the function
