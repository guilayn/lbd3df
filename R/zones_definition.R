#' Function for internal use.
#'
#' Definition of zones.
#'
#'
#' @param user_option Default is "Default". It generates the 7 zones defined by Jimenez et a. (2015). Put anything different and set your own zones with the option below if you want. Otherwise, you must provide a data frame object containing three columns: one "id" column where you identify your zones for each xy coordinate. one "x" column where you put all the x coordinates (emission wavelenght). one "y" column where you put evey y coordinate (excitation wavelenght). The points of a polygon must be ordered in a way that the polygon is "closed" and must include repete the first point as a final point.
#' @keywords 3D fluorescence
#' @export
#' @examples
#' zones_definition() # The output is the data frame containing the polygons for the default zones defined in Jimenez et. al (2015). Follow this example if you want to put yours as input.
#' @references In progress
#'
zones_definition=function(
                          user_option = "Default"
                          )
                                                {
if (user_option == "Default") {

  polygon1=data.frame(id="I",
                      x=c(250,330,330,280,250,250),
                      y=c(199.5,199.5,249.5,249.5,219.5,199.5))
  polygon2=data.frame(id="II",
                      x=c(330.5,390,390,330.5,330.5),
                      y=c(199.5,199.5,249.5,249.5,199.5))
  polygon3=data.frame(id="III",
                      x=c(280,	390,	390,	331.5,	280),
                      y=c(250,	250,	299.5,	299.5,	250))
  polygon4=data.frame(id="IV",
                      x=c(390.5,	488,	390.5,	390.5),
                      y=c(204.5,	259.5,	259.5,	204.5))
  polygon5=data.frame(id="V",
                      x=c(390.5,	488,	560,	390.5,	390.5),
                      y=c(260,	260,	299.5,	299.5,	260))
  polygon6=data.frame(id="VI",
                      x=c(331.5,	560,	595.5,	595.5,	413,	331.5),
                      y=c(300,	300,	319.5,	379.5,	379.5,	300))
  polygon7=data.frame(id="VII",
                      x=c(413,	595.5,	595.5,	413),
                      y=c(380,	380,	559.5,	380))
  polygons_df=rbind(polygon1,polygon2,polygon3,polygon4,polygon5,polygon6,polygon7)
} else {
  if (is.na(user_option)) {stop("You didn't provide a data.frame containing the zones' polygons. Check ?zones_definition for details.")}
  polygons_df=user_option}
  return(polygons_df)
}









