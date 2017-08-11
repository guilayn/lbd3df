#' Function for internal use.
#'
#' Definition of zones.
#'
#'
#' @param user_option Default is "Default". It generates the 7 zones defined by Jimenez et a. (2015). Put anything different and set your own zones with the option below if you want.
#' @param polygon_dataframe a data frame object containing three columns: one "id" column where you identify your zones for each xy coordinate. one "x" column where you put all the x coordinates (emission wavelenght). one "y" column where you put evey y coordinate (excitation wavelenght). The points of a polygon must be ordered in a way that the polygon is "closed" and must include repete the first point as a final point.
#' @keywords 3D fluorescence
#' @export
#' @examples
#' zones_definition() # The output is the data frame containing the polygons for the default zones defined in Jimenez et. al (2015). Follow this example if you want to put yours as input.
#' @references In progress
#'
zones_definition=function(
                          user_option = "Default",
                          polygon_dataframe= NA)
                                                {
if (user_option == "Default") {

  polygon1=data.frame(id="I",
                      x=c(216,325,325,275,216),
                      y=c(200,200,260,260,200))
  polygon2=data.frame(id="II",
                      x=c(325,390,390,325,325),
                      y=c(200,200,260,260,200))
  polygon3=data.frame(id="III",
                      x=c(275,390,390,325,275),
                      y=c(260,260,310,310,260))
  polygon4=data.frame(id="IV",
                      x=c(390,490,390,390),
                      y=c(200,260,260,200))
  polygon5=data.frame(id="V",
                      x=c(390,490,600,390,390),
                      y=c(260,260,310,310,260))
  polygon6=data.frame(id="VI",
                      x=c(325,600,600,396,325),
                      y=c(310,310,380,380,310))
  polygon7=data.frame(id="VII",
                      x=c(396,600,600,396),
                      y=c(380,380,590,380))
  polygons_df=rbind(polygon1,polygon2,polygon3,polygon4,polygon5,polygon6,polygon7)
} else {
  if (is.na(polygon_dataframe)) {stop("You didn't provide a data.frame containing the zones' polygons. Check ?zones_definition for details.")}
  polygons_df=polygon_dataframe}
  return(polygons_df)
}
