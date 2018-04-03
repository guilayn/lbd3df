#' Extraction of polygons volumes, as defined by zones().
#'
#' @param data_list The default value of "data_interpol" is assuming that "LBE_fluo3D_processing" have just been used. Otherwise a list containing x=vector of emission wavelenghts, y=vector of excitation wavelenghts, z= a matrix containing the z values (intensity) where X must be the lines, Y must be the columns and Z must be the values of the table.
#' @param xy_vector_ The default value of "xy_vector" is assuming that "LBE_fluo3D_processing" have just been used.
#' @param sample_name_ Default is the output from processing function. Better not change it.
#' @param directory Default is the working directory. Better not change it.
#' @param zones_polygons_ Default corresponds to the 7 zones defined in Jimenez et. al 2015. Otherwise, you must provide a data frame object containing three columns: one "id" column where you identify your zones for each xy coordinate. one "x" column where you put all the x coordinates (emission wavelenght). one "y" column where you put evey y coordinate (excitation wavelenght). The points of a polygon must be ordered in a way that the polygon is "closed" and must include repete the first point as a final point.
#' @keywords 3D fluorescence
#' @export
#' @examples
#' setwd(XXXX) #XXX is your directory contaning the .sp files (copy and paste from the brownse and replace all the "\" by "/").
#' LBE_fluo3D_processing() #include your options if you want.
#' emission_spectrum(420) #export pdf of a emission spectrum plot at excitation = 420 nm
LBE_fluo3D_zones_volume=function(data_list=data_interpol,
                                 xy_vector_=xy_vector,
                                 sample_name_=sample_name,
                                 directory=directory_processing,
                                 zones_polygons_="Default")
{
list_polygons=zones_definition(zones_polygons_)
print("Defined polygons:")
print(list_polygons)
xy_vector_=data.frame(emission=data_interpol$x,xy_vector_)
colnames(xy_vector_)[2:ncol(xy_vector_)]=data_interpol$y
melten_vector=melt(xy_vector_,id="emission")
colnames(melten_vector)=c("x_emission","y_excitation","z_intensity")
melten_vector[,2]=as.numeric(levels(melten_vector[,2]))[melten_vector[,2]]

number_zones=length(table(list_polygons$id))
zones=names(table(list_polygons$id))
polygons_summary<<-data.frame(sample=sample_name_,zone=zones,Volume=NA,Vol_per=NA,barycenter_X=NA,barycenter_Y=NA)

#calculating volumes and centroids
for (i in 1:number_zones) {

id_temp=zones[i]

zone_coords=list_polygons[which(list_polygons$id==id_temp),]
print(paste0("Calculation of volume and barycenters. Zone ",i,"/",number_zones))


#print(1)

coordinates_matrix=as.matrix(zone_coords[,-1])
melten_vector_i=ddply(melten_vector,c("x_emission","y_excitation"),here(mutate),z_intensity=polygon_detect(coordinates=coordinates_matrix,
                                                                                                     x=x_emission,
                                                                                                     y=y_excitation,
                                                                                                     z=z_intensity))

#print(2)

xy_vector_i=dcast(melten_vector_i,x_emission ~ y_excitation, value.var = "z_intensity")

#print(3)

rownames(xy_vector_i)=data_interpol$x
xy_vector_i=xy_vector_i[,-1]
#print(plot_ly(z=t(as.matrix(xy_vector_i)),x=as.numeric(x_vector), y=as.numeric(y_vector), type="contour"))

#print(4)

emission_areas=ddply(melten_vector_i,c("x_emission"),
                      summarise,
                       area=trapz(x=y_excitation,
                                  y=z_intensity))
#print(5)
polygons_summary$barycenter_X[i]<<-weighted.mean(emission_areas$x_emission, emission_areas$area)

#print(6)

excitation_areas=ddply(melten_vector_i,c("y_excitation"),
                       summarise,
                       area=trapz(x=x_emission,
                                  y=z_intensity))

#print(7)

polygons_summary$barycenter_Y[i]<<-weighted.mean(excitation_areas$y_excitation, excitation_areas$area)

#print(8)

volume_polygon_i=trapz(x=excitation_areas$y_excitation,y=excitation_areas$area)

#print(9)

polygons_summary$Volume[i]<<-volume_polygon_i
}
#print(10)
polygons_summary<<-ddply(polygons_summary,"zone",mutate,Vol_per=100*Volume/(sum(polygons_summary$Volume)))

polygons_summary<<-polygons_summary

#exporting results
wd_out=paste0(directory,"/Output_data")
write.csv2(polygons_summary,paste0(wd_out,"/",sample_name_,"_3DF_zones_calculations.csv"),row.names = FALSE)

} #end of the function
