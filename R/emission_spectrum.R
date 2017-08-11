#' Extraction of 2D emission spectrum, given any excitation wavelenght.
#'
#' @param select_excitation Selected excitation wavelenght in nm. An emission spectrum will be generated for this specific excitation. Default is 350 for exampling purposes.
#' @param data_list The default value of "data_interpol" is assuming that "LBE_fluo3D_processing" have just been used. Otherwise a list containing x=vector of emission wavelenghts, y=vector of excitation wavelenghts, z= a matrix containing the z values (intensity) where X must be the lines, Y must be the columns and Z must be the values of the table.
#' @param emission_range The default value of "data_interpol$x" is assuming that "LBE_fluo3D_processing" have just been used. Otherwise it must be a vector containing the emission wavelenghts range.
#' @param export_pdf TRUE or FALSE.
#' @param export_png TRUE or FALSE.
#' @param directory Default is the working directory. Better not change it.
#' @param area_under_graph TRUE or FALSE. If any calculation of area under curve will be performed.
#' @param start_emi Emission start if area_under_graph is TRUE.
#' @param end_emi Emission end if area_under_graph is TRUE.
#' @param sample_name_ Default is the output from processing function. Better not change it.
#' @keywords 3D fluorescence
#' @export
#' @examples
#' setwd(XXXX) #XXX is your directory contaning the .sp files (copy and paste from the brownse and replace all the "\" by "/").
#' LBE_fluo3D_processing() #include your options if you want.
#' emission_spectrum(420) #export pdf of a emission spectrum plot at excitation = 420 nm

emission_spectrum=function(select_excitation=254,
                             data_list=data_interpol, #assuming that "LBE_fluo3D_processing" have just been used
                             emission_range=data_interpol$x,
                           area_under_graph=F,
                           start_emi=200,
                           end_emi=400,
                           export_pdf=T,
                           export_png=F,
                           directory=directory_processing,
                           sample_name_=sample_name) {
data_plot_selected_excitation=data.frame(emission_range,select_excitation_col=select_excitation,intensity=0)

data_plot_selected_excitation=ddply(data_plot_selected_excitation,
                                     c("emission_range"),
                                    here(mutate),
                                    intensity=interp.surface(data_list,loc=matrix(c(emission_range,select_excitation_col),nrow = 1)))
#creating the plot
plot=(ggplot(data_plot_selected_excitation,
       aes(x=emission_range,y=data_plot_selected_excitation$intensity),
       group=1) +
      ggtitle(paste0("Excitation at ",select_excitation," nm")) +
  geom_path() +
  labs(x="Emission wavelenght (nm)", y = "Intensity (U.A.)") +
  theme_light())
print(plot)

#exporting to pdf and or png
if (export_pdf || export_png) {
  wd_out=paste0(directory,"/Output_data")
  dir.create(wd_out)
if (export_pdf) {
  if (file.exists(paste0(wd_out,"/",sample_name_,"_emission_spectra_excitation",select_excitation,".pdf"))) {
    warning("The same plot already existis in the output folder. Detele it or verify the selected wavelenght.")
  } else {
  pdf(paste0(wd_out,"/",sample_name_,"_emission_spectra_excitation",select_excitation,".pdf"))
  plot(plot)
  dev.off()}}
if (export_png) {
  if (file.exists(paste0(wd_out,"/",sample_name_,"_emission_spectra_excitation",select_excitation,".png"))) {
  warning("The same plot already existis in the output folder. Detele it or verify the selected wavelenght.")
} else {
  pdf(paste0(wd_out,"/",sample_name_,"_emission_spectra_excitation",select_excitation,".png"))
  plot(plot)
  dev.off()}}
}
#area under the curve
if (area_under_graph) {
  selected_area=data_plot_selected_excitation[which(data_plot_selected_excitation==start_emi):which(data_plot_selected_excitation==end_emi),]
  auc_emi<<-trapz(x=selected_area$emission_range,y=selected_area$intensity)
  return(auc_emi)
}
} #end of the function
