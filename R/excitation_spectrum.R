#' Extraction of 2D excitation spectrum, given any emission wavelenght.
#'
#' @param select_emission Selected emission wavelenght in nm. An excitation spectrum will be generated for this specific emission.  Default is 350 for exampling purposes.
#' @param data_list The default value of "data_interpol" is assuming that "LBE_fluo3D_processing" have just been used. Otherwise a list containing x=vector of emission wavelenghts, y=vector of excitation wavelenghts, z= a matrix containing the z values (intensity) where X must be the lines, Y must be the columns and Z must be the values of the table.
#' @param excitation_range The default value of "data_interpol$y" is assuming that "LBE_fluo3D_processing" have just been used. Otherwise it must be a vector containing the excitation wavelenghts range.
#' @param export_pdf TRUE or FALSE.
#' @param export_png TRUE or FALSE.
#' @param directory Default is the working directory. Better not change it.
#' @param area_under_graph TRUE or FALSE. If any calculation of area under curve will be performed.
#' @param start_exci Excitation start if area_under_graph is TRUE.
#' @param end_exci Excitation end if area_under_graph is TRUE.
#' @param sample_name_ Default is the output from processing function. Better not change it.
#' @keywords 3D fluorescence
#' @export
#' @examples
#' setwd(XXXX) #XXX is your directory contaning the .sp files (copy and paste from the brownse and replace all the "\" by "/").
#' LBE_fluo3D_processing() #include your options if you want.
#' excitation_spectrum(420) #export pdf of a excitation spectrum plot at emission = 420 nm

excitation_spectrum=function(select_emission=254,
                             data_list=data_interpol, #assuming that "LBE_fluo3D_processing" have just been used
                             excitation_range=data_interpol$y,
                             area_under_graph=F,
                             start_exci=200,
                             end_exci=400,
                             export_pdf=T,
                             export_png=F,
                             directory=directory_processing,
                             sample_name_=sample_name) {

  data_plot_selected_emission=data.frame(excitation_range,select_emission_col=select_emission,intensity=0)

  data_plot_selected_emission=ddply(data_plot_selected_emission,
                                    c("excitation_range"),
                                    here(mutate),
                                    intensity=interp.surface(data_list,loc=matrix(c(select_emission_col,excitation_range),nrow = 1)))

  #creating the plot

  plot=(ggplot(data_plot_selected_emission,
               aes(x=excitation_range,y=data_plot_selected_emission$intensity),
               group=1) +
          ggtitle(paste0("Emission at ",select_emission," nm")) +
          geom_path() +
          labs(x="Excitation wavelenght (nm)", y = "Intensity (U.A.)") +
          theme_light())
  print(plot)

  #exporting to pdf and org png
  if (export_pdf || export_png) {
    wd_out=paste0(directory,"/Output_data")
    dir.create(wd_out)
    if (export_pdf) {
      if (file.exists(paste0(wd_out,"/",sample_name_,"_excitation_spectra_emission",select_emission,".pdf"))) {
      warning("The same plot already existis in the output folder. Detele it or verify the selected wavelenght.")
    } else {
      pdf(paste0(wd_out,"/",sample_name_,"_excitation_spectra_emission",select_emission,".pdf"))
      plot(plot)
      dev.off()}}
    if (export_png) {
      if (file.exists(paste0(wd_out,"/",sample_name_,"_excitation_spectra_emission",select_emission,".png"))) {
        warning("The same plot already existis in the output folder. Detele it or verify the selected wavelenght.")
      } else {
        pdf(paste0(wd_out,"/",sample_name_,"_excitation_spectra_emission",select_emission,".png"))
        plot(plot)
        dev.off()}}
  }
  #area under the curve
  if (area_under_graph) {
    selected_area=data_plot_selected_emission[which(data_plot_selected_emission==start_exci):which(data_plot_selected_emission==end_exci),]
    auc_exci<<-trapz(x=selected_area$excitation_range,y=selected_area$intensity)
    return(auc_exci)
  }
} #end of the function

