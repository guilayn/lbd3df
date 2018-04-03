#' Applies 3D Fluoresce processing and plotting to every subfolder within a given folder.
#'
#'
#' IMPORTANT:
#'
#'
#' 1) EVERY SUBFOLDER MUST PRESENT THE SAME CONFIGURATION OF SPECTRA (ANALYSIS METHOD), FOR A SINGLE SPECTRA
#' Reminder: The default internal method results in 40 .sp files called XXX#01, XXX#02, ..., XXX#40 where XXX is the label defined by the user.
#'
#'
#' 2) THE CODE WILL DETECT AND STOP IF THERE IS ANY SUBFOLDER THAT DOESN'T SATISTIFY THE CONDITION ABOVE
#'
#'
#' @param plot_all Default = TRUE. Use FALSE to avoid plotting.
#' @param exci_spec Default = FALSE. Use TRUE to include this function and specify emission as in excitation_spectrum()
#' @param emi_spec Default = FALSE. Use TRUE to include this function and specify excitation as in emission_spectrum()
#' @param indic Default = TRUE. Will include calculation of indicators to every spectrum.
#' @param zones_calc Default = TRUE. Will include calculation of volumes and barycenters of each zone to every spectrum.
#' @param args_process List containing the conditions for the function LBE_fluo3D_processing. See individual help files for details (?LBE_fluo3D_processing)
#' @param args_plot List containing the conditions for the function LBE_fluo3D_levelplot. See individual help files for details (?LBE_fluo3D_levelplot)
#' @param args_volume List containing the conditions for the function LBE_fluo3D_zones_volume. See individual help files for details (?LBE_fluo3D_zones_volume)
#' @param args_exci List containing the conditions for the function excitation_spectrum. See individual help files for details (?excitation_spectrum)
#' @param args_emi List containing the conditions for the function emission_spectrum. See individual help files for details (?emission_spectrum)
#' @param args_indic List containing the conditions for the function LBE_fluo3D_indicators. See individual help files for details (?LBE_fluo3D_indicators)
#' @param all_directory Directory containing subfolders with .sp 3D fluorescence files. Default is the directory used by the user. Might be useful to manually specify another.
#' @keywords 3D fluorescence
#' @export
#' @examples
#' # (!!!) First of all, verify the conditions hilighted in "IMPORTANT". (!!!)
#' > setwd("C:\\XXXX")  #where XXXX is your directory (don't forget to replace \ by / or \\)
#' > LBE_fluo3D_process_all(exci_spec = T, emi_spec = T,
#'                          args_process = list(remove_difdif=F),
#'                          args_plot = list(plot_zones_polygons=F,color_palette=2),
#'                          args_exci = list(select_emission=327,export_png=T,export_pdf=F),
#'                          args_emi = list(select_excitation=328,export_png=F,export_pdf=T),
#'                          args_indic = list())
#
#' # The code above will plot the gray spectrum (color_pallette=2, see ?LBE_fluo3D_levelplot for details), for all subfolders of XXXX.
#' # It will not remove diffusion and diffraction peaks and outside areas (remove_difdif=F, see ?LBE_fluo3D_processing for details)
#' # It will also plot excitation and emission 2D spectras for 327 and 328 emission and excitation wavelenghts, respectively.
#' # Observe that emission 2D spectra will be exported as png while excitation will be exported as pdf (just as example).
#' # By default, all the available indicators will be exported into each subfolder and a summary in the master folder.
#'
LBE_fluo3D_process_all=function(
  plot_all = TRUE,
  exci_spec = FALSE,
  emi_spec = FALSE,
  indic = TRUE,
  zones_calc = TRUE,
  zones_polygons = "Default",
  args_process = list(increase_resolution=FALSE, remove_difdif=TRUE),
  args_volume = list(zones_polygons_=zones_polygons),
  args_plot = list(zones_polygons_=zones_polygons, plot_zones_polygons = TRUE, export_html = TRUE,export_png = TRUE, export_pdf = TRUE),
  args_exci= list(),
  args_emi = list(),
  args_indic = list(),
  all_directory=getwd()
  ) {
  print("Processing all folders")
  subfolders=list.dirs(path = all_directory, full.names = TRUE, recursive = F)
  subfolders_initial=subfolders ## I don't think it's necessary but I was debuging the code and decided to let it
  number_subfolders=length(subfolders_initial)
  print(subfolders_initial)

  for (k in 1:number_subfolders) {
    print(paste0("WORKING ON DIRECTORY: ",k,"/",number_subfolders," (",subfolders_initial[k],")"))
    do.call(LBE_fluo3D_processing,c(list(directory=subfolders_initial[k]),args_process))
    if (plot_all) {
      do.call(LBE_fluo3D_levelplot,c(list(directory=subfolders_initial[k]),args_plot))}
    if (exci_spec) {
      do.call(excitation_spectrum,c(list(directory=subfolders_initial[k]),args_exci))}
    if (emi_spec) {
      do.call(emission_spectrum,c(list(directory=subfolders_initial[k]),args_emi))}

    if (zones_calc) {

      do.call(LBE_fluo3D_zones_volume,c(list(directory=subfolders_initial[k]),args_volume))

      if (k==1) {
        summary_zones_calculation=polygons_summary
      } else {
        summary_zones_calculation=rbind(summary_zones_calculation,polygons_summary)} #add the next summary on the botton

      if (k==number_subfolders) {
        write.csv2(summary_zones_calculation,paste0(all_directory,"/Summary_zones_calculations.csv"),row.names = FALSE)
        cast_summary_zones_calculation = dcast(summary_zones_calculation, sample ~ zone, value.var = "Vol_per", mean)
        write.csv2(cast_summary_zones_calculation,paste0(all_directory,"/Summary_zones_calculations_Vol_per_CAST.csv"),row.names = FALSE)
      }
    }

    if (indic) {

      do.call(LBE_fluo3D_indicators,c(list(directory=subfolders_initial[k]),args_indic))

      if (k==1) {summary_indicators=data.frame(row.names=names(result_indicators),Value=unlist(result_indicators))
      } else {
        summary_temp=data.frame(row.names=names(result_indicators),Value=unlist(result_indicators))
        summary_indicators=data.frame(summary_indicators,summary_temp)}

      if (k==number_subfolders) {
        if (zones_calc) {
        complex_ratio = ddply(cast_summary_zones_calculation, .(sample), summarize, complex_ratio = (I + II + III)/(IV + V + VI + VII))
        complex_ratio = complex_ratio[,2]
        summary_indicators_final = t(summary_indicators)
        summary_indicators_final=cbind(summary_indicators_final,complex_ratio = complex_ratio)
        }
        write.csv2(summary_indicators_final,paste0(all_directory,"/Summary_3DF_indicators.csv"),row.names = FALSE)
      }
      }

} #end of loop k
} #end of the functions
