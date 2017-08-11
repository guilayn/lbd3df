#' 3D Fluorescence level plot by plotly, after processing
#'
#' After running LBE_fluo3D_processing() with your options, run this function to visualize and export your surface plot. Results (plots in pdf and/or png and/or interactive html) will be exported to a folder inside your directory called "Output_data".
#' @param data_list The default value of "data_interpol" is assuming that "LBE_fluo3D_processing" have just been used. Otherwise a list containing x=vector of emission wavelenghts, y=vector of excitation wavelenghts, z= a matrix containing the z values (intensity) where X must be the lines, Y must be the columns and Z must be the values of the table.
#' @param peak_ratio_AT TRUE or FALSE. Include the calculation or not.
#' @param peak_ratio_CA TRUE or FALSE. Include the calculation or not.
#' @param peak_ratio_CT TRUE or FALSE. Include the calculation or not.
#' @param Humif_index TRUE or FALSE. Include the calculation or not.
#' @param export_csv TRUE or FALSE.
#' @param directory Default is the working directory. Better not change it.
#' @param sample_name_ Default is the output from processing function. Better not change it.
#' @keywords 3D fluorescence
#' @export
#' @examples
#' #In progress
#' @references In progress

LBE_fluo3D_indicators = function(
  data_list=data_interpol,
  peak_ratio_AT = TRUE,
  peak_ratio_CA = TRUE,
  peak_ratio_CT = TRUE,
  Humif_index = TRUE,
  export_csv = TRUE,
  directory = directory_processing,
  sample_name_=sample_name)
  {
  options_function=c(peak_ratio_AT=peak_ratio_AT,peak_ratio_CA=peak_ratio_CA,peak_ratio_CT=peak_ratio_CT,Humif_index=Humif_index)
  result_indicators=vector("list", 1+length((which(options_function==T))))
  names(result_indicators)=c("Sample",names(options_function)[which(options_function==T)])
  result_indicators$Sample=sample_name_
  if (peak_ratio_AT) {
    peak_A=LBE_fluo3D_single_point(450,250,data_list)
    peak_T=LBE_fluo3D_single_point(304,275,data_list)
    result_indicators$peak_ratio_AT=peak_A/peak_T
  }
  if (peak_ratio_CA) {
    peak_C=LBE_fluo3D_single_point(440,340,data_list)
    peak_A=LBE_fluo3D_single_point(450,250,data_list)
    result_indicators$peak_ratio_CA=peak_C/peak_A
  }
  if (peak_ratio_CT) {
    peak_C=LBE_fluo3D_single_point(440,340,data_list)
    peak_T=LBE_fluo3D_single_point(304,275,data_list)
    result_indicators$peak_ratio_CT=peak_C/peak_T
  }
  if (Humif_index) {
    area1=emission_spectrum(select_excitation=254,data_list,area_under_graph=T,start_emi=435,end_emi=480,export_pdf=T,export_png=F,directory=directory) #humic acids
    area2=emission_spectrum(select_excitation=254,data_list,area_under_graph=T,start_emi=300,end_emi=345,export_pdf=F,export_png=F,directory=directory)
    result_indicators$Humif_index=area1/(area1+area2)
  }
  result_indicators<<-result_indicators #turns definitive to workspace
  if (export_csv) {
wd_out=paste0(directory,"/Output_data")
write.csv2(data.frame(Value=unlist(result_indicators)),paste0(wd_out,"/",sample_name_,"_3DF_indicators.csv"),col.names = FALSE)
  }

 }
