#' 3D Fluorescence spectrum processing
#'
#' Internal use, this function simply checks the conformity of a folder containing Fluo3D .sp files.
#'
#' @param first_spectrum_line Normally you shouldn't change this. It is the first line containing the spectrum in the .sp files, 55 by default
#' @param excitation_spectrum_line Normally you shouldn't change this. It is the line containing the selected excitation in the .sp files, 22 by default
#' @param excitation_start Default = 200. Only change if you are not using the default scan3D method. Otherwise, it is your first excitation wavelenght (nm).
#' @param excitation_end Default = 590. Only change if you are not using the default scan3D method. Otherwise, it is your last excitation wavelenght (nm).
#' @param excitation_step Default = 10. Only change if you are not using the default scan3D method. Change to 20 might be useful if you are running the fast method. Otherwise, it is your excitation step (nm).
#' @param emission_start Default = 200. See excitation_start for details. Your first emission wavelenght (nm).
#' @param emission_end Default = 600 See excitation_start for details. Your last emission wavelenght (nm).
#' @param emission_step Default = 0.5. See excitation_step for details.
#' @param directory Default is the working directory. Better not change it.
#' @keywords 3D fluorescence
#' @export
#' @examples
#' In progress

check_folders=function(first_spectrum_line=55,
                               excitation_spectrum_line=22,
                               excitation_start=200,
                               excitation_end=590,
                               excitation_step=10,
                               emission_start=200,
                               emission_end=600,
                               emission_step=0.5,
                               directory=getwd()
) {
  print("setting directories")
  directory_processing<<-directory
  wd_out=paste0(directory,"/Output_data")

  #### Getting list of .sp files ####
  print("Getting list of .sp files")
  files=list.files(directory,pattern="\\.sp$")
  #print(files)
  filename=substring(files,1,nchar(files)-6)
  summary_file=table(filename)
  summary_file<<-summary_file
  print(summary_file)

  #### verifying if number of steps == number of files ####
  print("Verifying number of excitation steps and available files")
  number_steps=1+(excitation_end - excitation_start)/(excitation_step)
  if (number_steps != length(files))
  {
    print(paste0("ERROR: inserted number of steps (=",
                number_steps,
                ") doesn't match with number of files (=",length(files),")"))
  }else {print("number of steps and number of .sp files validated")}

  #### loop through files in data folder, importing as x,z (emission and intensity) tables + creating y-vector table####

  if (!length(summary_file)==1) {
  print(paste0("ERROR: The folder contains more than 1 set of 3D spectrum files: ",
                      toString(rownames(summary_file)))) }
}
