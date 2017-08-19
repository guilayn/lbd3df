#' Checks if every subfolder is OK to pass the LBE_fluo3D_process_all() function.
#'
#' @param args_check List containing the conditions for the function chech_folders See individual help files for details (?check_folders)
#' @param all_directory Directory containing subfolders with .sp 3D fluorescence files. Default is the directory used by the user. Might be useful to manually specify another.
#' @keywords 3D fluorescence
#' @export
#' @examples
#' # (!!!) First of all, verify the conditions hilighted in "IMPORTANT". (!!!)
#' > setwd("C:\\XXXX")  #where XXXX is your directory (don't forget to replace \ by / or \\)
#' > LBE_check_folders()
#
#'
LBE_fluo3D_check_folders=function(
  args_check = list(),
  all_directory=getwd()
) {
  print("Processing all folders")
  subfolders=list.dirs(path = all_directory, full.names = TRUE, recursive = F)
  subfolders_initial=subfolders ## I don't think it's necessary but I was debuging the code and decided to let it
  number_subfolders=length(subfolders_initial)
  print(subfolders_initial)

  for (k in 1:number_subfolders) {
      do.call(check_folders,c(list(directory=subfolders_initial[k]),args_check))
    text=substr(subfolders_initial[k],nchar(subfolders_initial[k])-(5-1),nchar(subfolders_initial[k]))
if (k ==1) {summary_files_tot = data.frame(folder_abrev = paste0("...",text), summary_file)} else {
  summary_temp=data.frame(folder_abrev = paste0("...",text), summary_file)
  summary_files_tot=rbind(summary_files_tot,summary_temp)
}
if (k == number_subfolders) {print(summary_files_tot)}

  } #end of loop k
} #end of the functions
