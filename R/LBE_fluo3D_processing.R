#' 3D Fluorescence spectrum processing
#'
#' This function treats your set of ".sp" files withou affecting them. You just have to set the R's working directory to your folder containing your .sp files of a 3D spectra. Results (tables) will be exported to a folder inside your directory called "Output_data" .
#'
#' @param first_spectrum_line Normally you shouldn't change this. It is the first line containing the spectrum in the .sp files, 55 by default
#' @param excitation_spectrum_line Normally you shouldn't change this. It is the line containing the selected excitation in the .sp files, 22 by default
#' @param excitation_start Default = 200. Only change if you are not using the default scan3D method. Otherwise, it is your first excitation wavelenght (nm).
#' @param excitation_end Default = 590. Only change if you are not using the default scan3D method. Otherwise, it is your last excitation wavelenght (nm).
#' @param excitation_step Default = 10. Only change if you are not using the default scan3D method. Change to 20 might be useful if you are running the fast method. Otherwise, it is your excitation step (nm).
#' @param export_3D_matrix Default = TRUE. Accepts TRUE or FALSE. Use it to export or not the processed data matrix into csv files.
#' @param increase_resolution If you want a more beautiful plot chose TRUE to increase the excitation step from your original to new_excitation_step.
#' @param new_excitation_step See increase_resolution. No effect if increase_resolution is FALSE.
#' @param emission_start Default = 200. See excitation_start for details. Your first emission wavelenght (nm).
#' @param emission_end Default = 600 See excitation_start for details. Your last emission wavelenght (nm).
#' @param emission_step Default = 0.5. See excitation_step for details.
#' @param remove_max_height Default = FALSE. Pick up TRUE if you want to replace all values greater than max_height by replace_max.
#' @param max_height See remove_max_height. No effect if remove_max_height is FALSE.
#' @param replace_max See remove_max_height. No effect if remove_max_height is FALSE.
#' @param remove_difdif Default = TRUE. You can chose if you want to remove the diffusion and diffraction peaks and the outside areas from your plot.
#' @param replace_difdif_by Anological to replace_max. No effect if remove_difdif is FALSE.
#' @param directory Default is the working directory. Better not change it.
#' @keywords 3D fluorescence
#' @export
#' @examples
#' > setwd(XXXX) #XXX is your directory contaning the .sp files (copy and paste from the brownse and replace all the "\" by "/").
#' > LBE_fluo3D_processing() #include your options if you want.

LBE_fluo3D_processing=function(first_spectrum_line=55,
                               excitation_spectrum_line=22,
                               excitation_start=200,
                               excitation_end=590,
                               excitation_step=10,
                               export_3D_matrix=TRUE,
                               increase_resolution=FALSE,
                               new_excitation_step=2,
                               emission_start=200,
                               emission_end=600,
                               emission_step=0.5,
                               remove_max_height=FALSE,
                               max_height=800,
                               replace_max=NA,
                               remove_difdif=TRUE,
                               replace_difdif_by=0,
                               directory=getwd()
                               ) {
  print("setting directories")
  directory_processing<<-directory
  wd_out=paste0(directory,"/Output_data")
  dir.create(wd_out)

  #### Getting list of .sp files ####
  print("Getting list of .sp files")
  files=list.files(directory,pattern="\\.sp$")
  #print(files)
  filename=substring(files,1,nchar(files)-6)
  summary_file=table(filename)
  print(summary_file)

  #### verifying if number of steps == number of files ####
  print("Verifying number of excitation steps and available files")
  number_steps=1+(excitation_end - excitation_start)/(excitation_step)
  if (number_steps != length(files))
  {
    stop(paste0("ERROR: inserted number of steps (=",
                number_steps,
                ") doesn't match with number of files (=",length(files),")"))
  }else {print("number of steps and number of .sp files validated")}

  #### loop through files in data folder, importing as x,z (emission and intensity) tables + creating y-vector table####
  print("Importing .sp files")
  if (length(summary_file)==1) {
    y_vector = 1:summary_file
    sample_name=rownames(summary_file)
  } else {stop(paste0("ERROR: The folder contains more than 1 set of 3D spectrum files: ",
                      toString(rownames(summary_file)))) }

  for (i in 1:summary_file)
  {
    print(paste0("reading spectra ",i,"/",length(files)))
    assign(paste0("spectra_",i),
           (as.matrix(read.table(paste0(directory,"/",files[i]),skip=(first_spectrum_line-1)))))

    y_vector[i]=as.numeric((read.table(paste0(directory,"/",files[i]),skip=(excitation_spectrum_line-1),nrows=1)))[1]
  }
  y_vector=unlist(y_vector)

  ### associating excitation wavelengh to emission tables ####
  print("Reading excitation wavelenghts of emission .sp files")
  #### and applying max height if asked for####
  for (i in 1:number_steps)
  {
    exci_spectra_i=get(paste0("spectra_",i))
    exci_spectra_i=data.frame(y_vector[i],exci_spectra_i)
    exci_spectra_i[,c(1,2,3)]=exci_spectra_i[,c(2,1,3)]
    colnames(exci_spectra_i)=c("x_emission","y_excitation","z_intensity")
    if (remove_max_height) {
      for (j in 1:nrow(exci_spectra_i))
      {
        if (exci_spectra_i$z_intensity[j] > max_height) {
          exci_spectra_i$z_intensity[j] = replace_max
        }}
    }
    assign(paste0("exci_spectra_",i),exci_spectra_i)

  }

  #### Creating complete 3D vector: casted (short) and melted (long) table versions ####
  print("Creating xyz melten data frame")
  complete_vector=exci_spectra_1
  for (i in 2:number_steps)
  {
    exci_spectra_i=get(paste0("exci_spectra_",i))
    complete_vector=rbind(complete_vector,exci_spectra_i)
  }
  complete_vector_orig=complete_vector

  print("Creating xyz casted data frame")
  xy_vector=dcast(complete_vector, x_emission ~ y_excitation, value.var = "z_intensity")  #from melt to casted
  x_vector=xy_vector[,1]
  x_vector_orig=x_vector
  xy_vector=xy_vector[,2:ncol(xy_vector)]
  row.names(xy_vector)=x_vector
  xy_vector_orig=xy_vector


  dataplot_orig = list (x=x_vector,
                        y=y_vector,
                        z=as.matrix(xy_vector_orig))

  #### Increasing resolution ####
  print(paste0("Increase resolution? ",increase_resolution))
  if (increase_resolution) {
    excitation_new_range=seq(from=excitation_start,to=excitation_end,by=new_excitation_step) #extending resolution to the same as emission
    xy_vector_resmax = data.frame(matrix(0, nrow = length(x_vector),ncol = length(excitation_new_range))) ## starting the new resolution table
    for (i in 1:length(x_vector)) {
      print(paste0("Increasing resolution. Emission: ",x_vector[i],"/",x_vector[length(x_vector)]))
      for (j in 1:length(excitation_new_range)) {
        xy_vector_resmax[i,j]=interp.surface(dataplot_orig,loc=matrix(c(x_vector[i],excitation_new_range[j]),nrow = 1))
      }
    }

    colnames(xy_vector_resmax)=colnames(xy_vector)
    rownames(xy_vector_resmax)=rownames(xy_vector)
    complete_resmax=data.frame(x_vector,xy_vector_resmax)
    colnames(complete_resmax)=c("emission",excitation_new_range)
    complete_resmax=melt(complete_resmax,id="emission")
    colnames(complete_resmax)=colnames(complete_vector)
    complete_resmax$y_excitation=as.numeric(levels(complete_resmax$y_excitation))[complete_resmax$y_excitation]

    dataplot_resmax = list (x=x_vector,
                            y=excitation_new_range,
                            z=as.matrix(xy_vector_resmax))
  }
  #### CLEANING THE MATRIX ####
  print(paste0("Remove diffusion and diffraction peaks? ",remove_difdif))
  if (increase_resolution) {
    y_vector_temp=excitation_new_range
    complete_vector_temp=complete_resmax
  } else {
    y_vector_temp=y_vector
    complete_vector_temp=complete_vector
  }


  if (remove_difdif) {
    print("Removing diffusion peaks and outside areas. Might take a moment, especially if the resolution has been increased")
    complete_vector_temp=ddply(complete_vector_temp,c("x_emission","y_excitation"),transform,z_new=diffu_detect(x_emission, y_excitation, z_intensity))
    #kept to test the function: complete_vector_temp$z_intensity[which(complete_vector_temp$x==400 & complete_vector_temp$y==390)]
    #kept to test the function: complete_vector_temp$z_new[which(complete_vector_temp$x==400 & complete_vector_temp$y==390)]
    complete_vector_temp=complete_vector_temp[,c(1,2,4)]
    colnames(complete_vector_temp)[3]="z_intensity"
  }

  if (remove_difdif) {
    print("Removing diffraction peaks and outside areas. Might take a moment, especially if the resolution has been increased.")
    complete_vector_temp=ddply(complete_vector_temp,c("x_emission","y_excitation"),transform,z_new=diffra_detect(x_emission, y_excitation, z_intensity))
    #kept to test the function: complete_vector_temp$z_intensity[which(complete_vector_temp$x==400 & complete_vector_temp$y==210)]
    #kept to test the function: complete_vector_temp$z_new[which(complete_vector_temp$x==503 & complete_vector_temp$y==260)]
    complete_vector_temp=complete_vector_temp[,c(1,2,4)]
    colnames(complete_vector_temp)[3]="z_intensity"
  }

  #### Re-creating complete 3D vector: casted (short) and melted (long) table versions ####
  print("Creating final vectors")
  xy_vector_final=dcast(complete_vector_temp, x_emission ~ y_excitation, value.var = "z_intensity")  #from melt to casted
  x_vector_final=xy_vector_final[,1]
  xy_vector_final=xy_vector_final[,2:ncol(xy_vector_final)]
  row.names(xy_vector_final)=x_vector_final
  y_vector_final=y_vector_temp

  if (export_3D_matrix) {
  write.csv2(xy_vector_orig,file=paste0(wd_out,"/",sample_name,"_original_3Dtable.csv"),row.names = T)
  write.csv2(xy_vector_final,file=paste0(wd_out,"/",sample_name,"_final_3Dtable.csv"),row.names = T)
  }

  return(list("x_vector"<<-x_vector_final,
              "y_vector"<<-y_vector_final,
              "xy_vector"<<-xy_vector_final,
              "sample_name"<<-sample_name,
              "data_interpol"<<-list(x=x_vector,
                                     y=y_vector,
                                     z=as.matrix(xy_vector)))
         )
  }
