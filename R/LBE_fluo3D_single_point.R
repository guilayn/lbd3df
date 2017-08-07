#' Extraction of intensity singles, given any excitation and emission wavelenghts.
#'
#' @param x_emission Selected emission wavelenght in nm.  Default is 350 for exampling purposes.
#' @param y_excitation Selected excitation wavelenght in nm.  Default is 350 for exampling purposes.
#' @param data The default value of "data_interpol" is assuming that "LBE_fluo3D_processing" have just been used. Otherwise a list containing x=vector of emission wavelenghts, y=vector of excitation wavelenghts, z= a matrix containing the z values (intensity) where X must be the lines, Y must be the columns and Z must be the values of the table.
#' @keywords 3D fluorescence
#' @export
#' @examples
#' > setwd(XXXX) #XXX is your directory contaning the .sp files (copy and paste from the brownse and replace all the "\" by "/").
#' > LBE_fluo3D_processing() #include your options if you want.
#' > single_point(350,420) #returns an intensity value at emission = 350 and excitation = 420 nm.

LBE_fluo3D_single_point=function(x_emission=350,
                                    y_excitation=350,
                                    data=data_interpol ##assuming that "LBE_fluo3D_processing" have just been used
                                    ) {
  result=interp.surface(data_interpol,loc=matrix(c(x_emission,y_excitation),nrow = 1))
  print(result)
  return("result_extrapolation_xy"<<-result)
} #end of the function
