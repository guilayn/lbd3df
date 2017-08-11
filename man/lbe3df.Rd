\name{package: lbe3df}
\alias{lbe3df}

\title{3D Fluorescence spectra processing}

\description{This package allows you to better process your .sp files from our 3D fluorescence spectra.

Currently, the available functions are:

LBE_fluo3D_processing(), to process your .sp files. It produces the necessary objects for the following functions.

LBE_fluo3D_levelplot(), generates interactive or static level plots with plotly.

excitation_spectrum(), generates excitation spectras (2D) given any emission wavelenght.

emission_spectrum(), generates emission spectras (2D) given any emission wavelenght.

single_point_extrapolation(), returns intensity value given any emission and excitation
}

\examples{
>setwd("XXXX") #XXX is your directory contaning the .sp files (copy and paste from the brownse and replace all the "\" by "/").
>LBE_fluo3D_processing() #include your options if you want. ?LBE_fluo3D_processing for details.
>LBE_fluo3D_levelplot(color_palette="gray.colors") #export pdf and png with a black and white surface plot. ?LBE_fluo3D_processing for details.
>excitation_spectrum(420) #export pdf of a excitation spectrum plot at emission = 420 nm. ?excitation_spectrum for details.
>emission_spectrum(420) #export pdf of a emission spectrum plot at excitation = 420 nm. ?emission_spectrum for details.
>single_point_extrapolation(350,420) #returns an intensity value at emission = 350 and excitation = 420 nm. ?single_point_extrapolation for details
>LBE_fluo3D_process_all() #Used to apply the functions above to a whole set of 3D spectra. See ?LBE_fluo3D_process_all for details.
}
