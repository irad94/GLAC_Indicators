#Load r packages
library(readxl)
library(writexl)
library(plyr)
library(reshape)
library(data.table)
library(tidyverse)

#Definición de funciones

render_report <- function(depend,indep,ruta,...){
  
  template <- paste0(ruta,"analisis_val.Rmd")
  outfile <- paste0(ruta,"Documentos/","analisis_val_",depend,"_",indep,".docx")
  parameters <- list(dep = depend,indep = indep,...)
  rmarkdown::render(template, output_file = outfile,
                    params = parameters)
  
  invisible(TRUE)
  
}

#Definición de variables

depend <- c("FGF21_OFICIAL")
indep <- c("ACTANTIOX_OFICIAL")

ruta <- "~/v_analisis/"

#Construcción de documentos

render_report(indep,depend,ruta)
