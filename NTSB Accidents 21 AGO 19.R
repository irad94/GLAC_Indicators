#Librearías que se implementarán----
library(readxl)
library(writexl)
library(plyr)
library(dplyr)
library(reshape)
library(data.table)
library(DT)
library(tidyverse)
library(shiny)
library(stringr)
library(ggrepel)
library(zoo)

#Funciones propias----

cleantext <- function(ttt){
  gsub('[[:digit:]]+|[^a-zA-Z0-9./ /]', '', ttt)
}


cleannum <- function(ttt){
  as.numeric(gsub('[^a-zA-Z0-9./ /]', '', ttt))
}


textotitle <- function(str){
  
  df <- paste(toupper(substr(str,1,1)), tolower(substr(str,2,1000)), sep = "")
  
  return(df)
  
}


read_NTBSA <- function(ttt){
  
  df <- read_excel(ttt, sheet = 2, skip = 1) %>% 
    mutate(year = substr(ttt,68,71),
           value = 1) 
}


#Lectura de lotes----
Isos_eu <- read.csv("Z:/INDICADORES/Bases/USDA/ISOS.EEUU.States.csv")

df <-
  list.files(path = "C:/Users/GLAC/Documents/BASES DE DATOS/NTSB/Accidents/",
             pattern = "*.xls",
             full.names = T) %>%
  map_df(~read_NTBSA(.)) %>% 
  mutate(Y = Isos_eu$Y[match(ev_state, Isos_eu$COD)]) %>% drop_na("Y") %>% 
  gather(9:10, key = "type_injury", value = "cant_injury")

####Construcción de lotes####

#Total de accidentes----

df %>% 
group_by(Y,year) %>% 
  summarise(value = sum(value, na.rm = T)) %>% cast(Y~year) %>% 
  write.csv(.,"C:/Users/GLAC/Documents/BASES DE DATOS/NTSB/Accidents/Lotes/Lote_Estatal_EEUU_Accidentes aéreos.csv",
            na="", row.names = F)


#Por categorías----

constrl <- function(categ, value = "value", name){
  
  catalog <- unique(df$categ)
  
  for (i in seq_along(catalog)){
    
    filter(df, categ == catalog[i]) %>% 
      group_by(Y,year) %>% 
      summarise(value = sum(value, na.rm = T)) %>% cast(Y~year) %>% 
      write.csv(paste0("C:/Users/GLAC/Documents/BASES DE DATOS/NTSB/Accidents/Lotes/Lote_Estatal_EEUU_Accidentes aéreos_", name,"_",catalog[i],".csv"),
                na="", row.names = F)
  
  }
}


#Por cantidad y tipo de lesionados

catalog <- unique(df$type_injury)

for (i in seq_along(catalog)){

  filter(df, type_injury == catalog[i]) %>% 
  group_by(Y,year) %>% 
  summarise(value = sum(cant_injury, na.rm = T)) %>% cast(Y~year) %>% 
  write.csv(paste0("C:/Users/GLAC/Documents/BASES DE DATOS/NTSB/Accidents/Lotes/Lote_Estatal_EEUU_Accidentes aéreos_", catalog[i],".csv"),
            na="", row.names = F)
}

#Por cantidad y tipo de daño

catalog <- unique(df$damage)

for (i in seq_along(catalog)){
  
  filter(df, damage == catalog[i]) %>% 
    group_by(Y,year) %>% 
    summarise(value = sum(value, na.rm = T)) %>% cast(Y~year) %>% 
    write.csv(paste0("C:/Users/GLAC/Documents/BASES DE DATOS/NTSB/Accidents/Lotes/Lote_Estatal_EEUU_Accidentes aéreos_damage_", catalog[i],".csv"),
              na="", row.names = F)
}


#Por tipo de daño y cantidad de lesionados

catalog <- unique(df$damage)

for (i in seq_along(catalog)){
  
  filter(df, damage == catalog[i]) %>% 
    group_by(Y,year) %>% 
    summarise(value = sum(cant_injury, na.rm = T)) %>% cast(Y~year) %>% 
    write.csv(paste0("C:/Users/GLAC/Documents/BASES DE DATOS/NTSB/Accidents/Lotes/Lote_Estatal_EEUU_Accidentes aéreos_damage_injury_", catalog[i],".csv"),
              na="", row.names = F)
}


#Por cantidad y tipo de aeropuerto

catalog <- unique(df$acft_category)

for (i in seq_along(catalog)){
  
  filter(df, acft_category == catalog[i]) %>% 
    group_by(Y,year) %>% 
    summarise(value = sum(value, na.rm = T)) %>% cast(Y~year) %>% 
    write.csv(paste0("C:/Users/GLAC/Documents/BASES DE DATOS/NTSB/Accidents/Lotes/Lote_Estatal_EEUU_Accidentes aéreos_acft_category_", catalog[i],".csv"),
              na="", row.names = F)
}


#Por tipo de aeropuerto y cantidad de heridos

catalog <- unique(df$acft_category)

for (i in seq_along(catalog)){
  
  filter(df, acft_category == catalog[i]) %>% 
    group_by(Y,year) %>% 
    summarise(value = sum(cant_injury, na.rm = T)) %>% cast(Y~year) %>% 
    write.csv(paste0("C:/Users/GLAC/Documents/BASES DE DATOS/NTSB/Accidents/Lotes/Lote_Estatal_EEUU_Accidentes aéreos_acft_category_injury_", catalog[i],".csv"),
              na="", row.names = F)
}




