library(tidyverse)


df <- read.csv("C:/Users/IOR_C/Downloads/conjunto_de_datos_defunciones_registradas_2018.CSV")

dfl <- read.csv("C:/Users/IOR_C/Downloads/causa_defuncion.csv")


df1 <- table(df['causa_def']) %>% as.data.frame() %>% mutate(nombre = dfl$DESCRIP[match(Var1,dfl$CVE)])


write.csv(df1,"~/foreign_proyects/v_analisis/data/listamex_2018.csv",row.names = F,na="")
