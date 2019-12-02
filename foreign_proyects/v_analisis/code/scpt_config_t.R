knitr::opts_chunk$set(echo = FALSE)
library(haven)
library(plyr)
library(tidyverse)
library(knitr)
library(magrittr)
library(officer)
library(flextable)
library(normtest)
library(nortest)
library(moments)
library(fifer)


#Función para construir tabla

ctabla <- function(df, x, d = 1){
  
  dfp <- df %>% filter(PP == 1,Diabetesmellitus == d) %>% 
    select(c(x,grupodetratamiento)) %>% drop_na(x) %>% as.data.frame()
  
  df1 <- dfp %>% group_by(grupodetratamiento) %>% 
    summarise(value = quantile(get(x),.25, na.rm = T, type = 6) %>% round(1)) %>% 
    spread(key="grupodetratamiento",value = "value") %>% dplyr::rename(P25_1=1,P25_2=2,P25_3=3) %>% 
    mutate(Variable = x)
  
  
  dfl <- dfp %>%  
    group_by(grupodetratamiento) %>% summarise(value = quantile(get(x),.75, na.rm = T, type = 6) %>% round(1)) %>% 
    spread(key="grupodetratamiento",value = "value") %>% dplyr::rename(P75_1=1,P75_2=2,P75_3=3) %>% 
    mutate(Variable = x)
  
  df1 <- left_join(df1,dfl, by="Variable")
  
  
  dfl <- dfp %>% 
    group_by(grupodetratamiento) %>% summarise(value = median(get(x), na.rm = T) %>% round(1)) %>% 
    spread(key="grupodetratamiento",value = "value") %>% dplyr::rename(MEDIANA_1=1,MEDIANA_2=2,MEDIANA_3=3) %>% 
    mutate(Variable = x)
  
  df1 <- left_join(df1,dfl, by="Variable")
  
  dfl <- dfp %>% 
    group_by(grupodetratamiento) %>% summarise(value = n()) %>% 
    spread(key="grupodetratamiento",value = "value") %>% dplyr::rename(N_1=1,N_2=2,N_3=3) %>% 
    mutate(Variable = x)
  
  df1 <- left_join(df1,dfl, by="Variable") %>% 
    mutate(`MEDIANA P25 - P75 1`=paste0(MEDIANA_1," [",P25_1," a ",P75_1,"]"),
           `MEDIANA P25 - P75 2`=paste0(MEDIANA_2," [",P25_2," a ",P75_2,"]"),
           `MEDIANA P25 - P75 3`=paste0(MEDIANA_3," [",P25_3," a ",P75_3,"]")) %>% 
    select(c(Variable, N_1,`MEDIANA P25 - P75 1`, N_2, `MEDIANA P25 - P75 2`, N_3, `MEDIANA P25 - P75 3`))
  
  pvalue <- dfp %>% 
    kruskal.test(get(x) ~ grupodetratamiento, data = .)
  
  pvalue <- pvalue$p.value %>% round(3)
  
  pvalue2 <- pairwise.wilcox.test(x = dfp[,x], g = dfp$grupodetratamiento, p.adjust.method = "bonf", correct=FALSE)
  
  valores = c()
  
  for (i in 1:length(pvalue2$p.value)) {
    
    valores[i] <- ifelse(pvalue2$p.value[i] < 0.001,"p < 0.001", pvalue2$p.value[i] %>% round(2))
    
  }
  
  
  df1 %<>% 
    mutate(`Kruskall Wallis`= ifelse(pvalue < 0.001,"p < 0.001",pvalue %>% round(3)) %>% as.character(),
           `Inter-grupo`=paste0("Grupo 1 vs 2: ",valores[1]," Grupo 1 vs 3: ",
                                valores[2]," Grupo 2 vs 3: ",valores[4]) %>% as.character())
  return(df1)
  
}

ctabla2 <- function(df, x, d = 1){
  
  dfp <- df %>% filter(PP == 1,`categoría_riesgo_ProteínaCreactiva` == d) %>% 
    select(c(x,grupodetratamiento)) %>% drop_na(x) %>% as.data.frame()
  
  df1 <- dfp %>% group_by(grupodetratamiento) %>% 
    summarise(value = quantile(get(x),.25, na.rm = T, type = 6) %>% round(1)) %>% 
    spread(key="grupodetratamiento",value = "value") %>% dplyr::rename(P25_1=1,P25_2=2,P25_3=3) %>% 
    mutate(Variable = x)
  
  
  dfl <- dfp %>%  
    group_by(grupodetratamiento) %>% summarise(value = quantile(get(x),.75, na.rm = T, type = 6) %>% round(1)) %>% 
    spread(key="grupodetratamiento",value = "value") %>% dplyr::rename(P75_1=1,P75_2=2,P75_3=3) %>% 
    mutate(Variable = x)
  
  df1 <- left_join(df1,dfl, by="Variable")
  
  
  dfl <- dfp %>% 
    group_by(grupodetratamiento) %>% summarise(value = median(get(x), na.rm = T) %>% round(1)) %>% 
    spread(key="grupodetratamiento",value = "value") %>% dplyr::rename(MEDIANA_1=1,MEDIANA_2=2,MEDIANA_3=3) %>% 
    mutate(Variable = x)
  
  df1 <- left_join(df1,dfl, by="Variable")
  
  dfl <- dfp %>% 
    group_by(grupodetratamiento) %>% summarise(value = n()) %>% 
    spread(key="grupodetratamiento",value = "value") %>% dplyr::rename(N_1=1,N_2=2,N_3=3) %>% 
    mutate(Variable = x)
  
  df1 <- left_join(df1,dfl, by="Variable") %>% 
    mutate(`MEDIANA P25 - P75 1`=paste0(MEDIANA_1," [",P25_1," a ",P75_1,"]"),
           `MEDIANA P25 - P75 2`=paste0(MEDIANA_2," [",P25_2," a ",P75_2,"]"),
           `MEDIANA P25 - P75 3`=paste0(MEDIANA_3," [",P25_3," a ",P75_3,"]")) %>% 
    select(c(Variable, N_1,`MEDIANA P25 - P75 1`, N_2, `MEDIANA P25 - P75 2`, N_3, `MEDIANA P25 - P75 3`))
  
  pvalue <- dfp %>% 
    kruskal.test(get(x) ~ grupodetratamiento, data = .)
  
  pvalue <- pvalue$p.value %>% round(3)
  
  pvalue2 <- pairwise.wilcox.test(x = dfp[,x], g = dfp$grupodetratamiento, p.adjust.method = "bonf", correct=FALSE)
  
  valores = c()
  
  for (i in 1:length(pvalue2$p.value)) {
    
    valores[i] <- ifelse(pvalue2$p.value[i] < 0.001,"p < 0.001", pvalue2$p.value[i] %>% round(2))
    
  }
  
  
  df1 %<>% 
    mutate(`Kruskall Wallis`= ifelse(pvalue < 0.001,"p < 0.001",pvalue %>% round(3)) %>% as.character(),
           `Inter-grupo`=paste0("Grupo 1 vs 2: ",valores[1]," Grupo 1 vs 3: ",
                                valores[2]," Grupo 2 vs 3: ",valores[4]) %>% as.character())
  return(df1)
  
}

ctabla3 <- function(df, x) {
  
  dfp <- df %>% filter(PP == 1) %>% 
    select(c(x,grupodetratamiento)) %>% drop_na(x) %>% as.data.frame()
  
  df1 <- dfp %>% group_by(grupodetratamiento) %>% 
    summarise(value = quantile(get(x),.25, na.rm = T, type = 6) %>% round(1)) %>% 
    spread(key="grupodetratamiento",value = "value") %>% dplyr::rename(P25_1=1,P25_2=2,P25_3=3) %>% 
    mutate(Variable = x)
  
  
  dfl <- dfp %>%  
    group_by(grupodetratamiento) %>% summarise(value = quantile(get(x),.75, na.rm = T, type = 6) %>% round(1)) %>% 
    spread(key="grupodetratamiento",value = "value") %>% dplyr::rename(P75_1=1,P75_2=2,P75_3=3) %>% 
    mutate(Variable = x)
  
  df1 <- left_join(df1,dfl, by="Variable")
  
  
  dfl <- dfp %>% 
    group_by(grupodetratamiento) %>% summarise(value = median(get(x), na.rm = T) %>% round(1)) %>% 
    spread(key="grupodetratamiento",value = "value") %>% dplyr::rename(MEDIANA_1=1,MEDIANA_2=2,MEDIANA_3=3) %>% 
    mutate(Variable = x)
  
  df1 <- left_join(df1,dfl, by="Variable")
  
  dfl <- dfp %>% 
    group_by(grupodetratamiento) %>% summarise(value = n()) %>% 
    spread(key="grupodetratamiento",value = "value") %>% dplyr::rename(N_1=1,N_2=2,N_3=3) %>% 
    mutate(Variable = x)
  
  df1 <- left_join(df1,dfl, by="Variable") %>% 
    mutate(`MEDIANA P25 - P75 1`=paste0(MEDIANA_1," [",P25_1," a ",P75_1,"]"),
           `MEDIANA P25 - P75 2`=paste0(MEDIANA_2," [",P25_2," a ",P75_2,"]"),
           `MEDIANA P25 - P75 3`=paste0(MEDIANA_3," [",P25_3," a ",P75_3,"]")) %>% 
    select(c(Variable,`MEDIANA P25 - P75 1`,`MEDIANA P25 - P75 2`,`MEDIANA P25 - P75 3`))
  
  pvalue <- dfp %>% 
    kruskal.test(get(x) ~ grupodetratamiento, data = .)
  
  pvalue <- pvalue$p.value %>% round(3)
  
  df1 %<>% 
    mutate(P = ifelse(pvalue < 0.001,"p < 0.001",pvalue %>% round(3)) %>% as.character())
  
  return(df1)
  
}


ctabla4 <- function(df, x) {
  
  dfp <- df %>% filter(PP == 1) %>% 
    select(c(x,grupodetratamiento)) %>% drop_na(x) %>% as.data.frame()
  
  df1 <- dfp %>% group_by(grupodetratamiento) %>% 
    summarise(value = quantile(get(x),.25, na.rm = T, type = 6) %>% round(1)) %>% 
    spread(key="grupodetratamiento",value = "value") %>% dplyr::rename(P25_1=1,P25_2=2,P25_3=3) %>% 
    mutate(Variable = x)
  
  
  dfl <- dfp %>%  
    group_by(grupodetratamiento) %>% summarise(value = sd(get(x), na.rm = T) %>% round(2)) %>% 
    spread(key="grupodetratamiento",value = "value") %>% dplyr::rename(P75_1=1,P75_2=2,P75_3=3) %>% 
    mutate(Variable = x)
  
  df1 <- left_join(df1,dfl, by="Variable")
  
  
  dfl <- dfp %>% 
    group_by(grupodetratamiento) %>% summarise(value = mean(get(x), na.rm = T) %>% round(1)) %>% 
    spread(key="grupodetratamiento",value = "value") %>% dplyr::rename(MEDIANA_1=1,MEDIANA_2=2,MEDIANA_3=3) %>% 
    mutate(Variable = x)
  
  df1 <- left_join(df1,dfl, by="Variable")
  
  dfl <- dfp %>% 
    group_by(grupodetratamiento) %>% summarise(value = n()) %>% 
    spread(key="grupodetratamiento",value = "value") %>% dplyr::rename(N_1=1,N_2=2,N_3=3) %>% 
    mutate(Variable = x)
  
  df1 <- left_join(df1,dfl, by="Variable") %>% 
    mutate(`MEDIANA P25 - P75 1`=paste0(MEDIANA_1," ± ",P75_1),
           `MEDIANA P25 - P75 2`=paste0(MEDIANA_2," ± ",P75_2),
           `MEDIANA P25 - P75 3`=paste0(MEDIANA_3," ± ",P75_3)) %>% 
    select(c(Variable,`MEDIANA P25 - P75 1`, `MEDIANA P25 - P75 2`, `MEDIANA P25 - P75 3`))
  
  pvalue <- aov(dfp[,x] ~ dfp$grupodetratamiento) %>% summary()
  
  pvalue <- pvalue[[1]][["Pr(>F)"]][1]
  
  df1 %<>% 
    mutate(P = ifelse(pvalue < 0.001,"p < 0.001",pvalue %>% round(3)) %>% as.character())
  
  return(df1)
  
}


chisqtable <- function(df,x){
  
  df1 <- df %>% filter(PP == 1) %>% as.data.frame()
  
  dft <- table(df1[,'grupodetratamiento'],df1[,x]) %>% as.data.frame() %>% 
    spread(key = "Var2", value = "Freq") %>% rename(NO=2,SI=3)
  
  
  pvalue <- chisq.test(df1[,'grupodetratamiento'],df1[,x])$p.value
  pvalue2 <- chisq.post.hoc(table(df1[,'grupodetratamiento'],df1[,x]),
                            test='chisq.test', control = "bonferroni")
  
  valores = c()
  
  for (i in 1:length(pvalue2$adj.p)) {
    
    valores[i] <- ifelse(pvalue2$adj.p[i] < 0.001,"< 0.001", pvalue2$adj.p[i] %>% round(3))
    
  }
  
  dft %<>% 
    mutate(p_chi = ifelse(pvalue < 0.001, "< 0.001", pvalue %>% round(2)),
           p_intg = paste0("Grupo 1 vs 2: ",valores[1]," Grupo 1 vs 3: ",
                           valores[2]," Grupo 2 vs 3: ",valores[3]) %>% as.character(),
           n_per = ((NO/(NO+SI))*100) %>% round(1), s_per = ((SI/(NO+SI))*100) %>% round(1),
           Var1 = ifelse(Var1 == "1","Grupo 1 (Atorvastatina 20 mg + Fenofibrato 160 mg)\n",
                    ifelse(Var1 == "2","Grupo 2 (Atorvastatina 20 mg)\n",
                      ifelse(Var1 == "3","Grupo 3 (Fenofibrato 160 mg)\n",NA))),
           Var1 = paste0(Var1," N = ",NO+SI),
           NO = paste0(n_per,"%"," ","(",NO,")"),
           SI = paste0(s_per,"%"," ","(",SI,")")) %>% 
    dplyr::select(1,3,2,p_chi,p_intg)
  
  return(dft)
}


cleanum <- function(ttt){
  as.numeric(gsub('[^0-9\\.]', '', ttt))
}

# Error estándar

std <- function(x) sd(x, na.rm = T)/sqrt(length(x))

# Gráficas de errores

gfunction <- function(df,x,title,ytitle){

dfp <- df %>% filter(PP == 1) %>% as.data.frame()

dfg <- dfp %>% 
  group_by(grupodetratamiento) %>% summarise(value = median(get(x),na.rm = T))

dfg <- left_join(dfg,dfp %>% 
                   group_by(grupodetratamiento) %>% summarise(value = std(get(x))), 
                 by = "grupodetratamiento") %>% rename(Var1=1,value=2,sd=3) %>% 
  mutate(Var1 = ifelse(Var1 == "1","Grupo 1 (Atorvastatina 20 mg + Fenofibrato 160 mg)\n",
                       ifelse(Var1 == "2","Grupo 2 (Atorvastatina 20 mg)\n",
                              ifelse(Var1 == "3","Grupo 3 (Fenofibrato 160 mg)\n",NA))))


pvalue2 <- pairwise.wilcox.test(x = dfp[,x], g = dfp$grupodetratamiento, p.adjust.method = "bonf", correct=FALSE)

valores = c()

for (i in 1:length(pvalue2$p.value)) {
  
  valores[i] <- ifelse(pvalue2$p.value[i] < 0.001,"p < 0.001", pvalue2$p.value[i] %>% round(2))
  
}

interg <- paste0("Grupo 1 vs 2: ",valores[1],"\nGrupo 1 vs 3: ",
                 valores[2],"\nGrupo 2 vs 3: ",valores[4]) %>% as.character()


ggp <- ggplot(dfg) +
  geom_bar(aes(x=Var1, y=value, fill = "manual"), stat="identity", alpha=0.8) +
  geom_errorbar(aes(x=Var1, ymin=value-sd*2, ymax=value+sd*2), width=0.4, colour="black", alpha=0.6, size=0.65)+
  theme_bw()+
  scale_fill_manual("",values = c("#33658A"))+
  labs(title = title,
       y= ytitle,
       x="Grupo de tratamiento",
       caption = "Barras de error: +/- 2 SE")+
  theme(axis.text.x = element_text(size=7.5),
        axis.title.x = element_text(size=9, face = "bold", 
                                    margin = margin(t=0.5,unit = "cm"), color = "#666666"),
        axis.title.y = element_text(size=9, face = "bold",color = "#666666"),
        plot.caption = element_text(hjust = 0.5, size=7.5,color = "#666666"),
        plot.title = element_text(vjust=-2.5, size = 13.5, hjust = 0.5, 
                                  color = "#666666", face = "bold", 
                                  margin = margin(b=1, unit = "cm")),
        legend.title = element_text(face="bold",color = "#666666"),
        panel.grid = element_blank(),
        legend.position = "none")+
  annotate("label", x = 3, y = 1.05*(min(dfg$value))-((dfg[3,"sd"] %>% as.numeric())), 
           label = interg, size=2, hjust = 0)+
  ylim(c(1.05*(min(dfg$value))-((dfg[3,"sd"] %>% as.numeric()*2)),0))

return(ggp)}

gfunction2 <- function(df,x,title,ytitle){
  
  dfp <- df %>% filter(PP == 1) %>% as.data.frame()
  
  dfg <- dfp %>% 
    group_by(grupodetratamiento) %>% summarise(value = median(get(x),na.rm = T))
  
  dfg <- left_join(dfg,dfp %>% 
                     group_by(grupodetratamiento) %>% summarise(value = std(get(x))), 
                   by = "grupodetratamiento") %>% rename(Var1=1,value=2,sd=3) %>% 
    mutate(Var1 = ifelse(Var1 == "1","Grupo 1 (Atorvastatina 20 mg + Fenofibrato 160 mg)\n",
                         ifelse(Var1 == "2","Grupo 2 (Atorvastatina 20 mg)\n",
                                ifelse(Var1 == "3","Grupo 3 (Fenofibrato 160 mg)\n",NA))))
  
  
  pvalue2 <- pairwise.wilcox.test(x = dfp[,x], g = dfp$grupodetratamiento, p.adjust.method = "bonf", correct=FALSE)
  
  valores = c()
  
  for (i in 1:length(pvalue2$p.value)) {
    
    valores[i] <- ifelse(pvalue2$p.value[i] < 0.001,"p < 0.001", pvalue2$p.value[i] %>% round(2))
    
  }
  
  interg <- paste0("Grupo 1 vs 2: ",valores[1],"\nGrupo 1 vs 3: ",
                   valores[2],"\nGrupo 2 vs 3: ",valores[4]) %>% as.character()
  
  
  ggp <- ggplot(dfg) +
    geom_bar(aes(x=Var1, y=value, fill = "manual"), stat="identity", alpha=0.8) +
    geom_errorbar(aes(x=Var1, ymin=value-sd*2, ymax=value+sd*2), width=0.4, colour="black", alpha=0.6, size=0.65)+
    theme_bw()+
    scale_fill_manual("",values = c("#33658A"))+
    labs(title = title,
         y= ytitle,
         x="Grupo de tratamiento",
         caption = "Barras de error: +/- 2 SE")+
    theme(axis.text.x = element_text(size=7.5),
          axis.title.x = element_text(size=9, face = "bold", 
                                      margin = margin(t=0.5,unit = "cm"), color = "#666666"),
          axis.title.y = element_text(size=9, face = "bold",color = "#666666"),
          plot.caption = element_text(hjust = 0.5, size=7.5,color = "#666666"),
          plot.title = element_text(vjust=-2.5, size = 13.5, hjust = 0.5, 
                                    color = "#666666", face = "bold", 
                                    margin = margin(b=1, unit = "cm")),
          legend.title = element_text(face="bold",color = "#666666"),
          panel.grid = element_blank(),
          legend.position = "none")+
    annotate("label", x = 3, y = 1.05*(min(dfg$value))-((dfg[3,"sd"] %>% as.numeric()*1.5)), 
             label = interg, size=2, hjust = 0)+
    ylim(c(1.05*(min(dfg$value))-((dfg[3,"sd"] %>% as.numeric()*2)),
           1.1*(max(dfg$value)+(2*dfg[3,"sd"] %>% as.numeric()))))

return(ggp)}
