library(tidyverse)
library(sf)
library(readxl)
library(geobr)

#Importando os dados
covidalag <- read_excel("Dados/covidalag.xlsx")
View(covidalag)

#Baixando o mapa de Alagoas
mapaal <- read_municipality(code_muni=27, year=2019)

#Agregando os dados com o mapa
datacov_alag <- left_join(mapaal,covidalag, by = c("code_muni"="cod_muni"))
attach(datacov_alag)

#Plotando o gráfico
mapacovid_alag <- ggplot(data = datacov_alag, aes(fill= casos_conf))+
  geom_sf()+
  theme_minimal()+
  scale_fill_gradientn(name = "Casos Confirmados", colours = c("#3B9AB2","#E1AF00","#F21A00"))+
  theme(axis.title=element_blank(), axis.text=element_blank(), axis.ticks=element_blank())+
  labs(subtitle = "Casos Confirmados de Coronavírus em Alagoas")
mapacovid_alag



