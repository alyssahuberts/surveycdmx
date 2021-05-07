################
# Pull Sample 
# Date created: April 15, 2021
# Date last update April 15, 2021
################
# Seccion level sampling frame 
library(tidyverse)
library(sf)
library(lubridate)
library(modeest)
library(factoextra)
library(magrittr)
library(janitor)
library(xlsx)


load("/Users/alyssahuberts/Dropbox/1_Dissertation/8_Survey/8_Data/sampling_frame.Rdata")

# set seed 
set.seed(112521)

sampled_secciones <- sample_secciones %>% group_by(block) %>% sample_n(105) %>% mutate(id = row_number())%>% ungroup()
sampled_secciones$list <- case_when(sampled_secciones$id < 36~ "Primary",
                                    sampled_secciones$id >=36& sampled_secciones$id <71 ~"Alternate 1",
                                    sampled_secciones$id >=71 ~ "Alternate 2") 

sampled_secciones$block_number <- case_when(sampled_secciones$block == "Votes = 1, Megaphone = 0" ~ "1",
                                            sampled_secciones$block == "Votes = 2, Megaphone = 0" ~ "2",
                                            sampled_secciones$block == "Votes = 3, Megaphone = 0" ~ "3",
                                            sampled_secciones$block == "Votes = 1, Megaphone = 1" ~ "4",
                                            sampled_secciones$block == "Votes = 2, Megaphone = 1" ~ "5",
                                            sampled_secciones$block == "Votes = 3, Megaphone = 1" ~ "6",)

sampled_secciones$municipio <- case_when(sampled_secciones$municipio == 5 ~"Gustavo A Madero",
                                         sampled_secciones$municipio == 6 ~"Iztacalco",
                                         sampled_secciones$municipio == 7 ~"Iztapalapa",
                                         sampled_secciones$municipio == 11 ~"Tlahuac",
                                         sampled_secciones$municipio == 17 ~"Venustiano Carranza")

sampled_secciones <- sampled_secciones %>% select(municipio,seccion, list, block_number)

sampled_secciones_primary <- sampled_secciones %>% filter(list == "Primary")
write_csv(sampled_secciones_primary, path = "/Users/alyssahuberts/Dropbox/1_Dissertation/8_Survey/8_Data/muestra_principal.csv")
sampled_secciones_alt_1 <- sampled_secciones %>% filter(list == "Alternate 1")
write_csv(sampled_secciones_alt_1, path = "/Users/alyssahuberts/Dropbox/1_Dissertation/8_Survey/8_Data/muestra_alt1.csv")
sampled_secciones_alt_2 <- sampled_secciones %>% filter(list == "Alternate 2")
write_csv(sampled_secciones_alt_2, path = "/Users/alyssahuberts/Dropbox/1_Dissertation/8_Survey/8_Data/muestra_alt2.csv")


# now make the map of the sample (by alcaldia, so it's big enough to see)
secciones <- read_csv("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/9_Administrative/Estadisticas_censales_a_escalas_geoelectorales/secciones.csv") %>% 
  select(SECCION, MUNICIPIO)
sample_expanded <- read_csv("/Users/alyssahuberts/Dropbox/1_Dissertation/8_Survey/8_Data/muestra_principal_ampliada.csv")
secciones$muestra_primaria <- ifelse(secciones$SECCION %in% sample_expanded$seccion,1,0)
write_csv(secciones, "/Users/alyssahuberts/Dropbox/1_Dissertation/8_Survey/8_Data/secciones_with_sample.csv")


# read in shapefile of secciones
secciones <- read_sf("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/9_Administrative/Estadisticas_censales_a_escalas_geoelectorales/secciones.shp")
secciones$muestra_primaria <- ifelse(secciones$SECCION %in% sample_expanded$seccion,"Seleccionada","No seleccionada")
# read in roads
roads <- read_sf("/Users/alyssahuberts/Dropbox/1_Dissertation/8_Survey/8_Data/mexico-latest-free.shp/gis_osm_roads_free_1.shp")

roads <- st_set_crs(roads, st_crs(secciones))
# do each alcaldia separately so the maps are bigger 
gam <- secciones %>% filter(MUNICIPIO == 5)
gam_selected <- secciones %>% filter(MUNICIPIO==5 & muestra_primaria==1)
pdf("/Users/alyssahuberts/Dropbox/1_Dissertation/8_Survey/8_Data/sample_maps/gam.pdf", width = 10)
ggplot(gam) + geom_sf(aes(fill = factor(muestra_primaria))) +
  theme_bw() + 
  labs(title = "Gustavo A Madero", fill = "Muestra Principal")+
  theme(legend.position = "bottom") +
  geom_sf_text(aes(label = SECCION), size = 1, color = "white")
dev.off()

iztc <- secciones %>% filter(MUNICIPIO == 6)
pdf("/Users/alyssahuberts/Dropbox/1_Dissertation/8_Survey/8_Data/sample_maps/izt.pdf", width = 10)
ggplot(iztc) + geom_sf(aes(fill = factor(muestra_primaria))) + theme_bw()+
  labs(title = "Iztacalco", fill = "Muestra Principal")+
  theme(legend.position = "bottom") +
  geom_sf_text(aes(label = SECCION), size = 1, color = "white")
dev.off()

iztp <- secciones %>% filter(MUNICIPIO == 7)
pdf("/Users/alyssahuberts/Dropbox/1_Dissertation/8_Survey/8_Data/sample_maps/iztp.pdf", width = 10)
ggplot(iztp) + geom_sf(aes(fill = factor(muestra_primaria))) + theme_bw()+
  labs(title = "Iztapalapa", fill = "Muestra Principal")+
  theme(legend.position = "bottom") +
  geom_sf_text(aes(label = SECCION), size = 1, color = "white")
dev.off()

vc <- secciones %>% filter(MUNICIPIO == 17)
pdf("/Users/alyssahuberts/Dropbox/1_Dissertation/8_Survey/8_Data/sample_maps/vc.pdf", width = 10)
ggplot(vc) + geom_sf(aes(fill = factor(muestra_primaria))) + theme_bw()+
  labs(title = "Venustiano Carranza", fill = "Muestra Principal")+
  theme(legend.position = "bottom") +
  geom_sf_text(aes(label = SECCION), size = 1, color = "white")
dev.off()

tlh <- secciones %>% filter(MUNICIPIO ==11)
pdf("/Users/alyssahuberts/Dropbox/1_Dissertation/8_Survey/8_Data/sample_maps/tlh.pdf", width = 10)
ggplot(tlh) + geom_sf(aes(fill = factor(muestra_primaria))) + theme_bw()+
  labs(title = "Tlahuac", fill = "Muestra Principal")+
  theme(legend.position = "bottom") +
  geom_sf_text(aes(label = SECCION), size = 1, color = "white")
dev.off()


roads_cdmx <- st_intersection(roads, secciones)
st_write(roads_cdmx, "/Users/alyssahuberts/Dropbox/1_Dissertation/8_Survey/8_Data/sample_maps/roads_cdmx.shp", 
         driver = "ESRI Shapefile")
roads_gam <- st_intersection(roads, gam)
st_write(roads_gam, "/Users/alyssahuberts/Dropbox/1_Dissertation/8_Survey/8_Data/sample_maps/roads_gam.shp", 
         driver = "ESRI Shapefile")

roads_iztc <- st_intersection(roads, iztc)
st_write(roads_iztc, "/Users/alyssahuberts/Dropbox/1_Dissertation/8_Survey/8_Data/sample_maps/roads_izt.shp", 
         driver = "ESRI Shapefile")

roads_iztp <- st_intersection(roads, iztp)
st_write(roads_iztp, "/Users/alyssahuberts/Dropbox/1_Dissertation/8_Survey/8_Data/sample_maps/roads_iztp.shp", 
         driver = "ESRI Shapefile")

roads_vc <- st_intersection(roads, vc)
st_write(roads_vc, "/Users/alyssahuberts/Dropbox/1_Dissertation/8_Survey/8_Data/sample_maps/roads_vc.shp", 
         driver = "ESRI Shapefile")

roads_tlh <- st_intersection(roads, tlh)
st_write(roads_tlh, "/Users/alyssahuberts/Dropbox/1_Dissertation/8_Survey/8_Data/sample_maps/roads_tlh.shp", 
         driver = "ESRI Shapefile")


roads_gam <- st_intersection(gam, roads_cdmx)
ggplot() + geom_sf(data = roads_gam, aes(color = "red"))
ggplot() + geom_sf(data = gam, aes(fill = muestra_primaria)) + geom_sf(data= roads_gam)
