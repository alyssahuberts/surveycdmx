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

  