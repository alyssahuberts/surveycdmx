################
# Sampling frame 
# Date created: 3/8/2021
# Date last edited: 3/8/2021
################
library(tidyverse)
library(sf)
library(magrittr)
library(factoextra)
library(fuzzyjoin)

# Putting everything at the colonia level

  # need to match ageb data onto colonias http://rstudio-pubs-static.s3.amazonaws.com/255550_1c983c8a6a5a45a0aee5a923206f4818.html
  
  # read in colonias 
  colonias_shp <- st_read("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/9_Administrative/coloniascdmx/coloniascdmx.shp")
  colonias_shp <- st_set_crs(colonias_shp, 4326)
  colonias_shp$colonia_area <- st_area(colonias_shp)
  pdf("/Users/alyssahuberts/Dropbox/1_Dissertation/8_Survey/4_plots/colonias.pdf")
  ggplot(colonias_shp) + geom_sf() + theme_bw() + labs(title = "Colonias")
  dev.off()
  
  # read in ageb shapefile 
  ageb_shp <- st_read("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/4_Demographics/scince_2010/shps/df/df_ageb_urb.shp")
  ageb_shp <- st_set_crs(ageb_shp, 4326)

  # for every colonia, identify which agebs it intersects and the area for each (so I can do weighted averages)
  colonias_ageb <- st_intersection(colonias_shp,ageb_shp[c("CVEGEO", "OID", "geometry")])
  colonias_ageb$int_area <- st_area(colonias_ageb)
  colonias_ageb$int_perc_colonia_area <- as.numeric(colonias_ageb$int_area/colonias_ageb$colonia_area)
  # get rid of places where it's just map areas/boundaries (i'm defining this as less than 5% of colonia is in the ageb)
  colonias_ageb <- colonias_ageb %>% filter(int_perc_colonia_area >.05)

#######################
# Census Data 
#######################  
  # read in the shapefile at the ageb level. Note that we're only using the urban agebs, which I think is ok
  # 2020 ageb - note that this database contains both manzanas and agebs but we'll stick to ageb
  # from https://en.www.inegi.org.mx/programas/ccpv/2020/#Microdata
  census <- read_csv("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/4_Demographics/2020_census_ageb.csv",
                     col_types = cols_only(
                       MUN = col_character(), 
                       NOM_MUN = col_character(), 
                       NOM_LOC = col_character(), 
                       AGEB = col_character(),
                       POBTOT = col_character(),
                       PROM_OCUP = col_character(),
                       TVIVHAB = col_character(),
                       TVIVPAR = col_character(), 
                       VIVPAR_HAB = col_character(), 
                       VPH_AEASP= col_character(), 
                       VPH_TINACO = col_character(),
                       VPH_CISTER = col_character(),
                       VPH_REFRI =col_character(),
                       VPH_LAVAD = col_character(),
                       VPH_AUTOM = col_character(),
                       VPH_MOTO = col_character(),
                       VPH_BICI = col_character(),
                       VPH_RADIO = col_character(),
                       VPH_TV = col_character(),
                       VPH_PC = col_character(),
                       VPH_TELEF = col_character(),
                       VPH_CEL = col_character(),
                       VPH_INTER = col_character()
                     )
                     )
  # drop totals and manzana level data
  census_ageb <- census[census$NOM_LOC == "Total AGEB urbana",] %>% 
    select( MUN, NOM_MUN, NOM_LOC,AGEB, POBTOT, PROM_OCUP, TVIVHAB, TVIVPAR, VIVPAR_HAB,
           VPH_AEASP, VPH_TINACO, VPH_CISTER,VPH_REFRI,
           VPH_LAVAD,VPH_AUTOM, VPH_MOTO, VPH_BICI, 
           VPH_RADIO, VPH_TV, VPH_PC, VPH_TELEF, VPH_CEL,
           VPH_INTER)
  rm(census)
  
  census_ageb[,6:23] <- lapply(census_ageb[,6:23],as.numeric)
  percents <- census_ageb[,8:23]
  colnames(percents) <- paste("p_", colnames(percents), sep = "")
  census_ageb <- cbind(census_ageb, percents)
  rm(percents)
  census_ageb[,-(1:24)] %<>% sapply(`/`, census_ageb[,7])
  
  # create wealth index using pca and asset variables 
  census_percents_ageb_complete <- na.omit(census_ageb[,c(29:39)]) 
  assets <- prcomp(census_percents_ageb_complete, center = TRUE,scale. = TRUE)
  census_percents_ageb_complete <- na.omit(census_ageb[,c(1:4, 29:39)]) 
  census_percents_ageb_complete$pca_1 <-  get_pca_ind(assets)$coord[,1]
  census_ageb <- left_join(census_ageb, census_percents_ageb_complete[,c("MUN", "NOM_MUN", "AGEB","pca_1")], by = c("MUN", "NOM_MUN", "AGEB"))
   
  rm(list=setdiff(ls(), c("census_percents_ageb", "census_ageb")))
  
  ####################
  # Water quality data 
  ####################
  # read in colonia level rationing data
  tandeo <- read.csv("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/3_Tandeo/Florencio_FOIA/programa_tandeo_csv.csv")
  colnames(tandeo) <- c( "alcaldia", "colonia", "dias_tandeo", "horas_tandeo", "status", "cve_col")
  days_week <- read_csv("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/3_Tandeo/Florencio_FOIA/days_week.csv") 
  colnames(days_week) <- c("dias_tandeo", "days_estiaje", "days_normal")
  hours_day <- read_csv("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/3_Tandeo/Florencio_FOIA/hours_day.csv")
  colnames(hours_day) <- c("horas_tandeo", "hours")
  tandeo$horas_tandeo <- trimws(tandeo$horas_tandeo)
  tandeo$dias_tandeo <- trimws(tandeo$dias_tandeo)
  tandeo <- left_join(tandeo, days_week, by = "dias_tandeo")
  tandeo <- left_join(tandeo, hours_day, by = "horas_tandeo")
  tandeo$hours_week_estiaje <- tandeo$days_estiaje*tandeo$hours
  tandeo$hours_week_normal <- tandeo$days_normal*tandeo$hours
  
  # some colonias are duplicated because of how they're listed in the register
  # (like if they're partial secciones, etc). I could go back and get the exact
  # manzanas, but for now I'm just taking the average
  tandeo_hours_col <- tandeo %>% 
    group_by(cve_col) %>% 
    summarize(hours_week_normal = mean(hours_week_normal, na.rm=TRUE),
              hours_week_estiaje = mean(hours_week_estiaje, na.rm=TRUE))
  
  # what about status?
  tandeo_status_col <- tandeo %>% 
    group_by(cve_col) %>% 
    summarize(status_list = paste(status, collapse = ""))
  tandeo_status_col$status <- NA
  tandeo_status_col[str_detect(tandeo_status_col$status_list, "Tandeo")==TRUE &str_detect(tandeo_status_col$status_list, "Condonado")==TRUE,]$status <- "Rationed/condoned"
  tandeo_status_col[str_detect(tandeo_status_col$status_list, "Tandeo")==TRUE &str_detect(tandeo_status_col$status_list, "Condonado")==FALSE,]$status <- "Rationed"
  tandeo_status_col[str_detect(tandeo_status_col$status_list, "Tandeo")==FALSE &str_detect(tandeo_status_col$status_list, "Condonado")==TRUE,]$status <- "Condoned"
  
  tandeo_out <- left_join(tandeo_hours_col,tandeo_status_col[,c("cve_col", "status")] )
  # read in colonias 
  colonias <- read.csv("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/9_Administrative/coloniascdmx/coloniascdmx.csv")
  colonias <- left_join(colonias, tandeo_out, by = "cve_col")
  colonias$status <-ifelse(is.na(colonias$status), "None", colonias$status)
  colonias$hours_week_normal <- ifelse(is.na(colonias$hours_week_normal),168, colonias$hours_week_normal)
  colonias$hours_week_estiaje <- ifelse(is.na(colonias$hours_week_estiaje),168, colonias$hours_week_estiaje)
  
  
  