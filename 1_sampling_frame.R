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
library(RColorBrewer)
library(gridExtra)
library(grid)
library(ggsflabel)
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
  x <- colonias_ageb %>% group_by(alcaldia, cve_col) %>% summarize(sum_area = sum(int_perc_colonia_area))
  
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
                       LOC = col_character(),
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
    select( MUN, NOM_MUN, LOC, NOM_LOC,AGEB, POBTOT, PROM_OCUP, TVIVHAB, TVIVPAR, VIVPAR_HAB,
           VPH_AEASP, VPH_TINACO, VPH_CISTER,VPH_REFRI,
           VPH_LAVAD,VPH_AUTOM, VPH_MOTO, VPH_BICI, 
           VPH_RADIO, VPH_TV, VPH_PC, VPH_TELEF, VPH_CEL,
           VPH_INTER)
  rm(census)
  
  census_ageb[,7:24] <- lapply(census_ageb[,7:24],as.numeric)
  percents <- census_ageb[,9:24]
  colnames(percents) <- paste("p_", colnames(percents), sep = "")
  census_ageb <- cbind(census_ageb, percents)
  rm(percents)
  census_ageb[,-(1:25)] %<>% sapply(`/`, census_ageb[,8])
  
  # create wealth index using pca and asset variables 
  census_percents_ageb_complete <- na.omit(census_ageb[,c(30:40)]) 
  assets <- prcomp(census_percents_ageb_complete, center = TRUE,scale. = TRUE)
  census_percents_ageb_complete <- na.omit(census_ageb[,c(1:5, 30:40)]) 
  census_percents_ageb_complete$pca_1 <-  get_pca_ind(assets)$coord[,1]
  census_ageb <- left_join(census_ageb, census_percents_ageb_complete[,c("MUN", "NOM_MUN", "LOC", "AGEB","pca_1")], by = c("MUN", "NOM_MUN", "AGEB", "LOC"))
   
  # merge the values onto the colonia-ageb level data 
  # For urban areas (the areas I have here) the CVEGEO can be broken into: 
  # Urban Areas
  # EE+MMM+LLLL+AAA-A+NNN
  # EE: State (2 digits)
  # MMM: Municipality (3 digits)
  # LLLL: Locality (four digits)
  # AAA-A: AGEB (3 digits, dash, 1 digit) 
  # NNN: Manzana/block (3 digits) 
  
  census_ageb$CVEGEO <- paste("09", census_ageb$MUN, census_ageb$LOC, census_ageb$AGEB, sep = "")
  colonias_ageb <- left_join(colonias_ageb, census_ageb[,c("CVEGEO", "POBTOT", "PROM_OCUP", "TVIVHAB", "TVIVPAR", "VIVPAR_HAB", "VPH_AEASP",
                                                           "p_VPH_TINACO", "p_VPH_CISTER", "pca_1")], by = "CVEGEO")
  rm(census_percents_ageb_complete, assets)
  
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
  
  tandeo_out <- left_join(tandeo_hours_col,tandeo_status_col[,c("cve_col", "status")], by = "cve_col" ) %>% rename(rationing_status = status)
  write_csv(tandeo_out, path = "/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/3_Tandeo/Florencio_FOIA/tandeo_clean.csv")
  # read in colonias 
  colonias_tandeo <- read.csv("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/9_Administrative/coloniascdmx/coloniascdmx.csv") %>% 
    select(cve_col)
  colonias_tandeo <- left_join(colonias_tandeo, tandeo_out, by = "cve_col")
  colonias_tandeo$rationing_status <-ifelse(is.na(colonias_tandeo$rationing_status), "None", colonias_tandeo$rationing_status)
  colonias_tandeo$hours_week_normal <- ifelse(is.na(colonias_tandeo$hours_week_normal),168, colonias_tandeo$hours_week_normal)
  colonias_tandeo$hours_week_estiaje <- ifelse(is.na(colonias_tandeo$hours_week_estiaje),168, colonias_tandeo$hours_week_estiaje)
  
  rm(days_week, hours_day, tandeo, tandeo_hours_col, tandeo_status_col, tandeo_out)
  
  colonias_ageb <- left_join(colonias_ageb, colonias_tandeo, by = "cve_col")
  
  colonias_ageb$water_hours_normal <- case_when((colonias_ageb$hours_week_normal < 24) ~ "< 1 day", 
                                                (colonias_ageb$hours_week_normal > 24 & colonias_ageb$hours_week_normal <48) ~ "1-2 days", 
                                                (colonias_ageb$hours_week_normal > 48 & colonias_ageb$hours_week_normal < 96) ~ "3-4 days", 
                                                (colonias_ageb$hours_week_normal > 125 & colonias_ageb$hours_week_normal < 144) ~ "5-6 days",
                                                (colonias_ageb$hours_week_normal > 144) ~ "5-6 days",
                                                TRUE ~ NA)
  
  colonias_ageb %<>%
    mutate(water_hours_normal=case_when(
      hours_week_normal %in% 0:24 ~ "<1 day a week",
      hours_week_normal %in% 25:48 ~ "1-2 days",
      hours_week_normal %in% 49:96 ~ "3-4 days",
      hours_week_normal %in% 96:166 ~ "5-7 days",
      hours_week_normal %in% 167:169 ~ "Continuous",
      TRUE ~ "NA"
    ))
  colonias_ageb[colonias_ageb$water_hours_normal == "NA",]$water_hours_normal <- NA
  
  colonias_ageb %<>%
    mutate(water_hours_estiaje=case_when(
      hours_week_estiaje %in% 0:24 ~ "<1 day a week",
      hours_week_estiaje %in% 25:48 ~ "1-2 days",
      hours_week_estiaje %in% 49:96 ~ "3-4 days",
      hours_week_estiaje %in% 96:166 ~ "5-7 days",
      hours_week_estiaje %in% 167:169 ~ "Continuous",
      TRUE ~ "NA"
    ))
  colonias_ageb[colonias_ageb$water_hours_estiaje == "NA",]$water_hours_estiaje <- NA
  
  # correct for the colonias that don't use tandeo (BJ, MH, Iztaclaco, V Carranza)
  colonias_ageb[colonias_ageb$alcaldia== "BENITO JUAREZ"|
                colonias_ageb$alcaldia== "IZTACALCO"|
                colonias_ageb$alcaldia== "MIGUEL HIDALGO"|
                colonias_ageb$alcaldia== "VENUSTIANO CARRANZA","water_hours_normal"]<- NA
 
   colonias_ageb[colonias_ageb$alcaldia== "BENITO JUAREZ"|
                  colonias_ageb$alcaldia== "IZTACALCO"|
                  colonias_ageb$alcaldia== "MIGUEL HIDALGO"|
                  colonias_ageb$alcaldia== "VENUSTIANO CARRANZA","water_hours_estiaje"]<- NA
   pdf("/Users/alyssahuberts/Dropbox/1_Dissertation/8_Survey/4_plots/estiaje.pdf")
   ggplot(colonias_ageb) + geom_sf(aes(fill = water_hours_estiaje))+
    theme_classic()+
     geom_sf(data = alcaldias_shp,fill = NA)+
    labs(title = "Hours of Water Per Week* \n During Dry Season",
         caption = "*According to official rationing records.\n  Excludes alcaldias that do not formally ration. \n  Understates scarcity and excludes rural AGEBS.",
  bottom = textGrob("grid caption", x = 1, hjust = 1, gp = gpar(fontface = 3L, fontsize = 9)))+
     theme(legend.position = "bottom")
  dev.off()
  
  alcaldias_shp$selected <- ifelse(alcaldias_shp$nomgeo == "Gustavo A. Madero"|
                               alcaldias_shp$nomgeo == "Iztacalco"|
                              alcaldias_shp$nomgeo == "Iztapalapa"|
                                alcaldias_shp$nomgeo == "Tláhuac"|
                                alcaldias_shp$nomgeo == "Venustiano Carranza",1,0)
  alcaldias_shp$name <- factor(alcaldias_shp$nomgeo, 
                               levels = levels(as.factor(alcaldias_shp$nomgeo)),
                               labels = c("Álvaro \nObregón", "Azcapotzalco", "Benito \nJuárez",
                                          "Coyoacán", "Cuajimalpa \n de \nMorelos", "Cuauhtémoc",
                                          "Gustavo \n A. \nMadero", "Iztacalco",  "Iztapalapa",
                                          "Magdalena \n Contreras", "Miguel \nHidalgo", "Milpa\n Alta",
                                          "Tláhuac","Tlalpan","Venustiano\n Carranza" ,"Xochimilco" ))
  #devtools::install_github("yutannihilation/ggsflabel")
  pdf("/Users/alyssahuberts/Dropbox/1_Dissertation/8_Survey/4_plots/alcaldias.pdf")
  ggplot(alcaldias_shp) + geom_sf(aes(fill = factor(selected)))+
    geom_sf_label_repel(aes(label = name))+
    labs(title = "Alcaldias") + theme(legend.position = "none")
         dev.off()
  # bring in altitude at colonia level
  altitude <- read_csv("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/5_Altitude/altitude_colonia.csv")
  colonias_ageb <- left_join(colonias_ageb, altitude,  by = c("nombre", "entidad", "cve_alc", "alcaldia", "cve_col", "secc_com", "secc_par"))
  
  # now bring in the elections data. The easiest way to do this is to take my
  # elections database, create weighted averages of colonias, and then merge
  # that on to the colonia db
  load("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/2_Elections/elections.Rdata")
  elections <- elections %>% select(cve_secc, year, vspri_jd, vspt_jd, vspan_jd, vsprd_jd, vsmorena_jd) %>% filter(year == 2018) 
  # 2018 data is missing alcalde 
  load("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/1_Core/db_precinct.Rdata")
  db <- db %>% group_by(cve_secc, cve_alc) %>% tally() 
  elections <- left_join(elections,db[,c("cve_secc", "cve_alc")], by = "cve_secc" )
  # we want the 2018 vote share for the current incumbent party, ie, did they
  # bet on the right horse? This is true even in places where the party ditched
  # the current incumbent, because "core" or "swing" status would still be based
  # on that outcome. Note that the JD column in this db is retrospective, so we
  # need to bring in the new alcaldes. 

 
  elections$cve_alc <- as.character(elections$cve_alc)
  
  jd <- read_csv("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/2_Elections/jefes_delegacionales.csv") %>% 
    filter(year == 2018) %>% 
    select(DEL, PARTIDO_JD) %>% rename(party_alcalde_2018 = PARTIDO_JD, cve_alc = DEL)
  jd$cve_alc <- as.character(jd$cve_alc)
  jd$party_alcalde_2018 <- str_replace(jd$party_alcalde_2018, "P.R.D|PRD", "prd")
  jd$party_alcalde_2018 <- str_replace(jd$party_alcalde_2018, "MORENA", "morena")
  jd$party_alcalde_2018 <- str_replace(jd$party_alcalde_2018, "P.A.N|PAN", "pan")
  jd$party_alcalde_2018 <- str_replace(jd$party_alcalde_2018, "P.R.I|PRI", "pri")
  
  # we need a weighted average of vote shares by colonia
  colonias <- st_read("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/9_Administrative/coloniascdmx/coloniascdmx.shp")
  colonias <- st_set_crs(colonias, 4326)
  colonias$area <- st_area(colonias)
  secciones <- st_read("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/9_Administrative/Estadisticas_censales_a_escalas_geoelectorales/secciones.shp")
  secciones <- st_set_crs(secciones, 4326)
  secciones <- secciones %>% select(cve_secc)
  colonias_secciones_int <- st_intersection(colonias, secciones)
  colonias_secciones_int$area_intersection <- st_area(colonias_secciones_int)
  colonias_secciones_int$int_percent_area <- as.numeric(colonias_secciones_int$area_intersection/colonias_secciones_int$area)
  
  # again, get rid of the trivially overlapping secciones (goal here is to make it look like the "official" report of which precincts are contained)
  colonias_secciones_int <- colonias_secciones_int %>% filter(colonias_secciones_int$int_percent_area >.05)
  colonias_secciones_int$cve_alc <- as.character(colonias_secciones_int$cve_alc)
  colonias_secciones_int <- left_join(colonias_secciones_int, elections, by = c("cve_secc", "cve_alc"))
  
  colonias_vs <- colonias_secciones_int %>%
    group_by(cve_col, cve_alc) %>%
    summarize(vspri_jd= weighted.mean(x = vspri_jd, w = int_percent_area),
              vspan_jd= weighted.mean(x = vspan_jd, w = int_percent_area),
              vsprd_jd= weighted.mean(x = vsprd_jd, w = int_percent_area),
              vsmorena_jd= weighted.mean(x = vsmorena_jd, w = int_percent_area)
              )
  
  colonias_vs <- left_join(colonias_vs, jd, by = c("cve_alc"))
  
  colonias_vs <-as.tibble(colonias_vs)
  vote_shares_jd <- colonias_vs %>% 
    select(cve_alc, cve_col, vspri_jd, vspan_jd, vsprd_jd, vsmorena_jd) %>%
    pivot_longer(cols = c(vspri_jd, vspan_jd, vsprd_jd, vsmorena_jd),names_to = "party", names_prefix = "vs", values_to = "vote_share")
  vote_shares_jd$party <- str_replace(vote_shares_jd$party, pattern = "_jd",replacement= "")
  
  vote_shares_jd <- vote_shares_jd %>%
    group_by(cve_col) %>%
    arrange(desc(vote_share)) %>% 
    mutate(rank = row_number())
  
  margins <- vote_shares_jd %>%  pivot_wider(id_cols = c("cve_col", "cve_alc"), 
                                       names_from = rank, 
                                       names_prefix = "votes_for",
                                       values_from = vote_share)
  margins$margin <- margins$votes_for1 - margins$votes_for2
  
  parties <- vote_shares_jd %>%  pivot_wider(id_cols = c("cve_col", "cve_alc"), 
                                             names_from = rank, 
                                             names_prefix = "party",
                                             values_from = party)
   vote_outcomes <- left_join(margins[c("cve_col", "cve_alc", "votes_for1", "votes_for2", "margin")], parties[,c("cve_col", "cve_alc", "party1", "party2")], by = c("cve_col", "cve_alc"))
  vote_outcomes <- left_join(vote_outcomes, jd, by = "cve_alc")
  
  # standard deviation of vote share is 15 points
  # if the neighborhood's first most supported party was the party that won, and the margin was more than 15 pts, call it core
  # if the neighborhood's first most supported party was not the party that won, and the margin was more than 15 pts, call it opposition
  # if the margin between 1 and 2 is less than 15 pts, call it swing 
  
  vote_outcomes$type <- NA
  vote_outcomes$type <- ifelse(vote_outcomes$party1 == vote_outcomes$party_alcalde_2018 & vote_outcomes$margin >.1, "core", vote_outcomes$type)
  vote_outcomes$type <- ifelse(vote_outcomes$party1 != vote_outcomes$party_alcalde_2018 & vote_outcomes$margin >.1,"opposition", vote_outcomes$type)
  vote_outcomes$type <- ifelse(vote_outcomes$margin <.1, "swing", vote_outcomes$type)
  # what to do about this? Basically no opposition strongholds left- may be hard
  # to get places that vary in the permutations across other vars
  
  
  

  

  
  

  vote_shares_jd <- vote_shares_jd %>% filter(party_alcalde_2018 == party)
  vote_shares_jd <- vote_shares_jd %>% rename(ivs_alc = vote_share)
  ggplot(vote_shares_jd) + geom_histogram(aes(x = ivs_alc)) + theme_classic() +
    labs(title = "Distribution of Incumbent Vote Share")
  
  # UNIDADES HABITACIONALES 
  # read in data
  uh_data <- c()
  for(i in c("ALVARO OBREGON", "AZCAPOTZALCO", "BJ", "COYOACAN", "CUAJIMALPA", "CUAUHTEMOC", "GAM", "Iztacalco", "Iztapalapa", "MH", "TLAHUAC", "TLALPAN", "VC","XOCHIMILCO")){
    alc <- read_excel(path = "/Users/alyssahuberts/Dropbox/1_Dissertation/6_Background/Unidades Habitacionales/Universe of UH's 2010/data.xlsx", 
                      sheet = i, col_names = FALSE)
    colnames(alc) <- c("alcaldia", "uh", "ut", "viviendas", "poblacion", "year", "direccion")
    uh_data <- bind_rows(uh_data, alc)
  }
  
  uh_data$full_address_name <- paste(uh_data$uh, uh_data$direccion, uh_data$alcaldia, "Ciudad México", sep = ", ")
  unique_addresses <- unique(uh_data$full_address_name)
  register_google(key = Sys.getenv("ggmap_key"))
  #coordinates <- geocode(unique_addresses)
  # filter to within mexico city bounding box 
  #coordinates$in_box <-ifelse((coordinates$lon >= -99.364924 &
  #   coordinates$lon <= -98.940303 & 
  #   coordinates$lat >= 19.048237 &
  #   coordinates$lat <= 19.592757),1,0)
  # coordinates$lat <- ifelse(coordinates$in_box ==1,coordinates$lat, NA)
  # coordinates$lon <- ifelse(coordinates$in_box ==1,coordinates$lon, NA)
  # write_csv(coordinates, path = "/Users/alyssahuberts/Dropbox/1_Dissertation/6_Background/Unidades Habitacionales/universe_uh_coords_complete.csv")
  # note that on the back end I'm manually looking up the coordinates that I'm missing 
  
  coordinates <- read_csv("/Users/alyssahuberts/Dropbox/1_Dissertation/6_Background/Unidades Habitacionales/universe_uh_coords_complete.csv") %>% 
    select(lon, lat, full_address_name ) %>% filter(!is.na(lon))
  coordinates$in_box <-ifelse((coordinates$lon >= -99.364924 &
                                 coordinates$lon <= -98.940303 & 
                                 coordinates$lat >= 19.048237 &
                                 coordinates$lat <= 19.592757),1,0)
  coordinates <- coordinates[coordinates$in_box==1,]
  
  uh_data <- left_join(uh_data, coordinates, by = "full_address_name")
  uh_data$lon <-as.numeric(uh_data$lon)
  uh_data$lat <-as.numeric(uh_data$lat)
  uh_data <- uh_data[!is.na(uh_data$lat),]
  uh_sf <- uh_data %>%
    mutate_at(vars(lon, lat), as.numeric) %>%   # coordinates must be numeric
    st_as_sf(
      coords = c("lon", "lat"),
      agr = "constant",
      crs = 4326,        # nad83 / new york long island projection
      stringsAsFactors = FALSE,
      remove = TRUE
    )
  
  
  colonias <- read_sf("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/9_Administrative/coloniascdmx/coloniascdmx.shp")
  alcaldias <- read_sf("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/9_Administrative/alcaldias/alcaldias.shp")
  
  pdf("/Users/alyssahuberts/Dropbox/1_Dissertation/8_Survey/4_plots/uh.pdf")
  ggplot() + geom_sf(data = colonias)+ 
    geom_point(data = uh_data, aes(x= lon, y = lat)) + theme_classic() +
    labs(title = "Unidades Habitacionales")
  dev.off()
  
  # Calculate values by colonia for whether or not there is a UH in the colonia 
  # start by joining the data (https://mattherman.info/blog/point-in-poly/)
  uh_in_colonia <- st_join(uh_sf, colonias, join = st_within)
  # count uhs in colonia
  uh_colonia_count <- count(as_tibble(uh_in_colonia), cve_col)
  colnames(uh_colonia_count) <- c("cve_col", "n_uh")
  colonias <- left_join(colonias, uh_colonia_count, by = "cve_col")
  colonias$n_uh <- ifelse(is.na(colonias$n_uh), 0, colonias$n_uh)
  colonias$has_uh <- ifelse(colonias$n_uh>0,1,0)
  colonias %>% group_by(alcaldia) %>% summarize(sum(has_uh))
  colonias_uh <- colonias[,c("cve_col", "has_uh")]
  save(colonias_uh, file = "/Users/alyssahuberts/Dropbox/1_Dissertation/6_Background/Unidades Habitacionales/colonias_uh.Rdata")
  
  # now bring in population data of those uhs 
  uh_pop <- as.data.frame(uh_in_colonia) %>% group_by(cve_col) %>% summarize(pop_in_uhs = sum(poblacion))
  colonias <- left_join(colonias, uh_pop, by = "cve_col")
  colonias$pop_in_uhs <- ifelse(is.na(colonias$pop_in_uhs), 0, colonias$pop_in_uhs)
  # ultimately, we'll want to make this a share. I'm not doing this now because
  # i don't have census data easily available at colonia level (will just
  # require adding up agebs or manzanas, shouldn't be hard)
  
  #colonia populations 
  colonias_pop <- colonias_ageb %>% 
    group_by(cve_col) %>% 
    summarize(POBTOT = sum(x = as.numeric(POBTOT)/as.numeric(int_perc_colonia_area)))
  colonias_pop <- left_join(colonias_pop, uh_pop, by = "cve_col")
  colonias_pop$pop_in_uhs <-ifelse(is.na(colonias_pop$pop_in_uhs), 0, colonias_pop$pop_in_uhs)
  colonias_pop$share_pop_in_uhs <- colonias_pop$pop_in_uhs/colonias_pop$POBTOT
  colonias <- read_csv("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/9_Administrative/coloniascdmx/coloniascdmx.csv")
  colonias_pop <- left_join(colonias_pop, colonias, by = "cve_col")
  # somehow, the aggregating by ageb doesn't seem to get at the most accurate population measures for the UH's.
  colonias_pop$uhab_present <- ifelse(colonias_pop$share_pop_in_uhs >0, 1,0)
  pdf("/Users/alyssahuberts/Dropbox/1_Dissertation/8_Survey/4_plots/uh.pdf")
  ggplot(colonias_pop) + geom_sf(aes(fill = factor(uhab_present)))+
    theme_classic()  + scale_fill_brewer(palette = "Set2")+
    labs(title = "Colonias with at least one Unidad Habitacional")
  dev.off()
  
  
  ##############
  # MODULOS #
  ##############
  modulos <- read_csv("/Users/alyssahuberts/Dropbox/1_Dissertation/6_Background/CongresoCDMXReports/Modulos_geocoded.csv")
  # read in puntos de acuerdo info and merge to the modulos data 
  puntos_acuerdo <- read_excel("/Users/alyssahuberts/Dropbox/1_Dissertation/6_Background/CongresoCDMXReports/punto_acuerdo_abbreviated.xlsx")
  modulos <- left_join(modulos, puntos_acuerdo, by = "name")
  modulos$percent_shaming <- modulos$`2019_alcalde_tags`/modulos$`2019_total`
  modulos_sf <- modulos %>%
    filter(!is.na(lon)& !is.na(lat)) %>% 
    mutate_at(vars(lon, lat), as.numeric) %>%   # coordinates must be numeric
    st_as_sf(
      coords = c("lon", "lat"),
      agr = "constant",
      crs = 4326,        
      stringsAsFactors = FALSE,
      remove = TRUE
    )
  alcaldias_shp <- read_sf("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/9_Administrative/alcaldias/alcaldias.shp")
  modulos_sf <- st_join(modulos_sf, alcaldias_shp)
  alcaldes <- read_csv("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/2_Elections/jefes_delegacionales.csv") %>% filter(year == 2018)
  alcaldes$cve_mun <- str_pad(alcaldes$DEL, 3, side = "left", pad = "0")
  modulos_sf <- left_join(modulos_sf, alcaldes, by = "cve_mun")
  modulos_sf <-left_join(modulos_sf, modulos[,c("name", "lat", "lon")], by = "name")
  modulos_sf$unaligned <- ifelse(modulos_sf$party == modulos_sf$PARTIDO_JD,"aligned","unaligned")
  modulos_sf$unaligned <- factor(modulos_sf$unaligned, levels = c("aligned", "unaligned"))
  modulos_sf$contentious <- ifelse(modulos_sf$percent_shaming > mean(modulos_sf$percent_shaming, na.rm=TRUE),"Contentious","Not Contentious")
  modulos_sf$contentious <- ifelse(is.na(modulos_sf$contentious), "Not Contentious",modulos_sf$contentious)
  modulos_sf$contentious <- factor(modulos_sf$contentious, levels = c("Not Contentious", "Contentious"))
  
  pdf("/Users/alyssahuberts/Dropbox/1_Dissertation/8_Survey/4_plots/modulos_1.pdf")
  ggplot(data = alcaldias_shp) + geom_sf() +
    geom_point(data = modulos_sf, aes(x = lon, y = lat, color = factor(unaligned))) +
    coord_sf(crs = 4326) +
    theme_classic()+
    labs(title = "Modulos de Atencion \n For Aligned/Unaligned Diputados",
         color = "Diputado is") +
    theme(legend.position = "bottom")
  dev.off()
  
  pdf("/Users/alyssahuberts/Dropbox/1_Dissertation/8_Survey/4_plots/modulos_2.pdf")
  ggplot(data = alcaldias_shp) + geom_sf() +
    geom_point(data = modulos_sf, aes(x = lon, y = lat, color = factor(contentious))) +
    coord_sf(crs = 4326) +
    theme_classic()+
    labs(title = "Modulos de Atencion \n For Contentious* Diputados",
         color = "Diputado is", 
         caption="*Above average percent of filed puntos de acuerdo shame alcaldes", 
        bottom = textGrob("grid caption", x = 1, hjust = 1, gp = gpar(fontface = 3L, fontsize = 9)))+
    theme(legend.position = "bottom")
  dev.off()
  