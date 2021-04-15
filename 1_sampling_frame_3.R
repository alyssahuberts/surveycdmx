################
# Sampling frame 3
# Date created: 4/1/2021
# Date last edited: 4/15/2021
################
# Seccion level sampling frame 
library(tidyverse)
library(sf)
library(lubridate)
library(modeest)
library(factoextra)
library(magrittr)
library(janitor)


# read in shapefile of secciones
  secciones_shp <- read_sf("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/9_Administrative/Estadisticas_censales_a_escalas_geoelectorales/secciones.shp") %>% 
    select(CLAVEGEO, ENTIDAD, DISTRITO, MUNICIPIO, SECCION,POBTOT) %>% clean_names()
  secciones_shp$seccion_area <- st_area(secciones_shp)
  
  secciones <- read_sf("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/9_Administrative/Estadisticas_censales_a_escalas_geoelectorales/secciones.shp") %>% 
    clean_names() %>% 
    select(clavegeo, entidad, distrito, municipio, seccion, pobtot) %>% st_drop_geometry()

# Which secciones contain a UH?
  load("/Users/alyssahuberts/Dropbox/1_Dissertation/6_Background/Unidades Habitacionales/data/uh_data.Rdata")
  # convert the UH to a shapefile 
  uh_data_shp <- st_as_sf(uh_data[!is.na(uh_data$lat),], coords = c("lon", "lat"),crs = 4326)
  uh_data_shp <- st_join(uh_data_shp,secciones_shp)
  
  # tally the UH's in an seccion
  secciones_uh <- uh_data_shp %>% 
    group_by(clavegeo) %>%
    summarize(n_uh = n()) %>% 
    st_drop_geometry()
  secciones <- left_join(secciones, secciones_uh, by = "clavegeo")
  secciones$n_uh <-ifelse(is.na(secciones$n_uh), 0, secciones$n_uh)
  secciones$uh_in_seccion <-ifelse(secciones$n_uh>0, 1,0)
  
  # past protests taking place in seccion
  load("/Users/alyssahuberts/Dropbox/1_Dissertation/6_Background/Protests:Blockades/data/protest_events_with_coords.Rdata")
  events <- events %>% filter(!is.na(lon))
  events_shp <- st_as_sf(events, coords = c("lon", "lat"), crs = 4326)
  seccion_protests <- st_join(events_shp, secciones_shp)
  seccion_protests$year <- year(seccion_protests$date)
  seccion_protests <- seccion_protests %>% 
    st_drop_geometry %>% 
    group_by(clavegeo, year) %>% 
    tally()
  seccion_protests <- seccion_protests %>%
    pivot_wider(id_cols = clavegeo, 
                names_from = year, 
                names_prefix = "protests_",
                values_from = n)
  seccion_protests[, 2:10][is.na(seccion_protests[, 2:10])] <- 0
  seccion_protests$protests_2013_2020 <- rowSums(seccion_protests[,c("protests_2013", "protests_2014", "protests_2015", "protests_2016",
                                                               "protests_2017", "protests_2018", "protests_2019", "protests_2020")])
  
  seccion_protests$protests_2013_2017 <- rowSums(seccion_protests[,c("protests_2013", "protests_2014", "protests_2015", "protests_2016",
                                                               "protests_2017")])
  seccion_protests <- seccion_protests %>% select(clavegeo, protests_2013_2017, protests_2013_2020) %>% ungroup
  secciones <- left_join(secciones, seccion_protests, by = "clavegeo")
  secciones[, c("protests_2013_2017", "protests_2013_2020")][is.na(secciones[,  c("protests_2013_2017", "protests_2013_2020")])] <- 0
  secciones$protest_yes_no <- ifelse(secciones$protests_2013_2017 >0, 1,0)
  
  # bring in traffic stats and merge
  load("/Users/alyssahuberts/Dropbox/1_Dissertation/6_Background/Protests:Blockades/traffic/jobs_2219528_results_Mexico_City_Traffic.shapefile/traffic_stats_by_seccion.Rdata")
  secciones$cve_secc <- str_pad(secciones$seccion, 4, "left", "0")
  secciones <- left_join(secciones, traffic_maxes, by = "cve_secc")
  
  # add in census level data. Note that as this data is collected at the
  # manzana/ageb level, we're taking weighted averages across overlapping
  # territories
  
  # Start by loading in all AGEB level variables
  # read in the shapefile at the ageb level. Note that we're only using the urban agebs, which I think is ok
  ageb_shp <- st_read("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/4_Demographics/scince_2010/shps/df/df_ageb_urb.shp") %>% select(CVEGEO)
  ageb_shp <- st_set_crs(ageb_shp, 4326)
  ageb_shp$CVEGEO <- as.character(ageb_shp$CVEGEO)
  
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
  # give the census code the same structure as the CVEGEO in the shapefile 
  # For urban areas (the areas I have here) the CVEGEO can be broken into: 
  # Urban Areas
  # EE+MMM+LLLL+AAA-A+NNN
  # EE: State (2 digits)
  # MMM: Municipality (3 digits)
  # LLLL: Locality (four digits)
  # AAA-A: AGEB (3 digits, dash, 1 digit) 
  # NNN: Manzana/block (3 digits) 
  
  census_ageb$CVEGEO <- paste("09", census_ageb$MUN, census_ageb$LOC, census_ageb$AGEB, sep = "")
  
  
  # Convert census data to percents
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
  rm(census_percents_ageb_complete, assets)
  census_ageb <- census_ageb %>% select(CVEGEO, AGEB, MUN, NOM_MUN, LOC, NOM_LOC, POBTOT, TVIVHAB, p_VPH_TINACO, p_VPH_CISTER, pca_1) %>% 
    clean_names()
  
  # now we need to do spatial weighted averages of these values at the seccion level
  # read in ageb shapefile 
  ageb_shp <- st_read("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/4_Demographics/scince_2010/shps/df/df_ageb_urb.shp")
  ageb_shp <- st_set_crs(ageb_shp, 4326)
  
  # for every ageb, identify which seccion it intersects and the area for each (so I can do weighted averages)
  ageb_seccion <- st_intersection(ageb_shp[c("CVEGEO", "OID", "geometry")], secciones_shp[,c("clavegeo", "seccion_area")])
  ageb_seccion$int_area <- st_area(ageb_seccion)
  ageb_seccion$int_perc_seccion_area <- as.numeric(ageb_seccion$int_area)/as.numeric(ageb_seccion$seccion_area)
 ageb_seccion <- ageb_seccion  %>%   rename(clavegeo_seccion =clavegeo, cvegeo_ageb =CVEGEO)
  # confirm that these sum to 1
  x <- ageb_seccion %>% group_by(clavegeo_seccion) %>% summarize(sum_area = sum(int_perc_seccion_area))
  # take weighted averages of census variables by seccion 
  census_ageb <- census_ageb %>% rename(cvegeo_ageb = cvegeo)
  ageb_seccion <- left_join(ageb_seccion, census_ageb, by = "cvegeo_ageb")
  ageb_seccion <- ageb_seccion %>% st_drop_geometry()
  seccion_level_census <- ageb_seccion %>% group_by(clavegeo_seccion) %>% 
    summarize(p_vph_tinaco = weighted.mean(p_vph_tinaco, w = int_perc_seccion_area,na.rm=TRUE),
              p_vph_cister = weighted.mean(p_vph_cister, w = int_perc_seccion_area, na.rm=TRUE),
              pca_1 = weighted.mean(pca_1, w = int_perc_seccion_area, na.rm=TRUE)) 
  seccion_level_census <- seccion_level_census %>% rename(clavegeo = clavegeo_seccion)

  # join census data to seccion level dataframe
  secciones <- left_join(secciones, seccion_level_census, by = "clavegeo")
  
  
  # Now join in election data (from 0_elections_preprocessing_seccion)
  load("/Users/alyssahuberts/Dropbox/1_Dissertation/8_Survey/8_Data/vote_shares_15_18.Rdata")
  secciones$cve_secc <- str_pad(secciones$seccion, 4, "left", "0")
  secciones <- left_join(secciones, elections, by = "cve_secc")
  # note that 5501 seems to be removed from the map in 2015 and 2018, so we'll eliminate it
  secciones <- secciones[secciones$cve_secc!= "5501",]
  
  
  # read in distance to alcaldias
  d2alc <- read_csv("/Users/alyssahuberts/Dropbox/1_Dissertation/6_Background/dist_to_alcaldia.csv")
  # join in the seccion level information
  d2alc <- d2alc %>% rename(clavegeo= InputID, cve_alc = TargetID, dist_to_alcaldia = Distance)
  d2alc <-left_join(d2alc, secciones[,c("clavegeo", "entidad", "distrito", "municipio", "seccion")], by = "clavegeo")
  d2alc <- d2alc %>% filter(municipio == cve_alc)
  secciones <- left_join(secciones, d2alc[,c("clavegeo", "dist_to_alcaldia")], by = "clavegeo")
  
  sample_secciones <- secciones %>% filter( municipio == 5|
                                            municipio == 6|
                                            municipio == 11|
                                            municipio == 7|
                                            municipio == 17)
  
  
  # create IVS
  sample_secciones$ivs_2018 <- ifelse(sample_secciones$cve_alc==17,
                                      sample_secciones$vsprd_jd_2018,
                                      sample_secciones$vsmorena_jd_2018)
  
  # what percentile are the different cutpoints at? 
  get_percentile <- ecdf(sample_secciones$ivs_2018)
  get_percentile(.36)
  get_percentile(.46)
  
  get_percentile_dist <- ecdf(sample_secciones$dist_to_alcaldia)
  get_percentile_dist(1500)
  
  # get the 25th percentile traffic
  sample_secciones$max_traffic <- ifelse(sample_secciones$max_traffic==-Inf, 0, sample_secciones$max_traffic)
  sample_secciones$max_traffic <- ifelse(is.na(sample_secciones$max_traffic), 0, sample_secciones$max_traffic)
  
  quantile(sample_secciones$max_traffic, probs = c(.75), na.rm=TRUE)
  
  sample_secciones$megaphone <- ifelse(sample_secciones$max_traffic>2799|
                                       sample_secciones$uh_in_seccion ==1|
                                       sample_secciones$dist_to_alcaldia <1500,1,0)
  
 sample_secciones$voteshare <- ifelse(sample_secciones$ivs_2018 <.36, 1, NA)
 sample_secciones$voteshare <- ifelse(sample_secciones$ivs_2018 >=.36& sample_secciones$ivs_2018 <.46, 2, sample_secciones$voteshare)
 sample_secciones$voteshare <- ifelse(sample_secciones$ivs_2018 >=.46, 3, sample_secciones$voteshare)
 
 sample_secciones$block <-paste("Votes = ", sample_secciones$voteshare,", Megaphone = ", sample_secciones$megaphone,sep ="")
 
  map <-left_join(secciones_shp[(secciones_shp$municipio == 5|
                                secciones_shp$municipio == 6|
                                secciones_shp$municipio ==7|
                                secciones_shp$municipio ==11|
                                  secciones_shp$municipio == 17),], sample_secciones, by = "seccion")
  ggplot(map)+ geom_sf(aes(fill = factor(megaphone)))
  
  pdf(file = "/Users/alyssahuberts/Dropbox/1_Dissertation/8_Survey/4_plots/blocks.pdf")
  ggplot(map) + geom_sf(aes(fill = factor(block))) +theme_classic()
  dev.off()
  
  
  seccion_totals <- sample_secciones %>% group_by(cve_alc, block) %>% tally() %>% 
    pivot_wider(values_from = n, names_from=cve_alc)
  
  
  seccion_totals_2 <- sample_secciones %>% group_by( block) %>% tally()
  seccion_totals_3 <- sample_secciones %>% group_by( block) %>% summarise(sum(pobtot))
  
 

  save(sample_secciones, file = "/Users/alyssahuberts/Dropbox/1_Dissertation/8_Survey/8_Data/sampling_frame.Rdata")
  
  
