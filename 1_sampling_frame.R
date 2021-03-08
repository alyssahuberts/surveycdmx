################
# Sampling frame 
# Date created: 3/8/2021
# Date last edited: 3/8/2021
################
library(tidyverse)
library(sf)
library(magrittr)

#######################
# read in data 
#######################  # 2020 manzana level census data
  # from https://en.www.inegi.org.mx/programas/ccpv/2020/#Microdata
  census <- read_csv("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/4_Demographics/2020_census_ageb.csv")
  census_ageb <- census[census$NOM_LOC == "Total AGEB urbana",]
  census_mza <- census[census$NOM_LOC != "Total AGEB urbana",]
  
  # read in the shapefile at the manzana level. Note that we're only using the urban agebs, which I think is ok
  mza_shp <- read_sf("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/4_Demographics/scince_2010/shps/df/df_manzanas.shp")
  
  # read in the shapefile at the ageb level. Note that we're only using the urban agebs, which I think is ok
  ageb_shp <- read_sf("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/4_Demographics/scince_2010/shps/df/df_ageb_urb.shp")

  
  #######################
  # clean census data 
  #######################
  # filter to the variables I think I'll want to keep:
  # MUN, NOM_MUN, LOC, NOM_LOC, AGEB, MZA, POBTOT, PNACENT (People born in the same entidad), PROM_OCUP (average number of occupants in a vivienda), 
  # TVIVHAB (total de viviendas  habitadas), TVIVPAR (total de vivendas particulares), VIVPAR_HAB (viviendas particulares habitadas) VPH_AGUADV (viviendas particulares habitadas with water inside vivienda)
  # VPH_AEASP (viviendas particulaares habitadas que disponen de agua entubada en el ambito de la vivienda y se abastecen del servicio publico del agua)
  # VPH_TINACO (viviendas particulares habitadas que disponen de tinaco), VPH_CISTER (viviendas particulares habitadas que disponen de cisterna o aljibe) 
  # Assets: VPH_REFRI, VPH_LAVAD,VPH_HMICRO, VPH_AUTOM, VPH_MOTO, VPH_BICI, VPH_RADIO, VPH_TV, VPH_PC, VPH_TELEF, VPH_CEL,
  # VPH_INTER)
  census_mza <- census_mza %>% select(MUN, NOM_MUN, LOC, NOM_LOC, AGEB, MZA, POBTOT, PNACENT, PROM_OCUP,TVIVHAB, TVIVPAR, VIVPAR_HAB,VPH_AGUADV,VPH_AEASP, VPH_TINACO,VPH_CISTER, 
                              VPH_REFRI, VPH_LAVAD, VPH_AUTOM, VPH_MOTO, VPH_BICI, VPH_RADIO, VPH_TV, VPH_PC, VPH_TELEF, VPH_CEL, VPH_INTER)
  census_percents_mza <- census_mza %>% select(POBTOT, PNACENT, PROM_OCUP,TVIVHAB, TVIVPAR, VIVPAR_HAB,VPH_AGUADV,VPH_AEASP, VPH_TINACO,VPH_CISTER, 
                              VPH_REFRI, VPH_LAVAD, VPH_AUTOM, VPH_MOTO, VPH_BICI, VPH_RADIO, VPH_TV, VPH_PC, VPH_TELEF, VPH_CEL, VPH_INTER) %>% 
    mutate_all(as.numeric)
  for(i in c("VPH_AEASP", "VPH_TINACO","VPH_CISTER", "VPH_REFRI", "VPH_LAVAD", "VPH_AUTOM", "VPH_MOTO", "VPH_BICI", "VPH_RADIO", "VPH_TV", "VPH_PC", "VPH_TELEF", "VPH_CEL", "VPH_INTER")){
    x <- as.data.frame(census_percents_mza[,i]/census_percents_mza[,"VIVPAR_HAB"]) 
      colnames(x)[1] <- paste("percent_", i, sep = "")
    census_percents_mza <- cbind(census_percents_mza, x)
  }
  census_mza <- cbind(census_mza, census_percents_mza)
  
  # create wealth index using pca and asset variables 
  #wealth index (PCA)
  for(i in c("VPH_RADIO", "VPH_TV", "VPH_REFRI", "VPH_LAVAD", "VPH_AUTOM", "VPH_PC", "VPH_TELEF", "VPH_CEL", "VPH_INTER")){
    new_name <- paste(i, "_percent", sep ="")
    percent <- census[,i]/census$VIVPAR_HAB
    assign(new_name, percent, envir = .GlobalEnv)
  }
  assets <- cbind(census[,c("cve_secc", "cve_alc")], VPH_AUTOM_percent, VPH_INTER_percent,VPH_LAVAD_percent, VPH_PC_percent, VPH_RADIO_percent, VPH_TELEF_percent, VPH_TV_percent, VPH_REFRI_percent, VPH_CEL_percent)
  rm(VPH_AUTOM_percent, VPH_INTER_percent,VPH_LAVAD_percent, VPH_PC_percent, VPH_RADIO_percent, VPH_TELEF_percent, VPH_TV_percent, VPH_REFRI_percent, VPH_CEL_percent)
  
  #remove the outliers where everyone is reported as having all the services or no one is reported having any of the services 
  assets <- assets[rowSums(assets[3:11])!=0,]
  assets <- assets[rowSums(assets[3:11])!=9,]
  assets <-na.omit(assets)
  wealth <- prcomp(assets[3:11], center = TRUE,scale. = FALSE)
  assets$comp_1 <- get_pca_ind(wealth)$coord[,1]
  wealth <- assets[,c("cve_secc", "cve_alc", "comp_1")]
  colnames(wealth) <- c("cve_secc", "cve_alc", "wealth_index_pca")
  census <- left_join(census, wealth, by = c("cve_secc","cve_alc" ))
  census <- census[,c( "cve_alc", "alcaldia","cve_secc", "non_migrant", "wealth_index_pca", "employment", "piped_water")]
  
  #merge the unidad data with the census data  
  het <- left_join(secciones, census, by = c("cve_secc", "cve_alc"))
  het$colonia <- NULL
  het$high_wealth <- ifelse(het$wealth_index_pca> median(het$wealth_index_pca, na.rm=TRUE),1,0)
  het$migrant <- ifelse(het$non_migrant> median(het$non_migrant, na.rm=TRUE),1,0)
  
  
  #generate some categorical variables for het effects
  
