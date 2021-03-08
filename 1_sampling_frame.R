################
# Sampling frame 
# Date created: 3/8/2021
# Date last edited: 3/8/2021
################
library(tidyverse)
library(sf)

# read in data 
  # 2020 manzana level census data
  # from https://en.www.inegi.org.mx/programas/ccpv/2020/#Microdata
  census <- read_csv("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/4_Demographics/2020_census_ageb.csv")
  # filter to the variables I think I'll want to keep:
    # MUN, NOM_MUN, LOC, NOM_LOC, AGEB, MZA, POBTOT, PNACENT (People born in the same entidad)
  
  # read in the shapefile at the manzana level. Note that we're only using the urban agebs, which I think is ok
  mza_shp <- read_sf("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/4_Demographics/scince_2010/shps/df/df_manzanas.shp")
  
  # read in the shapefile at the ageb level. Note that we're only using the urban agebs, which I think is ok
  ageb_shp <- read_sf("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/4_Demographics/scince_2010/shps/df/df_ageb_urb.shp")

