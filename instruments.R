################
# ID strategies
# Date created: 3/9/2021
# Date last edited: 3/9/2021
################
library(tidyverse)
library(readxl)
library(ggmap)
library(sf)
library(rgdal)
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
    
    ggplot() + geom_sf(data = colonias)+ 
      geom_point(data = uh_data, aes(x= lon, y = lat)) + theme_classic() +
      labs(title = "Unidades Habitacionales")
  
  # Calculate values by colonia for whether or not there is a UH in the colonia 
    # start by joining the data (https://mattherman.info/blog/point-in-poly/)
    uh_in_colonia <- st_join(uh_sf, colonias, join = st_within)
    # count uhs in colonia
    uh_colonia_count <- count(as_tibble(uh_in_colonia), cve_col)
    colnames(uh_colonia_count) <- c("cve_col", "n_uh")
    colonias <- left_join(colonias, uh_colonia_count, by = "cve_col")
    colonias$n_uh <- ifelse(is.na(colonias$n_uh), 0, colonias$n_uh)
    
  # now bring in population data of those uhs 
   uh_pop <- as.data.frame(uh_in_colonia) %>% group_by(cve_col) %>% summarize(pop_in_uhs = sum(poblacion))
   colonias <- left_join(colonias, uh_pop, by = "cve_col")
   colonias$pop_in_uhs <- ifelse(is.na(colonias$pop_in_uhs), 0, colonias$pop_in_uhs)
   # ultimately, we'll want to make this a share. I'm not doing this now because
   # i don't have census data easily available at colonia level (will just
   # require adding up agebs or manzanas, shouldn't be hard)
   
  