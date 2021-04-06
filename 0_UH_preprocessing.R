################
# Geocoding UH's
# Date created: 3/22/21
# Date last edited: 4/6/21
################
library(tidyverse)
library(readxl)
library(ggmap)
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
coordinates <- geocode(unique_addresses)
# filter to within mexico city bounding box 
coordinates$in_box <-ifelse((coordinates$lon >= -99.364924 &
   coordinates$lon <= -98.940303 & 
   coordinates$lat >= 19.048237 &
   coordinates$lat <= 19.592757),1,0)
 coordinates$lat <- ifelse(coordinates$in_box ==1,coordinates$lat, NA)
 coordinates$lon <- ifelse(coordinates$in_box ==1,coordinates$lon, NA)
 coords <- cbind(as.data.frame(unique_addresses), coordinates)
 colnames(coords) <- c("full_address_name", "lon", "lat", "in_box")
 coords <- left_join(uh_data, coords, by = "full_address_name")
 write_csv(coords, path = "/Users/alyssahuberts/Dropbox/1_Dissertation/6_Background/Unidades Habitacionales/universe_uh_coords_geocoded.csv")
# note that on the back end I'm manually looking up the coordinates that I'm missing 
 uh_coordinates <- read_csv( "/Users/alyssahuberts/Dropbox/1_Dissertation/6_Background/Unidades Habitacionales/universe_uh_coords_geocoded_ajh.csv")
  uh_data <- cbind(uh_data, uh_coordinates[,c("lat", "lon")])


uh_data$lon <-as.numeric(uh_data$lon)
uh_data$lat <-as.numeric(uh_data$lat)
#uh_data <- uh_data[!is.na(uh_data$lat),]
uh_data$in_box <- NULL
save(uh_data, file = "/Users/alyssahuberts/Dropbox/1_Dissertation/6_Background/Unidades Habitacionales/data/uh_data.Rdata")
