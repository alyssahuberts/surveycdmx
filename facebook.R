################
# Facebook
# Date created: April 23, 2021
# Date last update April 29, 2021
################
library(ggmap)
library(tidyverse)
# load centroids of secciones electorales 
seccion_centroids <- read_csv("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/9_Administrative/Estadisticas_censales_a_escalas_geoelectorales/secciones_centroids.csv") %>% 
  select(X,Y, CLAVEGEO, DISTRITO, MUNICIPIO, SECCION)
# geocode the centroids 
register_google(key = Sys.getenv("ggmap_key"))


# start with a baby sample 
seccion_centroids <- sample_n(seccion_centroids,100)
seccion_centroids_crd <- list()
for (i in 1:dim(seccion_centroids)[1]) {
  lon <- seccion_centroids$X[i]
  lat <- seccion_centroids$Y[i]
  seccion_centroids_crd[[i]] <- c(lon, lat)
}

# reverse geocode the coordinates and save them to the list
seccion_centroids_address <- list()
for (i in 1:length(seccion_centroids_crd)) {
  seccion <- seccion_centroids$SECCION[i]
  crd <- seccion_centroids_crd[[i]]
  address <- revgeocode(location = crd, output = "address")
  seccion_centroids_address[[i]] <- list(seccion, crd, address)
}
seccion_centroids_address[[1]][3]

# now put the data in a format facebook can use (cleanest to do semicolon separated)
addresses <- c()
for(i in 1:length(seccion_centroids_address)){
address <- seccion_centroids_address[[i]][3]
addresses[i] <- address}
addresses <- unlist(addresses)
write.table(addresses, file = "/Users/alyssahuberts/Dropbox/1_Dissertation/8_Survey/10_Facebook/seccion_centroid_addresses_sample_100.txt", sep = ";", row.names = FALSE)


