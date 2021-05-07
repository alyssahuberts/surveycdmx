################
# Facebook
# Date created: April 23, 2021
# Date last update April 23, 2021
################
library(ggmap)
# load centroids of secciones electorales 
seccion_centroids <- read_csv("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/9_Administrative/Estadisticas_censales_a_escalas_geoelectorales/secciones_centroids.csv") %>% 
  select(X,Y, CLAVEGEO, DISTRITO, MUNICIPIO, SECCION)
# geocode the centroids 
register_google(key = Sys.getenv("ggmap_key"))
seccion_centroids$coords <- as.numeric(paste(seccion_centroids$Y,seccion_centroids$X))
revgeocode(seccion_centroids$X[1], seccion_centroids$Y[1])