library(tidyverse)
library(ggmap)
library(jsonlite)
library(ggmap)
library(sf)
library(lubridate)
library(stargazer)

    # read in the jsons scraped from python.Because this is a lot of files, only keep the ones that mention blockades
    files_list <- list.files("/Users/alyssahuberts/Dropbox/1_research_general/Methods-Tools/Python/twitter_api/twitter_queries/c5/")
    bloqueo_tweets <- c()
    for(i in files_list){
      file <- paste("/Users/alyssahuberts/Dropbox/1_research_general/Methods-Tools/Python/twitter_api/twitter_queries/c5/",i,sep = "")
      result <- jsonlite::fromJSON(file, simplifyDataFrame = TRUE)
      mydf <- result$data 
      bloqueos <- mydf[str_detect(mydf$text, "bloqueo|Bloqueo")==TRUE,]
      bloqueo_tweets <- bind_rows(bloqueo_tweets, bloqueos)
    }

    # separate out the location info so we can geocode it
    bloqueo_tweets$text <- tolower(bloqueo_tweets$text)
    bloqueo_tweets$location <-NA
    bloqueo_tweets$location <- ifelse(str_detect(bloqueo_tweets$text, "sobre") == TRUE, str_split_fixed(bloqueo_tweets$text, "sobre",2)[,2], bloqueo_tweets$text)
    bloqueo_tweets$location <- ifelse(str_detect(bloqueo_tweets$text, "#") == TRUE, str_split_fixed(bloqueo_tweets$text, "#",2)[,2], bloqueo_tweets$location)
    bloqueo_tweets$location <- ifelse(str_detect(bloqueo_tweets$text, "en") == TRUE, str_split_fixed(bloqueo_tweets$text, "en",2)[,2], bloqueo_tweets$location)
    bloqueo_tweets$location <-str_replace(bloqueo_tweets$location, "@tualcaldiagam", "Gustavo A Madero, Ciudad México")
    bloqueo_tweets$location <-str_replace(bloqueo_tweets$location, "@alccuauhtemocmx", "Cuauhtémoc, Ciudad México")
    bloqueo_tweets$location <-str_replace(bloqueo_tweets$location, "@alcaldiaao", "Álvaro Obregón, Ciudad México")
    bloqueo_tweets$location <-str_replace(bloqueo_tweets$location, "@alcaldia_coy", "Benito Juárez, Ciudad México")
    bloqueo_tweets$location <-str_replace(bloqueo_tweets$location, "@alc_iztapalapa", "Iztapalapa, Ciudad México")
    bloqueo_tweets$location <-str_replace(bloqueo_tweets$location, "@azcapotzalcomx", "Azcapotzalco, Ciudad México")
    bloqueo_tweets$location <-str_replace(bloqueo_tweets$location, "@alccuajimalpa", "Cuajimalpa, Ciudad México")
    bloqueo_tweets$location <-str_replace(bloqueo_tweets$location, "@iztacalcoal", "Cuajimalpa, Ciudad México")
    bloqueo_tweets$location <-str_replace(bloqueo_tweets$location, "@alamagdalenac", "Magdalena Contreras, Ciudad México")
    bloqueo_tweets$location <-str_replace(bloqueo_tweets$location, "@alcaldiamhmx", "Miguel Hidalgo, Ciudad México")
    bloqueo_tweets$location <-str_replace(bloqueo_tweets$location, "@gobmilpaalta", "Milpa Alta, Ciudad México")
    bloqueo_tweets$location <-str_replace(bloqueo_tweets$location, "@alc_tlahuac", "Tláhuac, Ciudad México")
    bloqueo_tweets$location <-str_replace(bloqueo_tweets$location, "@alcaldiatlalpan", "Tlalpan, Ciudad México")
    bloqueo_tweets$location <-str_replace(bloqueo_tweets$location, "@a_vcarranza", "Venustiano Carranza, Ciudad México")
    bloqueo_tweets$location <-str_replace(bloqueo_tweets$location, "@xochimilcoAl", "Xochimilco, Ciudad México")
    bloqueo_tweets$location <-str_replace(bloqueo_tweets$location, "@bjalcaldia", "Benito Juárez, Ciudad México")

    bloqueo_tweets$location <- ifelse(str_detect(bloqueo_tweets$location, "alternativa")==TRUE, 
                                      str_split_fixed(bloqueo_tweets$location, "alternativa",2)[,1], 
                                      bloqueo_tweets$location)
    
    bloqueo_tweets$location <- ifelse(str_detect(bloqueo_tweets$location, "como")==TRUE, 
                                      str_split_fixed(bloqueo_tweets$location, "como",2)[,1], 
                                      bloqueo_tweets$location)
    
    bloqueo_tweets$location <- ifelse(str_detect(bloqueo_tweets$location, "utili")==TRUE, 
                                      str_split_fixed(bloqueo_tweets$location, "utili",2)[,1], 
                                      bloqueo_tweets$location)
    
    bloqueo_tweets$location <- ifelse(str_detect(bloqueo_tweets$location, "toma")==TRUE, 
                                      str_split_fixed(bloqueo_tweets$location, "toma",2)[,1], 
                                      bloqueo_tweets$location)
    save(bloqueo_tweets, file = "/Users/alyssahuberts/Dropbox/1_Dissertation/6_Background/Protests:Blockades/bloqueo_tweets.Rdata")
    locations <- unique(bloqueo_tweets$location)
    save(locations, file = "/Users/alyssahuberts/Dropbox/1_Dissertation/6_Background/Protests:Blockades/blockade_locations.Rdata")
    # ^ This saves a database of all the unique tweet locations we find in the dataset
    
    # geocode locations
    load("/Users/alyssahuberts/Dropbox/1_Dissertation/6_Background/Protests:Blockades/blockade_locations.Rdata")
    load("/Users/alyssahuberts/Dropbox/1_Dissertation/6_Background/Protests:Blockades/bloqueo_tweets.Rdata")
    # google maps credentials
    register_google(key = Sys.getenv("ggmap_key"))
    #coordinates <- geocode(locations)
    #save(coordinates, file = "/Users/alyssahuberts/Dropbox/1_Dissertation/6_Background/Protests:Blockades/blockade_coordinates.Rdata")

    # load in the geocoded coordinates and reduce the sampel to Mexico City bounding box
    load("/Users/alyssahuberts/Dropbox/1_Dissertation/6_Background/Protests:Blockades/blockade_coordinates.Rdata")
    x <- as.data.frame(locations) 
    x <- cbind(x, coordinates)
    x <- x %>% rename(location = locations)
    
    # filter to within mexico city bounding box 
    x$in_box <-ifelse((x$lon >= -99.364924 &
                               x$lon <= -98.940303 & 
                               x$lat >= 19.048237 &
                               x$lat <= 19.592757),1,0)
    x <- x[x$in_box==1,]
    # join back coordinates to tweets
    bloqueo_tweets <- left_join(bloqueo_tweets, x, by = "location")

    # reduce tweets to one location/day 
    bloqueo_tweets$date <- as.Date(bloqueo_tweets$created_at)
    events <- bloqueo_tweets %>% group_by(location, date) %>% slice(1)
    # This generates a set of unique "events" which are a combination of a location and a date
    save(events, file = "/Users/alyssahuberts/Dropbox/1_Dissertation/6_Background/Protests:Blockades/data/protest_events_with_coords.Rdata")
 
    
    # The stuff that comes after here is mostly mapping/me trying to figure out how to use the roads. Kind of a mess currently 
    
    
    
    
    
#load in colonias
colonias <- read_sf("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/9_Administrative/coloniascdmx/coloniascdmx.shp")
alcaldias <- read_sf("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/9_Administrative/alcaldias/alcaldias.shp")
# load in roads 
ejes <- read_sf("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/4_Demographics/scince_2010/shps/df/df_eje_vial.shp")

events$year <- year(events$date)
# get a feel for it 
pdf(file = "/Users/alyssahuberts/Dropbox/1_Dissertation/8_Survey/4_plots/bloqueos.pdf")
ggplot(ejes[ejes$TIPOVIAL == "AUTOPISTA"|
            ejes$TIPOVIAL == "AVENIDA"|
            ejes$TIPOVIAL == "CARRETERA"|
            ejes$TIPOVIAL == "CIRCUITO"|
              ejes$TIPOVIAL == "DIAGONAL"|
              ejes$TIPOVIAL == "EJE VIAL"|          
            ejes$TIPOVIAL == "BOULEVARD"|
            ejes$TIPOVIAL == "PERIFÉRICO"|
            ejes$TIPOVIAL == "VIADUCTO",]) +
  geom_sf(lwd = .3, color = "blue", fill = NA) +
  geom_sf(data = ejes[ejes$TIPOVIAL != "AUTOPISTA"&
                ejes$TIPOVIAL != "AVENIDA"&
                ejes$TIPOVIAL != "CARRETERA"&
                ejes$TIPOVIAL != "DIAGONAL"&
                ejes$TIPOVIAL != "CIRCUITO" &
                  ejes$TIPOVIAL != "EJE VIAL" &
                ejes$TIPOVIAL != "BOULEVARD"&
                ejes$TIPOVIAL != "PERIFÉRICO"&
                ejes$TIPOVIAL != "VIADUCTO",], lwd = .05, color = "blue", fill = NA)+
  geom_sf(data = alcaldias, lwd=.05, color = "red", fill = NA)+
 theme_classic()+
  geom_point(data = events, aes(x= lon, y = lat), size = .5,alpha = .3) + labs(title = "Blockade Locations")  
dev.off()

ggplot(ejes) +
  geom_sf(lwd = .3, color = "blue", fill = NA) +
  geom_sf(data = alcaldias, lwd=.05, color = "red", fill = NA)+
  theme_classic()+
  geom_point(data = events, aes(x= lon, y = lat), size = .5,alpha = .3) + labs(title = "Blockade Locations")  

# determine intersection between roads and pre-2018 protests
pre_18_blockades <- events %>% filter(!is.na(lat) & !is.na(lon)& year <2018) 
write_csv(pre_18_blockades, path = "/Users/alyssahuberts/Dropbox/1_Dissertation/6_Background/Protests:Blockades/pre_2018_blockades.csv")
st_crs(pre_18_blockades)=st_crs(ejes)

ejes_buffered <- st_buffer(ejes, dist =.002)
st_crs(ejes_buffered) = st_crs(pre_18_blockades)
pre_18_blockades_sf <- st_as_sf(pre_18_blockades, coords = c("lat", "lon"),crs = 4326)


# for every colonia, determine which types of roads it intersects 
colonias_shp <- read_sf("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/9_Administrative/coloniascdmx/coloniascdmx.shp")
st_crs(colonias_shp)= st_crs(ejes)
ejes_with_colonia <-st_join(ejes, colonias_shp)
types_of_road_in_colonia <- ejes_with_colonia %>%
  group_by(cve_col,TIPOVIAL ) %>% tally()
types_of_road_in_colonia <- st_drop_geometry(types_of_road_in_colonia)
colonias_with_road_types <- types_of_road_in_colonia %>% 
  select(cve_col, TIPOVIAL, n) %>% 
  pivot_wider(id_cols = cve_col, names_from = TIPOVIAL, values_from = n)
colonias_with_road_types[, 2:22][is.na(colonias_with_road_types[, 2:22])] <- 0

# past history of protest in this colonia
protests_with_colonia <- read_csv("/Users/alyssahuberts/Dropbox/1_Dissertation/6_Background/Protests:Blockades/protests_with_colonia.csv") %>% 
  st_as_sf(coords = c("lat", "lon"))

colonias_num_protests <- protests_with_colonia %>% 
  group_by(cve_col) %>% 
  tally()
colonias_num_protests <- left_join(colonias, colonias_num_protests)
colonias_num_protests$num_protests <- ifelse(is.na(colonias_num_protests$n), 0, colonias_num_protests$n)
colonias_num_protests$n <- NULL
colonias_num_protests <- colonias_num_protests %>% 
  select(cve_col, num_protests)

colonias_num_protests <- left_join(colonias_num_protests, colonias_with_road_types, by = "cve_col")
colonias_num_protests <- colonias_num_protests %>% 
  rename(CALLEJON=CALLEJÓN, PROLONGACION=PROLONGACIÓN, 
         PERIFERICO=PERIFÉRICO, AMPLIACION= AMPLIACIÓN,
         EJE = 'EJE VIAL')
m1 <- lm(num_protests ~ CERRADA + AVENIDA + CALLE + EJE+ PRIVADA + CIRCUITO +  CALLEJON + ANDADOR + CALZADA + PEATONAL + PROLONGACION + OTRO + RETORNO + BOULEVARD+ PERIFERICO + PASAJE + CARRETERA + AUTOPISTA + VIADUCTO + AMPLIACION + DIAGONAL, data = colonias_num_protests)
stargazer(m1, type = "text")

colonias_num_protests$cve_alc <- substr(colonias_num_protests$cve_col, 0,2)
colonias_num_protests$cve_mun <- str_pad(colonias_num_protests$cve_alc, 3, "left",0)

colonias_num_protests <- left_join(colonias_num_protests,alcaldias, by = "cve_mun" )
colonias_num_protests$has_blockade <-ifelse(colonias_num_protests$num_protests>0,1,0)
x <- colonias_num_protests %>% group_by(nomgeo, has_blockade) %>% tally()
# Different definitions of "blockadable" 
  # number of protests happening within the manzana/AGEB/colonia 
  # distance from manzana centroid to location of any protest
  # distance from manzana centroid to location where multiple protests have happened 


