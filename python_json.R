library(tidyverse)
library(ggmap)
library(jsonlite)
library(ggmap)
library(sf)

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

# google maps credentials
register_google(key = Sys.getenv("ggmap_key"))

# geocode locations
# first get rid of duplicates
locations <- unique(bloqueo_tweets$location)
save(locations, file = "/Users/alyssahuberts/Dropbox/1_Dissertation/6_Background/Protests:Blockades/blockade_locations_temp.Rdata")

#coordinates <- geocode(locations)
#save(coordinates, file = "/Users/alyssahuberts/Dropbox/1_Dissertation/6_Background/Protests:Blockades/blockade_coordinates.Rdata")
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


#load in colonias
colonias <- read_sf("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/9_Administrative/coloniascdmx/coloniascdmx.shp")
alcaldias <- read_sf("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/9_Administrative/alcaldias/alcaldias.shp")
# load in roads 
ejes <- read_sf("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/4_Demographics/scince_2010/shps/df/df_eje_vial.shp")

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
  geom_point(data = events, aes(x= lon, y = lat)) + labs(title = "Blockade Locations, 2018-2021")

