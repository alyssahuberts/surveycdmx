################
# Power calculations and simulations
# Date created: 4/5/2021
# Date last edited: 4/5/2021
################

load("/Users/alyssahuberts/Dropbox/1_Dissertation/8_Survey/8_Data/sampling_frame.Rdata")

sample_secciones %>% group_by(uh_in_seccion) %>% summarize(n=n(),
                                                           mean_pop = mean(pobtot,na.rm=TRUE))
sample_secciones %>% group_by(protest_yes_no) %>% summarize(n=n(),
                                                           mean_pop = mean(pobtot,na.rm=TRUE))

sample_secciones %>% group_by(protest_yes_no, uh_in_seccion) %>% tally()

# Get correlations of the three "predisposed to megaphone" factors with measures of demand-making 
  # read in Twitter data 
# Twitter complaints 
    load("/Users/alyssahuberts/Dropbox/2_mx_water/3_Twitter/2_Data/2_Use/dat3.Rdata")
    # note that this is using "groups" of colonias that share a name, taking the cve_col of the first observation
    colonia_groups <- read.csv("/Users/alyssahuberts/Dropbox/2_mx_water/3_Twitter/2_Data/2_Use/coloniascdmx_grouped_simplified.csv") %>% 
      group_by(alcaldia, colonia_group) %>%
      mutate(group_name= first(colonia_group)) %>%
      ungroup() 
    colonia_groups <- colonia_groups %>% 
      group_by(alcaldia,colonia_group) %>% 
      summarize(num_colonias =n(),
                cve_col =first(cve_col)) %>%
      ungroup() 
    
    # tweet totals by colonia groups. Don't filter retweets here because we want the megaphone (not the events) 
    col <- orig_tweets_located %>% 
      filter(!is.na(cve_col)) %>% 
      group_by(cve_alc, cve_col, colonia) %>% 
      tally() %>% 
      ungroup() %>% 
      rename(num_tweets=n) %>% 
      filter(cve_col!="17-072"&
               cve_col!="15-037"
      ) 
    
    # now project these values onto all colonias in the group (imperfect,
    # but the best we have. also likely proxies real life decently well)
    colonia_group_template <- read.csv("/Users/alyssahuberts/Dropbox/2_mx_water/3_Twitter/2_Data/2_Use/coloniascdmx_grouped_simplified.csv") %>% 
      mutate(colonia=tolower(colonia_group))
    tweet_totals_colonias <- left_join(colonia_group_template[,c("cve_col", "colonia")], col[,c("colonia", "num_tweets")], by = "colonia")
    tweet_totals_colonias$num_tweets <- ifelse(is.na(tweet_totals_colonias$num_tweets), 0, tweet_totals_colonias$num_tweets)
    
    # get colonia area
    colonias_shp <- st_read("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/9_Administrative/coloniascdmx/coloniascdmx.shp")
    colonias_shp <- st_set_crs(colonias_shp, 4326)
    colonias_shp$colonia_area <- st_area(colonias_shp)
    # join on tweet totals 
    colonias_shp <- left_join(colonias_shp, tweet_totals_colonias, by = "cve_col")
     
    secciones_shp <- st_read("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/9_Administrative/Estadisticas_censales_a_escalas_geoelectorales/secciones.shp")
    secciones_shp <- st_set_crs(secciones_shp, 4326)
    secciones_shp$seccion_area <- st_area(secciones_shp)
    
    # what percentage of the ageb is each colonia?
    colonias_seccion <- st_intersection(colonias_shp,secciones_shp[c("CLAVEGEO", "cve_secc", "seccion_area")])
    colonias_seccion$int_area <- st_area(colonias_seccion)
    colonias_seccion$int_perc_seccion_area <- as.numeric(colonias_seccion$int_area)/as.numeric(colonias_seccion$seccion_area)
    colonias_seccion <- st_drop_geometry(colonias_seccion)
    # check that these sum to 1
    x <- colonias_seccion %>% group_by(CLAVEGEO) %>% summarize(total_percent = sum(int_perc_seccion_area))
    
    # summarize # complaints at the ageb level
    tweets_seccion_wtd <- colonias_seccion %>% group_by(CLAVEGEO) %>% summarize(twitter_complaints = weighted.mean(x =num_tweets, w = int_perc_seccion_area)) %>% 
      rename(clavegeo = CLAVEGEO)
    
    # bring in iztapalapa pipas data. This data is in coords so you can actually do it properly by seccion
    load("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/11_SaidIztapalapa/pipas_clean.Rdata")
    pipas$in_box <-ifelse((pipas$lon >= -99.364924 &
                             pipas$lon <= -98.940303 & 
                             pipas$lat >= 19.048237 &
                             pipas$lat <= 19.592757),1,0)
    pipas$lat <- ifelse(pipas$in_box ==1,pipas$lat, NA)
    pipas$lon <- ifelse(pipas$in_box ==1,pipas$lon, NA)
    pipas <- pipas %>% filter(in_box == 1)
    pipas_shp <- st_as_sf(pipas[!is.na(pipas$lon),], coords = c("lon", "lat"))
    st_crs(pipas_shp) <- 4326
    pipas_shp <- st_join(pipas_shp, secciones_shp[,c("CLAVEGEO", "cve_secc")])
    pipas_final <- pipas_shp %>% st_drop_geometry() %>% rename(clavegeo = CLAVEGEO) %>% 
      group_by(clavegeo) %>% tally() %>% rename(n_pipa_request = n)

# do 1) presence of a UH, 2) traffic within seccion, 3) distance to alcaldia shape likelihood of claim-making?
    sample_secciones <- left_join(sample_secciones, pipas_final, by = "clavegeo") 
    sample_secciones <- left_join(sample_secciones, tweets_seccion_wtd, by = "clavegeo")
    
# First just do difference in means 
    sample_secciones %>% group_by(cve_alc, uh_in_seccion) %>% summarize(mean(protests_2013_2017))
    
    
    
# how different do places with UH's look from places without?
  sample_secciones %>%
    group_by(municipio, uh_in_seccion) %>% 
    summarize(mean(protests_2013_2017))



# pull a random sample of secciones 1000 times and count the number of secciones that have a UH in them 
  n_uh <- c()
  for(i in 1:10000){
    x <- sample_n(sample_secciones, 250)
    y <- x %>% filter(uh_in_seccion ==1) %>% tally() %>% pull()
    n_uh[i] <- y
  }
  
  mean(n_uh) # 8.5
  
