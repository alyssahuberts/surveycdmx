################
# Power calculations and simulations
# Date created: 4/5/2021
# Date last edited: 4/5/2021
################
library(lfe)
library(stargazer)
library(tidyverse)
library(sf)
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
    sample_secciones$n_pipa_request <- ifelse(sample_secciones$municipio == 7 & is.na(sample_secciones$n_pipa_request), 0, sample_secciones$n_pipa_request)
    sample_secciones <- left_join(sample_secciones, tweets_seccion_wtd, by = "clavegeo")
    
# First just do difference in means 
    sample_secciones %>% group_by( uh_in_seccion) %>% summarize(mean(protests_2013_2017))
    sample_secciones %>% group_by(uh_in_seccion) %>% summarize(mean(twitter_complaints))
    sample_secciones %>% filter(municipio == 7) %>%  group_by(uh_in_seccion) %>% summarize(mean(n_pipa_request))
    
# NO CONTROLS    
# regress the three outcomes on having a UH 
    u1 <- felm(protests_2013_2020 ~ uh_in_seccion |municipio|0|0, data = sample_secciones)
    u2 <- felm(twitter_complaints ~ uh_in_seccion|municipio|0|0, data = sample_secciones)
    u3 <- felm(n_pipa_request ~ uh_in_seccion|0|0|0, data = sample_secciones[sample_secciones$municipio==7,])
    
    # regress the three outcomes on max traffic
    sample_secciones$max_traffic <-ifelse(is.na(sample_secciones$max_traffic) | sample_secciones$max_traffic=="-Inf",0,sample_secciones$max_traffic)
    t1 <- felm(protests_2013_2020 ~ max_traffic|municipio|0|0, data = sample_secciones)
    t2 <- felm(twitter_complaints ~ max_traffic|municipio|0|0, data = sample_secciones)
    t3 <- felm(n_pipa_request ~ max_traffic|0|0|0, data = sample_secciones[sample_secciones$municipio==7,])
  
    # regress the three outcomes on distance to alcaldia
    d1 <- felm(protests_2013_2020 ~ dist_to_alcaldia|municipio|0|0, data = sample_secciones)
    d2 <- felm(twitter_complaints ~ dist_to_alcaldia|municipio|0|0, data = sample_secciones)
    d3 <- felm(n_pipa_request ~ dist_to_alcaldia|0|0|0, data = sample_secciones[sample_secciones$municipio==7,])
    
    all_1 <-felm(protests_2013_2020 ~ uh_in_seccion + max_traffic + dist_to_alcaldia|municipio|0|0, data = sample_secciones)
    all_2 <- felm(twitter_complaints ~ uh_in_seccion + max_traffic + dist_to_alcaldia|municipio|0|0, data = sample_secciones)
    all_3 <-felm(n_pipa_request ~ uh_in_seccion + max_traffic + dist_to_alcaldia|0|0|0, data = sample_secciones[sample_secciones$municipio==7,])
    
    stargazer(u1,t1,d1,all_1, type = "text")
    stargazer(u2,t2,d2, all_2, type = "text")
    stargazer(u3,t3,d3, all_3, type = "text")

    sample_secciones$traffic_2500 <- ifelse(sample_secciones$max_traffic>2500,1,0)
    sample_secciones$dist_2500 <- ifelse(sample_secciones$dist_to_alcaldia <2500,1,0)
    #CONTROLS FOR VOTE SHARE, POPULATION AND INCOME    
    # regress the three outcomes on having a UH 
    u1 <- felm(protests_2013_2020 ~ uh_in_seccion + pca_1 + vsmorena_jd_2018 + pobtot |municipio|0|0, data = sample_secciones)
    u2 <- felm(twitter_complaints ~ uh_in_seccion+ pca_1 + vsmorena_jd_2018 + pobtot|municipio|0|0, data = sample_secciones)
    u3 <- felm(n_pipa_request ~ uh_in_seccion|municipio|0|0, data = sample_secciones[sample_secciones$municipio==7,])
    
    # regress the three outcomes on max traffic
    t1 <- felm(protests_2013_2020 ~ max_traffic+ pca_1 + vsmorena_jd_2018 + pobtot|municipio|0|0, data = sample_secciones)
    t2 <- felm(twitter_complaints ~ max_traffic+ pca_1 + vsmorena_jd_2018 + pobtot|municipio|0|0, data = sample_secciones)
    t3 <- felm(n_pipa_request ~ max_traffic+ pca_1 + vsmorena_jd_2018 + pobtot|municipio|0|0, data = sample_secciones[sample_secciones$municipio==7,])
    
    # regress the three outcomes on distance to alcaldia
    d1 <- felm(protests_2013_2020 ~ dist_to_alcaldia+ pca_1 + vsmorena_jd_2018 + pobtot|municipio|0|0, data = sample_secciones)
    d2 <- felm(twitter_complaints ~ dist_to_alcaldia+ pca_1 + vsmorena_jd_2018 + pobtot|municipio|0|0, data = sample_secciones)
    d3 <- felm(n_pipa_request ~ dist_to_alcaldia+ pca_1 + vsmorena_jd_2018 + pobtot|municipio|0|0, data = sample_secciones[sample_secciones$municipio==7,])
    
    all_1 <-felm(protests_2013_2020 ~ uh_in_seccion + max_traffic + dist_to_alcaldia+ pca_1 + vsmorena_jd_2018 + pobtot|municipio|0|0, data = sample_secciones)
    all_2 <- felm(twitter_complaints ~ uh_in_seccion + max_traffic + dist_to_alcaldia+ pca_1 + vsmorena_jd_2018 + pobtot|municipio|0|0, data = sample_secciones)
    all_3 <-felm(n_pipa_request ~ uh_in_seccion + max_traffic + dist_to_alcaldia+ pca_1 + vsmorena_jd_2018 + pobtot|municipio|0|0, data = sample_secciones[sample_secciones$municipio==7,])
    
    
    all_3_bin <-felm(n_pipa_request ~ uh_in_seccion + traffic_2500 + dist_2500 + pca_1 + vsmorena_jd_2018 + pobtot|municipio|0|0, data = sample_secciones[sample_secciones$municipio==7,])
    sample_secciones$megaphone <- ifelse(sample_secciones$uh_in_seccion ==1|
                                         sample_secciones$traffic_2500 ==1|
                                         sample_secciones$dist_2500==1,1,0)
    
    megaphone_pipa <-felm(n_pipa_request ~ megaphone + pca_1 + vsmorena_jd_2018 + pobtot|municipio|0|0, data = sample_secciones[sample_secciones$municipio==7,])
    megaphone_protest <-felm(protests_2013_2020 ~ megaphone + pca_1 + vsmorena_jd_2018 + pobtot|municipio|0|0, data = sample_secciones[sample_secciones$municipio==7,])
    
    
    stargazer(u1,t1,d1,all_1, type = "text", title = "Protests (With Controls)")
    stargazer(u2,t2,d2, all_2, type = "text", title = "Twitter (With Controls)")
    stargazer(u3,t3,d3, all_3, type = "text", title = "Pipas (With Controls)")  
    
    
# pull a random sample of secciones 1000 times and count the number of secciones that have a UH in them 
  n_uh <- c()
  for(i in 1:10000){
    x <- sample_n(sample_secciones, 250)
    y <- x %>% filter(uh_in_seccion ==1) %>% tally() %>% pull()
    n_uh[i] <- y
  }
  
  mean(n_uh) # 8.5
  
