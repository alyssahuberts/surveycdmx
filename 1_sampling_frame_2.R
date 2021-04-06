################
# Sampling frame 2
# Date created: 3/19/2021
# Date last edited: 3/19/2021
################
library(modeest)
# AGEB level sampling frame: 

# Start by loading in all AGEB level variables
    # read in the shapefile at the ageb level. Note that we're only using the urban agebs, which I think is ok
    ageb_shp <- st_read("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/4_Demographics/scince_2010/shps/df/df_ageb_urb.shp") %>% select(CVEGEO)
    ageb_shp <- st_set_crs(ageb_shp, 4326)
    ageb_shp$CVEGEO <- as.character(ageb_shp$CVEGEO)
    
    # 2020 ageb - note that this database contains both manzanas and agebs but we'll stick to ageb
    # from https://en.www.inegi.org.mx/programas/ccpv/2020/#Microdata
    census <- read_csv("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/4_Demographics/2020_census_ageb.csv",
                       col_types = cols_only(
                         MUN = col_character(), 
                         NOM_MUN = col_character(),
                         LOC = col_character(),
                         NOM_LOC = col_character(), 
                         AGEB = col_character(),
                         POBTOT = col_character(),
                         PROM_OCUP = col_character(),
                         TVIVHAB = col_character(),
                         TVIVPAR = col_character(), 
                         VIVPAR_HAB = col_character(), 
                         VPH_AEASP= col_character(), 
                         VPH_TINACO = col_character(),
                         VPH_CISTER = col_character(),
                         VPH_REFRI =col_character(),
                         VPH_LAVAD = col_character(),
                         VPH_AUTOM = col_character(),
                         VPH_MOTO = col_character(),
                         VPH_BICI = col_character(),
                         VPH_RADIO = col_character(),
                         VPH_TV = col_character(),
                         VPH_PC = col_character(),
                         VPH_TELEF = col_character(),
                         VPH_CEL = col_character(),
                         VPH_INTER = col_character()
                       )
       )
      # drop totals and manzana level data
      census_ageb <- census[census$NOM_LOC == "Total AGEB urbana",] %>% 
        select( MUN, NOM_MUN, LOC, NOM_LOC,AGEB, POBTOT, PROM_OCUP, TVIVHAB, TVIVPAR, VIVPAR_HAB,
                VPH_AEASP, VPH_TINACO, VPH_CISTER,VPH_REFRI,
                VPH_LAVAD,VPH_AUTOM, VPH_MOTO, VPH_BICI, 
                VPH_RADIO, VPH_TV, VPH_PC, VPH_TELEF, VPH_CEL,
                VPH_INTER)
      rm(census)
      # give the census code the same structure as the CVEGEO in the shapefile 
      # For urban areas (the areas I have here) the CVEGEO can be broken into: 
      # Urban Areas
      # EE+MMM+LLLL+AAA-A+NNN
      # EE: State (2 digits)
      # MMM: Municipality (3 digits)
      # LLLL: Locality (four digits)
      # AAA-A: AGEB (3 digits, dash, 1 digit) 
      # NNN: Manzana/block (3 digits) 
      
      census_ageb$CVEGEO <- paste("09", census_ageb$MUN, census_ageb$LOC, census_ageb$AGEB, sep = "")
      
      
      # Convert census data to percents
      census_ageb[,7:24] <- lapply(census_ageb[,7:24],as.numeric)
      percents <- census_ageb[,9:24]
      colnames(percents) <- paste("p_", colnames(percents), sep = "")
      census_ageb <- cbind(census_ageb, percents)
      rm(percents)
      census_ageb[,-(1:25)] %<>% sapply(`/`, census_ageb[,8])

      # create wealth index using pca and asset variables 
      census_percents_ageb_complete <- na.omit(census_ageb[,c(30:40)]) 
      assets <- prcomp(census_percents_ageb_complete, center = TRUE,scale. = TRUE)
      census_percents_ageb_complete <- na.omit(census_ageb[,c(1:5, 30:40)]) 
      census_percents_ageb_complete$pca_1 <-  get_pca_ind(assets)$coord[,1]
      census_ageb <- left_join(census_ageb, census_percents_ageb_complete[,c("MUN", "NOM_MUN", "LOC", "AGEB","pca_1")], by = c("MUN", "NOM_MUN", "AGEB", "LOC"))
      rm(census_percents_ageb_complete, assets)
      census_ageb <- census_ageb %>% select(CVEGEO, AGEB, MUN, NOM_MUN, LOC, NOM_LOC, POBTOT, TVIVHAB, p_VPH_TINACO, p_VPH_CISTER, pca_1)
      
      
      # Identify whether or not the AGEB contains a Unidad Habitacional 
      load("/Users/alyssahuberts/Dropbox/1_Dissertation/6_Background/Unidades Habitacionales/data/uh_data.Rdata")
      
      # convert the UH to a shapefile 
      uh_data_shp <- st_as_sf(uh_data, coords = c("lon", "lat"),crs = 4326)
      ageb_uh <- st_join(uh_data_shp,ageb_shp)
      # tally the UH's in an AGEB
      # note that the population will potentially be funky here if a UH is split across multiple agebs
      ageb_uh <- ageb_uh %>% 
        group_by(CVEGEO) %>%
        summarize(n_uh = n(),
        total_uh_pop = sum(poblacion, na.rm=TRUE)) %>% 
        st_drop_geometry()
      ageb <- left_join(census_ageb, ageb_uh, by = "CVEGEO")
      ageb$n_uh <-ifelse(is.na(ageb$n_uh), 0, ageb$n_uh)
      ageb$total_uh_pop <-ifelse(is.na(ageb$total_uh_pop), 0, ageb$total_uh_pop)
      ageb$uh_in_ageb <-ifelse(ageb$n_uh>0, 1,0)
      
      # past protests taking place in ageb
      load("/Users/alyssahuberts/Dropbox/1_Dissertation/6_Background/Protests:Blockades/data/protest_events_with_coords.Rdata")
      events <- events %>% filter(!is.na(lon))
      events_shp <- st_as_sf(events, coords = c("lon", "lat"), crs = 4326)
      ageb_protests <- st_join(events_shp, ageb_shp)
      ageb_protests$year <- year(ageb_protests$date)
      ageb_protests <- ageb_protests %>% 
        st_drop_geometry %>% 
        group_by(CVEGEO, year) %>% 
        tally()
      ageb_protests <- ageb_protests %>%
        pivot_wider(id_cols = CVEGEO, 
                    names_from = year, 
                    names_prefix = "protests_",
                    values_from = n)
       ageb_protests[, 2:10][is.na(ageb_protests[, 2:10])] <- 0
      ageb_protests$protests_2013_2020 <- rowSums(ageb_protests[,c("protests_2013", "protests_2014", "protests_2015", "protests_2016",
                                                                "protests_2017", "protests_2018", "protests_2019", "protests_2020")])
      
      ageb_protests$protests_2013_2017 <- rowSums(ageb_protests[,c("protests_2013", "protests_2014", "protests_2015", "protests_2016",
                                                                   "protests_2017")])
      ageb_protests <- ageb_protests %>% select(CVEGEO, protests_2013_2017, protests_2013_2020) %>% ungroup
      ageb <- left_join(ageb, ageb_protests, by = "CVEGEO")
      ageb[, c("protests_2013_2017", "protests_2013_2020")][is.na(ageb[,  c("protests_2013_2017", "protests_2013_2020")])] <- 0
      
      table(ageb[ageb$NOM_MUN=="Gustavo A. Madero"|
                   ageb$NOM_MUN == "Iztacalco"|
                   ageb$NOM_MUN == "Iztapalapa"|
                   ageb$NOM_MUN=="Tláhuac"|
                   ageb$NOM_MUN== "Venustiano Carranza",]$uh_in_ageb)
      table(uh_data$alcaldia)
      
      
      # Now load in the colonia level data. Weight by land area
        colonias <- read_csv("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/9_Administrative/coloniascdmx/coloniascdmx.csv")
        # rationing status
        tandeo <- read_csv("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/3_Tandeo/Florencio_FOIA/tandeo_clean.csv")
        colonias_tandeo <- left_join(colonias, tandeo, by = "cve_col")
        colonias_tandeo[((colonias_tandeo$alcaldia == "BENITO JUAREZ"|
                colonias_tandeo$alcaldia == "IZTACALCO"|
                colonias_tandeo$alcaldia == "VENUSTIANO CARRANZA") & is.na(colonias_tandeo$rationing_status)),"rationing_status"] <- "Unknown"
        colonias_tandeo[is.na(colonias_tandeo$rationing_status),"rationing_status"] <- "Presumed continuous"
        colonias_tandeo[colonias_tandeo$rationing_status == "Presumed continuous","hours_week_normal"] <- 168
        colonias_tandeo[colonias_tandeo$rationing_status == "Presumed continuous","hours_week_estiaje"] <- 168
        
        # Twitter complaints 
        # read in Twitter complaints (note that this is an outcome to quasi-test the first stage on, not to block or stratify on)
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
        # join on rationing status
        colonias_shp <- left_join(colonias_shp, colonias_tandeo[,c("cve_col", "hours_week_normal", "hours_week_estiaje", "rationing_status")])
        
        ageb_shp <- st_read("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/4_Demographics/scince_2010/shps/df/df_ageb_urb.shp")
        ageb_shp <- st_set_crs(ageb_shp, 4326)
        ageb_shp$ageb_area <- st_area(ageb_shp)
        
        # what percentage of the ageb is each colonia?
        colonias_ageb <- st_intersection(colonias_shp,ageb_shp[c("CVEGEO", "OID", "geometry", "ageb_area")])
        colonias_ageb$int_area <- st_area(colonias_ageb)
        colonias_ageb$int_perc_ageb_area <- as.numeric(colonias_ageb$int_area)/as.numeric(colonias_ageb$ageb_area)
        
        # summarize # complaints at the ageb level
        colonias_ageb$rationing_status <- as.factor(colonias_ageb$rationing_status)
        mode <- function(codes){
          which.max(tabulate(codes))
        }
        ageb_wtd <- colonias_ageb %>% group_by(CVEGEO) %>% summarize(twitter_complaints = weighted.mean(x =num_tweets, w = int_perc_ageb_area),
                                                         hours_week_estiaje = weighted.mean(x = hours_week_estiaje, w = int_perc_ageb_area),
                                                         hours_week_normal = weighted.mean(x= hours_week_normal, w = int_perc_ageb_area),
                                                         rationing_status  = mode(rationing_status))
    
        ageb_wtd <- st_drop_geometry(ageb_wtd)
  
      # ELECTIONS DATA (SECCION LEVEL)
        
      # merge in the colonia level and seccion level data 
      ageb <- left_join(ageb, vote_outcomes)
      ageb <- left_join(ageb, ageb_wtd)

      sample_agebs <- ageb %>% filter(NOM_MUN == "Gustavo A. Madero"|
                                      NOM_MUN == "Iztacalco"|
                                      NOM_MUN == "Iztapalapa"|
                                      NOM_MUN == "Tláhuac"|
                                      NOM_MUN == "Venustiano Carranza")
      sample_agebs$POBTOT <- as.numeric(sample_agebs$POBTOT)
      sample_agebs_sum1 <- sample_agebs %>% 
        group_by(NOM_MUN) %>% 
        summarize(n_agebs = n(),
                  total_pop = sum(POBTOT),
                  avg_pop_per_ageb = mean(POBTOT, na.rm=TRUE),
                  avg_tinaco = mean(p_VPH_TINACO, na.rm=TRUE),
                  avg_cistern = mean(p_VPH_CISTER, na.rm=TRUE),
                  mean_wealth_index = mean(pca_1, na.rm=TRUE),
                  mean_twitter_complaints_per_ageb = mean(twitter_complaints, na.rm=TRUE),
                  max_twitter_complaints_per_ageb = max(twitter_complaints, na.rm=TRUE),
                  total_twitter_complaints = sum(twitter_complaints, na.rm=TRUE),
                  mean_protests = mean(protests_2013_2017, na.rm=TRUE),
                  max_protests = max(protests_2013_2017, na.rm=TRUE),
                  total_protests = sum(protests_2013_2017, na.rm=TRUE))
      
      sample_agebs_sum2 <- sample_agebs %>% 
        group_by(NOM_MUN, rationing_status) %>% 
        tally()
      sample_agebs_sum3 <- sample_agebs %>% 
        group_by(NOM_MUN, type) %>% 
        tally()
      
      sample_agebs_sum4 <- sample_agebs %>% 
        group_by(NOM_MUN) %>% 
        tally(mean(hours_week_estiaje, na.rm=TRUE))
      
      # similuate sampling 200 AGEBS 1000 times, plot # of unidades habitacionales 
      
      n_uhs <- c()
      n_opp <- c()
      n_core <- c()
      n_swing <- c()
      for(i in 1:50000){
        sample <- sample_n(sample_agebs,200, replace = FALSE)
        n_uhs[i] <-sum(sample$uh_in_ageb)
        n_opp[i] <- length(sample[sample$type == "opposition", "CVEGEO"])
        n_core[i] <- length(sample[sample$type == "core", "CVEGEO"])
        n_swing[i] <- length(sample[sample$type == "swing", "CVEGEO"])
      }
    pdf(file = "/Users/alyssahuberts/Dropbox/1_Dissertation/8_Survey/4_plots/dist_uhs_random_sample.pdf" )
    ggplot()+ geom_histogram(data = as.data.frame(n_uhs),aes(x = n_uhs)) + theme_classic() + geom_vline(aes(xintercept = mean(n_uhs)), color = "red")
    dev.off()