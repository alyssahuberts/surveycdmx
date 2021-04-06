# Traffic data
# created 3/22/2021 based on report generated from Tom Tom
library(foreign)
library(janitor)
traffic_files_hrly <- list.files("/Users/alyssahuberts/Dropbox/1_Dissertation/6_Background/Protests:Blockades/traffic/jobs_2219528_results_Mexico_City_Traffic.shapefile/dbfs")
for(i in 1:length(traffic_files_hrly)){
  x <- read.dbf(paste("/Users/alyssahuberts/Dropbox/1_Dissertation/6_Background/Protests:Blockades/traffic/jobs_2219528_results_Mexico_City_Traffic.shapefile/dbfs/", traffic_files_hrly[i], sep = ""))
  filename = paste("traffic_", substr(traffic_files_hrly[i], 12,nchar(traffic_files_hrly[i])-4), sep = "")
  colnames(x) <- c("id","hits")
  x$time <- substr(traffic_files_hrly[i], 12,nchar(traffic_files_hrly[i])-4)
  assign(filename,x, env= .GlobalEnv)
  rm(x)
}
traffic <- bind_rows(`traffic_7_00-8_00`, `traffic_8_00-9_00`, `traffic_9_00-10_00`, `traffic_10_00-11_00`, 
          `traffic_11_00-12_00`, `traffic_11_00-12_00`, `traffic_12_00-13_00`,
          `traffic_13_00-14_00`, `traffic_14_00-15_00`, `traffic_15_00-16_00`, 
          `traffic_16_00-17_00`, `traffic_17_00-18_00`,`traffic_18_00-19_00`, `traffic_19_00-20_00`)

network <-read.dbf("/Users/alyssahuberts/Dropbox/1_Dissertation/6_Background/Protests:Blockades/traffic/jobs_2219528_results_Mexico_City_Traffic.shapefile/network.dbf") %>% clean_names()
traffic_stats <- traffic %>% 
  group_by(id) %>% 
  summarize(mean_hits = mean(hits),
            max_hits = max(hits),
            total_hits = sum(hits))
traffic_stats$max_hits_cat <- case_when(
  traffic_stats$max_hits <1000 ~ 0,
  traffic_stats$max_hits >1000 & traffic_stats$max_hits <2000 ~1,
  traffic_stats$max_hits >3000 & traffic_stats$max_hits <4000 ~2,
  traffic_stats$max_hits >4000 & traffic_stats$max_hits <5000 ~3,
  traffic_stats$max_hits >=5000~4)

network_stats <- left_join(network, traffic_stats, by = "id")

road_type <- network_stats %>% group_by(frc) %>% summarize(mean(max_hits, na.rm=TRUE), 
                                              mean(mean_hits, na.rm=TRUE))

write_csv(traffic_stats, path = "/Users/alyssahuberts/Dropbox/1_Dissertation/6_Background/Protests:Blockades/traffic/jobs_2219528_results_Mexico_City_Traffic.shapefile/traffic_stats.csv")
traffic <- read_csv("/Users/alyssahuberts/Dropbox/1_Dissertation/6_Background/Protests:Blockades/traffic/jobs_2219528_results_Mexico_City_Traffic.shapefile/traffic_stats.csv")


# bring in segments with seccion
segments_secciones <- read_csv("/Users/alyssahuberts/Dropbox/1_Dissertation/6_Background/Protests:Blockades/traffic/jobs_2219528_results_Mexico_City_Traffic.shapefile/traffic_with_seccion.csv")
segments_secciones <- segments_secciones[,c(1:7, 173)]
segments_secciones <- segments_secciones %>% rename(id = Id)
segments_secciones <- left_join(segments_secciones,traffic_stats, by = "id")

# for each segment, come up with the max hits
traffic_maxes <- segments_secciones %>% group_by(cve_secc) %>% summarize(max_traffic = max(max_hits, na.rm=TRUE)) 
save(traffic_maxes, file = "/Users/alyssahuberts/Dropbox/1_Dissertation/6_Background/Protests:Blockades/traffic/jobs_2219528_results_Mexico_City_Traffic.shapefile/traffic_stats_by_seccion.Rdata")

