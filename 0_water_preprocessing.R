####################
# Preprocessing water quality data 
# Created 3/22/21
####################
# read in colonia level rationing data
tandeo <- read.csv("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/3_Tandeo/Florencio_FOIA/programa_tandeo_csv.csv")
colnames(tandeo) <- c( "alcaldia", "colonia", "dias_tandeo", "horas_tandeo", "status", "cve_col")
days_week <- read_csv("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/3_Tandeo/Florencio_FOIA/days_week.csv") 
colnames(days_week) <- c("dias_tandeo", "days_estiaje", "days_normal")
hours_day <- read_csv("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/3_Tandeo/Florencio_FOIA/hours_day.csv")
colnames(hours_day) <- c("horas_tandeo", "hours")
tandeo$horas_tandeo <- trimws(tandeo$horas_tandeo)
tandeo$dias_tandeo <- trimws(tandeo$dias_tandeo)
tandeo <- left_join(tandeo, days_week, by = "dias_tandeo")
tandeo <- left_join(tandeo, hours_day, by = "horas_tandeo")
tandeo$hours_week_estiaje <- tandeo$days_estiaje*tandeo$hours
tandeo$hours_week_normal <- tandeo$days_normal*tandeo$hours

# some colonias are duplicated because of how they're listed in the register
# (like if they're partial secciones, etc). I could go back and get the exact
# manzanas, but for now I'm just taking the average
tandeo_hours_col <- tandeo %>% 
  group_by(cve_col) %>% 
  summarize(hours_week_normal = mean(hours_week_normal, na.rm=TRUE),
            hours_week_estiaje = mean(hours_week_estiaje, na.rm=TRUE))

# what about status?
tandeo_status_col <- tandeo %>% 
  group_by(cve_col) %>% 
  summarize(status_list = paste(status, collapse = ""))
tandeo_status_col$status <- NA
tandeo_status_col[str_detect(tandeo_status_col$status_list, "Tandeo")==TRUE &str_detect(tandeo_status_col$status_list, "Condonado")==TRUE,]$status <- "Rationed/condoned"
tandeo_status_col[str_detect(tandeo_status_col$status_list, "Tandeo")==TRUE &str_detect(tandeo_status_col$status_list, "Condonado")==FALSE,]$status <- "Rationed"
tandeo_status_col[str_detect(tandeo_status_col$status_list, "Tandeo")==FALSE &str_detect(tandeo_status_col$status_list, "Condonado")==TRUE,]$status <- "Condoned"

tandeo_out <- left_join(tandeo_hours_col,tandeo_status_col[,c("cve_col", "status")], by = "cve_col" ) %>% rename(rationing_status = status)
write_csv(tandeo_out, path = "/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/3_Tandeo/Florencio_FOIA/tandeo_clean.csv")
