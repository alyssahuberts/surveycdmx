################
# Elections preprocessing ageb level
# Date created: 4/1/2021
# Date last edited: 4/1/2021
################
library(tidyverse)
load("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/2_Elections/elections.Rdata")
elections <- elections %>% select(cve_secc, year, vspri_jd, vspt_jd, vspan_jd, vsprd_jd, vsmorena_jd) %>% filter(year == 2018 |year == 2015) 
# 2018 data is missing alcaldia
load("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/1_Core/db_precinct.Rdata")
db <- db %>% group_by(cve_secc, cve_alc) %>% tally() 
elections <- left_join(elections,db[,c("cve_secc", "cve_alc")], by = "cve_secc" )
elections$cve_alc <- as.character(elections$cve_alc)

jd <- read_csv("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/2_Elections/jefes_delegacionales.csv") %>% 
  filter(year == 2018|year == 2015) %>% 
  select(DEL, PARTIDO_JD) %>% rename(party_alcalde_2018 = PARTIDO_JD, cve_alc = DEL)
jd$cve_alc <- as.character(jd$cve_alc)
jd$party_alcalde_2018 <- str_replace(jd$party_alcalde_2018, "P.R.D|PRD", "prd")
jd$party_alcalde_2018 <- str_replace(jd$party_alcalde_2018, "MORENA", "morena")
jd$party_alcalde_2018 <- str_replace(jd$party_alcalde_2018, "P.A.N|PAN", "pan")
jd$party_alcalde_2018 <- str_replace(jd$party_alcalde_2018, "P.R.I|PRI", "pri")
jd$cve_alc <- str_pad(jd$cve_alc,3,   "left","0")

# since we're now using 2015 and 2018, we have to pivot wider so that we have vote share 2015 and vote share 2018
elections <- elections %>% pivot_wider(id_cols = c("cve_secc", "cve_alc"),
                                       names_from = year,
                                       values_from = c("vspri_jd", "vspt_jd", "vspan_jd", "vsprd_jd", "vsmorena_jd"))



elections$cve_alc <- str_pad(elections$cve_alc,side= "left", 2, pad= "0")
save(elections, file = "/Users/alyssahuberts/Dropbox/1_Dissertation/8_Survey/8_Data/vote_shares_15_18.Rdata")

# above/below median vote share for incumbent in 2018
medians <- elections %>% group_by(cve_alc) %>% summarize(median_pan_15 = median(vspan_jd_2015, na.rm=TRUE),
                                                    median_pri_15 = median(vspri_jd_2015, na.rm=TRUE),
                                                    median_prd_15 = median(vsprd_jd_2015, na.rm=TRUE),
                                                    median_morena_15 = median(vsmorena_jd_2015, na.rm=TRUE),
                                                    median_pan_18 = median(vspan_jd_2018, na.rm=TRUE),
                                                    median_pri_18 = median(vspri_jd_2018, na.rm=TRUE),
                                                    median_prd_18 = median(vsprd_jd_2018, na.rm=TRUE),
                                                    median_morena_18 = median(vsmorena_jd_2018, na.rm=TRUE))

vote_shares_jd <- elections %>% 
  select(cve_secc, cve_alc, vspri_jd_2015, vspan_jd_2015, vsprd_jd_2015, vsmorena_jd_2015, vspri_jd_2018, vspan_jd_2018, vsprd_jd_2018, vsmorena_jd_2018) %>%
  pivot_longer(cols = c(vspri_jd_2015, vspan_jd_2015, vsprd_jd_2015, vsmorena_jd_2015, vspri_jd_2018, vspan_jd_2018, vsprd_jd_2018, vsmorena_jd_2018),names_to = "party_year", names_prefix = "vs", values_to = "vote_share")
vote_shares_jd$year <- substr(vote_shares_jd$party_year, nchar(vote_shares_jd$party_year)-3, nchar(vote_shares_jd$party_year))
vote_shares_jd$party_year <- str_replace(vote_shares_jd$party_year, pattern = "_jd_2015",replacement= "")
vote_shares_jd$party <- str_replace(vote_shares_jd$party_year, pattern = "_jd_2018",replacement= "")
vote_shares_jd$party_year <- NULL

vote_shares_jd <- vote_shares_jd %>%
  group_by(cve_secc, year) %>%
  arrange(desc(vote_share)) %>% 
  mutate(rank = row_number())

#rank of each party in each alcaldía
totals <- vote_shares_jd %>% group_by(year, party, rank,cve_alc) %>% tally()



margins <- vote_shares_jd %>%  pivot_wider(id_cols = c("CVEGEO", "cve_alc"), 
                                           names_from = rank, 
                                           names_prefix = "votes_for",
                                           values_from = vote_share)
margins$margin <- margins$votes_for1 - margins$votes_for2

parties <- vote_shares_jd %>%  pivot_wider(id_cols = c("CVEGEO", "cve_alc"), 
                                           names_from = rank, 
                                           names_prefix = "party",
                                           values_from = party)
vote_outcomes <- left_join(margins[c("CVEGEO", "cve_alc", "votes_for1", "votes_for2", "margin")], parties[,c("CVEGEO", "cve_alc", "party1", "party2")], by = c("CVEGEO", "cve_alc"))
vote_outcomes <- left_join(vote_outcomes, jd, by = "cve_alc")

# standard deviation of vote share is 15 points
# if the neighborhood's first most supported party was the party that won, and the margin was more than 15 pts, call it core
# if the neighborhood's first most supported party was not the party that won, and the margin was more than 15 pts, call it opposition
# if the margin between 1 and 2 is less than 15 pts, call it swing 

vote_outcomes$type <- NA
vote_outcomes$type <- ifelse(vote_outcomes$party1 == vote_outcomes$party_alcalde_2018 & vote_outcomes$margin >.1, "core", vote_outcomes$type)
vote_outcomes$type <- ifelse(vote_outcomes$party1 != vote_outcomes$party_alcalde_2018 & vote_outcomes$margin >.1,"opposition", vote_outcomes$type)
vote_outcomes$type <- ifelse(vote_outcomes$margin <.1, "swing", vote_outcomes$type)
# what to do about this? Basically no opposition strongholds left- may be hard
# to get places that vary in the permutations across other vars







#####################
load("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/2_Elections/elections.Rdata")
elections <- elections %>% select(cve_secc, year, vspri_jd, vspt_jd, vspan_jd, vsprd_jd, vsmorena_jd) %>% filter(year == 2018) 
# 2018 data is missing alcalde 
load("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/1_Core/db_precinct.Rdata")
db <- db %>% group_by(cve_secc, cve_alc) %>% tally() 
elections <- left_join(elections,db[,c("cve_secc", "cve_alc")], by = "cve_secc" )
# we want the 2018 vote share for the current incumbent party, ie, did they
# bet on the right horse? This is true even in places where the party ditched
# the current incumbent, because "core" or "swing" status would still be based
# on that outcome. Note that the JD column in this db is retrospective, so we
# need to bring in the new alcaldes. 


elections$cve_alc <- as.character(elections$cve_alc)

jd <- read_csv("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/2_Elections/jefes_delegacionales.csv") %>% 
  filter(year == 2018) %>% 
  select(DEL, PARTIDO_JD) %>% rename(party_alcalde_2018 = PARTIDO_JD, cve_alc = DEL)
jd$cve_alc <- as.character(jd$cve_alc)
jd$party_alcalde_2018 <- str_replace(jd$party_alcalde_2018, "P.R.D|PRD", "prd")
jd$party_alcalde_2018 <- str_replace(jd$party_alcalde_2018, "MORENA", "morena")
jd$party_alcalde_2018 <- str_replace(jd$party_alcalde_2018, "P.A.N|PAN", "pan")
jd$party_alcalde_2018 <- str_replace(jd$party_alcalde_2018, "P.R.I|PRI", "pri")
jd$cve_alc <- str_pad(jd$cve_alc,3,   "left","0")

# assign each seccion to an ageb 
secciones <- st_read("/Users/alyssahuberts/Dropbox/2_mx_water/1_Tandeo/2_Data/9_Administrative/Estadisticas_censales_a_escalas_geoelectorales/secciones.shp")
secciones <- st_set_crs(secciones, 4326)
secciones <- secciones %>% select(cve_secc)
ageb_secciones_int <- st_intersection(ageb_shp[,c("CVEGEO", "ageb_area")], secciones)
ageb_secciones_int$area_intersection <- st_area(ageb_secciones_int)
ageb_secciones_int$int_percent_area <- as.numeric(ageb_secciones_int$area_intersection)/as.numeric(ageb_secciones_int$ageb_area)
ageb_secciones_int <- left_join(ageb_secciones_int, elections, by = c("cve_secc"))

ageb_vs <- ageb_secciones_int %>%
  group_by(CVEGEO) %>%
  summarize(vspri_jd= weighted.mean(x = vspri_jd, w = int_percent_area),
            vspan_jd= weighted.mean(x = vspan_jd, w = int_percent_area),
            vsprd_jd= weighted.mean(x = vsprd_jd, w = int_percent_area),
            vsmorena_jd= weighted.mean(x = vsmorena_jd, w = int_percent_area)
  )
ageb_vs$cve_alc <- substr(ageb_vs$CVEGEO, 3,5)
ageb_vs <- left_join(ageb_vs, jd, by = c("cve_alc"))

ageb_vs <- st_drop_geometry(ageb_vs)
vote_shares_jd <- ageb_vs %>% 
  select(cve_alc, CVEGEO, vspri_jd, vspan_jd, vsprd_jd, vsmorena_jd) %>%
  pivot_longer(cols = c(vspri_jd, vspan_jd, vsprd_jd, vsmorena_jd),names_to = "party", names_prefix = "vs", values_to = "vote_share")
vote_shares_jd$party <- str_replace(vote_shares_jd$party, pattern = "_jd",replacement= "")

vote_shares_jd <- vote_shares_jd %>%
  group_by(CVEGEO) %>%
  arrange(desc(vote_share)) %>% 
  mutate(rank = row_number())

margins <- vote_shares_jd %>%  pivot_wider(id_cols = c("CVEGEO", "cve_alc"), 
                                           names_from = rank, 
                                           names_prefix = "votes_for",
                                           values_from = vote_share)
margins$margin <- margins$votes_for1 - margins$votes_for2

parties <- vote_shares_jd %>%  pivot_wider(id_cols = c("CVEGEO", "cve_alc"), 
                                           names_from = rank, 
                                           names_prefix = "party",
                                           values_from = party)
vote_outcomes <- left_join(margins[c("CVEGEO", "cve_alc", "votes_for1", "votes_for2", "margin")], parties[,c("CVEGEO", "cve_alc", "party1", "party2")], by = c("CVEGEO", "cve_alc"))
vote_outcomes <- left_join(vote_outcomes, jd, by = "cve_alc")

# standard deviation of vote share is 15 points
# if the neighborhood's first most supported party was the party that won, and the margin was more than 15 pts, call it core
# if the neighborhood's first most supported party was not the party that won, and the margin was more than 15 pts, call it opposition
# if the margin between 1 and 2 is less than 15 pts, call it swing 

vote_outcomes$type <- NA
vote_outcomes$type <- ifelse(vote_outcomes$party1 == vote_outcomes$party_alcalde_2018 & vote_outcomes$margin >.1, "core", vote_outcomes$type)
vote_outcomes$type <- ifelse(vote_outcomes$party1 != vote_outcomes$party_alcalde_2018 & vote_outcomes$margin >.1,"opposition", vote_outcomes$type)
vote_outcomes$type <- ifelse(vote_outcomes$margin <.1, "swing", vote_outcomes$type)
# what to do about this? Basically no opposition strongholds left- may be hard
# to get places that vary in the permutations across other vars

