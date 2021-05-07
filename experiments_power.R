################
# Script to conduct power calculations for survey experiments 
# Date created: May 7, 2021
# Date last update May 7, 2021
################
library(tidyverse)
library(lfe)
library(gridExtra)

#############################
# Format Facebook Pilot Data
#############################
set.seed(112521)
# read in Facebook pilot to use for power calcs
fb_pilot <- read_csv("/Users/alyssahuberts/Dropbox/1_Dissertation/8_Survey/8_Data/Pilots/Facebook_Pilot_5_7.csv")
fb_pilot <-fb_pilot[3:125,]

#Format vignette database from pilot (Vignette 1)
  vignette_1_data <- fb_pilot[30:123,c("Q118", "choice1_freq1", "choice1_freq2", 
                                 "choice1_pred1", "choice1_pred2",
                                 "choice1_price1", "choice1_price2")]
  vignette_1_data$colonia1 <- ifelse(vignette_1_data$Q118=="Colonia 1", 1,0)
  vignette_1_data$colonia2 <- ifelse(vignette_1_data$Q118=="Colonia 2", 1,0)
  
  # each pair of observations needs to become its own outcome
  choice1s <- vignette_1_data[,c("choice1_freq1", "choice1_pred1", "choice1_price1", "colonia1")] %>% 
    rename(freq =choice1_freq1, pred = choice1_pred1, price = choice1_price1, choice = colonia1)
  choice2s <- vignette_1_data[,c("choice1_freq2", "choice1_pred2", "choice1_price2", "colonia2")] %>% 
    rename(freq =choice1_freq2, pred = choice1_pred2, price = choice1_price2, choice = colonia2)
  vignette_1_data <- bind_rows(choice1s, choice2s)
  # calculate control mean and standard deviation.Because we're only using a
  # tiny slice of data, using the sd of the control group for all levels would
  # be huge. Instead, I take the control mean and sd for a single level (frequency)
  control <- vignette_1_data[vignette_1_data$freq == "tres días de la semana",]
  mean(control$choice, na.rm=TRUE)
  sd(control$choice,na.rm=TRUE)
  
  
  #Format vignette database from pilot (Vignette 2)
  vignette_2_data <- fb_pilot[30:123,c("Q119", "choice1_vign1", "choice1_vign2")]
  vignette_2_data$choice <- ifelse(vignette_2_data$Q119== "¿${e://Field/choice1_vign1}?",1,2)
  vignette_2_data$service_1 <- ifelse((vignette_2_data$choice1_vign1 == "Un corte inesperado de electricidad"|vignette_2_data$choice1_vign1 =="Un aumento inesperado de electricidad"), "Electricidad", "Agua")
  vignette_2_data$service_2 <- ifelse((vignette_2_data$choice1_vign2 == "Un corte inesperado de electricidad"|vignette_2_data$choice1_vign2 =="Un aumento inesperado de electricidad"), "Electricidad", "Agua")
  vignette_2_data$pq_1 <- ifelse((vignette_2_data$choice1_vign1 == "Un corte inesperado de electricidad"|vignette_2_data$choice1_vign1 =="Un corte inesperado de agua"), "Outage", "Price")
  vignette_2_data$pq_2 <- ifelse((vignette_2_data$choice1_vign2 == "Un corte inesperado de electricidad"|vignette_2_data$choice1_vign2 =="Un corte inesperado de agua"), "Outage", "Price")
  
  vignette_2_choice1 <- vignette_2_data[,c("service_1", "pq_1", "choice")] %>% rename(service = service_1, pq = pq_1) 
  vignette_2_choice1$choice <- ifelse(vignette_2_choice1$choice ==1,1,0)
  vignette_2_choice2 <- vignette_2_data[,c("service_2", "pq_2", "choice")] %>% rename(service = service_2, pq = pq_2) 
  vignette_2_choice2$choice <- ifelse(vignette_2_choice2$choice ==2,1,0)
  
  vignette_2_data <- bind_rows(vignette_2_choice1, vignette_2_choice2)
  control <- vignette_2_data[vignette_2_data$service =="Agua"&
                             vignette_2_data$pq == "Outage",]
  mean(control$choice, na.rm=TRUE)
  sd(control$choice, na.rm=TRUE)
  
  # Format facebook data for vignette 3
  vignette_3_data <- fb_pilot[,c("Q120", "Q121_1", "Q122_1", "Q123_1", "Q124_1", "choice1_image1", "choice1_desc1")]
  vignette_3_data <- vignette_3_data[30:124,]
  vignette_3_data$treatment <- factor(vignette_3_data$choice1_desc1, 
                                      levels = c("Imagina una alcaldía parecida a la suya. La alcaldía está al lado oriente de la ciudad y tiene problemas de agua; ha habido cortes inesperados y problemas con la presión del agua. Esta es una foto de la alcaldía.",
                                      "Imagina una alcaldía parecida a la suya. La alcaldía está al lado oriente de la ciudad y tiene problemas de agua; ha habido cortes inesperados y problemas con la presión del agua. En una colonia, los vecinos tienen año y medio sin agua. Tienen que comprar pipas porque el alcalde no les apoya con pipas de agua. Esta es una foto de la alcaldía.",
                                      "Imagina una alcaldía parecida a la suya. La alcaldía está al lado oriente de la ciudad y tiene problemas de agua; ha habido cortes inesperados y problemas con la presión del agua. En una colonia, los vecinos tienen año y medio sin agua. Esta es una publicidad que realizaron en una página de Facebook de vecinos de la alcaldía.",
                                      "Imagina una alcaldía parecida a la suya. La alcaldía está al lado oriente de la ciudad y tiene problemas de agua; ha habido cortes inesperados y problemas con la presión del agua. En una colonia, los vecinos tienen año y medio sin agua, y bloquearon una avenida central cómo respuesta. Esta es una foto del bloqueo."),
                                      labels =c(1:4))
  vignette_3_data$Q121_1 <-as.numeric(vignette_3_data$Q121_1)
  vignette_3_data$Q122_1 <-as.numeric(vignette_3_data$Q122_1)
  vignette_3_data$Q123_1 <-as.numeric(vignette_3_data$Q123_1)
  vignette_3_data$Q124_1 <-as.numeric(vignette_3_data$Q124_1)
  
  vignette_3_control <-vignette_3_data[vignette_3_data$treatment==1,]
  sum_stats <- vignette_3_control %>% summarize(mean(Q121_1, na.rm=TRUE), sd(Q121_1, na.rm=TRUE),
                                                mean(Q122_1, na.rm=TRUE), sd(Q122_1, na.rm=TRUE),
                                                mean(Q123_1, na.rm=TRUE), sd(Q123_1, na.rm=TRUE),
                                                mean(Q124_1, na.rm=TRUE), sd(Q124_1, na.rm=TRUE))
  
#######################
  # Power Calc for Vignette 1
#######################
# This first example varies the sample size. Because my sample size is already
# fixed, I don't focus on this analysis, but I leave it here to have the code
  
possible.ns <- seq(from=100, to=2000, by=50)     # The sample sizes we'll be considering
powers <- rep(NA, length(possible.ns))           # Empty object to collect simulation estimates
alpha <- 0.05                                    # Standard significance level
sims <- 500                                      # Number of simulations to conduct for each N
cl_size <- 6                                      # cluster size

#### Outer loop to vary the number of subjects ####
for (j in 1:length(possible.ns)){
  N <- possible.ns[j]                              # Pick the jth value for N
  
  significant.experiments <- rep(NA, sims)         # Empty object to count significant experiments
  
  #### Inner loop to conduct experiments "sims" times over for each N ####
  for (i in 1:sims){
    Y0 <-  rnorm(n=N, mean=.3, sd=.47)              # control potential outcome
    tau <- .1                                       # Hypothesize treatment effect
    Y1 <- Y0 + tau                                 # treatment potential outcome
    Cl <- rep((1:cl_size),length=N)
    Z.sim <- rbinom(n=N, size=1, prob=.5)          # Do a random assignment
    Y.sim <- Y1*Z.sim + Y0*(1-Z.sim)               # Reveal outcomes according to assignment
    fit.sim <- felm(Y.sim ~ Z.sim|0|0|Cl)                   # Do analysis (Regression with clusters)
    p.value <- summary(fit.sim)$coefficients[2,4]  # Extract p-values
    significant.experiments[i] <- (p.value <= alpha) # Determine significance according to p <= 0.05
  }
  
  powers[j] <- mean(significant.experiments)       # store average success rate (power) for each N
}
plot(possible.ns, powers, ylim=c(0,1))

possible.ns[24]
powers[24]

##### Power by effect size #####

possible.es <- seq(from=0, to=.5, by=.025)     # The possible effect sizes we'll be considering
powers <- rep(NA, length(possible.es))           # Empty object to collect simulation estimates
alpha <- 0.05                                    # Standard significance level
sims <- 500                                      # Number of simulations to conduct for each N
cl_size <- 6                                     # cluster size
N <- 1250                                         # Number of respondents

#### Outer loop to vary the effect size ####
for (j in 1:length(possible.es)){
  tau <- possible.es[j]                              # Pick the jth value for N
  
  significant.experiments <- rep(NA, sims)         # Empty object to count significant experiments
  
  #### Inner loop to conduct experiments "sims" times over for each N ####
  for (i in 1:sims){
    Y0 <-  rnorm(n=N, mean=.3, sd=.47)              # control potential outcome
    Y1 <- Y0 + tau                                 # treatment potential outcome
    Cl <- rep((1:cl_size),length=N)
    Z.sim <- rbinom(n=N, size=1, prob=.5)          # Do a random assignment
    Y.sim <- Y1*Z.sim + Y0*(1-Z.sim)               # Reveal outcomes according to assignment
    fit.sim <- felm(Y.sim ~ Z.sim|0|0|Cl)                   # Do analysis (Regression with clusters)
    p.value <- summary(fit.sim)$coefficients[2,4]  # Extract p-values
    significant.experiments[i] <- (p.value <= alpha) # Determine significance according to p <= 0.05
  }
  
  powers[j] <- mean(significant.experiments)       # store average success rate (power) for each N
}
p1 <- ggplot() +
  geom_point(aes(x= possible.es, y = powers)) +
  theme_bw() +
  labs(x= "Estimated Effect Size (AMCE)",y = "Power", title = "Power Simulations for Vignette 1 \n With 1250 Observations") 

g <- grid.arrange(p1, 
                  bottom = textGrob("Using parameters from 94-observation Facebook \n sample, with clusters of six respondents", x = 1, 
                                    hjust = 1, gp = gpar(fontface = 3L, fontsize = 9)))
ggsave("/Users/alyssahuberts/Dropbox/1_Dissertation/8_Survey/4_plots/vign_1.pdf", g)

######################
  # Vignette 2 #
######################

possible.es <- seq(from=0, to=.5, by=.025)     # The possible effect sizes we'll be considering
powers <- rep(NA, length(possible.es))           # Empty object to collect simulation estimates
alpha <- 0.05                                    # Standard significance level
sims <- 500                                      # Number of simulations to conduct for each N
cl_size <- 6                                     # cluster size
N <- 1250                                         # Number of respondents

#### Outer loop to vary the effect size ####
for (j in 1:length(possible.es)){
  tau <- possible.es[j]                              # Pick the jth value for N
  
  significant.experiments <- rep(NA, sims)         # Empty object to count significant experiments
  
  #### Inner loop to conduct experiments "sims" times over for each N ####
  for (i in 1:sims){
    Y0 <-  rnorm(n=N, mean= 0.7837838, sd= 0.4173418)              # control potential outcome
    Y1 <- Y0 + tau                                 # treatment potential outcome
    Cl <- rep((1:cl_size),length=N)
    Z.sim <- rbinom(n=N, size=1, prob=.5)          # Do a random assignment
    Y.sim <- Y1*Z.sim + Y0*(1-Z.sim)               # Reveal outcomes according to assignment
    fit.sim <- felm(Y.sim ~ Z.sim|0|0|Cl)                   # Do analysis (Regression with clusters)
    p.value <- summary(fit.sim)$coefficients[2,4]  # Extract p-values
    significant.experiments[i] <- (p.value <= alpha) # Determine significance according to p <= 0.05
  }
  
  powers[j] <- mean(significant.experiments)       # store average success rate (power) for each N
}
p2 <- ggplot() +
  geom_point(aes(x= possible.es, y = powers)) +
  theme_bw() +
  labs(x= "Estimated Effect Size (AMCE)",y = "Power", title = "Power Simulations for Vignette 2 \n With 1250 Observations") 

g <- grid.arrange(p2, 
                  bottom = textGrob("Using parameters from 94-observation Facebook \n sample, with clusters of six respondents", x = 1, 
                                    hjust = 1, gp = gpar(fontface = 3L, fontsize = 9)))
ggsave("/Users/alyssahuberts/Dropbox/1_Dissertation/8_Survey/4_plots/vign_2.pdf", g)

######################
# Vignette 3 #
######################
# Note that here we're reducing the sample size to half, because we're looking at subgroup analysis
# We also use the 1-10 outcome scale, rather than the discrete choice

# Question 1 
possible.es <- seq(from=0, to=5, by=.2)     # The possible effect sizes we'll be considering
powers_1 <- rep(NA, length(possible.es))           # Empty object to collect simulation estimates
alpha <- 0.05                                    # Standard significance level
sims <- 500                                      # Number of simulations to conduct for each N
cl_size <- 6                                     # cluster size
N <- 625                                       # Number of respondents

#### Outer loop to vary the effect size ####
for (j in 1:length(possible.es)){
  tau <- possible.es[j]                              # Pick the jth value for N
  
  significant.experiments <- rep(NA, sims)         # Empty object to count significant experiments
  
  #### Inner loop to conduct experiments "sims" times over for each N ####
  for (i in 1:sims){
    Y0 <-  rnorm(n=N, mean= 8, sd= 2.8)              # control potential outcome
    Y1 <- Y0 + tau                                 # treatment potential outcome
    Cl <- rep((1:cl_size),length=N)
    Z.sim <- rbinom(n=N, size=1, prob=.5)          # Do a random assignment
    Y.sim <- Y1*Z.sim + Y0*(1-Z.sim)               # Reveal outcomes according to assignment
    fit.sim <- felm(Y.sim ~ Z.sim|0|0|Cl)                   # Do analysis (Regression with clusters)
    p.value <- summary(fit.sim)$coefficients[2,4]  # Extract p-values
    significant.experiments[i] <- (p.value <= alpha) # Determine significance according to p <= 0.05
  }
  
  powers_1[j] <- mean(significant.experiments)       # store average success rate (power) for each N
}
p3 <- ggplot() +
  geom_point(aes(x= possible.es, y = powers_1)) +
  theme_bw() +
  labs(x= "Estimated Effect Size (AMCE)",y = "Power", title = "R1:Likelihood of Making \n Private Request") 

#### Outer loop to vary the effect size ####
powers_2 <- rep(NA, length(possible.es))           # Empty object to collect simulation estimates

for (j in 1:length(possible.es)){
  tau <- possible.es[j]                              # Pick the jth value for N
  
  significant.experiments <- rep(NA, sims)         # Empty object to count significant experiments
  
  #### Inner loop to conduct experiments "sims" times over for each N ####
  for (i in 1:sims){
    Y0 <-  rnorm(n=N, mean= 6.3, sd= 3.6)              # control potential outcome
    Y1 <- Y0 + tau                                 # treatment potential outcome
    Cl <- rep((1:cl_size),length=N)
    Z.sim <- rbinom(n=N, size=1, prob=.5)          # Do a random assignment
    Y.sim <- Y1*Z.sim + Y0*(1-Z.sim)               # Reveal outcomes according to assignment
    fit.sim <- felm(Y.sim ~ Z.sim|0|0|Cl)                   # Do analysis (Regression with clusters)
    p.value <- summary(fit.sim)$coefficients[2,4]  # Extract p-values
    significant.experiments[i] <- (p.value <= alpha) # Determine significance according to p <= 0.05
  }
  
  powers_2[j] <- mean(significant.experiments)       # store average success rate (power) for each N
}
p4 <- ggplot() +
  geom_point(aes(x= possible.es, y = powers_2)) +
  theme_bw() +
  labs(x= "Estimated Effect Size (AMCE)",y = "Power", title = "R2:Likelihood of \n Joining Protest") 

#### Outer loop to vary the effect size ####
powers_3 <- rep(NA, length(possible.es))           # Empty object to collect simulation estimates

for (j in 1:length(possible.es)){
  tau <- possible.es[j]                              # Pick the jth value for N
  
  significant.experiments <- rep(NA, sims)         # Empty object to count significant experiments
  
  #### Inner loop to conduct experiments "sims" times over for each N ####
  for (i in 1:sims){
    Y0 <-  rnorm(n=N, mean= 7, sd= 3.5)              # control potential outcome
    Y1 <- Y0 + tau                                 # treatment potential outcome
    Cl <- rep((1:cl_size),length=N)
    Z.sim <- rbinom(n=N, size=1, prob=.5)          # Do a random assignment
    Y.sim <- Y1*Z.sim + Y0*(1-Z.sim)               # Reveal outcomes according to assignment
    fit.sim <- felm(Y.sim ~ Z.sim|0|0|Cl)                   # Do analysis (Regression with clusters)
    p.value <- summary(fit.sim)$coefficients[2,4]  # Extract p-values
    significant.experiments[i] <- (p.value <= alpha) # Determine significance according to p <= 0.05
  }
  
  powers_3[j] <- mean(significant.experiments)       # store average success rate (power) for each N
}
p5 <- ggplot() +
  geom_point(aes(x= possible.es, y = powers_3)) +
  theme_bw() +
  labs(x= "Estimated Effect Size (AMCE)",y = "Power", title = "R3:Likelihood of Voting Against Mayor''") 

#### Outer loop to vary the effect size ####
powers_4 <- rep(NA, length(possible.es))           # Empty object to collect simulation estimates

for (j in 1:length(possible.es)){
  tau <- possible.es[j]                              # Pick the jth value for N
  
  significant.experiments <- rep(NA, sims)         # Empty object to count significant experiments
  
  #### Inner loop to conduct experiments "sims" times over for each N ####
  for (i in 1:sims){
    Y0 <-  rnorm(n=N, mean= 3, sd= 2.9)              # control potential outcome
    Y1 <- Y0 + tau                                 # treatment potential outcome
    Cl <- rep((1:cl_size),length=N)
    Z.sim <- rbinom(n=N, size=1, prob=.5)          # Do a random assignment
    Y.sim <- Y1*Z.sim + Y0*(1-Z.sim)               # Reveal outcomes according to assignment
    fit.sim <- felm(Y.sim ~ Z.sim|0|0|Cl)                   # Do analysis (Regression with clusters)
    p.value <- summary(fit.sim)$coefficients[2,4]  # Extract p-values
    significant.experiments[i] <- (p.value <= alpha) # Determine significance according to p <= 0.05
  }
  
  powers_4[j] <- mean(significant.experiments)       # store average success rate (power) for each N
}
p6 <- ggplot() +
  geom_point(aes(x= possible.es, y = powers_4)) +
  theme_bw() +
  labs(x= "Estimated Effect Size (AMCE)",y = "Power", title = "R3: Agree that mayor is \n doing best he can''") 

g <- grid.arrange(p3,p4,p5,p6, bottom = textGrob("Outcomes on a 1-10 scale \n Using parameters from 94-observation Facebook \n sample, with clusters of six respondents", x = 1, 
                                                 hjust = 1, gp = gpar(fontface = 3L, fontsize = 11)),
                  top = textGrob("Power Simulations for Vignette 3 With 625 Observations", x = 1, 
                                  hjust = 1.5, gp = gpar(fontface = 3L, fontsize = 15)))

ggsave("/Users/alyssahuberts/Dropbox/1_Dissertation/8_Survey/4_plots/vign_3.pdf", g)



