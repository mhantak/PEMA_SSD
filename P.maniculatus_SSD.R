###This has all models for "Body size trends in response to climate and urbanization 
#in the widespread North American deer mouse, Peromyscus maniculatus" by Guralnick et al.

library(tidyverse)
library(tidyr)
library(lme4)
library(effects)
library(car)
library(MuMIn)
library(ggplot2)
library(lmerTest) 
library(sjPlot)
library(dplyr)
library(broom)
library(tidyr)

setwd("/Users/Maggie/Dropbox/Mammal_SSD/PEMA_SSD/")

################################################
######## Body mass - without decade and without juvs & with NEON
pema_nodecade_nojuv_bodymass2 <- read.csv("pema_new_rezone_bodymass_nojuv_rezone_centroids.csv", header = TRUE, stringsAsFactors = FALSE)
pema_nodecade_nojuv_bodymass$season <-factor(pema_nodecade_nojuv_bodymass$season)
pema_nodecade_nojuv_bodymass$sex <-factor(pema_nodecade_nojuv_bodymass$sex)
#scaling & centering
pema_nodecade_nojuv_bodymass2 <- transform(pema_nodecade_nojuv_bodymass, MAT=scale(MAT), MAP=scale(MAP), pop_10km2_log10=scale(pop_10km2_log10))
#Remove missing data 
pema_nodecade_nojuv_bodymass2 <- pema_nodecade_nojuv_bodymass2 %>% drop_na(ecoregion3)
pema_nodecade_nojuv_bodymass2 <- pema_nodecade_nojuv_bodymass2 %>% drop_na(sex)
pema_nodecade_nojuv_bodymass2 <- pema_nodecade_nojuv_bodymass2 %>% drop_na(zone)
pema_nodecade_nojuv_bodymass2 <- pema_nodecade_nojuv_bodymass2 %>% mutate(id = row_number())
str(pema_nodecade_nojuv_bodymass2)
pema_nodecade_nojuv_bodymass2$log_BM <- log(pema_nodecade_nojuv_bodymass2$body_mass) 
hist(pema_nodecade_nojuv_bodymass2$log_BM)

#Get mean male and female size per zone
BM_sex_zone_mass <- pema_nodecade_nojuv_bodymass2 %>%
  group_by(zone, sex, zone.lat) %>%
  dplyr::summarize(Mean_BM = mean(body_mass, na.rm=TRUE)) 

##Need to seperate female and male mean body mass and get into columns for stats
#str(BM_sex_zone_mass)
Male = BM_sex_zone_mass[BM_sex_zone_mass$sex == "male", ] 
names(Male)[names(Male) == "Mean_BM"] <- "Male_Mean_BM"
Female = BM_sex_zone_mass[BM_sex_zone_mass$sex == "female", ] 
names(Female)[names(Female) == "Mean_BM"] <- "Female_Mean_BM"

BM.sex <- cbind(Male, Female) #Messy
str(BM.sex)
names(BM.sex)[names(BM.sex) == "zone...1"] <- "zone"
names(BM.sex)[names(BM.sex) == "sex...2"] <- "Sex.M"
names(BM.sex)[names(BM.sex) == "sex...6"] <- "Sex.F"
names(BM.sex)[names(BM.sex) == "zone.lat...3"] <- "zone.lat"
BM.sex <-subset (BM.sex, select = -zone...5)
BM.sex <-subset (BM.sex, select = -zone.lat...7)

#rensch's rule
BM.rensch <- lm(log(Male_Mean_BM) ~ log(Female_Mean_BM), data = BM.sex)
summary(BM.rensch)
plot(log(Male_Mean_BM) ~ log(Female_Mean_BM), data = BM.sex)

ggplot(BM.sex) + geom_point(aes(x = log(Female_Mean_BM), y = log(Male_Mean_BM), color = zone.lat))

###########
##calculate index of SSD for HBL
#https://cdnsciencepub.com/doi/pdf/10.1139/cjz-2018-0005?casa_token=sDv_f3tt1w0AAAAA:jUHXteo2GL0KRrvpO6z4LHM_MOw7xBCcUwEm2lX661FIVPfjW5iuBG_zeBcqREz6iFKFsdxf4lY
#calculate SSDI as (– mean size of males / mean size of females) + 1

BM.SSD <- BM.sex %>%
  group_by(zone, zone.lat) %>%
  dplyr::summarize(SSD = (-Male_Mean_BM/Female_Mean_BM)+1) 

hist(BM.SSD$SSD) # positve when females are large
abline(v=0, col="red") #most females are larger than males - not huge variation in SSD

#averaging predictor variables 
Avg_MAT <- pema_nodecade_nojuv_bodymass2 %>%
  group_by(zone, zone.lat) %>%
  dplyr::summarize(Mean_MAT = mean(MAT, na.rm=TRUE)) 

Avg_MAP <- pema_nodecade_nojuv_bodymass2 %>%
  group_by(zone, zone.lat) %>%
  dplyr::summarize(Mean_MAP = mean(MAP, na.rm=TRUE)) 

Avg_Human_pop_density <- pema_nodecade_nojuv_bodymass2 %>%
  group_by(zone, zone.lat) %>%
  dplyr::summarize(Mean_pop_10km2_log10 = mean(pop_10km2_log10, na.rm=TRUE)) 

ssd_envt_variables_BM <- cbind(BM.SSD, Avg_MAT, Avg_MAP, Avg_Human_pop_density)

str(ssd_envt_variables_BM) #Messy
names(ssd_envt_variables_BM)[names(ssd_envt_variables_BM) == "zone...1"] <- "zone"
names(ssd_envt_variables_BM)[names(ssd_envt_variables_BM) == "zone.lat...2"] <- "zone.lat"
ssd_envt_variables_BM <-subset (ssd_envt_variables_BM, select = -zone...4)
ssd_envt_variables_BM <-subset (ssd_envt_variables_BM, select = -zone.lat...5)
ssd_envt_variables_BM <-subset (ssd_envt_variables_BM, select = -zone...7)
ssd_envt_variables_BM <-subset (ssd_envt_variables_BM, select = -zone.lat...8)
ssd_envt_variables_BM <-subset (ssd_envt_variables_BM, select = -zone...10)
ssd_envt_variables_BM <-subset (ssd_envt_variables_BM, select = -zone.lat...11)
str(ssd_envt_variables_BM)

BM.mod1 <- lm(SSD ~ Mean_MAT + Mean_MAP + Mean_pop_10km2_log10 + zone.lat, data=ssd_envt_variables_BM)
summary(BM.mod1) #nothing is sig. 
plot(allEffects(BM.mod1))

options(na.action=na.fail)
BM.red <- dredge(BM.mod1)
BM.red

#reduced mod
BM.mod2 <- lm(SSD ~ Mean_pop_10km2_log10, data=ssd_envt_variables_BM)
summary(BM.mod2) #nothing is sig. 
plot(allEffects(BM.mod2)) #female-biased SSD decreases with increasing pop. density 
#Mean SSD is negatively related to mean pop density 


################################################################################################
################################################################################################
######## HB Length - without decade and without juvs
pema_nodecade_nojuv_bodylength_NN <- read.csv("pema_new_rezone_hblength_nojuv_rezone_centroids_noNEON.csv", header = TRUE, stringsAsFactors = FALSE)
pema_nodecade_nojuv_bodylength_NN$season <-factor(pema_nodecade_nojuv_bodylength_NN$season)
pema_nodecade_nojuv_bodylength_NN$sex <-factor(pema_nodecade_nojuv_bodylength_NN$sex)
#scaling & centering
pema_nodecade_nojuv_bodylength_NN2 <- transform(pema_nodecade_nojuv_bodylength_NN, MAT=scale(MAT), MAP=scale(MAP), pop_10km2_log10=scale(pop_10km2_log10))
#Remove missing data 
pema_nodecade_nojuv_bodylength_NN2 <- pema_nodecade_nojuv_bodylength_NN2 %>% drop_na(ecoregion3)
pema_nodecade_nojuv_bodylength_NN2 <- pema_nodecade_nojuv_bodylength_NN2 %>% drop_na(sex)
pema_nodecade_nojuv_bodylength_NN2 <- pema_nodecade_nojuv_bodylength_NN2 %>% drop_na(zone)
pema_nodecade_nojuv_bodylength_NN2 <- pema_nodecade_nojuv_bodylength_NN2 %>% mutate(id = row_number())
str(pema_nodecade_nojuv_bodylength_NN2)
pema_nodecade_nojuv_bodylength_NN2$log_HBL <- log(pema_nodecade_nojuv_bodylength_NN2$HB.Length) 
hist(pema_nodecade_nojuv_bodylength_NN2$HB.Length)
hist(pema_nodecade_nojuv_bodylength_NN2$log_HBL)

#Get mean male and female size per zone
HBL_sex_zone <- pema_nodecade_nojuv_bodylength_NN2 %>%
  group_by(zone, sex, zone.lat) %>%
  dplyr::summarize(Mean_HBL = mean(HB.Length, na.rm=TRUE)) 

##Need to seperate female and male mean HBL and get into columns for stats
str(HBL_sex_zone)
Male2 = HBL_sex_zone[HBL_sex_zone$sex == "male", ] 
names(Male2)[names(Male2) == "Mean_HBL"] <- "Male_Mean_HBL"
Female2 = HBL_sex_zone[HBL_sex_zone$sex == "female", ] 
names(Female2)[names(Female2) == "Mean_HBL"] <- "Female_Mean_HBL"

HBL.sex <- cbind(Male2, Female2) #Messy
str(HBL.sex)
names(HBL.sex)[names(HBL.sex) == "zone...1"] <- "zone"
names(HBL.sex)[names(HBL.sex) == "sex...2"] <- "Sex.M"
names(HBL.sex)[names(HBL.sex) == "sex...6"] <- "Sex.F"
names(HBL.sex)[names(HBL.sex) == "zone.lat...3"] <- "zone.lat"
HBL.sex <-subset (HBL.sex, select = -zone...5)
HBL.sex <-subset (HBL.sex, select = -zone.lat...7)

HBL.rensch <- lm(log(Male_Mean_HBL) ~ log(Female_Mean_HBL), data = HBL.sex)
summary(HBL.rensch)
plot(log(Male_Mean_HBL) ~ log(Female_Mean_HBL), data = HBL.sex)

ggplot(HBL.sex) + geom_point(aes(x = log(Male_Mean_HBL), y = log(Female_Mean_HBL), color = zone.lat))

############
##calculate index of SSD for HBL
#https://cdnsciencepub.com/doi/pdf/10.1139/cjz-2018-0005?casa_token=sDv_f3tt1w0AAAAA:jUHXteo2GL0KRrvpO6z4LHM_MOw7xBCcUwEm2lX661FIVPfjW5iuBG_zeBcqREz6iFKFsdxf4lY
#calculate SSDI as (– mean size of males / mean size of females) + 1

HBL.SSD <- HBL.sex %>%
  group_by(zone, zone.lat) %>%
  dplyr::summarize(SSD = (-Male_Mean_HBL/Female_Mean_HBL)+1) 

hist(HBL.SSD$SSD) # positve when females are large
abline(v=0, col="red") #most females are larger than males - not huge variation in SSD

#averaging predictor variables 
Avg_MAT2 <- pema_nodecade_nojuv_bodylength_NN2 %>%
  group_by(zone, zone.lat) %>%
  dplyr::summarize(Mean_MAT = mean(MAT, na.rm=TRUE)) 

Avg_MAP2 <- pema_nodecade_nojuv_bodylength_NN2 %>%
  group_by(zone, zone.lat) %>%
  dplyr::summarize(Mean_MAP = mean(MAP, na.rm=TRUE)) 

Avg_Human_pop_density2 <- pema_nodecade_nojuv_bodylength_NN2 %>%
  group_by(zone, zone.lat) %>%
  dplyr::summarize(Mean_pop_10km2_log10 = mean(pop_10km2_log10, na.rm=TRUE)) 

ssd_envt_variables_HBL <- cbind(HBL.SSD, Avg_MAT2, Avg_MAP2, Avg_Human_pop_density2)

str(ssd_envt_variables_HBL)
names(ssd_envt_variables_HBL)[names(ssd_envt_variables_HBL) == "zone...1"] <- "zone"
names(ssd_envt_variables_HBL)[names(ssd_envt_variables_HBL) == "zone.lat...2"] <- "zone.lat"
ssd_envt_variables_HBL <-subset (ssd_envt_variables_HBL, select = -zone...4)
ssd_envt_variables_HBL <-subset (ssd_envt_variables_HBL, select = -zone.lat...5)
ssd_envt_variables_HBL <-subset (ssd_envt_variables_HBL, select = -zone...7)
ssd_envt_variables_HBL <-subset (ssd_envt_variables_HBL, select = -zone.lat...8)
ssd_envt_variables_HBL <-subset (ssd_envt_variables_HBL, select = -zone...10)
ssd_envt_variables_HBL <-subset (ssd_envt_variables_HBL, select = -zone.lat...11)
str(ssd_envt_variables_HBL)

HBL.mod1 <- lm(SSD ~ Mean_MAT + Mean_MAP + Mean_pop_10km2_log10 + zone.lat, data=ssd_envt_variables_HBL)
summary(HBL.mod1) #mean MAT is sig. 
plot(allEffects(HBL.mod1))

options(na.action=na.fail)
HBL.red <- dredge(HBL.mod1)
HBL.red

HBL.mod2 <- lm(SSD ~ Mean_MAT, data=ssd_envt_variables_HBL)
summary(HBL.mod2) #mean MAT is sig. 
plot(allEffects(HBL.mod2))


