# Analysis script


#### 0. Planing of script ####

# 1. Import packages
# 2. Import data
# 3. Verify normality of traits in Salix & transform data if needed
# 4. Matrix of TE accumulation and environmental factors
# 5. Linear models (LM) - Zn only database
#       5a. Verify normality & transform data if needed
#       5b. Summary of zn_ba lm analysis
#       5c. Testing lm.zn_ba_env
#       5d. Testing lm.zn_ba.1
# 6. Linear models (LM) - Cd only database
#       6a. Verify normality & transform data if needed
#       6b. Summary of cd_ba lm analysis
#       6c. Testing lm.cd_ba_env
#       6d. Testing lm.cd_ba.1
# 7. Linear models (LM) - Pb only database
#       7a. Verify normality & transform data if needed
#       7b. Summary of pb_ba lm analysis
#       7c. Testing lm.pb_ba_env
#       7d. Testing lm.pb_ba.1
# 8. PCA (not working)
# 9. RDA (not working)
# 10. Plots of traits & zn_ba
# 11. Plots of species & TE_concentrations
# 12. Anova [TE] ~ species
#       12a. Testing anova [zn_ba] ~ species
#       12b. Testing anova [zn_br] ~ species
#       12c. Testing anova [cd_ba] ~ species
#       12d. Testing anova [cd_br] ~ species
#       12e. Testing anova [pb_ba] ~ species
#       12f. Testing anova [pb_br] ~ species


#### 1. Import packages ####
library(vegan)
library(HH)
library('plotrix')
library(lmtest)
library(lmerTest)
library(dplyr)
library(tidyr)
library(stringr)
library('lmerTest')


#### 2. Import data ####
data_std <- read.csv('./Pei yin/data_cleaning_final/data_std_cleaned.csv', sep=',',header = T, dec = '.')
traits <- readRDS('./complete_data.rds')

# identify the willow on the global range (to add the LA, SLA & LDMC traits to database)
# willow
traits$Salix <- 1
salix_complete <- traits %>% filter(str_detect(sp, 'Salix')) # 27 sp
traits$Salix[traits$sp %in% salix_complete$sp] <- 2
traits <- traits %>% arrange(Salix)

# filter the Salix sp. in the traits table 
salix_complete_2 <- filter(traits, Salix == 2)

# remove HA & Salix columns
salix_complete_2 <- salix_complete_2[,-c(6:7)]

# join the traits to your data
data_std <- left_join(data_std, salix_complete_2, by=c('AccSpeciesName_cor' = 'sp'))

# remove the sp. that don't have LA, SLA and LDMC values 
# i.e. remove the lines in which LA values are 'NA'
data_std_salix <- filter(data_std, LA != 'NA') # 60 lines

# check number of Salix sp. remaining in the database
unique(data_std_salix$AccSpeciesName_cor) 
# "Salix viminalis" "Salix triandra"  "Salix alba"  "Salix gmelinii"  "Salix caprea"  "Salix purpurea"
# so 6 Salix sp. left in database


#### 3. Verify normality of traits in Salix & transform data if needed ####

# defining a cube root function
cuberoot = function(x){
  ifelse(x < 0, - (-x)^(1/3), x^(1/3))
}


# check normality for LA trait
dev.new(noRStudioGD = TRUE) # opening a new window
par(mfrow = c(2,3))
hist(data_std_salix$LA) # original histogram 
hist(log(data_std_salix$LA)) ## best transformation ##
hist(log10(data_std_salix$LA))
hist(log2(data_std_salix$LA))
hist(logit(data_std_salix$LA)) # error message
hist(sqrt(data_std_salix$LA))
hist(data_std_salix$LA^(1/3)) 

# add the transformed values
data_std_salix$LA_log <- log(data_std_salix$LA)


# check normality for SLA trait
dev.new(noRStudioGD = TRUE) # opening a new window
par(mfrow = c(2,3))
hist(data_std_salix$SLA) # original histogram, relatively normal
hist(log(data_std_salix$SLA)) 
hist(log10(data_std_salix$SLA))
hist(log2(data_std_salix$SLA))
hist(logit(data_std_salix$SLA)) # error message
hist(sqrt(data_std_salix$SLA))
hist(data_std_salix$SLA^(1/3)) 


# check normality for LDMC trait
dev.new(noRStudioGD = TRUE) # opening a new window
par(mfrow = c(2,3))
hist(data_std_salix$LDMC) # original histogram, relatively normal
hist(log(data_std_salix$LDMC)) 
hist(log10(data_std_salix$LDMC))
hist(log2(data_std_salix$LDMC))
hist(logit(data_std_salix$LDMC)) # error message
hist(sqrt(data_std_salix$LDMC))
hist(data_std_salix$LDMC^(1/3)) 

## LA trait has being transformed, while SLA and LDMC are relatively normal


#### 4. Matrix of TE accumulation and environmental factors ####

# matrix of useful variables
# modified all character variable into factorial
data_std_salix <- data_std_salix %>%
  mutate(
    AccSpeciesName_cor = as.factor(AccSpeciesName_cor)
    , country = as.factor(country))

# if need no NA
data_clean <- data_std_salix[,c('as_ba',	'cd_ba',	'cu_ba',	'pb_ba',	'zn_ba',	'se_ba',	'ni_ba',	'co_ba',	'mn_ba',	'cr_ba',	'hg_ba', 'LA' , 'SLA' ,'LDMC' , 'ph' ,'AccSpeciesName_cor', 'covidence')]
data_clean <- na.omit(data_std_salix[,c('cd_ba','zn_ba','LA' , 'SLA' ,'LDMC' , 'ph' ,'AccSpeciesName_cor', 'covidence')])
# 36 lines
# Cd and Zn are the most abundant metal tested (without NA)
TE_leaves <- data_clean[,c('cd_ba', 'zn_ba')]



# check how many lines of data there are for some TE (cd, zn, pb and ni)
# cd_ba
cd_ba <- na.omit(data_std_salix$cd_ba)
cd_ba # 39 lines

# zn_ba
zn_ba <- na.omit(data_std_salix$zn_ba)
zn_ba # 38 lines

# pb_ba
pb_ba <- na.omit(data_std_salix$pb_ba)
pb_ba # 36 lines
# so pb is also one of the most abundant metals tested

# ni_ba
ni_ba <- na.omit(data_std_salix$ni_ba)
ni_ba # 22 lines


#### 5. Linear mixed model (LMM) - Zn only database ####
data_zn <- data_std_salix %>% filter(!is.na(zn_ba)) # 38 obs.

##### 5a. Verify normality of variables & transform data if needed #####

# check normality of zn_ba
dev.new(noRStudioGD = TRUE) # opening a new window
par(mfrow = c(2,3))
hist(data_zn$zn_ba) # original histogram 
hist(log(data_zn$zn_ba))
hist(log10(data_zn$zn_ba))
hist(log2(data_zn$zn_ba))
hist(logit(data_zn$zn_ba)) # error message
hist(sqrt(data_zn$zn_ba))
hist(data_zn$zn_ba^(1/3)) ## best transformation ##

# cuberoot was the best transformation
# add cuberoot transformation in column
data_zn$zn_ba_cuberoot <- cuberoot(data_zn$zn_ba)

# check normality of zn_s
dev.new(noRStudioGD = TRUE) # opening a new window
par(mfrow = c(2,3))
hist(data_zn$zn_s) # original histogram
hist(log(data_zn$zn_s))
hist(log10(data_zn$zn_s)) ## best transformation ##
hist(log2(data_zn$zn_s)) ## second best ##
hist(logit(data_zn$zn_s)) # error message
hist(sqrt(data_zn$zn_s))
hist(data_zn$zn_s^(1/3))
hist(asin(sqrt(data_zn$zn_s))) # error message
hist(decostand(data_zn$zn_s, method = 'log', MARGIN = 2))  # error message

# transform zn_s data and add them in columns
data_zn$zn_s_log10 <- log10(data_zn$zn_s) # log10 transformation (best one)
data_zn$zn_s_log10 # view data

data_zn$zn_s_log2 <- log2(data_zn$zn_s) # log2 transformation (second best)
data_zn$zn_s_log2 # view data


# check normality of ph
dev.new(noRStudioGD = TRUE) # opening a new window
par(mfrow = c(2,3))
hist(data_zn$ph) # original histogram
hist(log(data_zn$ph))
hist(log10(data_zn$ph)) ## best transformation ##
hist(log2(data_zn$ph)) 
hist(logit(data_zn$ph))
hist(sqrt(data_zn$ph))
hist(data_zn$ph^(1/3))
hist(asin(sqrt(data_zn$ph))) # error message
hist(decostand(data_zn$ph, method = 'log', MARGIN = 2))

# check normality of clay
data_zn_txt <- data_zn %>% filter(!is.na(clay)) # 16 obs

dev.new(noRStudioGD = TRUE) # opening a new window
par(mfrow = c(2,3))
hist(data_zn_txt$clay) # original histogram
hist(log(data_zn_txt$clay))
hist(log10(data_zn_txt$clay)) 
hist(log2(data_zn_txt$clay)) ## best transformation ##
hist(logit(data_zn_txt$clay))
hist(sqrt(data_zn_txt$clay))
hist(data_zn_txt$clay^(1/3))
hist(asin(sqrt(data_zn_txt$clay))) # error message
hist(decostand(data_zn_txt$clay, method = 'log', MARGIN = 2))

data_zn$clay_log2 <- log2(data_zn$clay)
data_zn_txt$clay_log2 <- log2(data_zn_txt$clay)

# check normality of sand
dev.new(noRStudioGD = TRUE) # opening a new window
par(mfrow = c(2,3))
hist(data_zn_txt$sand) # original histogram
hist(log(data_zn_txt$sand))
hist(log10(data_zn_txt$sand)) 
hist(log2(data_zn_txt$sand))
hist(logit(data_zn_txt$sand))
hist(sqrt(data_zn_txt$sand))
hist(data_zn_txt$sand^(1/3))
hist(asin(sqrt(data_zn_txt$sand))) # error message
hist(decostand(data_zn_txt$sand, method = 'log', MARGIN = 2))

## the transformations didn't really make the data more normal
## keeping it 


##### 5b. Summary of zn_ba lmer analysis #####

# First see influence of envir. variables on zn_ba
# Then see influence of traits on zn_ba (while taking envir. var as control)


##### 5c. For lmer.zn_ba_env ####

## For lmer.zn_ba_env: model for environmental significant variables
lmer.zn_ba_env <- lmer(data_zn$zn_ba_cuberoot ~ zn_s_log10 + ph + sand + clay_log2 + (1|covidence) + (1|AccSpeciesName_cor), data = data_zn)
summary(lmer.zn_ba_env)
r.squaredGLMM(lmer.zn_ba_env)
# zn_s_log10 is significant (p-value = 0.000371 ***)
# R2 0.185

# remove non significant variable from the model (i.e. ph)
# lm.zn_ba_env <- lm(data_zn$zn_ba_cuberoot ~ zn_s_log10 + sand + clay_log2 + (1|covidence) + (1|AccSpeciesName_cor), data = data_zn)
# significant p-value ?
# anova(lm.zn_ba_env)
# zn_s, sand and clay are significant
# summary(lm.zn_ba_env) # Adjusted R-squared:  0.6535

library('MuMIn')
# Assumptions verification for lmer.zn_ba_env

# Normality (Shapiro-Wilk test)
shapiro.test(resid(lmer.zn_ba_env)) # normal distribution (p-value = 0.1852)

# Homoscedasticity (Goldfeld–Quandt test)

# Number of obs: 14
# then 20% of total obs. is 2.8 (around 3), so fraction = 3 in gqtest()
gqtest(lmer.zn_ba_env, order.by = ~ zn_s_log10 + ph + sand + clay_log2 + (1|covidence) + (1|AccSpeciesName_cor), data = data_zn, fraction = 3)
# distribution is homoscedastic (p-value = 0.4077)

plot(resid(lmer.zn_ba_env) ~ fitted(lmer.zn_ba_env))
# check the model assumptions
plot(lmer.zn_ba_env) # mostly random points, looks homoscedastic

#bartlett.test(resid(lmer.zn_ba_env))


##### 5d. For lmer.zn_ba ####

## For lmer.zn_ba: model of traits and environmental controls
lmer.zn_ba <- lmer(data_zn$zn_ba_cuberoot ~ LA_log + log(SLA) log(LDMC) + (1|ph) + (1|zn_s_log10) + (1|covidence) + (1|AccSpeciesName_cor), data = data_zn)
summary(lmer.zn_ba)
r.squaredGLMM(lmer.zn_ba)
## the significant p-values are:
# SLA:    p-value = 0.0492 *


# Assumptions verification for lmer.zn_ba

# Normality (Shapiro-Wilk test)
shapiro.test(resid(lmer.zn_ba)) # normal distribution (p-value = 0.7236)

# Homoscedasticity (Goldfeld–Quandt test)

# Number of obs: 13
# then 20% of total obs. is 2.6 (around 2), so fraction = 2 in gqtest()
gqtest(lm.zn_ba.1, order.by = ~ LA_log + SLA + LDMC + (1|sand) + (1|clay_log2) + (1|zn_s_log10) + (1|covidence), data = data_zn, fraction = 2)
# distribution is heteroscedastic (p-value = 0.02671)

plot(resid(lmer.zn_ba) ~ fitted(lmer.zn_ba))
# check the model assumptions
plot(lmer.zn_ba) # funnel shape, seems heteroscedastic 



#### 6. Linear Mixed Model (LMM) - Cd only database ####
data_cd <- data_std_salix %>% filter(!is.na(cd_ba)) # 39 obs.

##### 6a. Verify normality of variables & transform data if needed #####

# check normality of cd_ba
dev.new(noRStudioGD = TRUE) # opening a new window
par(mfrow = c(2,3))
hist(data_cd$cd_ba) # original histogram 
hist(log(data_cd$cd_ba))
hist(log10(data_cd$cd_ba)) ## best transformation ##
hist(log2(data_cd$cd_ba))
hist(logit(data_cd$cd_ba))
hist(sqrt(data_cd$cd_ba))
hist(data_cd$cd_ba^(1/3)) 

# log10 was the best transformation
# add log10 transformation in column
data_cd$cd_ba_log10 <- log10(data_cd$cd_ba)
data_cd$cd_ba_log2 <- log2(data_cd$cd_ba)

# check normality of cd_s
dev.new(noRStudioGD = TRUE) # opening a new window
par(mfrow = c(2,3))
hist(data_cd$cd_s) # original histogram
hist(log(data_cd$cd_s))
hist(log10(data_cd$cd_s)) ## best transformation ##
hist(log2(data_cd$cd_s))
hist(logit(data_cd$cd_s))
hist(sqrt(data_cd$cd_s))
hist(data_cd$cd_s^(1/3))
hist(asin(sqrt(data_cd$cd_s)))
hist(decostand(data_cd$cd_s, method = 'log', MARGIN = 2))

# log10 was the best transformation
# add log10 transformation in column
data_cd$cd_s_log10 <- log10(data_cd$cd_s)

# check normality of ph
dev.new(noRStudioGD = TRUE) # opening a new window
par(mfrow = c(2,3))
hist(data_cd$ph) # original histogram
hist(log(data_cd$ph))
hist(log10(data_cd$ph))
hist(log2(data_cd$ph)) 
hist(logit(data_cd$ph))
hist(sqrt(data_cd$ph))
hist(data_cd$ph^(1/3))
hist(asin(sqrt(data_cd$ph))) # error message
hist(decostand(data_cd$ph, method = 'log', MARGIN = 2))

## the transformations didn't really make the data more normal
## keeping ph as it is


# check normality of clay
data_cd_txt <- data_cd %>% filter(!is.na(clay)) # 17 obs

dev.new(noRStudioGD = TRUE) # opening a new window
par(mfrow = c(2,3))
hist(data_cd_txt$clay) # original histogram
hist(log(data_cd_txt$clay))
hist(log10(data_cd_txt$clay)) 
hist(log2(data_cd_txt$clay))
hist(logit(data_cd_txt$clay))
hist(sqrt(data_cd_txt$clay))
hist(data_cd_txt$clay^(1/3))
hist(asin(sqrt(data_cd_txt$clay)))
hist(decostand(data_cd_txt$clay, method = 'log', MARGIN = 2))

## the transformations didn't really make the data more normal
## keeping clay as it is


# check normality of sand
dev.new(noRStudioGD = TRUE) # opening a new window
par(mfrow = c(2,3))
hist(data_cd_txt$sand) # original histogram
hist(log(data_cd_txt$sand))
hist(log10(data_cd_txt$sand)) 
hist(log2(data_cd_txt$sand))
hist(logit(data_cd_txt$sand))
hist(sqrt(data_cd_txt$sand))
hist(data_cd_txt$sand^(1/3))
hist(asin(sqrt(data_cd_txt$sand)))
hist(decostand(data_cd_txt$sand, method = 'log', MARGIN = 2))

## the transformations didn't really make the data more normal
## keeping sand as it is 


##### 6b. Summary of cd_ba lmer analysis #####

# First see influence of envir. variables on cd_ba
# Then see influence of traits on cd_ba (while taking envir. var as control)


##### 6c. For lmer.cd_ba_env #####
## See influence of envir. variables on cd_ba

## For lmer.cd_ba_env: model for environmental significant variables
lmer.cd_ba_env <- lmer(data_cd$cd_ba_log10 ~ cd_s_log10 + log(ph) + log(sand) + log(clay) + (1|covidence) + (1|AccSpeciesName_cor), data = data_cd)
summary(lmer.cd_ba_env)
# the significant p-values are:
# cd_s_log10  2.38e-05 ***
# ph          0.00486 **

# remove non significant variable from the model (i.e. clay and sand)
lmer.cd_ba_env <- lmer(data_cd$cd_ba_log10 ~ cd_s_log10 + ph + (1|covidence) + (1|AccSpeciesName_cor), data = data_cd)
summary(lmer.cd_ba_env)

# Assumptions verification for lmer.cd_ba_env

# Normality (Shapiro-Wilk test)
shapiro.test(resid(lmer.cd_ba_env)) # not normal distribution (p-value = 0.04552)

# Homoscedasticity (Goldfeld–Quandt test)

# Number of obs: 17
# then 20% of total obs. is 3.4 (around 3), so fraction = 3 in gqtest()
gqtest(lm.cd_ba_env, order.by = ~ cd_s_log10 + sand + (1|covidence), data = data_cd, fraction = 3)
# distribution is homoscedastic (p-value = 0.3115)

plot(resid((lmer.cd_ba_env)) ~ fitted((lmer.cd_ba_env)))
# check the model assumptions
plot((lmer.cd_ba_env)) # seems like random points, homoscedastic


##### 6d. For lmer.cd_ba #####
## See influence of traits on cd_ba (with envir controls)

## For lmer.cd_ba : model of traits and environmental controls
data_cd$ph_log <-log(data_cd$ph)
data_cd_out <- data_cd[-39,]
lmer.cd_ba <- lmer(cd_ba_log10 ~ LA_log + SLA + LDMC + (1|cd_s_log10) + (1|ph) + (1|covidence) + (1|AccSpeciesName_cor), data = data_cd_out)
summary(lmer.cd_ba) # no significant p-values

# Assumptions verification for lmer.cd_ba

# Normality (Shapiro-Wilk test)
shapiro.test(resid(lmer.cd_ba)) # normal distribution (p-value = 0.000106)
hist(resid(lmer.cd_ba))
# Homoscedasticity (Goldfeld–Quandt test) 

# Number of obs: 39 (see summary of lmer.zn_ba.1 in lmer section)
# then 20% of total obs. is 7.8 (around 8), so fraction = 8 in gqtest()
gqtest(lm.cd_ba.1, order.by = ~ LA_log + SLA + LDMC + (1|cd_s_log10) + (1|sand) + (1|covidence), data = data_cd, fraction = 8)
# distribution is homoscedastic (p-value = 0.4639)

plot(resid(lmer.cd_ba) ~ fitted(lmer.cd_ba))
# check the model assumptions
plot(lmer.cd_ba) # funnel shape, seems heteroscedastic



#### 7. Linear model (LM) - Pb only database ####
data_pb <- data_std_salix %>% filter(!is.na(pb_ba)) # 36 obs.

##### 7a.1 Verify normality of variables & transform data if needed #####

# check normality of pb_ba
dev.new(noRStudioGD = TRUE) # opening a new window
par(mfrow = c(2,3))
hist(data_cd$pb_ba) # original histogram 
hist(log(data_cd$pb_ba))  ## best transformation ##
hist(log10(data_cd$pb_ba)) ## second best ##
hist(log2(data_cd$pb_ba))
hist(logit(data_cd$pb_ba))
hist(sqrt(data_cd$pb_ba))
hist(data_cd$pb_ba^(1/3)) ## keeping this transfo too, seems ok ##

# log and log10 were the best transformations
# add log and log10 transformations in column
data_pb$pb_ba_log10 <- log10(data_pb$pb_ba)
data_pb$pb_ba_log2 <- log2(data_pb$pb_ba)
# add cuberoot transformation also
data_pb$pb_ba_cuberoot <- cuberoot(data_pb$pb_ba)

# check normality of pb_s
dev.new(noRStudioGD = TRUE) # opening a new window
par(mfrow = c(2,3))
hist(data_pb$pb_s) # original histogram
hist(log(data_pb$pb_s))
hist(log10(data_pb$pb_s)) 
hist(log2(data_pb$pb_s)) ## best transformation ##
hist(logit(data_pb$pb_s))
hist(sqrt(data_pb$pb_s))
hist(data_pb$pb_s^(1/3))
hist(asin(sqrt(data_pb$pb_s)))
hist(decostand(data_pb$pb_s, method = 'log', MARGIN = 2))

# log2 was the best transformation
# add log2 transformation in column
data_pb$pb_s_log2 <- log2(data_pb$pb_s)


# check normality of ph
dev.new(noRStudioGD = TRUE) # opening a new window
par(mfrow = c(2,3))
hist(data_pb$ph) # original histogram
hist(log(data_pb$ph))
hist(log10(data_pb$ph)) ## keep this transformation ##
hist(log2(data_pb$ph)) 
hist(logit(data_pb$ph))
hist(sqrt(data_pb$ph))
hist(data_pb$ph^(1/3))
hist(asin(sqrt(data_pb$ph)))
hist(decostand(data_pb$ph, method = 'log', MARGIN = 2))

## the transformations didn't really make the data more normal
## except maybe log10
# add log10 transformation in column
data_pb$ph_log10 <- log10(data_pb$ph)


# check normality of clay
data_pb_txt <- data_pb %>% filter(!is.na(clay)) # 12 obs

dev.new(noRStudioGD = TRUE) # opening a new window
par(mfrow = c(2,3))
hist(data_pb_txt$clay) # original histogram
hist(log(data_pb_txt$clay))
hist(log10(data_pb_txt$clay)) 
hist(log2(data_pb_txt$clay))
hist(logit(data_pb_txt$clay))
hist(sqrt(data_pb_txt$clay))
hist(data_pb_txt$clay^(1/3))
hist(asin(sqrt(data_pb_txt$clay)))
hist(decostand(data_pb_txt$clay, method = 'log', MARGIN = 2))

## the transformations didn't really make the data more normal
## keeping clay as it is


# check normality of sand
dev.new(noRStudioGD = TRUE) # opening a new window
par(mfrow = c(2,3))
hist(data_pb_txt$sand) # original histogram
hist(log(data_pb_txt$sand))
hist(log10(data_pb_txt$sand)) 
hist(log2(data_pb_txt$sand))
hist(logit(data_pb_txt$sand))
hist(sqrt(data_pb_txt$sand))
hist(data_pb_txt$sand^(1/3))
hist(asin(sqrt(data_pb_txt$sand)))
hist(decostand(data_pb_txt$sand, method = 'log', MARGIN = 2))

## the transformations didn't really make the data more normal
## keeping sand as it is 


##### 7b. Summary of pb_ba lm analysis #####

# First see influence of envir. variables on pb_ba
# Then see influence of traits on pb_ba (while taking envir. var as control)


##### 7c. For lm.pb_ba_env #####
## Influence of envir. variables on pb_ba

## For lm.pb_ba_env: model for environmental significant variables
lm.pb_ba_env <- lm(data_pb$pb_ba_cuberoot ~ pb_s_log2 + ph + sand + clay + (1|covidence), data = data_pb)
# 14 lines
# the variable needs to be pb_ba_cuberoot (with cuberoot)
# since the 2 other transfo (log10 & log2) have an '-Inf' value in them
# which makes the model not work (error)

# significant p-value ?
anova(lm.pb_ba_env)
# sand, clay & pb_s are significant (so ph not significant)
summary(lm.pb_ba_env) # Adjusted R-squared:  0.8713

# Assumptions verification for lm.pb_ba_env

# Normality (Shapiro-Wilk test)
shapiro.test(resid(lm.pb_ba_env)) # normal distribution (p-value = 0.8846)

# Homoscedasticity (Goldfeld–Quandt test)

# Number of obs: 36
# then 20% of total obs. is 7.2 (around 7), so fraction = 7 in gqtest()
gqtest(lm.pb_ba_env, order.by = ~ pb_s_log2 + ph + sand + clay + (1|covidence), data = data_pb, fraction = 7)
# Error: inadmissable breakpoint/too many central observations omitted

plot(resid(lm.pb_ba_env) ~ fitted(lm.pb_ba_env)) # 
# check the model assumptions
plot(lm.pb_ba_env)
# Warning messages for the last plot:
# 1: In sqrt(crit * p * (1 - hh)/hh) : NaNs produced
# 2: In sqrt(crit * p * (1 - hh)/hh) : NaNs produced


##### 7d. For lm.pb_ba.1 #####
## Influence of traits on pb_ba (with envir controls)

## For lm.pb_ba.1 : model of traits and environmental controls
lm.pb_ba.1 <- lm(data_pb$pb_ba_cuberoot ~ LA_log + SLA + LDMC + (1|sand) + (1|clay) +(1|pb_s_log2) + (1|covidence), data = data_pb, na.action = na.exclude)

# significant p-value ?
anova(lm.pb_ba.1)
## the significant p-values are:
# LA_log :  p-value = 0.006372 **
summary(lm.pb_ba.1) # Adjusted R-squared:  0.1938 

# Assumptions verification for lm.pb_ba.1

# Normality (Shapiro-Wilk test)
shapiro.test(resid(lm.pb_ba.1)) # normal distribution (p-value = 0.2073)

# Homoscedasticity (Goldfeld–Quandt test) 

# Number of obs: 13 (see summary of lmer.zn_ba.1 in lmer section)
# then 20% of total obs. is 2.6 (around 2), so fraction = 2 in gqtest()
gqtest(lm.pb_ba.1, order.by = ~ LA_log + SLA + LDMC + (1|sand) + (1|clay) +(1|pb_s_log2) + (1|covidence), data = data_pb, fraction = 2)
# distribution is homoscedastic (p-value = 0.9897)

plot(resid(lm.pb_ba.1) ~ fitted(lm.pb_ba.1))
# check the model assumptions
plot(lm.pb_ba.1)



#### 8. PCA of traits vs zn concentrations # NOT WORKING ####

## matrix of traits
# create a matrix only with the functionla traits and without controls (dataCT)
traits <- data_zn[,c('LA_log','SLA','LDMC')]
# Standardize traits because they all have different units
traits.s<-decostand(traits, method='standardize', MARGIN=2)

# generate the PCA
pca_zn <- rda(traits.s)
# Analyse the PCA
summary(pca_zn)
# Visualise the significance of each axis
# axis 1 = 62.37 and axis 2 = 21.14

# Visualise 
plot(pca_zn)
## not really working since only 6 species points

#### 9. RDA of willow traits # NOT WORKING ####

# matrix of TE in willow
data_clean <- na.omit(data_std_salix[,c('cd_ba','zn_ba','pb_ba','LA' , 'SLA' ,'LDMC' , 'ph' ,'AccSpeciesName_cor', 'covidence')])
#23 obser
TE_salix <- data_clean[,c('cd_ba','zn_ba', 'pb_ba')]

rda <- rda(TE_salix ~ LA + SLA + LDMC + Condition(covidence), data = data_clean)
plot(rda) # too few species for that
## not working

# ajout
pca <-rda(data_zn[,c('zn_ba','SLA', 'LA_log','LDMC')], scale=TRUE)
plot(pca)
# mieux de visualizer un par un

#### 10. plots of traits and TE ####

dev.new(noRStudioGD = TRUE) # opening a new window
par(mfrow = c(2,3))
plot(zn_ba ~ LA_log, data = data_zn)
plot(zn_ba ~ SLA, data = data_zn)
plot(zn_ba ~ LDMC, data = data_zn)
plot(cd_ba ~ LA_log, data = data_cd)
plot(cd_ba ~ SLA, data = data_cd)
plot(cd_ba ~ LDMC, data = data_cd)
plot(pb_ba ~ LA_log, data = data_pb)
plot(pb_ba ~ SLA, data = data_pb)
plot(pb_ba ~ LDMC, data = data_pb)


#### 11. plot TE for different species ####

# accumulation of zn per species
plot(zn_ba ~ AccSpeciesName_cor, data = data_zn_aov)
plot(zn_br ~ AccSpeciesName_cor, data = data_zn_aov)

# accumulation of cd per species
plot(cd_ba ~ AccSpeciesName_cor, data = data_cd_aov)
plot(cd_br ~ AccSpeciesName_cor, data = data_cd_aov)

# accumulation of pb per species
plot(pb_ba ~ AccSpeciesName_cor, data = data_pb_aov)
plot(pb_br ~ AccSpeciesName_cor, data = data_pb_aov)


#### 12. Anova of [TE] ~ species ####

###### 12a. anova of [zn_ba] ~ species ######

# View species in database
data_zn$AccSpeciesName_cor # 38 obs.

# [1] Salix viminalis Salix viminalis Salix triandra  Salix alba      Salix alba      Salix alba     
# [7] Salix alba      Salix alba      Salix alba      Salix alba      Salix gmelinii  Salix gmelinii 
# [13] Salix gmelinii  Salix caprea    Salix gmelinii  Salix gmelinii  Salix viminalis Salix alba     
# [19] Salix viminalis Salix gmelinii  Salix gmelinii  Salix viminalis Salix alba      Salix alba     
# [25] Salix gmelinii  Salix gmelinii  Salix gmelinii  Salix viminalis Salix alba      Salix alba     
# [31] Salix gmelinii  Salix viminalis Salix viminalis Salix viminalis Salix gmelinii  Salix gmelinii 
# [37] Salix gmelinii  Salix viminalis

# Need to remove 'Salix triandra' and 'Salix caprea', since there are only 1 obs. of each
# there are 0 obs. of 'Salix purpurea'

# 'Salix triandra' is in line 3
# 'Salix caprea' is in line 14

# remove the lines in which Species values are 'Salix triandra' and 'Salix caprea'
data_zn_aov <- data_zn[-c(3,14), ] 
# 36 obs. so 2 lines have been removed, good

# Build the anova model
zn_ba.sp.aov <- aov(data_zn_aov$zn_ba_cuberoot ~ data_zn_aov$AccSpeciesName_cor)

# Check normality (Shapiro test)
shapiro.test(resid(zn_ba.sp.aov)) 
# normal distribution (p-value = 0.1192)

# Check homogeneity of variances (Bartlett test)
bartlett.test(data_zn_aov$zn_ba_cuberoot, data_zn_aov$AccSpeciesName_cor)
# homoscedastic (p-value = 0.6325)

summary(zn_ba.sp.aov) 
# p-value = 0.0153 *
# at least 1 group significantly different from 1 other

# doing Tukey's post hoc test to see which it is
TukeyHSD(zn_ba.sp.aov)
#                                      diff         lwr       upr     p adj
# Salix gmelinii-Salix alba           3.651947  0.7277690 6.576125 0.0117232
# Salix viminalis-Salix alba          2.355741 -0.8269357 5.538418 0.1800002
# Salix viminalis-Salix gmelinii     -1.296206 -4.3738149 1.781402 0.5614145

# So Salix gmelinii is significantly different from Salix alba (p-value = 0.0117232)



##### 12b. anova of [zn_br] ~ species #####

# Build the anova model
zn_br.sp.aov <- aov(data_zn_aov$zn_br ~ data_zn_aov$AccSpeciesName_cor)

# Check normality (Shapiro test)
shapiro.test(resid(zn_br.sp.aov)) 
# distribution is not normal (p-value = p-value = 4.077e-05)

# Check homogeneity of variances with Bartlett test per permutation
source("bartlett.perm.R")

bartlett.perm(data_zn_aov$zn_ba_cuberoot, data_zn_aov$AccSpeciesName_cor, centr = "MEDIAN",
              nperm = 999, alpha = 0.05)
# Error in if (any(y.sd == 0)) { : missing value where TRUE/FALSE needed


# One-way anova with permutation test
source("anova.1way.R")

anova.1way(zn_br.sp.aov, nperm=999)
# $anova.table
#                                 Df     Sum Sq    Mean Sq   F value Prob(param) Prob(perm)
# data_zn_aov$AccSpeciesName_cor  2   191036.7   95518.34 0.0757663   0.9273115      0.922
# Residuals                      19 23953239.3 1260696.80        NA          NA         NA

# No significant p-value (0.922)


# But since the homogeneity of variances couldn't be verified above,
# Kruskal-Wallis test will be performed for this analysis
# especially since the sample (n = 14) is relatively small, so it should also work

# Kruskal-Wallis test
kruskal.test(data_zn_aov$zn_br, data_zn_aov$AccSpeciesName_cor)

# 	Kruskal-Wallis rank sum test

# data:  data_zn_aov$zn_br and data_zn_aov$AccSpeciesName_cor
# Kruskal-Wallis chi-squared = 0.99097, df = 2, p-value = 0.6093

# No significant p-value


##### 12c. anova of [cd_ba] ~ species #####

# View species in database
data_cd$AccSpeciesName_cor # 39 obs.

#  [1] Salix viminalis Salix viminalis Salix triandra  Salix alba      Salix alba      Salix alba     
#  [7] Salix alba      Salix alba      Salix alba      Salix alba      Salix gmelinii  Salix gmelinii 
#  [13] Salix gmelinii  Salix gmelinii  Salix gmelinii  Salix gmelinii  Salix caprea    Salix gmelinii 
#  [19] Salix gmelinii  Salix viminalis Salix gmelinii  Salix gmelinii  Salix viminalis Salix alba     
#  [25] Salix alba      Salix gmelinii  Salix gmelinii  Salix gmelinii  Salix viminalis Salix alba     
#  [31] Salix alba      Salix gmelinii  Salix viminalis Salix viminalis Salix viminalis Salix gmelinii 
#  [37] Salix gmelinii  Salix gmelinii  Salix viminalis

# Need to remove 'Salix triandra' and 'Salix caprea', since there are only 1 obs. of each
# there are 0 obs. of 'Salix purpurea'

# 'Salix triandra' is in line 3
# 'Salix caprea' is in line 17

# remove the lines in which Species values are 'Salix triandra' and 'Salix caprea'
data_cd_aov <- data_cd[-c(3,17), ] 
# 37 obs. so 2 lines have been removed, good

# Build the anova model
cd_ba.sp.aov <- aov(data_cd_aov$cd_ba_log10 ~ data_cd_aov$AccSpeciesName_cor)

# Check normality (Shapiro test)
shapiro.test(resid(cd_ba.sp.aov)) 
# normal distribution (p-value = 0.7952)

# Check homogeneity of variances (Bartlett test)
bartlett.test(data_cd_aov$cd_ba_log10, data_cd_aov$AccSpeciesName_cor)
# homoscedastic (p-value = 0.4526)

summary(cd_ba.sp.aov) 
# p-value = 0.303
# No significant p-value


##### 12d. anova of [cd_br] ~ species #####

# Build the anova model
cd_br.sp.aov <- aov(data_cd_aov$cd_br ~ data_cd_aov$AccSpeciesName_cor)

# Check normality (Shapiro test)
shapiro.test(resid(cd_br.sp.aov)) 
# distribution is not normal (p-value = p-value = 9.437e-05)

# Cannot check homogeneity of variances with Bartlett test per permutations 
# (it didn't work previously with the anova of [zn_br] ~ species)

# A Kruskal-Wallis test will be performed here too, 
# since the sample size is relatively small too (n = 14)

# Kruskal-Wallis test
kruskal.test(data_cd_aov$cd_br, data_cd_aov$AccSpeciesName_cor)
# 	  Kruskal-Wallis rank sum test

# data:  data_cd_aov$cd_br and data_cd_aov$AccSpeciesName_cor
# Kruskal-Wallis chi-squared = 2.9443, df = 2, p-value = 0.2294

# No significant p-value


##### 12e. anova of [pb_ba] ~ species #####

# View species in database
data_pb$AccSpeciesName_cor # 36 obs.

# [1] Salix alba      Salix alba      Salix alba      Salix alba      Salix alba      Salix alba     
# [7] Salix alba      Salix gmelinii  Salix gmelinii  Salix gmelinii  Salix alba      Salix viminalis
# [13] Salix viminalis Salix viminalis Salix viminalis Salix viminalis Salix viminalis Salix viminalis
# [19] Salix viminalis Salix viminalis Salix gmelinii  Salix gmelinii  Salix viminalis Salix alba     
# [25] Salix alba      Salix gmelinii  Salix gmelinii  Salix gmelinii  Salix viminalis Salix alba     
# [31] Salix alba      Salix gmelinii  Salix viminalis Salix viminalis Salix viminalis Salix gmelinii 

# There are only 'Salix alba', 'Salix gmelinii' and 'Salix viminalis', good

# create a copy for pb database
data_pb_aov <- data_pb

# Build the anova model
pb_ba.sp.aov <- aov(data_pb_aov$pb_ba_log10 ~ data_pb_aov$AccSpeciesName_cor)
# Error in lm.fit(x, y, offset = offset, singular.ok = singular.ok, ...) : 
# NA/NaN/Inf in 'y'

# Check the data in the x and y variables:

# x variable
unique(data_pb_aov$AccSpeciesName_cor)
# [1] Salix alba      Salix gmelinii  Salix viminalis
# No problem for x variable

# y variable
unique(data_pb_aov$pb_ba_log10)
# [1]  1.684526417  1.737771945  1.655237031  1.589970274  0.924859545         -Inf  0.004321374
# [8]  0.290034611  0.954242509  0.924279286  0.543319906  0.323959956  0.448495872  1.163856803
# [15]  0.431461983  0.263971176  0.413734276  0.729164790  1.068185862  0.586587305  0.576341350
# [22]  0.815577748  1.045322979  0.588831726  0.354108439  0.082785370 -0.210419288  0.665580991
# [29]  0.664641976 -1.096910013 -0.769551079 -0.721246399  0.960946196

# There is an "-Inf" value
# Need to find which lines it is in, then remove it

data_pb_aov$pb_ba_log10
# [1]  1.684526417  1.737771945  1.684526417  1.655237031  1.589970274  0.924859545         -Inf
# [8]  0.004321374  0.290034611  0.290034611  0.954242509  0.924279286  0.543319906  0.323959956
# [15]  0.448495872  1.163856803  0.431461983  0.263971176  0.263971176  0.413734276  0.729164790
# [22]  1.068185862  0.586587305  0.576341350  0.815577748  1.045322979  0.588831726  0.354108439
# [29]  0.082785370 -0.210419288  0.665580991  0.664641976 -1.096910013 -0.769551079 -0.721246399
# [36]  0.960946196

# So it's the 7th line

# Remove the 7th line
data_pb_aov <- data_pb_aov[-c(7), ] 
# 35 obs. now, so one less, good

# Build the anova model again
pb_ba.sp.aov <- aov(data_pb_aov$pb_ba_log10 ~ data_pb_aov$AccSpeciesName_cor)

# Check normality (Shapiro test)
shapiro.test(resid(pb_ba.sp.aov)) 
# normal distribution (p-value = 0.1113)

# Check homogeneity of variances (Bartlett test)
bartlett.test(data_pb_aov$pb_ba_log10, data_pb_aov$AccSpeciesName_cor)
# homoscedastic (p-value = 0.1923)

summary(pb_ba.sp.aov) 
# p-value = 0.00215 **
# at least 1 group significantly different from 1 other

# doing Tukey's post hoc test to see which it is
TukeyHSD(pb_ba.sp.aov)
#                                      diff        lwr        upr     p adj
# Salix gmelinii-Salix alba      -0.4984603 -1.1140063  0.1170857 0.1310416
# Salix viminalis-Salix alba     -0.8938256 -1.4614445 -0.3262066 0.0014270
# Salix viminalis-Salix gmelinii -0.3953653 -0.9786611  0.1879305 0.2338312

# So Salix viminalis is significantly different from Salix alba (p-value = 0.0014270)


##### 12f. anova of [pb_br] ~ species #####

# Build the anova model
pb_br.sp.aov <- aov(data_pb_aov$pb_br ~ data_pb_aov$AccSpeciesName_cor)

# Check normality (Shapiro test)
shapiro.test(resid(pb_br.sp.aov)) 
# normal distribution (p-value = p-value = 0.1671)

# Check homogeneity of variances (Bartlett test)
bartlett.test(data_pb_aov$pb_br, data_pb_aov$AccSpeciesName_cor)
# homoscedastic (p-value = 0.1934)

summary(pb_br.sp.aov) 
# p-value = 0.00738 **
# at least 1 group significantly different from 1 other

# doing Tukey's post hoc test to see which it is
TukeyHSD(pb_br.sp.aov)
#                                     diff       lwr       upr     p adj
# Salix gmelinii-Salix alba        19.2360 -181.2051 219.67710 0.9691737
# Salix viminalis-Salix alba     -175.7402 -366.3977  14.91736 0.0750283
# Salix viminalis-Salix gmelinii -194.9762 -346.4954 -43.45693 0.0098024

# So Salix viminalis is significantly different from Salix gmelinii (p-value = 0.0098024)



