# Analysis script


#### Planing of script ####

## Import packages
## Import data
## Verify normality of traits in Salix & transform data if needed
## Linear models (LM)
#     zn database
#           Verify normality & transform data if needed
#           Summary of zn_ba lm analysis
#           Testing lm.zn_ba_env
#           Testing lm.zn_ba.1

#     cd database
#           Verify normality & transform data if needed
#           Summary of cd_ba lm analysis
#           Testing lm.cd_ba_env
#           Testing lm.cd_ba.1

#     pb database
#           Verify normality & transform data if needed
#           Summary of pb_ba lm analysis
#           Testing lm.pb_ba_env
#           Testing lm.pb_ba.1

## PCA (not working)
## RDA (not working)
## Plots
#     plots of traits & zn_ba
#     plots of species & TE_concentrations
## Anova of [TE]
#     [TE] ~ traits
#     [TE] ~ species


#### Import packages ####
library(vegan)
library(HH)
library('plotrix')
library(lmtest)
library(lmerTest)
library(dplyr)
library(tidyr)
library(stringr)
library('lmerTest')


#### Import data ####
data_std <- read.csv('./Pei yin/data_cleaning_final/data_std_cleaned.csv', sep=',',header = T, dec = '.')
traits <- readRDS('./complete_data.rds')

# identify the willow on the global range
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


#### Verify normality of traits in Salix & transform data if needed####

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


#### Matrix of TE accumulation and environmental factors ####

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


#### Influence of traits and environmental factor on TE accumulation - Linear model (LM) ####

#### zn only database ####
data_zn <- data_std_salix %>% filter(!is.na(zn_ba)) # 38 obs.

##### 0. Verify normality of variables & transform data if needed #####

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


##### 0. Summary of zn_ba lm analysis #####

## Since for the lmer models, either I couldn't test the assumption verification,
## either the assumptions weren't ok when I tested them, I used lm instead

# First see influence of envir. variables on cd_ba
# Then see influence of traits on cd_ba (while taking envir. var as control)


##### 1. For lm.zn_ba_env ####

## For lm.zn_ba_env: model for environmental significant variables
lm.zn_ba_env <- lm(data_zn$zn_ba_cuberoot ~ zn_s_log10  + ph + sand + clay_log2 + (1|covidence), data = data_zn)
# 16 lines

# significant p-value ?
anova(lm.zn_ba_env)
# zn_s, sand and clay are significant
summary(lm.zn_ba_env) # Adjusted R-squared:  0.6998 

# remove non significant variable from the model (i.e. ph)
lm.zn_ba_env <- lm(data_zn$zn_ba_cuberoot ~ zn_s_log10 + sand + clay_log2 + (1|covidence), data = data_zn)
# 16 lines
# significant p-value ?
anova(lm.zn_ba_env)
# zn_s, sand and clay are significant
summary(lm.zn_ba_env) # Adjusted R-squared:  0.6535 

# Assumptions verification for lm.zn_ba_env

# Normality (Shapiro-Wilk test)
shapiro.test(resid(lm.zn_ba_env)) # normal distribution (p-value = 0.06662)

# Homoscedasticity (Goldfeld–Quandt test)

# Number of obs: 14
# then 20% of total obs. is 2.8 (around 3), so fraction = 3 in gqtest()
gqtest(lm.zn_ba_env, order.by = ~ zn_s_log10 + ph + sand + clay_log2 + (1|covidence), data = data_zn, fraction = 3)
# distribution is homoscedastic (p-value = 0.4077)

plot(resid(lm.zn_ba_env) ~ fitted(lm.zn_ba_env)) # mostly random points, looks homoscedastic
# check the model assumptions
plot(lm.zn_ba_env)


##### 2. For lm.zn_ba.1 ####

## For lm.zn_ba.1 : model of traits and environmental controls
lm.zn_ba.1 <- lm(data_zn$zn_ba_cuberoot ~ LA_log + SLA + LDMC + (1|sand) + (1|clay_log2) + (1|zn_s_log10) + (1|covidence), data = data_zn)

# significant p-value ?
anova(lm.zn_ba.1)
## the significant p-values are:
# SLA_log.1:    p-value = 0.01051 *

summary(lm.zn_ba.1) # Adjusted R-squared:  0.1643

# Assumptions verification for lm.zn_ba.1

# Normality (Shapiro-Wilk test)
shapiro.test(resid(lm.zn_ba.1)) # normal distribution (p-value = 0.2601)

# Homoscedasticity (Goldfeld–Quandt test)

# Number of obs: 13
# then 20% of total obs. is 2.6 (around 2), so fraction = 2 in gqtest()
gqtest(lm.zn_ba.1, order.by = ~ LA_log + SLA + LDMC + (1|sand) + (1|clay_log2) + (1|zn_s_log10) + (1|covidence), data = data_zn, fraction = 2)
# distribution is homoscedastic (p-value = 0.02671)

plot(resid(lm.zn_ba.1) ~ fitted(lm.zn_ba.1))
# check the model assumptions
plot(lm.zn_ba.1)



#### cd only database ####
data_cd <- data_std_salix %>% filter(!is.na(cd_ba)) # 39 obs.

##### 0. Verify normality of variables & transform data if needed #####

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


##### 0. Summary of cd_ba lm analysis #####

# First see influence of envir. variables on cd_ba
# Then see influence of traits on cd_ba (while taking envir. var as control)


##### 1. For lm.cd_ba_env #####
## See influence of envir. variables on cd_ba

## For lm.cd_ba_env: model for environmental significant variables
lm.cd_ba_env <- lm(data_cd$cd_ba_log10 ~ cd_s_log10 + ph + sand + clay + (1|covidence), data = data_cd)
# 17 lines

# significant p-value ?
anova(lm.cd_ba_env)
# only sand is significant 
summary(lm.cd_ba_env) # Adjusted R-squared:  0.3378 

# remove non significant variable from the model (i.e. clay and ph)
lm.cd_ba_env <- lm(data_cd$cd_ba_log10 ~ cd_s_log10 + sand + (1|covidence), data = data_cd)
# 17 lines
# significant p-value ?
anova(lm.cd_ba_env) # sand is significant
summary(lm.cd_ba_env) # sand and cd_s are significant

# Assumptions verification for lm.cd_ba_env

# Normality (Shapiro-Wilk test)
shapiro.test(resid(lm.cd_ba_env)) # normal distribution (p-value = 0.8956)

# Homoscedasticity (Goldfeld–Quandt test)

# Number of obs: 17
# then 20% of total obs. is 3.4 (around 3), so fraction = 3 in gqtest()
gqtest(lm.cd_ba_env, order.by = ~ cd_s_log10 + sand + (1|covidence), data = data_cd_txt, fraction = 3)
# distribution is homoscedastic (p-value = 0.3115)

plot(resid(lm.cd_ba_env) ~ fitted(lm.cd_ba_env))
# check the model assumptions
plot(lm.cd_ba_env)


##### 2. For lm.cd_ba.1 #####
## See influence of traits on cd_ba (with envir controls)

## For lm.cd_ba.1 : model of traits and environmental controls
lm.cd_ba.1 <- lm(data_cd$cd_ba_log10 ~ LA_log + SLA + LDMC + (1|cd_s_log10) + (1|sand) + (1|covidence), data = data_cd)

# significant p-value ?
anova(lm.cd_ba.1) # no significant p-values
summary(lm.cd_ba.1) # no significant p-values

# Assumptions verification for lm.cd_ba.1

# Normality (Shapiro-Wilk test)
shapiro.test(resid(lm.cd_ba.1)) # normal distribution (p-value = 0.8459)

# Homoscedasticity (Goldfeld–Quandt test) 

# Number of obs: 39 (see summary of lmer.zn_ba.1 in lmer section)
# then 20% of total obs. is 7.8 (around 8), so fraction = 8 in gqtest()
gqtest(lm.cd_ba.1, order.by = ~ LA_log + SLA + LDMC + (1|cd_s_log10) + (1|sand) + (1|covidence), data = data_cd, fraction = 8)
# distribution is homoscedastic (p-value = 0.4639)

plot(resid(lm.cd_ba.1) ~ fitted(lm.cd_ba.1))
# check the model assumptions
plot(lm.cd_ba.1)



#### pb only database ####
data_pb <- data_std_salix %>% filter(!is.na(pb_ba)) # 36 obs.

##### 0. Verify normality of variables & transform data if needed #####

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


##### 0. Summary of pb_ba lm analysis #####

# First see influence of envir. variables on pb_ba
# Then see influence of traits on pb_ba (while taking envir. var as control)


##### 1. For lm.pb_ba_env #####
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
# Warning messages:
# 1: In sqrt(crit * p * (1 - hh)/hh) : NaNs produced
# 2: In sqrt(crit * p * (1 - hh)/hh) : NaNs produced


##### 2. For lm.pb_ba.1 #####
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



#### PCA of traits vs zn concentrations # NOT WORKING ####

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

#### RDA of willow traits # NOT WORKING ####

# matrix of TE in willow
data_clean <- na.omit(data_std_salix[,c('cd_ba','zn_ba','pb_ba','LA' , 'SLA' ,'LDMC' , 'ph' ,'AccSpeciesName_cor', 'covidence')])
#23 obser
TE_salix <- data_clean[,c('cd_ba','zn_ba', 'pb_ba')]

rda <- rda(TE_salix ~ LA + SLA + LDMC + Condition(covidence), data = data_clean)
plot(rda) # too few species for that
## not working


#### plots of traits and zn_ba ####

dev.new(noRStudioGD = TRUE) # opening a new window
par(mfrow = c(2,3))
plot(zn_ba ~ LA, data = data_zn)
plot(zn_ba ~ SLA, data = data_zn)
plot(zn_ba ~ LDMC, data = data_zn)
plot(cd_ba ~ LA_log, data = data_cd)
plot(cd_ba ~ SLA, data = data_cd)
plot(cd_ba ~ LDMC, data = data_cd)



### plot TE for different species ####

# accumulation of zn per species
plot(zn_ba ~ AccSpeciesName_cor, data = data_zn)
plot(zn_br ~ AccSpeciesName_cor, data = data_zn)


# accumulation of cd per species
plot(cd_ba ~ AccSpeciesName_cor, data = data_cd)
plot(cd_br ~ AccSpeciesName_cor, data = data_cd)


#### Anova of [TE] ####

##### [TE] ~ traits #####




##### [TE] ~ species #####







