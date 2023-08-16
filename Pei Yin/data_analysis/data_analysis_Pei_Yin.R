# Analysis script

### importation
#








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


#### transform data ####
# (with log, sqrt root and sqrt cube) & add the transformed columns

#### to be deleted ####

# see which transformation is best
hist(salix_complete_2$LA)

dev.new(noRStudioGD = TRUE) # opening a new window
par(mfrow = c(2,3))
hist(salix_complete_2$LA) # original histogram 
hist(log(salix_complete_2$LA))
hist(log10(salix_complete_2$LA))
hist(log2(salix_complete_2$LA))
hist(logit(salix_complete_2$LA)) # error message
hist(sqrt(salix_complete_2$LA))
hist(salix_complete_2$LA^(1/3)) ## best transformation ##

# log transformation
salix_complete_2$LA_log.1 <- log(salix_complete_2$LA)
salix_complete_2$LDMC_abs_log.1 <- abs(log(salix_complete_2$LDMC))
salix_complete_2$SLA_log.1 <- log(salix_complete_2$SLA)

###BEA: pourquoi as-tu pris la valuer absolue de LDMC? (abs())

# sqrt root transformation
salix_complete_2$LA_sqrt.2 <- sqrt(salix_complete_2$LA)
salix_complete_2$LDMC_abs_sqrt.2 <- sqrt(abs(salix_complete_2$LDMC))
salix_complete_2$SLA_sqrt.2 <- sqrt(salix_complete_2$SLA)

# sqrt root(log) transformation
salix_complete_2$LA_sqrt_log.3 <- sqrt(log(salix_complete_2$LA))
salix_complete_2$LDMC_sqrt_abs_log.3 <- sqrt(abs(log(salix_complete_2$LDMC)))
salix_complete_2$SLA_sqrt_log.3 <- sqrt(log(salix_complete_2$SLA))

# defining a cube root function
cuberoot = function(x){
  ifelse(x < 0, - (-x)^(1/3), x^(1/3))
}

# cube root transformation
salix_complete_2$LA_cuberoot.4 <- cuberoot(salix_complete_2$LA)
salix_complete_2$LDMC_cuberoot.4 <- cuberoot(abs(salix_complete_2$LDMC))
salix_complete_2$SLA_cuberoot.4 <- cuberoot(salix_complete_2$SLA)
#### ####

###BEA: ici tu ne dis pas comment tu vérifie la nomralité, ni quel est choisit.

# join the traits to your data
data_std <- left_join(data_std, salix_complete_2, by=c('AccSpeciesName_cor' = 'sp'))

# remove the sp. that don't have LA, SLA and LDMC values 
# i.e. remove the lines in which LA values are 'NA'
data_std_salix <- filter(data_std, LA != 'NA') # 60 lines

# check number of Salix sp. remaining in the database
unique(data_std_salix$AccSpeciesName_cor) 
# "Salix viminalis" "Salix triandra"  "Salix alba"  "Salix gmelinii"  "Salix caprea"  "Salix purpurea"
# so 6 Salix sp. left in database

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


#### Influence of traits and environmental factor on TE accumulation - Linear mix model (LMM) ####

#### zn only database ####
data_zn <- data_std_salix %>% filter(!is.na(zn_ba)) # 38 obs.

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


##### 1. Summary of zn_ba lmer analysis #####

## There are 2 sets of lmer models that I tested:
## First set: lmer.1-3-5-7-9 are about zn_ba ~ LA + SLA + LDMC + ph + (1|zn_s_log10) + (1|covidence)
## I tested these variables to see if these traits could explain the variation of zn_ba

## all five models (lmer.1-3-5-7-9) are about these variables,
## but with the same data transformed differently (log, sqrt, sqrt(log), cuberoot...)
## I did five models because either I couldn't test the assumption verification,
## either the assumptions weren't ok when I tested them,
## so I wanted to find a model where the assumptions could be tested and would be ok
##      lmer.1 : all data of traits and of zn_ba are transformed by log
##      lmer.3 : same than lm.1 but with zn_ba_cuberoot instead
##      lmer.5 : with zn_ba_cuberoot and sqrt tranformed data for the traits
##      lmer.7 : with zn_ba_cuberoot and sqrt(log) tranformed data for the traits
##      lmer.9 : with zn_ba_cuberoot and cuberoot tranformed data for the traits


## Second set: lmer.2-4-6-8-10 are about zn_ba ~ clay + sand + LA + ph + (1|zn_s_log10) + (1|covidence)
## I tested clay and sand to see if these variables could have an influence too
## also, all other variables seemed to have too many NAs to be tested,
## and clay & sand seemed to have less NAs 

## all five models are about these variables, 
## but with the same data transformed differently (log, sqrt, sqrt(log), cuberoot...)
## just like for the first set
## I did five models (lmer.2-4-6-8-10) for the same reasons than for first set
## (i.e. assumption verification either not being able to be tested or not ok) 
##      lmer.2 : all data of traits and of zn_ba are transformed by log
##      lmer.4 : same than lm.1 but with zn_ba_cuberoot instead
##      lmer.6 : with zn_ba_cuberoot and sqrt tranformed data for the traits
##      lmer.8 : with zn_ba_cuberoot and sqrt(log) tranformed data for the traits
##      lmer.10 : with zn_ba_cuberoot and cuberoot tranformed data for the traits

## After testing the models, either their assumptions are not ok, either it cannot be verified


##### 1. lmer for zn_ba ####

## For lmer.zn_ba.1 ##

lmer.zn_ba.1 <- lmerTest::lmer(data_zn$zn_ba_log ~ LA_log.1 + SLA_log.1 + LDMC_abs_log.1 + log10(ph) + (1|zn_s_log10) + (1|covidence), data = data_zn)
anova(lmer.zn_ba.1)
## the significant p-value is:
# SLA_log.1:    p-value = 0.04236 *

summary(lmer.zn_ba.1) # Number of obs: 38

## Assumptions verification for lmer.zn_ba.1 :

# homogeneity of variance ?
plot(resid(lmer.zn_ba.1) ~ fitted(lmer.zn_ba.1)) # looks like a heteroscedastic dispersion
gqtest(lmer.zn_ba.1, order.by = ~ LA_log.1 + SLA_log.1 + LDMC_abs_log.1 + log10(ph) + (1|zn_s_log10) + (1|covidence), data = data_zn, fraction = 8)
## Error message appears :
## Error in formula$x : $ operator not defined for this S4 class
## gqtest() didn't work for any of the lmer.zn_ba

plot(resid(lmer.zn_ba.1)) # mostly random points

# normal distribution of residuals ?
qqmath(lmer.zn_ba.1, id = 0.05) # points are mostly in line so respected normality (outliers?) # no, look find to be
shapiro.test(resid(lmer.zn_ba.1)) # normal distribution, # p-value = 0.08066
hist(resid(lmer.zn_ba.1)) # seems like normal distribution, with one missing column in histogram



## For lmer.zn_ba.2 ##

lmer.zn_ba.2 <- lmerTest::lmer(data_zn$zn_ba_log ~ clay + sand + LA_log.1 + log10(ph) + (1|zn_s_log10) + (1|covidence), data = data_zn)
anova(lmer.zn_ba.2)
## the significant p-value is:
# LA_log.1:    p-value = 0.009344 **

summary(lmer.zn_ba.2) # Number of obs: 16

# Assumptions verification for lmer.zn_ba.2 :

# homogeneity of variance ?
plot(resid(lmer.zn_ba.2) ~ fitted(lmer.zn_ba.2)) # looks like a heteroscedastic dispersion

# normal distribution of residuals ?
shapiro.test(resid(lmer.zn_ba.2)) # not normal distribution, # p-value = 1.051e-05
hist(resid(lmer.zn_ba.2)) # doesn't quite seem like normal distribution. 2 missing columns in histogram



## For lmer.zn_ba.3 (same than .1 but with zn_ba_cuberoot) 

lmer.zn_ba.3 <- lmerTest::lmer(data_zn$zn_ba_cuberoot ~ LA_log.1 + SLA_log.1 + LDMC_abs_log.1 + log10(ph) + (1|zn_s_log10) + (1|covidence), data = data_zn)
anova(lmer.zn_ba.3)
## no significant p-value:
# SLA_log.1: p-value = 0.05257 .
# log10(ph): p-value = 0.08595 .

summary(lmer.zn_ba.3) # Number of obs: 38

# Assumptions verification for lmer.zn_ba.3 :

# homogeneity of variance ?
plot(resid(lmer.zn_ba.3) ~ fitted(lmer.zn_ba.3)) # looks like a heteroscedastic dispersion

# normal distribution of residuals ?
shapiro.test(resid(lmer.zn_ba.3)) # normal distribution, # p-value = 0.3242
hist(resid(lmer.zn_ba.3)) # seems like a normal distribution



## For lmer.zn_ba.4 (same than .2 but with zn_ba_cuberoot)

lmer.zn_ba.4 <- lmerTest::lmer(data_zn$zn_ba_cuberoot ~ clay + sand + LA_log.1 + log10(ph) + (1|zn_s_log10) + (1|covidence), data = data_zn)
anova(lmer.zn_ba.4)
## the significant p-value is:
# LA_log.1: p-value = 0.03888 *

summary(lmer.zn_ba.4) # Number of obs: 16

# Assumptions verification for lmer.zn_ba.4

# homogeneity of variance ?
plot(resid(lmer.zn_ba.4) ~ fitted(lmer.zn_ba.4)) # looks like a heteroscedastic dispersion

# normal distribution of residuals ?
shapiro.test(resid(lmer.zn_ba.4)) # normal distribution (very borderline), # p-value = 0.06084
hist(resid(lmer.zn_ba.4)) # doesn't quite seem like normal distribution. 3 missing columns in histogram



## For lmer.zn_ba.5 (same than .3 but with sqrt tranformed data)

lmer.zn_ba.5 <- lmerTest::lmer(data_zn$zn_ba_cuberoot ~ LA_sqrt.2 + SLA_sqrt.2 + LDMC_abs_sqrt.2 + log10(ph) + (1|zn_s_log10) + (1|covidence), data = data_zn)
anova(lmer.zn_ba.5)
## no significant p-value:
# SLA_log.1: p-value = 0.05958 .
# log10(ph): p-value = 0.08552 .

summary(lmer.zn_ba.5) # Number of obs: 38

# Assumptions verification for lmer.zn_ba.5

# homogeneity of variance ?
plot(resid(lmer.zn_ba.5) ~ fitted(lmer.zn_ba.5)) # looks like a heteroscedastic dispersion

# normal distribution of residuals ?
shapiro.test(resid(lmer.zn_ba.5)) # normal distribution, # p-value = 0.3199
hist(resid(lmer.zn_ba.5)) # seems like a normal distribution 



## For lmer.zn_ba.6 (same than .4 but with sqrt tranformed data)

lmer.zn_ba.6 <- lmerTest::lmer(data_zn$zn_ba_cuberoot ~ clay + sand + LA_sqrt.2 + log10(ph) + (1|zn_s_log10) + (1|covidence), data = data_zn)
anova(lmer.zn_ba.6)
## the significant p-value is:
# LA_log.1: p-value = 0.04236 *

summary(lmer.zn_ba.6) # Number of obs: 16

# Assumptions verification for lmer.zn_ba.6

# homogeneity of variance ?
plot(resid(lmer.zn_ba.6) ~ fitted(lmer.zn_ba.6)) # looks like a heteroscedastic dispersion

# normal distribution of residuals ?
shapiro.test(resid(lmer.zn_ba.6)) # almost not normal distribution (borderline), # p-value = 0.0826
hist(resid(lmer.zn_ba.6)) # doesn't quite seem like normal distribution. 3 missing columns in histogram



## For lmer.zn_ba.7 (same than .3 but with sqrt(log) tranformed data)

lmer.zn_ba.7 <- lmerTest::lmer(data_zn$zn_ba_cuberoot ~ LA_sqrt_log.3 + SLA_sqrt_log.3 + LDMC_sqrt_abs_log.3 + log10(ph) + (1|zn_s_log10) + (1|covidence), data = data_zn)
anova(lmer.zn_ba.7)
## no significant p-value:
# SLA_sqrt_log.3: p-value = 0.05182 .
# log10(ph):      p-value = 0.08576 .

summary(lmer.zn_ba.7) # Number of obs: 38

# Assumptions verification for lmer.zn_ba.7

# homogeneity of variance ?
plot(resid(lmer.zn_ba.7) ~ fitted(lmer.zn_ba.7)) # looks like a heteroscedastic dispersion

# normal distribution of residuals ?
shapiro.test(resid(lmer.zn_ba.7)) # normal distribution, # p-value = 0.3264
hist(resid(lmer.zn_ba.7)) # seems like a normal distribution



## For lmer.zn_ba.8 (same than .4 but with sqrt(log) tranformed data)

lmer.zn_ba.8 <- lmerTest::lmer(data_zn$zn_ba_cuberoot ~ clay + sand + LA_sqrt_log.3 + log10(ph) + (1|zn_s_log10) + (1|covidence), data = data_zn)
anova(lmer.zn_ba.8)
## the significant p-value is:
# LA_sqrt_log.3:  p-value = 0.03839 *

summary(lmer.zn_ba.8) # Number of obs: 16

# Assumptions verification for lmer.zn_ba.8

# homogeneity of variance ?
plot(resid(lmer.zn_ba.8) ~ fitted(lmer.zn_ba.8)) # looks like a heteroscedastic dispersion

# normal distribution of residuals ?
shapiro.test(resid(lmer.zn_ba.8)) # almost not normal distribution, # p-value = 0.05826
hist(resid(lmer.zn_ba.8)) # doesn't quite seem like normal distribution. 3 missing columns in histogram



## For lmer.zn_ba.9 (same than .3 but with cuberoot tranformed data)

lmer.zn_ba.9 <- lmerTest::lmer(data_zn$zn_ba_cuberoot ~ LA_cuberoot.4 + SLA_cuberoot.4 + LDMC_cuberoot.4 + log10(ph) + (1|zn_s_log10) + (1|covidence), data = data_zn)
anova(lmer.zn_ba.9)
## no significant p-value:
# SLA_cuberoot.4:   p-value = 0.05672 .
# log10(ph):        p-value = 0.08567 .

summary(lmer.zn_ba.9) # Number of obs: 38

# Assumptions verification for lmer.zn_ba.9

# homogeneity of variance ?
plot(resid(lmer.zn_ba.9) ~ fitted(lmer.zn_ba.9)) # looks like a heteroscedastic dispersion

# normal distribution of residuals ?
shapiro.test(resid(lmer.zn_ba.9)) # normal distribution, # p-value = 0.3214
hist(resid(lmer.zn_ba.9)) # seems like a normal distribution



## For lmer.zn_ba.10 (same than .4 but with cuberoot tranformed data)

lmer.zn_ba.10 <- lmerTest::lmer(data_zn$zn_ba_cuberoot ~ clay + sand + LA_cuberoot.4 + log10(ph) + (1|zn_s_log10) + (1|covidence), data = data_zn)
anova(lmer.zn_ba.10)
## the significant p-value is:
# LA_cuberoot.4:  p-value = 0.04116 *

summary(lmer.zn_ba.10) # Number of obs: 16

# Assumptions verification for lmer.zn_ba.10

# homogeneity of variance ?
plot(resid(lmer.zn_ba.10) ~ fitted(lmer.zn_ba.10)) # looks like a heteroscedastic dispersion

# normal distribution of residuals ?
shapiro.test(resid(lmer.zn_ba.10)) # almost not normal distribution, # p-value = 0.07475
hist(resid(lmer.zn_ba.10)) # doesn't quite seem like normal distribution. 3 missing columns in histogram


## None of the lmer has a homoscedastic dispersion, even with the transformed data
## Attempts to use lm instead have been done in the section below


### BEA: attention, ici tes log() et log10() ne te donnais pas tout a fait les meme resultats alors garde le log10()
### aussi, je ne suis pas certaine de pourquoi il y a la valeur ba_leaf et stem? Nous n'Avons pas homogénéiser les unités de la biomasse alors nous ne pouvons pas l'utiliser

### OK j'ai gardé le log10(ph) et log10(zn_s)
### j'avais oublié que ba_leaf et stem ne sont pas homogénéisées. je les ai enlevées alors




##### 2. Summary of zn_ba lm analysis #####

## Since for the lmer models, either I couldn't test the assumption verification,
## either the assumptions weren't ok when I tested them,
## I took the lmer models and used lm instead

## e.g.: lm.zn_ba.1 has the exact variables than lmer.zn_ba.1
## the only thing that changed is the lm model instead of lmer



##### 2. lm for zn_ba ####


## For lm.zn_ba_env :model for environmental significant variables
lm.zn_ba_env <- lm(data_zn_txt$zn_ba_cuberoot ~ zn_s_log10  + ph + sand + clay_log2 + (1|covidence), data = data_zn_txt)
# 16 lines

# significant p-value ?
anova(lm.zn_ba_env)
# zn_s, sand and clay are significant
summary(lm.zn_ba_env)

# remove non significant variable from the model
lm.zn_ba_env <- lm(data_zn_txt$zn_ba_cuberoot ~ zn_s_log10 + sand + clay_log2 + (1|covidence), data = data_zn_txt)
# 16 lines
# significant p-value ?
anova(lm.zn_ba_env)
# zn_so, sand and clay are significant
summary(lm.zn_ba_env)

# Assumptions verification for lm.zn_ba_env

# Normality (Shapiro-Wilk test)
shapiro.test(resid(lm.zn_ba_env)) # normal distribution (p-value = 0.116)

# Homoscedasticity (Goldfeld–Quandt test)

# Number of obs: 16
# then 20% of total obs. is 3.2 (around 3), so fraction = 3 in gqtest()
gqtest(lm.zn_ba_env, order.by = ~ zn_s_log10 + ph + sand + clay_log2 + (1|covidence), data = data_zn, fraction = 3)
# distribution is homoscedastic (p-value = 0.01365)

plot(resid(lm.zn_ba_env) ~ fitted(lm.zn_ba_env)) # mostly random points, looks homoscedastic
# check the model assumptions
plot(lm.zn_ba_env)



## For lm.zn_ba.1 : model of traits and environmental controls
lm.zn_ba.1 <- lm(data_zn$zn_ba_cuberoot ~ LA_log + SLA + LDMC + AccSpeciesName_cor + (1|sand) + (1|clay_log2) + (1|zn_s_log10) + (1|covidence), data = data_zn)


# significant p-value ?
anova(lm.zn_ba.1)
## the significant p-values are:
# SLA_log.1:    p-value = 0.005228 **
# log10(ph):    p-value = 0.030579 *

summary(lm.zn_ba.1)

# Assumptions verification for lm.zn_ba.1

# Normality (Shapiro-Wilk test)
shapiro.test(resid(lm.zn_ba.1)) # normal distribution (p-value = 0.06054)

# Homoscedasticity (Goldfeld–Quandt test)

# Number of obs: 38 (see summary of lmer.zn_ba.1 in lmer section)
# then 20% of total obs. is 7.6 (around 8), so fraction = 8 in gqtest()
gqtest(lm.zn_ba.1, order.by = ~ LA_log + SLA + LDMC + (1|ph) + (1|zn_s_log10) + (1|covidence), data = data_zn, fraction = 8)
# distribution is homoscedastic (p-value = 0.01365)

plot(resid(lm.zn_ba.1) ~ fitted(lm.zn_ba.1)) # mostly random points, looks homoscedastic
# check the model assumptions
plot(lm.zn_ba.1)



## For lm.zn_ba.2 :
lm.zn_ba.2 <- lm(data_zn$zn_ba_log ~ clay + sand + LA_log.1 + log10(ph) + (1|zn_s_log10) + (1|covidence), data = data_zn)


###BEA: pourquoi ce modele 2? Y a-t-il une logique dans la succession de tes modèles?


# significant p-value ?
anova(lmer.zn_ba.2)
## the significant p-value is:
# LA_log.1 :     p-value = 0.009344 **

# Assumptions verification for lm.zn_ba.2

# Normality (Shapiro-Wilk test)
shapiro.test(resid(lm.zn_ba.2)) # normal distribution (p-value = 0.8767)

# Homoscedasticity (Goldfeld–Quandt test)

# Number of obs: 16 (see summary of lmer.zn_ba.2 in lmer section)
# then 20% of total obs. is 3.2 (around 3), so fraction = 3 in gqtest()
gqtest(lm.zn_ba.2, order.by = ~ clay + sand + LA_log.1 + log10(ph) + (1|zn_s_log10) + (1|covidence), data = data_zn, fraction = 3)

## Error in gqtest(lm.zn_ba.2, order.by = ~clay + sand + LA_log.1 + log10(ph) +  :
## inadmissable breakpoint/too many central observations omitted

## Another attempt with gqtest() but fraction = 2

## 	Goldfeld-Quandt test

## data:  lm.zn_ba.2
## GQ = NaN, df1 = 0, df2 = 0, p-value = NA
## alternative hypothesis: variance increases from segment 1 to 2

## Didn't work. Then relying on the plot(resid) :
plot(resid(lm.zn_ba.2) ~ fitted(lm.zn_ba.2)) # looks heteroscedastic



## For lm.zn_ba.3 :

lm.zn_ba.3 <- lm(data_zn$zn_ba_cuberoot ~ LA_log.1 + SLA_log.1 + LDMC_abs_log.1 + log10(ph) + (1|zn_s_log10) + (1|covidence), data = data_zn)

# significant p-value ?
anova(lm.zn_ba.3)
## the significant p-values are :
# SLA_log.1 :     p-value = 0.004668 **
# log10(ph) :     p-value = 0.008338 **

# Assumptions verification for lm.zn_ba.3

# Normality (Shapiro-Wilk test)
shapiro.test(resid(lm.zn_ba.3)) # normal distribution (p-value = 0.148)

# Homoscedasticity (Goldfeld–Quandt test)

# Number of obs: 38 (see summary of lmer.zn_ba.3 in lmer section)
# then 20% of total obs. is 7.6 (around 8), so fraction = 8 in gqtest()
gqtest(lm.zn_ba.3, order.by = ~ LA_log.1 + SLA_log.1 + LDMC_abs_log.1 + log10(ph) + (1|zn_s_log10) + (1|covidence), data = data_zn, fraction = 8)
# distribution is homoscedastic (p-value = 0.1929)

plot(resid(lm.zn_ba.3) ~ fitted(lm.zn_ba.3)) # random points, looks homoscedastic



## For lm.zn_ba.4 :

lm.zn_ba.4 <- lm(data_zn$zn_ba_cuberoot ~ clay + sand + LA_log.1 + log10(ph) + (1|zn_s_log10) + (1|covidence), data = data_zn)

# significant p-value ?
anova(lm.zn_ba.4)
## the significant p-value is :
# clay:   p-value = 0.02628 *

# Assumptions verification for lm.zn_ba.4

# Normality (Shapiro-Wilk test)
shapiro.test(resid(lm.zn_ba.4)) # normal distribution (p-value = 0.09347)

# Homoscedasticity (Goldfeld–Quandt test)

# Number of obs: 16 (see summary of lmer.zn_ba.4 in lmer section)
# then 20% of total obs. is 3.2 (around 2), so fraction = 3 in gqtest()
gqtest(lm.zn_ba.4, order.by = ~ clay + sand + LA_log.1 + log10(ph) + (1|zn_s_log10) + (1|covidence), data = data_zn, fraction = 3)

## Error in gqtest(lm.zn_ba.4, order.by = ~clay + sand + LA_log.1 + log10(ph) +  :
## inadmissable breakpoint/too many central observations omitted

## Another attempt with gqtest() but fraction = 2

## 	Goldfeld-Quandt test

## data:  lm.zn_ba.4
## GQ = NaN, df1 = 0, df2 = 0, p-value = NA
## alternative hypothesis: variance increases from segment 1 to 2

## Didn't work. Then relying on the plot(resid) :
plot(resid(lm.zn_ba.4) ~ fitted(lm.zn_ba.4)) # don't seem like random points, looks heteroscedastic



## For lm.zn_ba.5 :

lm.zn_ba.5 <- lm(data_zn$zn_ba_cuberoot ~ LA_sqrt.2 + SLA_sqrt.2 + LDMC_abs_sqrt.2 + log10(ph) + (1|zn_s_log10) + (1|covidence), data = data_zn)

# significant p-value ?
anova(lm.zn_ba.5)
## the significant p-values are :
# SLA_sqrt.2 :    p-value = 0.002684 **
# log10(ph) :     p-value = 0.008334 **

# Assumptions verification for lm.zn_ba.5

# Normality (Shapiro-Wilk test)
shapiro.test(resid(lm.zn_ba.5)) # normal distribution (p-value = 0.1832)

# Homoscedasticity (Goldfeld–Quandt test)

# Number of obs: 38 (see summary of lmer.zn_ba.5 in lmer section)
# then 20% of total obs. is 7.6 (around 8), so fraction = 8 in gqtest()
gqtest(lm.zn_ba.5, order.by = ~ LA_log.1 + SLA_log.1 + LDMC_abs_log.1 + log10(ph) + (1|zn_s_log10) + (1|covidence), data = data_zn, fraction = 8)
# distribution is homoscedastic (p-value = 0.1852)

plot(resid(lm.zn_ba.5) ~ fitted(lm.zn_ba.5)) # random points, looks homoscedastic



## For lm.zn_ba.6 :

lm.zn_ba.6 <- lm(data_zn$zn_ba_cuberoot ~ clay + sand + LA_sqrt.2 + log10(ph) + (1|zn_s_log10) + (1|covidence), data = data_zn)

# significant p-value ?
anova(lm.zn_ba.6)
## the significant p-value is :
# clay:   p-value = 0.02638 *

# Assumptions verification for lm.zn_ba.6

# Normality (Shapiro-Wilk test)
shapiro.test(resid(lm.zn_ba.6)) # normal distribution (p-value = 0.08954)

# Homoscedasticity (Goldfeld–Quandt test)

# Number of obs: 16 (see summary of lmer.zn_ba.4 in lmer section)
# then 20% of total obs. is 3.2 (around 2), so fraction = 3 in gqtest()
gqtest(lm.zn_ba.6, order.by = ~ clay + sand + LA_sqrt.2 + log10(ph) + (1|zn_s_log10) + (1|covidence), data = data_zn, fraction = 3)

## Error in gqtest(lm.zn_ba.6, order.by = ~clay + sand + LA_sqrt.2 + log10(ph) +  :
## inadmissable breakpoint/too many central observations omitted

## Another attempt with gqtest() but fraction = 2

## 	Goldfeld-Quandt test

## data:  lm.zn_ba.6
## GQ = NaN, df1 = 0, df2 = 0, p-value = NA
## alternative hypothesis: variance increases from segment 1 to 2

## Didn't work. Then relying on the plot(resid) :
plot(resid(lm.zn_ba.6) ~ fitted(lm.zn_ba.6)) # don't seem like random points, looks heteroscedastic



## For lm.zn_ba.7 :

lm.zn_ba.7 <- lm(data_zn$zn_ba_cuberoot ~ LA_sqrt_log.3 + SLA_sqrt_log.3 + LDMC_sqrt_abs_log.3 + log10(ph) + (1|zn_s_log10) + (1|covidence), data = data_zn)

# significant p-value ?
anova(lm.zn_ba.7)
## the significant p-values are :
# SLA_sqrt_log.3 :  p-value = 0.005031 **
# log10(ph) :       p-value = 0.008156 **

# Assumptions verification for lm.zn_ba.7

# Normality (Shapiro-Wilk test)
shapiro.test(resid(lm.zn_ba.7)) # normal distribution (p-value = 0.1546)

# Homoscedasticity (Goldfeld–Quandt test)

# Number of obs: 38 (see summary of lmer.zn_ba.5 in lmer section)
# then 20% of total obs. is 7.6 (around 8), so fraction = 8 in gqtest()
gqtest(lm.zn_ba.7, order.by = ~ LA_sqrt_log.3 + SLA_sqrt_log.3 + LDMC_sqrt_abs_log.3 + log10(ph) + (1|zn_s_log10) + (1|covidence), data = data_zn, fraction = 8)
# distribution is homoscedastic (p-value = 0.1934)

plot(resid(lm.zn_ba.7) ~ fitted(lm.zn_ba.7)) # mostly random points, looks homoscedastic



## For lm.zn_ba.8 :

lm.zn_ba.8 <- lm(data_zn$zn_ba_cuberoot ~ clay + sand + LA_sqrt_log.3 + log10(ph) + (1|zn_s_log10) + (1|covidence), data = data_zn)

# significant p-value ?
anova(lm.zn_ba.8)
## the significant p-value is :
# clay:   p-value = 0.02627 *

# Assumptions verification for lm.zn_ba.8

# Normality (Shapiro-Wilk test)
shapiro.test(resid(lm.zn_ba.8)) # normal distribution (p-value = 0.09398)

# Homoscedasticity (Goldfeld–Quandt test)

# Number of obs: 16 (see summary of lmer.zn_ba.4 in lmer section)
# then 20% of total obs. is 3.2 (around 2), so fraction = 3 in gqtest()
gqtest(lm.zn_ba.8, order.by = ~ clay + sand + LA_sqrt_log.3 + log10(ph) + (1|zn_s_log10) + (1|covidence), data = data_zn, fraction = 3)

## Error in gqtest(lm.zn_ba.8, order.by = ~clay + sand + LA_sqrt_log.3 + log10(ph) +  :
## inadmissable breakpoint/too many central observations omitted

## Another attempt with gqtest() but fraction = 2

## 	Goldfeld-Quandt test

## data:  lm.zn_ba.8
## GQ = NaN, df1 = 0, df2 = 0, p-value = NA
## alternative hypothesis: variance increases from segment 1 to 2

## Didn't work. Then relying on the plot(resid) :
plot(resid(lm.zn_ba.8) ~ fitted(lm.zn_ba.8)) # don't seem like random points, looks heteroscedastic



## For lm.zn_ba.9 :

lm.zn_ba.9 <- lm(data_zn$zn_ba_cuberoot ~ LA_cuberoot.4 + SLA_cuberoot.4 + LDMC_cuberoot.4 + log10(ph) + (1|zn_s_log10) + (1|covidence), data = data_zn)

# significant p-value ?
anova(lm.zn_ba.9)
## the significant p-values are :
# SLA_cuberoot.4 :  p-value = 0.003167 **
# log10(ph) :       p-value = 0.008353 **

# Assumptions verification for lm.zn_ba.9

# Normality (Shapiro-Wilk test)
shapiro.test(resid(lm.zn_ba.9)) # normal distribution (p-value = 0.1713)

# Homoscedasticity (Goldfeld–Quandt test)

# Number of obs: 38 (see summary of lmer.zn_ba.5 in lmer section)
# then 20% of total obs. is 7.6 (around 8), so fraction = 8 in gqtest()
gqtest(lm.zn_ba.9, order.by = ~ LA_cuberoot.4 + SLA_cuberoot.4 + LDMC_cuberoot.4 + log10(ph) + (1|zn_s_log10) + (1|covidence), data = data_zn, fraction = 8)
# distribution is homoscedastic (p-value = 0.1713)

plot(resid(lm.zn_ba.9) ~ fitted(lm.zn_ba.9)) # mostly random points, looks homoscedastic



## For lm.zn_ba.10 :

lm.zn_ba.10 <- lm(data_zn$zn_ba_cuberoot ~ clay + sand + LA_cuberoot.4 + log10(ph) + (1|zn_s_log10) + (1|covidence), data = data_zn)

# significant p-value ?
anova(lm.zn_ba.10)
## the significant p-value is :
# clay:   p-value = 0.02634 *

# Assumptions verification for lm.zn_ba.10

# Normality (Shapiro-Wilk test)
shapiro.test(resid(lm.zn_ba.10)) # normal distribution (p-value = 0.09094)

# Homoscedasticity (Goldfeld–Quandt test)

# Number of obs: 16 (see summary of lmer.zn_ba.4 in lmer section)
# then 20% of total obs. is 3.2 (around 2), so fraction = 3 in gqtest()
gqtest(lm.zn_ba.10, order.by = ~ clay + sand + LA_cuberoot.4 + log10(ph) + (1|zn_s_log10) + (1|covidence), data = data_zn, fraction = 3)

## Error in gqtest(lm.zn_ba.10, order.by = ~clay + sand + LA_cuberoot.4 + log10(ph) +  :
## inadmissable breakpoint/too many central observations omitted

## Another attempt with gqtest() but fraction = 2

## 	Goldfeld-Quandt test

## data:  lm.zn_ba.10
## GQ = NaN, df1 = 0, df2 = 0, p-value = NA
## alternative hypothesis: variance increases from segment 1 to 2

## Didn't work. Then relying on the plot(resid) :
plot(resid(lm.zn_ba.10) ~ fitted(lm.zn_ba.10)) # don't seem like random points, looks heteroscedastic


##### 2. After testing zn_ba lm models ####

## out of the 10 lm models for zn_ba, only the models 1-3-5-7-9 work 
## (i.e. their assumptions (normality, homoscedasticity) are verified)
## the p-value of model lm.zn_ba.5 for SLA is slightly more significant (p-value = 0.002684)
## so that model will be incorporated in the report

## see the R-squared value for lm.zn_ba.5
summary(lm.zn_ba.5) # Adjusted R-squared:  0.3088 
# so this model explains about 30.88% of zn_ba


## I tried incorporating 'AccSpeciesName_cor' in the lm model
## whose assumption were verified and whose p-values were the most significant (i.e. lm.zn_ba.5)


## Naming it lm.zn_ba.11 :

lm.zn_ba.11 <- lm(data_zn$zn_ba_cuberoot ~ AccSpeciesName_cor + (1|zn_s_log10) + (1|covidence), data = data_zn)

# significant p-value ?
anova(lm.zn_ba.11)
## the significant p-values are :
# AccSpeciesName_cor:  p-value = 0.016227 *
# log10(ph):           p-value = 0.005359 **

# Assumptions verification for lm.zn_ba.11

# Normality (Shapiro-Wilk test)
shapiro.test(resid(lm.zn_ba.11)) # normal distribution (p-value = 0.2346)

# Homoscedasticity (Goldfeld–Quandt test)

# Number of obs: 38 (see summary of lmer.zn_ba.11 in lmer section)
# then 20% of total obs. is 7.6 (around 8), so fraction = 8 in gqtest()
gqtest(lm.zn_ba.11, order.by = ~ AccSpeciesName_cor + LA_sqrt.2 + SLA_sqrt.2 + LDMC_abs_sqrt.2 + log10(ph) + (1 | zn_s_log10) + (1 | covidence), data = data_zn, fraction = 8)
# distribution is homoscedastic (p-value = 0.1808)

plot(resid(lm.zn_ba.11) ~ fitted(lm.zn_ba.11)) # mostly random points, looks homoscedastic


summary(lm.zn_ba.11) 
# AccSpeciesName_cor Salix gmelinii :  p-value = 0.017407 *
# log10(ph) :                          p-value = 0.005359 **

# Adjusted R-squared:  0.3297, so this model explains about 33% of zn_ba
# F-statistic: 4.641 on 5 and 32 DF


##### 3. Summary of zn_br lm analysis #####

## lm.zn_br.1 : see if traits could explain the variation of zn_br
##              - but the model seem heteroscedastic

## lm.zn_br.2 : see if the variables that explain the most of zn_br are log10(ph) and zn_s_log10
##              - Adjusted R-squared:  0.8534, so this model explains about 85% of zn_br
##                but the model seem heteroscedastic too & distribution is not normal

## lm.zn_br.3 : since the previous model explained a large part of zn_br, 
##              see if adding the interaction between log10(ph) and zn_s_log10 (with *)
##              might explain more of zn_br
##              - Adjusted R-squared:  0.9406, so this model explains about 94% of zn_br
##                but the model seem heteroscedastic too & distribution is not normal

## next step: see if standardizing the data might solve the problem of assumption verification


##### 3. lm for zn_br ####

# check normality of zn_br
dev.new(noRStudioGD = TRUE) # opening a new window
par(mfrow = c(2,3))
hist(data_zn$zn_br) # original histogram 
hist(log(data_zn$zn_br)) ## best transformation ##
hist(log10(data_zn$zn_br)) ## third best ##
hist(log2(data_zn$zn_br)) ## second best ##
hist(logit(data_zn$zn_br))
hist(sqrt(data_zn$zn_br))
hist(data_zn$zn_br^(1/3))


## For lm.zn_br.1 ##
lm.zn_br.1 <- lm(zn_br ~ LA_log.1 + SLA_log.1 + LDMC_abs_log.1 + log10(ph) + (1|zn_s_log10) + (1|covidence), data = data_zn)

anova(lm.zn_br.1) # is something significant? with *
## the significant p-values are:
# log10(ph):    p-value = 0.0001695 ***

summary(lm.zn_br.1)

# Assumptions verification for lm.zn_br.1

# Normality (Shapiro-Wilk test)
shapiro.test(resid(lm.zn_br.1)) # normal distribution (p-value = 0.1718)

# Homoscedasticity (Goldfeld–Quandt test)

# Number of obs: 22 (according to lmer.zn_br.1)
# then 20% of total obs. is 4.4 (around 4), so fraction = 4 in gqtest()
gqtest(lm.zn_br.1, order.by = ~ LA_log.1 + SLA_log.1 + LDMC_abs_log.1 + log10(ph) + (1|zn_s_log10) + (1|covidence), data = data_zn, fraction = 4)
# Error in X[order(z), ] : subscript out of bounds

# relying on plot() then:
plot(resid(lm.zn_br.1) ~ fitted(lm.zn_br.1)) # looks very heteroscedastic



## For lm.zn_br.2 ##
## see if the variables that explain the most of zn_br are log10(ph) and zn_s_log10
lm.zn_br.2 <- lm(zn_br ~ log10(ph) + zn_s_log10, data = data_zn)

anova(lm.zn_br.2) # is something significant? with *
## the significant p-values are:
# log10(ph):     p-value = 4.255e-08 ***
# zn_s_log10:    p-value = 1.413e-06 ***

# Assumptions verification for lm.zn_br.2

# Normality (Shapiro-Wilk test)
shapiro.test(resid(lm.zn_br.2)) # not normal distribution (p-value = 0.01401)

# Homoscedasticity (Goldfeld–Quandt test)

# Number of obs: 22 (according to lmer.zn_br.2)
# then 20% of total obs. is 4.4 (around 4), so fraction = 4 in gqtest()
gqtest(lm.zn_br.2, order.by = ~ log10(ph) + zn_s_log10, data = data_zn, fraction = 4)
# Error in X[order(z), ] : subscript out of bounds

# relying on plot() then:
plot(resid(lm.zn_br.2) ~ fitted(lm.zn_br.2)) # looks very heteroscedastic

summary(lm.zn_br.2)
# Adjusted R-squared:  0.8534, so this model explains about 85% of zn_br
# (F-statistic: 62.13 on 2 and 19 DF)



## For lm.zn_br.3 ##
## see interaction between log10(ph) and zn_s_log10

lm.zn_br.3 <- lm(zn_br ~ log10(ph) * zn_s_log10, data = data_zn)

anova(lm.zn_br.3) # is something significant? with *
## the significant p-values are:
# log10(ph)               p-value = 5.409e-11 ***
# zn_s_log10              p-value = 2.568e-09 ***
# log10(ph):zn_s_log10    p-value = 4.147e-05 ***

# Assumptions verification for lm.zn_br.3

# Normality (Shapiro-Wilk test)
shapiro.test(resid(lm.zn_br.3)) # not normal distribution (p-value = 0.01426)

# Homoscedasticity (Goldfeld–Quandt test)

# Number of obs: 22 (according to lmer.zn_br.3)
# then 20% of total obs. is 4.4 (around 4), so fraction = 4 in gqtest()
gqtest(lm.zn_br.3, order.by = ~ log10(ph) * zn_s_log10, data = data_zn, fraction = 4)
# Error in X[order(z), ] : subscript out of bounds

# relying on plot() then:
plot(resid(lm.zn_br.3) ~ fitted(lm.zn_br.3)) # looks very heteroscedastic

summary(lm.zn_br.3)
# Adjusted R-squared:  0.9406, so this model explains about 94% of zn_br
# (F-statistic: 111.9 on 3 and 18 DF)





#### cd only database ####
data_cd <- data_std_salix %>% filter(!is.na(cd_ba)) # 39 obs.

# check normality of cd_ba
dev.new(noRStudioGD = TRUE) # opening a new window
par(mfrow = c(2,3))
hist(data_zn$cd_ba) # original histogram 
hist(log(data_zn$cd_ba))
hist(log10(data_zn$cd_ba)) ## best transformation ##
hist(log2(data_zn$cd_ba)) ## second best ##
hist(logit(data_zn$cd_ba))
hist(sqrt(data_zn$cd_ba))
hist(data_zn$cd_ba^(1/3)) 

# log10 was the best transformation
# add log10 transformation in column
data_cd$cd_ba_log10 <- log10(data_cd$cd_ba)


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







# For cd_ba
lmer.cd_ba <- lmer(cd_ba ~ LA_log + SLA_log + LDMC_log + cd_br + cd_s + ph + country + AccSpeciesName_cor + (1|covidence), data = data_std_salix)
anova(lmer.cd_ba)
summary(lmer.cd_ba)

plot(resid(lmer.cd_ba),data_std_salix$cd_ba) # 
plot(lmer.cd_ba) # 
qqmath(lmer.cd_ba, id=0.05) # points are mostly in line so respected normality (outliers?)
shapiro.test(resid(lmer.cd_ba)) # borderline normal, p-value = 0.05233


# For cd_br




#### pb only database ####



# For pb_ba





# For pb_br




#### PCA of willows vs all species ####


#### PCA of traits vs zn concnetrations

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

#### RDA of willow traits ####

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

