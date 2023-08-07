# Analysis script

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

# log transformation
salix_complete_2$LA_log.1 <- log(salix_complete_2$LA)
salix_complete_2$LDMC_abs_log.1 <- abs(log(salix_complete_2$LDMC))
salix_complete_2$SLA_log.1 <- log(salix_complete_2$SLA)

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

# join the traits to your data
data_std <- left_join(data_std, salix_complete_2, by=c('AccSpeciesName_cor' = 'sp'))

# remove the sp. that don't have LA, SLA and LDMC values 
# i.e. remove the lines in which LA values are 'NA'
data_std_salix <- filter(data_std, LA_log.1 != 'NA') # 60 lines

# check number of Salix sp. remaining in the database
unique(data_std_salix$AccSpeciesName_cor) 
# "Salix viminalis" "Salix triandra"  "Salix alba"  "Salix gmelinii"  "Salix caprea"  "Salix purpurea"
# so 6 Salix sp. left in database


#### Matrix of TE accumulation and environmental factors ####
# Distribution and normality
# Transformation

# matrix of useful variables
# modified all character variable into factorial
data_std_salix <- data_std_salix %>%
  mutate(
    AccSpeciesName_cor = as.factor(AccSpeciesName_cor)
    , country = as.factor(country))

# if need no NA
data_clean <- data_std_salix[,c('as_ba',	'cd_ba',	'cu_ba',	'pb_ba',	'zn_ba',	'se_ba',	'ni_ba',	'co_ba',	'mn_ba',	'cr_ba',	'hg_ba', 'LA' , 'SLA' ,'LDMC' , 'ph' ,'AccSpeciesName_cor', 'covidence')]
data_clean <- na.omit(data_std_salix[,c('cd_ba','zn_ba','LA' , 'SLA' ,'LDMC' , 'ph' ,'AccSpeciesName_cor', 'covidence')])
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
data_zn <- data_std_salix %>% filter(!is.na(zn_ba))

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


# transform zn_ba data and add them in columns
data_zn$zn_ba_log <- log(data_zn$zn_ba) # log transformation
data_zn$zn_ba_log # view data

data_zn$zn_ba_sqrt <- sqrt(data_zn$zn_ba) # sqrt transformation
data_zn$zn_ba_sqrt # view data

data_zn$zn_ba_sqrt_log <- sqrt(log(data_zn$zn_ba)) # sqrt(log) transformation
data_zn$zn_ba_sqrt_log # view data

data_zn$zn_ba_cuberoot <- cuberoot(data_zn$zn_ba) # cuberoot transformation
data_zn$zn_ba_cuberoot # view data
# cuberoot was the best transformation


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
hist(log2(data_zn$ph)) ## second best ##
hist(logit(data_zn$ph)) # error message
hist(sqrt(data_zn$ph))
hist(data_zn$ph^(1/3))
hist(asin(sqrt(data_zn$ph))) # error message
hist(decostand(data_zn$ph, method = 'log', MARGIN = 2))

#### LMM for zn_ba ####

lmer.zn_ba.1 <- lmerTest::lmer(data_zn$zn_ba_log ~ LA_log.1 + SLA_log.1 + LDMC_abs_log.1 + log10(ph) + (1|zn_s_log10) + (1|covidence), data = data_zn)
anova(lmer.zn_ba.1)
## the significant p-value is:
# SLA_log.1:    p-value = 0.04236 *
summary(lmer.zn_ba.1)

lmer.zn_ba.2 <- lmerTest::lmer(data_zn$zn_ba_log ~ clay + sand + LA_log.1 + log10(ph) + (1|zn_s_log10) + (1|covidence), data = data_zn)
anova(lmer.zn_ba.2)
## the significant p-value is:
# LA_log.1:    p-value = 0.009344 **


# lmer.3 & lmer.4 : same than .1 & .2 but with zn_ba_cuberoot:

lmer.zn_ba.3 <- lmerTest::lmer(data_zn$zn_ba_cuberoot ~ LA_log.1 + SLA_log.1 + LDMC_abs_log.1 + log10(ph) + (1|zn_s_log10) + (1|covidence), data = data_zn)
anova(lmer.zn_ba.3)
## no significant p-value:
# SLA_log.1: p-value = 0.05257 .
# log10(ph): p-value = 0.08595 .

lmer.zn_ba.4 <- lmerTest::lmer(data_zn$zn_ba_cuberoot ~ clay + sand + LA_log.1 + log10(ph) + (1|zn_s_log10) + (1|covidence), data = data_zn)
anova(lmer.zn_ba.4)
## the significant p-value is:
# LA_log.1: p-value = 0.03888 *


# lmer.5 & lmer.6 : same than .3 and .4 but with sqrt tranformed data (.2)
lmer.zn_ba.5 <- lmerTest::lmer(data_zn$zn_ba_cuberoot ~ LA_sqrt.2 + SLA_sqrt.2 + LDMC_abs_sqrt.2 + log10(ph) + (1|zn_s_log10) + (1|covidence), data = data_zn)
anova(lmer.zn_ba.5)
## no significant p-value:
# SLA_log.1: p-value = 0.05958 .
# log10(ph): p-value = 0.08552 .

lmer.zn_ba.6 <- lmerTest::lmer(data_zn$zn_ba_cuberoot ~ clay + sand + LA_sqrt.2 + log10(ph) + (1|zn_s_log10) + (1|covidence), data = data_zn)
anova(lmer.zn_ba.6)
## the significant p-value is:
# LA_log.1: p-value = 0.04236 *


# lmer.7 & lmer.8 : same than .3 and .4 but with sqrt(log) tranformed data (.3)
lmer.zn_ba.7 <- lmerTest::lmer(data_zn$zn_ba_cuberoot ~ LA_sqrt_log.3 + SLA_sqrt_log.3 + LDMC_sqrt_abs_log.3 + log10(ph) + (1|zn_s_log10) + (1|covidence), data = data_zn)
anova(lmer.zn_ba.7)
## no significant p-value:
# SLA_sqrt_log.3: p-value = 0.05182 .
# log10(ph):      p-value = 0.08576 .

lmer.zn_ba.8 <- lmerTest::lmer(data_zn$zn_ba_cuberoot ~ clay + sand + LA_sqrt_log.3 + log10(ph) + (1|zn_s_log10) + (1|covidence), data = data_zn)
anova(lmer.zn_ba.8)
## the significant p-value is:
# LA_sqrt_log.3:  p-value = 0.03839 *


# lmer.9 & lmer.10 : same than .3 and .4 but with cuberoot tranformed data (.4)
lmer.zn_ba.9 <- lmerTest::lmer(data_zn$zn_ba_cuberoot ~ LA_cuberoot.4 + SLA_cuberoot.4 + LDMC_cuberoot.4 + log10(ph) + (1|zn_s_log10) + (1|covidence), data = data_zn)
anova(lmer.zn_ba.9)
## no significant p-value:
# SLA_cuberoot.4:   p-value = 0.05672 .
# log10(ph):        p-value = 0.08567 .

lmer.zn_ba.10 <- lmerTest::lmer(data_zn$zn_ba_cuberoot ~ clay + sand + LA_cuberoot.4 + log10(ph) + (1|zn_s_log10) + (1|covidence), data = data_zn)
anova(lmer.zn_ba.10)
## the significant p-value is:
# LA_cuberoot.4:  p-value = 0.04116 *


### BEA: attention, ici tes log() et log10() ne te donnais pas tout a fait les meme resultats alors garde le log10()
### aussi, je ne suis pas certaine de pourquoi il y a la valeur ba_leaf et stem? Nous n'Avons pas homogénéiser les unités de la biomasse alors nous ne pouvons pas l'utiliser

### OK j'ai gardé le log10(ph) et log10(zn_s)
### j'avais oublié que ba_leaf et stem ne sont pas homogénéisées. je les ai enlevées alors


#### Assumption verification for lmer.zn_ba #### 

# Assumptions verification for lmer.zn_ba.1

# homogeneity of variance ?
plot(resid(lmer.zn_ba.1) ~ fitted(lmer.zn_ba.1)) # looks like a heteroscedastic dispersion
gqtest(lmer.zn_ba.1, order.by = ~ LA_log.1 + SLA_log.1 + LDMC_abs_log.1 + log10(ph) + (1|zn_s_log10) + (1|covidence), data = data_zn, fraction = 8)


str(lmer.zn_ba.1)
lmer.zn_ba.1@resp$mu

plot(resid(lmer.zn_ba.1)) # mostly random points
plot(lmer.zn_ba.1) # 
plot(data_zn$zn_ba_cuberoot~data_zn$SLA_log)

# normal distribution of residuals ?
qqmath(lmer.zn_ba.1, id = 0.05) # points are mostly in line so respected normality (outliers?) # no, look find to be
shapiro.test(resid(lmer.zn_ba.1)) # normal distribution, # p-value = 0.08066
hist(resid(lmer.zn_ba.1)) # seems like normal distribution, with one missing column in histogram


# Assumptions verification for lmer.zn_ba.2

# homogeneity of variance ?
plot(resid(lmer.zn_ba.2) ~ fitted(lmer.zn_ba.2)) # looks like a heteroscedastic dispersion

# normal distribution of residuals ?
shapiro.test(resid(lmer.zn_ba.2)) # not normal distribution, # p-value = 1.051e-05
hist(resid(lmer.zn_ba.2)) # doesn't quite seem like normal distribution. 2 missing columns in histogram


# Assumptions verification for lmer.zn_ba.3

# homogeneity of variance ?
plot(resid(lmer.zn_ba.3) ~ fitted(lmer.zn_ba.3)) # looks like a heteroscedastic dispersion

# normal distribution of residuals ?
shapiro.test(resid(lmer.zn_ba.3)) # normal distribution, # p-value = 0.3242
hist(resid(lmer.zn_ba.3)) # seems like a normal distribution


# Assumptions verification for lmer.zn_ba.4

# homogeneity of variance ?
plot(resid(lmer.zn_ba.4) ~ fitted(lmer.zn_ba.4)) # looks like a heteroscedastic dispersion

# normal distribution of residuals ?
shapiro.test(resid(lmer.zn_ba.4)) # normal distribution (very borderline), # p-value = 0.06084
hist(resid(lmer.zn_ba.4)) # doesn't quite seem like normal distribution. 3 missing columns in histogram


# Assumptions verification for lmer.zn_ba.5

# homogeneity of variance ?
plot(resid(lmer.zn_ba.5) ~ fitted(lmer.zn_ba.5)) # looks like a heteroscedastic dispersion

# normal distribution of residuals ?
shapiro.test(resid(lmer.zn_ba.5)) # normal distribution, # p-value = 0.3199
hist(resid(lmer.zn_ba.5)) # seems like a normal distribution 


# Assumptions verification for lmer.zn_ba.6

# homogeneity of variance ?
plot(resid(lmer.zn_ba.6) ~ fitted(lmer.zn_ba.6)) # looks like a heteroscedastic dispersion

# normal distribution of residuals ?
shapiro.test(resid(lmer.zn_ba.6)) # almost not normal distribution (borderline), # p-value = 0.0826
hist(resid(lmer.zn_ba.6)) # doesn't quite seem like normal distribution. 3 missing columns in histogram


# Assumptions verification for lmer.zn_ba.7

# homogeneity of variance ?
plot(resid(lmer.zn_ba.7) ~ fitted(lmer.zn_ba.7)) # looks like a heteroscedastic dispersion

# normal distribution of residuals ?
shapiro.test(resid(lmer.zn_ba.7)) # normal distribution, # p-value = 0.3264
hist(resid(lmer.zn_ba.7)) # seems like a normal distribution


# Assumptions verification for lmer.zn_ba.8

# homogeneity of variance ?
plot(resid(lmer.zn_ba.8) ~ fitted(lmer.zn_ba.8)) # looks like a heteroscedastic dispersion

# normal distribution of residuals ?
shapiro.test(resid(lmer.zn_ba.8)) # almost not normal distribution, # p-value = 0.05826
hist(resid(lmer.zn_ba.8)) # doesn't quite seem like normal distribution. 3 missing columns in histogram


# Assumptions verification for lmer.zn_ba.9

# homogeneity of variance ?
plot(resid(lmer.zn_ba.9) ~ fitted(lmer.zn_ba.9)) # looks like a heteroscedastic dispersion

# normal distribution of residuals ?
shapiro.test(resid(lmer.zn_ba.9)) # normal distribution, # p-value = 0.3214
hist(resid(lmer.zn_ba.9)) # seems like a normal distribution


# Assumptions verification for lmer.zn_ba.10

# homogeneity of variance ?
plot(resid(lmer.zn_ba.10) ~ fitted(lmer.zn_ba.10)) # looks like a heteroscedastic dispersion

# normal distribution of residuals ?
shapiro.test(resid(lmer.zn_ba.10)) # almost not normal distribution, # p-value = 0.07475
hist(resid(lmer.zn_ba.10)) # doesn't quite seem like normal distribution. 3 missing columns in histogram


## None of the lmer have a homoscedastic dispersion, even with the transformed data



#### For zn_br ####

lmer.zn_br <- lmer(zn_br ~ LA_log + SLA_log + LDMC_log + zn_s + cec + ph + country + AccSpeciesName_cor + (1|covidence), data = data_std_salix)
lmer.zn_br <- lmer(zn_br ~ LA_log + SLA_log + LDMC_log + zn_s + ph + country + AccSpeciesName_cor + (1|covidence), data = data_std_salix)

anova(lmer.zn_br) # is something significant? with *
summary(lmer.zn_br)



#### cd only database ####

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





