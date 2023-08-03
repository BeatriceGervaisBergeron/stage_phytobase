# Analysis script

#### Import packages ####
library(vegan)
library(HH)
library('plotrix')
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

# normalize the traits
# data3.tr <- traits[,c('sp','Salix')]
salix_complete_2$LA_log <- log(salix_complete_2$LA)
salix_complete_2$LDMC_log <- logit(salix_complete_2$LDMC)
salix_complete_2$SLA_log <- log(salix_complete_2$SLA)

# remove HA & Salix columns
salix_complete_2 <- salix_complete_2[,-c(6:7)]

# join the traits to your data
data_std <- left_join(data_std, salix_complete_2, by=c('AccSpeciesName_cor' = 'sp'))

# remove the sp. that don't have LA, SLA and LDMC values 
# i.e. remove the lines in which LA values are 'NA'
data_std_salix <- filter(data_std, LA_log != 'NA') # 60 lines

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
cd_ba <- na.omit(data_clean$cd_ba)
cd_ba # 39 lines

# zn_ba
zn_ba <- na.omit(data_clean$zn_ba)
zn_ba # 38 lines

# pb_ba
pb_ba <- na.omit(data_clean$pb_ba)
pb_ba # 36 lines
# so pb is also one of the most abundant metals tested

# ni_ba
ni_ba <- na.omit(data_clean$ni_ba)
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

# add a column with log(zn_ba) data
data_zn$zn_ba_log <- log(data_zn$zn_ba)
data_zn$zn_ba_log

# LMM for zn_ba
lmer.zn_ba <- lmerTest::lmer(data_zn$zn_ba_log ~ log(ba_leaf) + log(ba_stem) + LA_log + SLA_log + LDMC_log + log(ph) + (1|zn_s) + (1|covidence), data = data_zn)
anova(lmer.zn_ba) 
## the significant p-values are:
# log(ba_leaf): p-value = 0.00443 **
# LA_log:       p-value = 0.05195 .
# SLA_log:      p-value = 0.03100 *
summary(lmer.zn_ba)


###BEA: attention, ici tes log() et log10() ne te donnais pas tout a fait les meme resultats alors garde le log10()
### aussi, je ne suis pas certaine de pourquoi il y a la valeur ba_leaf et stem? Nous n'Avons pas homogénéiser les unités de la biomasse alors nous ne pouvons pas l'utiliser

# Assumptions verification

# homogeneity of variance ?
plot(resid(lmer.zn_ba) ~ fitted(lmer.zn_ba)) # looks like a heteroscedastic dispersion

plot(resid(lmer.zn_ba)) # mostly random points
plot(lmer.zn_ba) # 
plot(data_zn$zn_ba^(1/3)~data_zn$SLA_log)

# normal distribution of residuals ?
qqmath(lmer.zn_ba, id = 0.05) # points are mostly in line so respected normality (outliers?) # no, look find to be
shapiro.test(resid(lmer.zn_ba)) # normal distribution, # p-value = 0.8321
hist(resid(lmer.zn_ba)) # seems like normal distribution, with one missing column in histogram






# For zn_br
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





# For pb_ba





# For pb_br




#### PCA of willows vs all species ####





