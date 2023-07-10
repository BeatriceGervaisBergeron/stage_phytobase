# Analysis script

#### Import packages ####
library(vegan)
library(HH)
library('plotrix')
library(lmerTest)
library(dplyr)
library(tidyr)
library(stringr)
install.packages('lmerTest')

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
salix_complete_2$SLA_log <- log(salix_complete_2$SLA)
salix_complete_2$LDMC_log <- logit(salix_complete_2$LDMC)

# remove HA & Salix columns
salix_complete_2 <- salix_complete_2[,-c(6:7)]

# join the traits to your data
data_std <- left_join(data_std, salix_complete_2, by=c('AccSpeciesName_cor' = 'sp'))

# remove the sp. that don't have LA, SLA and LDMC values 
# i.e. remove the lines in which LA values are 'NA'
data_std_salix <- filter(data_std, LA_log != 'NA')


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
data_clean <- data_std[,c('as_ba',	'cd_ba',	'cu_ba',	'pb_ba',	'zn_ba',	'se_ba',	'ni_ba',	'co_ba',	'mn_ba',	'cr_ba',	'hg_ba', 'LA' , 'SLA' ,'LDMC' , 'ph' ,'AccSpeciesName_cor', 'covidence')]
data_clean <- na.omit(data_std[,c('cd_ba','zn_ba','LA' , 'SLA' ,'LDMC' , 'ph' ,'AccSpeciesName_cor', 'covidence')])


#### Influence of traits and environmental factor on TE accumulation- Linear mix model (LMM) ####

# zinc only database
data_zn <- data_std_salix %>% filter(!is.na(zn_ba))

# check normality of zn_ba
par(mfrow = c(2,3))
hist(data_zn$zn_ba)
hist(log(data_zn$zn_ba))
hist(log10(data_zn$zn_ba))
hist(log2(data_zn$zn_ba))
hist(logit(data_zn$zn_ba))
hist(sqrt(data_zn$zn_ba))
hist(data_zn$zn_ba^(1/3))# best transformation

# check normality of zn_s
par(mfrow = c(2,3))
hist(data_zn$zn_s)
hist(log(data_zn$zn_s))
hist(log10(data_zn$zn_s))# best transformation
hist(log2(data_zn$zn_s)) # second best
hist(logit(data_zn$zn_s))
hist(sqrt(data_zn$zn_s))
hist(data_zn$zn_s^(1/3))
hist(asin(sqrt(data_zn$zn_s)))
hist(decostand(data_zn$zn_s, method = 'log', MARGIN = 2))


# check normality of ph
par(mfrow = c(2,3))
hist(data_zn$ph)
hist(log(data_zn$ph))
hist(log10(data_zn$ph))# best transformation
hist(log2(data_zn$ph)) # second best
hist(logit(data_zn$ph))
hist(sqrt(data_zn$ph))
hist(data_zn$ph^(1/3))
hist(asin(sqrt(data_zn$ph)))
hist(decostand(data_zn$ph, method = 'log', MARGIN = 2))


data_zn$zn_ba_log <- log(data_zn$zn_ba)
# For zn_ba
lmer.zn_ba <- lmerTest::lmer(data_zn$zn_ba^(1/3) ~ LA_log + SLA_log + LDMC_log  + (1|zn_s)  + (1|covidence), data = data_zn)
anova(lmer.zn_ba) # is something significant? with *
summary(lmer.zn_ba)

plot(data_zn$zn_ba^(1/3)~data_zn$SLA_log)
unique(data_zn$AccSpeciesName_cor)


# Assumptions verification
plot(resid(lmer.zn_ba),data_zn$zn_ba) # 
plot(lmer.zn_ba) # 
qqmath(lmer.zn_ba, id=0.05) # points are mostly in line so respected normality (outliers?)
shapiro.test(resid(lmer.zn_ba)) # normal, p-value = 0.788

hist(resid(lmer.zn_ba))


# For zn_br
lmer.zn_br <- lmer(zn_br ~ LA_log + SLA_log + LDMC_log + zn_s + cec + ph + country + AccSpeciesName_cor + (1|covidence), data = data_std_salix)
lmer.zn_br <- lmer(zn_br ~ LA_log + SLA_log + LDMC_log + zn_s + ph + country + AccSpeciesName_cor + (1|covidence), data = data_std_salix)

anova(lmer.zn_br) # is something significant? with *
summary(lmer.zn_br)



# For cd_ba
lmer.cd_ba <- lmer(cd_ba ~ LA_log + SLA_log + LDMC_log + cd_br + cd_s + ph + country + AccSpeciesName_cor + (1|covidence), data = data_std_salix)
anova(lmer.cd_ba)
summary(lmer.cd_ba)

plot(resid(lmer.cd_ba),data_std_salix$cd_ba) # 
plot(lmer.cd_ba) # 
qqmath(lmer.cd_ba, id=0.05) # points are mostly in line so respected normality (outliers?)
shapiro.test(resid(lmer.cd_ba)) # borderline normal, p-value = 0.05233


# For 





##### PCA of willows vs all species ####





