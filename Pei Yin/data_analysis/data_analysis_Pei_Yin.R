# Analysis script

#### Import packages ####
library(vegan)
library(HH)
library('plotrix')
library(lme4)
library(dplyr)
library(tidyr)
library(stringr)


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

# Example of a full model selection
lmer.1 <- lmer(zn_ba ~ LA + SLA + LDMC + zn_br + ph + country + AccSpeciesName_cor + (1|covidence), data = data_std)
anova(lmer) # is something significant? with *

# Assumptions verification
plot(resid(lmer),data_std$zn_ba ) # mostly random points, but slightly in linear position
plot(lmer)# mostly random points so respected variance homogeneity
qqmath(lmer, id=0.05)# points are mostly in line so respected normality (outliers?)
shapiro.test(resid(lmer))# normal and not better with log transformation




##### PCA of willows vs all species ####







