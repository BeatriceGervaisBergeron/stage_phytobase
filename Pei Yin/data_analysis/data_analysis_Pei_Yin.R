# Analysis script


#### Planing of script ####

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
library('lmerTest')
library(dplyr)
library(tidyr)
library(stringr)
library('MuMIn')
library('ggpubr')


#### 2. Import data ####
data_std <- read.csv('./Pei Yin/data_cleaning_final/data_std_cleaned.csv', sep=',',header = T, dec = '.')
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


#### 3. Verify normality of Salix traits & transform data if needed ####

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


# check how many lines of data there are for some TE (cd, zn)
# cd_ba
cd_ba <- na.omit(data_std_salix$cd_ba)
cd_ba # 39 lines

# zn_ba
zn_ba <- na.omit(data_std_salix$zn_ba)
zn_ba # 38 lines



#### 5. Linear mixed model (LMM) - Zn only database ####
data_zn <- data_std_salix %>% filter(!is.na(zn_ba)) # 38 obs.

##### 5a. Verify normality of env. variables and [Zn TE], & transform data if needed #####

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
# Then see influence of traits on zn_ba (while taking envir. variables as control)


##### 5c. For lmer.zn_ba_env ####

## For lmer.zn_ba_env: model for environmental significant variables
lmer.zn_ba_env <- lmer(data_zn$zn_ba_cuberoot ~ zn_s_log10 + ph + sand + clay_log2 + (1|covidence) + (1|AccSpeciesName_cor), data = data_zn)
summary(lmer.zn_ba_env)
# zn_s_log10 is significant (p-value = 0.000371 ***)
r.squaredGLMM(lmer.zn_ba_env)
# R2 = 0.185


# Assumptions verification for lmer.zn_ba_env

# Normality (Shapiro-Wilk test)
shapiro.test(resid(lmer.zn_ba_env)) # normal distribution (p-value = 0.1852)

# Homoscedasticity (verify pattern in plot)
plot(resid(lmer.zn_ba_env) ~ fitted(lmer.zn_ba_env))
plot(lmer.zn_ba_env) # mostly random points, looks homoscedastic



##### 5d. For lmer.zn_ba ####

## For lmer.zn_ba: model of traits and environmental controls
lmer.zn_ba <- lmer(data_zn$zn_ba_cuberoot ~ LA_log + SLA + LDMC + (1|ph) + (1|zn_s_log10) + (1|covidence) + (1|AccSpeciesName_cor), data = data_zn)
summary(lmer.zn_ba)
## the significant p-values are:
# SLA:    p-value = 0.0492 *
r.squaredGLMM(lmer.zn_ba)
# R2 = 0.024

# Assumptions verification for lmer.zn_ba

# Normality (Shapiro-Wilk test)
shapiro.test(resid(lmer.zn_ba)) # normal distribution (p-value = 0.7236)

# Homoscedasticity (verify pattern in plot)
plot(resid(lmer.zn_ba) ~ fitted(lmer.zn_ba))
plot(lmer.zn_ba) # funnel shape, seems heteroscedastic 



#### 6. Linear Mixed Model (LMM) - Cd only database ####
data_cd <- data_std_salix %>% filter(!is.na(cd_ba)) # 39 obs.

##### 6a. Verify normality of env. variables and [Cd TE], & transform data if needed #####

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
# Then see influence of traits on cd_ba (while taking envir. variables as control)


##### 6c. For lmer.cd_ba_env #####
## See influence of envir. variables on cd_ba

## For lmer.cd_ba_env: model for environmental significant variables
lmer.cd_ba_env <- lmer(data_cd$cd_ba_log10 ~ cd_s_log10 + ph + sand + clay + (1|covidence) + (1|AccSpeciesName_cor), data = data_cd)
summary(lmer.cd_ba_env)
# the significant p-values are:
# cd_s_log10  2.38e-05 ***
# ph          0.00486 **
r.squaredGLMM(lmer.cd_ba_env)
# R2 = 0.315


# Assumptions verification for lmer.cd_ba_env

# Normality (Shapiro-Wilk test)
shapiro.test(resid(lmer.cd_ba_env)) # not normal distribution (p-value = 0.04552)

# Homoscedasticity (verify pattern in plot)
plot(resid((lmer.cd_ba_env)) ~ fitted((lmer.cd_ba_env)))
plot((lmer.cd_ba_env)) # seems like random points, homoscedastic


##### 6d. For lmer.cd_ba #####
## See influence of traits on cd_ba (with envir controls)

## For lmer.cd_ba : model of traits and environmental controls
lmer.cd_ba <- lmer(cd_ba_log10 ~ LA_log + SLA + LDMC + (1|cd_s_log10) + (1|ph) + (1|covidence) + (1|AccSpeciesName_cor), data = data_cd)
summary(lmer.cd_ba) # no significant p-values (SLA: p-value = 0.0651)
r.squaredGLMM(lmer.cd_ba)
# R2 = 0.020

# Assumptions verification for lmer.cd_ba

# Normality (Shapiro-Wilk test)
shapiro.test(resid(lmer.cd_ba)) # not normal distribution (p-value = 0.000106)
hist(resid(lmer.cd_ba))

# Homoscedasticity (verify pattern of plot) 
plot(resid(lmer.cd_ba) ~ fitted(lmer.cd_ba))
plot(lmer.cd_ba) # looks like a funnel shape, seems heteroscedastic



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
# 23 obser
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
# Simple/basic plots
plot(zn_ba ~ LA_log, data = data_zn)
plot(zn_ba ~ SLA, data = data_zn)
plot(zn_ba ~ LDMC, data = data_zn)
plot(cd_ba ~ LA_log, data = data_cd)
plot(cd_ba ~ SLA, data = data_cd)
plot(cd_ba ~ LDMC, data = data_cd)
plot(pb_ba ~ LA_log, data = data_pb)
plot(pb_ba ~ SLA, data = data_pb)
plot(pb_ba ~ LDMC, data = data_pb)


##### 10a. Plot (zn_ba ~ LA_log) with ggplot2 #####
plot_1 <- ggplot(data = data_zn) + # database of zn
  geom_point(aes(color = AccSpeciesName_cor, # légende de couleurs selon les spp.
                 x = LA_log, # x axis
                 y = zn_ba)) + # y axis
  theme_bw() + # white background and gray grid lines
  labs(titre = NULL, # remove title in the plot, since in the report, the title will be written with Word
       x = "log(LA)", # x axis name
       y = "[Zn] dans les parties aériennes") + # y axis name
  theme(legend.position = "none") # remove legend

plot_1 # show plot


##### 10b. Plot (zn_ba ~ SLA) with ggplot2 #####
plot_2 <- ggplot(data = data_zn) + # database of zn
  geom_point(aes(color = AccSpeciesName_cor, # légende de couleurs selon les spp.
                 x = SLA, # x axis
                 y = zn_ba)) + # y axis
  theme_bw() + # white background and gray grid lines
  labs(titre = NULL, # remove title in the plot, since in the report, the title will be written with Word
       x = "SLA", # x axis name
       y = "[Zn] dans les parties aériennes") + # y axis name
  theme(legend.position = "none") # remove legend

plot_2 # show plot


##### 10c. Plot (zn_ba ~ LDMC) with ggplot2 #####
plot_3 <- ggplot(data = data_zn) + # database of zn
  geom_point(aes(color = AccSpeciesName_cor, # légende de couleurs selon les spp.
                 x = LDMC, # x axis
                 y = zn_ba)) + # y axis
  theme_bw() + # white background and gray grid lines
  labs(titre = NULL, # remove title in the plot, since in the report, the title will be written with Word
       x = "LDMC", # x axis name
       y = "[Zn] dans les parties aériennes") + # y axis name
  theme(legend.position = "none") # remove legend

plot_3 # show plot


##### 10d. Plot (cd_ba ~ LA_log) with ggplot2 #####
plot_4 <- ggplot(data = data_cd) + # database of cd
  geom_point(aes(color = AccSpeciesName_cor, # légende de couleurs selon les spp.
                 x = LA_log, # x axis
                 y = cd_ba)) + # y axis
  theme_bw() + # white background and gray grid lines
  labs(titre = NULL, # remove title in the plot, since in the report, the title will be written with Word
       x = "log(LA)", # x axis name
       y = "[Cd] dans les parties aériennes") + # y axis name
  theme(legend.position = "none") # remove legend

plot_4 # show plot


##### 10e. Plot (cd_ba ~ SLA) with ggplot2 #####
plot_5 <- ggplot(data = data_cd) + # database of cd
  geom_point(aes(color = AccSpeciesName_cor, # légende de couleurs selon les spp.
                 x = SLA, # x axis
                 y = cd_ba)) + # y axis
  theme_bw() + # white background and gray grid lines
  labs(titre = NULL, # remove title in the plot, since in the report, the title will be written with Word
       x = "SLA", # x axis name
       y = "[Cd] dans les parties aériennes") + # y axis name
  theme(legend.position = "none") # remove legend

plot_5 # show plot


##### 10f. Plot (cd_ba ~ LDMC) with ggplot2 #####
plot_6 <- ggplot(data = data_cd) + # database of cd
  geom_point(aes(color = AccSpeciesName_cor, # légende de couleurs selon les spp.
                 x = LDMC, # x axis
                 y = cd_ba)) + # y axis
  theme_bw() + # white background and gray grid lines
  labs(titre = NULL, # remove title in the plot, since in the report, the title will be written with Word
       x = "LDMC", # x axis name
       y = "[Cd] dans les parties aériennes", # y axis name
       color = "Espèces") # legend name

plot_6 # show plot


##### 10g. Combine the plots on one figure #####

plot_combined <- ggarrange(plot_1, plot_2, plot_3, plot_4, plot_5, plot_6,
                    labels = c("a", "b", "c", "d", "e", "f", "g"),
                    ncol = 3, nrow = 2,
                    common.legend = TRUE)

plot_combined # show plot

# for an unknown reason, the legend title is "Acc_SpeciesName_cor"
# even though it was changed to "Espèces" in the plot_6 (which worked),
# but when combining plots together, the legend title reverted to its default title
# I tried to change it many times, but it still didn't work
# so leaving it like this for now


##### 10h. Save the combined plot #####

# Format PDF
ggsave("./Pei Yin/data_analysis/figures/plot_combined.pdf", # file name 
       plot_combined, # object name in R to save
       height = 8.5, width = 11, # size
       units = "in") # unit in inches

# Format PNG
ggsave("./Pei Yin/data_analysis/figures/plot_combined.png", # file name 
       plot_combined, # object name in R to save
       width = 11, # width in inches
       height = 8.5, # heigth in inches
       dpi = 1000) # resolution



#### 11. Anova of [TE] ~ species ####

###### 11a. anova of [zn_ba] ~ species ######

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

# see levels of Species
levels(data_zn_aov$AccSpeciesName_cor)
# [1] "Salix alba"      "Salix caprea"    "Salix gmelinii"  "Salix purpurea"  "Salix triandra" 
# [6] "Salix viminalis"

# change species factors into characters (to remove Salix triandra, Salix purpurea & Salix caprea)
data_zn_aov <- data_zn_aov %>%
  mutate(AccSpeciesName_cor = as.character(AccSpeciesName_cor))

# change species characters back into factors
data_zn_aov <- data_zn_aov %>%
  mutate(AccSpeciesName_cor = as.factor(AccSpeciesName_cor))

# see levels of Species (verify that only Salix alba, Salix gmelinii & Salix viminalis are left)
levels(data_zn_aov$AccSpeciesName_cor)
# [1] "Salix alba"      "Salix gmelinii"  "Salix viminalis"
# it has been changed, good
# there are 36 obs. in data_zn_aov


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



##### 11b. anova of [zn_br] ~ species #####

# remove data lines with NAs in zn_br
# to build data_zn_br database
data_zn_br <- data_std_salix %>% filter(!is.na(zn_br)) # 22 obs.

# see levels of Species
levels(data_zn_br$AccSpeciesName_cor)
# [1] "Salix alba"      "Salix caprea"    "Salix gmelinii"  "Salix purpurea"  "Salix triandra" 
# [6] "Salix viminalis"

# change species factors into characters (to remove Salix triandra, Salix purpurea & Salix caprea)
data_zn_br <- data_zn_br %>%
  mutate(AccSpeciesName_cor = as.character(AccSpeciesName_cor))

# change species characters back into factors
data_zn_br <- data_zn_br %>%
  mutate(AccSpeciesName_cor = as.factor(AccSpeciesName_cor))

# see levels of Species (verify that only Salix alba, Salix gmelinii & Salix viminalis are left)
levels(data_zn_br$AccSpeciesName_cor)
# [1] "Salix alba"      "Salix gmelinii"  "Salix viminalis"
# it has been changed, good
# there are 22 obs. in data_zn_br


# Build the anova model
zn_br.sp.aov <- aov(data_zn_br$zn_br ~ data_zn_br$AccSpeciesName_cor)

# Check normality (Shapiro test)
shapiro.test(resid(zn_br.sp.aov)) 
# distribution is not normal (p-value = 4.077e-05)

# Check homogeneity of variances with Bartlett test per permutation
source("./Pei Yin/data_analysis/bartlett.perm.R")

bartlett.perm(data_zn_br$zn_br, data_zn_br$AccSpeciesName_cor, centr = "MEDIAN",
              nperm = 999, alpha = 0.05)
#          Statistic Param.prob Permut.prob Bootstrap.prob
# Bartlett    0.5703     0.7519       0.611          0.625

# variances are homogeneous (p-value = 0.625)
# so a One-way anova with permutation can be executed

# One-way anova with permutation test
anova.1way(zn_br.sp.aov, nperm=999)
#                               Df     Sum Sq    Mean Sq   F value Prob(param) Prob(perm)
# data_zn_br$AccSpeciesName_cor  2   191036.7   95518.34 0.0757663   0.9273115      0.929
# Residuals                     19 23953239.3 1260696.80        NA          NA         NA

# no significant p-value (p-value = 0.929)



##### 11c. anova of [cd_ba] ~ species #####

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

# see levels of Species
levels(data_cd_aov$AccSpeciesName_cor)
# [1] "Salix alba"      "Salix caprea"    "Salix gmelinii"  "Salix purpurea"  "Salix triandra" 
# [6] "Salix viminalis"

# change species factors into characters (to remove Salix triandra, Salix purpurea & Salix caprea)
data_cd_aov <- data_cd_aov %>%
  mutate(AccSpeciesName_cor = as.character(AccSpeciesName_cor))

# change species characters back into factors
data_cd_aov <- data_cd_aov %>%
  mutate(AccSpeciesName_cor = as.factor(AccSpeciesName_cor))

# see levels of Species (verify that only Salix alba, Salix gmelinii & Salix viminalis are left)
levels(data_cd_aov$AccSpeciesName_cor)
# [1] "Salix alba"      "Salix gmelinii"  "Salix viminalis"
# it has been changed, good
# there are 37 obs. in data_cd_aov


# Build the anova model
cd_ba.sp.aov <- aov(data_cd_aov$cd_ba_log10 ~ data_cd_aov$AccSpeciesName_cor)

# Check normality (Shapiro test)
shapiro.test(resid(cd_ba.sp.aov)) 
# normal distribution (p-value = 0.7952)

# Check homogeneity of variances (Bartlett test)
bartlett.test(data_cd_aov$cd_ba_log10, data_cd_aov$AccSpeciesName_cor)
# homoscedastic (p-value = 0.4526)

summary(cd_ba.sp.aov) 
#                                 Df Sum Sq Mean Sq F value Pr(>F)
# data_cd_aov$AccSpeciesName_cor  2  1.252  0.6262   1.235  0.303
# Residuals                      34 17.238  0.5070               

# No significant p-value (p-value = 0.303)



##### 11d. anova of [cd_br] ~ species #####

# remove data lines with NAs in zn_br
# to build data_zn_br database
data_cd_br <- data_std_salix %>% filter(!is.na(cd_br)) # 23 obs.

# see levels of Species
levels(data_cd_br$AccSpeciesName_cor)
# [1] "Salix alba"      "Salix caprea"    "Salix gmelinii"  "Salix purpurea"  "Salix triandra" 
# [6] "Salix viminalis"

# change species factors into characters (to remove Salix triandra, Salix purpurea & Salix caprea)
data_cd_br <- data_cd_br %>%
  mutate(AccSpeciesName_cor = as.character(AccSpeciesName_cor))

# change species characters back into factors
data_cd_br <- data_cd_br %>%
  mutate(AccSpeciesName_cor = as.factor(AccSpeciesName_cor))

# see levels of Species (verify that only Salix alba, Salix gmelinii & Salix viminalis are left)
levels(data_cd_br$AccSpeciesName_cor)
# [1] "Salix alba"      "Salix gmelinii"  "Salix viminalis"
# it has been changed, good
# there are 23 obs. in data_cd_br


# Build the anova model
cd_br.sp.aov <- aov(data_cd_br$cd_br ~ data_cd_br$AccSpeciesName_cor)

# Check normality (Shapiro test)
shapiro.test(resid(cd_br.sp.aov)) 
# distribution is not normal (p-value = p-value = 9.437e-05)

# Check homogeneity of variances with Bartlett test per permutation
bartlett.perm(data_cd_br$cd_br, data_cd_br$AccSpeciesName_cor, centr = "MEDIAN",
              nperm = 999, alpha = 0.05)
#          Statistic Param.prob Permut.prob Bootstrap.prob
# Bartlett    0.0047     0.9976       0.998          0.994

# variances are homogeneous (p-value = 0.994)
# so an One-way anova with permutation can be executed


# One-way anova with permutation test
anova.1way(cd_br.sp.aov, nperm=999)
# $anova.table
#                               Df    Sum Sq   Mean Sq   F value Prob(param) Prob(perm)
# data_cd_br$AccSpeciesName_cor  2  1229.175  614.5876 0.4372453   0.6518401      0.654
# Residuals                     20 28111.799 1405.5900        NA          NA         NA

# No significant p-value (p-value = 0.654)



#### 12. barplots of species and TE ####

##### 12a. Simple barplots #####

# settings for new window
dev.new(noRStudioGD = TRUE) # opening a new window
par(mfrow = c(2,2))

# accumulation of zn per species
plot(zn_ba ~ AccSpeciesName_cor, data = data_zn_aov)
plot(zn_br ~ AccSpeciesName_cor, data = data_zn_br)

# accumulation of cd per species
plot(cd_ba ~ AccSpeciesName_cor, data = data_cd_aov)
plot(cd_br ~ AccSpeciesName_cor, data = data_cd_br)


##### 12b. Barplot of [zn_ba] ~ species #####

barplot_1 <- ggplot(data = data_zn_aov, aes(x = AccSpeciesName_cor, y = zn_ba)) + # data
  geom_boxplot() + # boxplot
  theme_bw() + # white background and gray grid lines
  labs(titre = NULL, # remove title in the plot, since in the report, the title will be written with Word
       x = "Espèces", # x axis name
       y = "[Zn] dans les parties aériennes") # y axis name

barplot_1 # view barplot


##### 12c. Barplot of [zn_br] ~ species #####

barplot_2 <- ggplot(data = data_zn_br, aes(x = AccSpeciesName_cor, y = zn_br)) + # data
  geom_boxplot() + # boxplot
  theme_bw() + # white background and gray grid lines
  labs(titre = NULL, # remove title in the plot, since in the report, the title will be written with Word
       x = "Espèces", # x axis name
       y = "[Zn] dans les parties aériennes") # y axis name

barplot_2 # view barplot


##### 12d. Barplot of [cd_ba] ~ species #####

barplot_3 <- ggplot(data = data_cd_aov, aes(x = AccSpeciesName_cor, y = cd_ba)) + # data
  geom_boxplot() + # boxplot
  theme_bw() + # white background and gray grid lines
  labs(titre = NULL, # remove title in the plot, since in the report, the title will be written with Word
       x = "Espèces", # x axis name
       y = "[Cd] dans les parties aériennes") # y axis name

barplot_3 # view barplot


##### 12e. Barplot of [cd_br] ~ species #####

barplot_4 <- ggplot(data = data_cd_br, aes(x = AccSpeciesName_cor, y = cd_br)) + # data
  geom_boxplot() + # boxplot
  theme_bw() + # white background and gray grid lines
  labs(titre = NULL, # remove title in the plot, since in the report, the title will be written with Word
       x = "Espèces", # x axis name
       y = "[Cd] dans les parties aériennes") # y axis name

barplot_4 # view barplot


##### 12f. Combine barplots in one image #####

barplot_combined <- ggarrange(barplot_1, barplot_2, barplot_3, barplot_4,
                           labels = c("a", "b", "c", "d"),
                           ncol = 2, nrow = 2)

barplot_combined # show barplot


##### 12g. Save the combined barplot #####

# Format PDF
ggsave("./Pei Yin/data_analysis/figures/barplot_combined.pdf", # file name 
       barplot_combined, # object name in R to save
       height = 8.5, width = 11, # size
       units = "in") # unit in inches

# Format PNG
ggsave("./Pei Yin/data_analysis/figures/barplot_combined.png", # file name 
       barplot_combined, # object name in R to save
       width = 11, # width in inches
       height = 8.5, # height in inches
       dpi = 1000) # resolution


