# Analysis script template
setwd("C:/Users/User/Documents/Scolaire/UDEM/Maîtrise/Hiver 2023/Stage phyto/stage_phytobase")
#### Import packages ####
library(HH)
library('plotrix')
library(lme4)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(lmerTest)
library(vegan)
library(RColorBrewer)

#### Import data ####
data <- read.csv('./Amélie/data_cleaning_final/data_std_cu_cleaned.csv', sep="\t", header = TRUE, dec = '.')

## join the traits to your data 
traits <- readRDS("./complete_data.rds")
data_std <- left_join(data , traits, by=c('AccSpeciesName_cor' = 'sp'))

# matrix of useful variables
# modified all character variable into factorial
data_std <- data_std %>%
  mutate(
    AccSpeciesName_cor = as.factor(AccSpeciesName_cor)
    , covidence = as.factor(covidence)
    , author = as.factor(author)
    , country = as.factor(country)
    , location = as.factor(location)
    , latitude = as.factor(latitude)
    , longitude = as.factor(longitude)
    , climate = as.factor(climate)
    , map_units = as.factor(map_units)
    , soil = as.factor(soil)
    , type = as.factor(type)
    , om_units = as.factor(om_units)
    , oc_units = as.factor(oc_units)
    , texture = as.factor(texture)
    , clay_units = as.factor(clay_units)
    , ec_units = as.factor(ec_units)
    , sand_units = as.factor(sand_units)
    , cec_units = as.factor(cec_units)
    , N_units = as.factor(N_units)
    , N_types = as.factor(N_types)
    , P_units = as.factor(P_units)
    , P_type = as.factor(P_type)
    , units_s = as.factor(units_s)
    , fraction_s = as.factor(fraction_s)
    , Bioavailable = as.factor(Bioavailable)
    , sp = as.factor(sp)
    , name = as.factor(name)
    , cultivar = as.factor(cultivar)
    , units_density = as.factor(units_density)
    , units_b = as.factor(units_b)
    , units_te_ba = as.factor(units_te_ba)
    , organs_ba = as.factor(organs_ba)
    , units_te_br = as.factor(units_te_br)
    , organs_br = as.factor(organs_br)
    , units_te_ba_1 = as.factor(units_te_ba_1)
    , organs_ba_1 = as.factor(organs_ba_1)
    , units_te_ba_2 = as.factor(units_te_ba_2)
    , organs_ba_2 = as.factor(organs_ba_2)
    , units_te_ba_3 = as.factor(units_te_ba_3)
    , organs_ba_3 = as.factor(organs_ba_3)
)

# remove the sp. that don't have LA, SLA and LDMC values 
# i.e. remove the lines in which LA values are 'NA'
data_std <- data_std %>%
  filter(!is.na(LA) & !is.na(SLA) & !is.na(LDMC))

##Standardisation
numeric_columns <- sapply(data, is.numeric)
data[numeric_columns] <- scale(data[numeric_columns])



# if need no NA
data_clean <-data_std[,c('as_ba',	'cd_ba',	'cu_ba',	'pb_ba',	'zn_ba',	'se_ba',	'ni_ba',	'co_ba',	'mn_ba',	'cr_ba',	'hg_ba', 'LA' , 'SLA' ,'LDMC' , 'ph' ,'AccSpeciesName_cor', 'covidence')]
data_clean <- na.omit(data_std[,c('cd_ba','zn_ba','LA' , 'SLA' ,'LDMC' , 'ph' ,'AccSpeciesName_cor', 'covidence')])
## Cd and Zn are the most abundant metal tested (without NA)
TE_leaves <- data_clean[,c(	'cd_ba',	'zn_ba')]

# TE scores
### 1- score of TE accumulation
# make a matrix only of TE concentrations in leaves
TE_leaves <- data_std %>%
  filter(organs_ba == "leaves") %>%
  select(cd_ba, zn_ba)

# make a score of all TE
TE_leaves_std <-decostand(TE_leaves, method='standardize', na.rm = T)
TE_leaves_std$ba_total <- apply(TE_leaves_std, MARGIN = 1, FUN = mean)


#### RDA of all factors #### 

# rda of the diffrent TE accumulation in functions of traits, covariable and controlling for the covariability of each study (covidence ID)
rda<-rda(TE_leaves ~ LA + SLA +LDMC + ph + Condition(covidence), data = data_clean, na.action = na.omit, scale=TRUE) 
anova(rda, permutation=how(nperm=99999, block=data_clean$covidence))
anova(rda, permutation=how(nperm=99999, block=data_clean$covidence), by='axis')
RsquareAdj(rda)$adj.r.squared
summary(rda)


#plot RDA (En construction)
#png("figures/RDA_full.png", width=2300, height=2200, pointsize=72) # to save your figure in a figure folder
rdap<-rda(TE_leaves ~ LA + SLA +LDMC +ph  , data=data_clean, scale=T)
scores_rda <- scores(rdap, display = "sites")
scores_df <- as.data.frame(scores_rda)

my_palette <- brewer.pal(n = 8, name = "Set2")
plot(rdap)

ggplot(scores_df, aes(x = RD1, y = RD2, color = LA)) +
  geom_point(size = 3) +
  scale_color_manual(values = my_palette) +
  theme_minimal()



pca <- rda(TE_leaves, scale=T)
plot(pca)

plot(rdap
     ,type='n'
     ,scaling=2, display=c('sp','cn', 'sites')
     ,xlab=' RDA 1 (?%)'
     ,ylab='RDA 2 (?%)'
     ,ylim=c(-1,1)
     ,xlim=c(-0.5,1.2)
     ,cex.lab=1
)
#col <- rep(c(4,1,5,6,2,3),each=8)
#po <- rep(c(15,17,18,1,16,25),each=8)
arrows(0,0,x1=-0.15, y1=0.6, lwd=3, length=0.05, angle=30, col='gray')
text(-0.15,0.68, 'LA', col='gray')
text(rdap, display='cn', scaling=2, col='black', cex=1, lwd=3)
# decide if you want point or letter to display your TE
#points(rdap, dis='sp', scaling=2, col=col, cex=1.4, pch=po, lwd=5)
label <- rep(c('mn','cu','zn','as','se','cd','ba','pb'), 6)
#legend('topright', legend =  c('PE','PS','TF','BCFt','BCFr','DC'), col=c(4,1,5,2,0,0), cex = 1 ,pch = c(15,17,18,16,1,25)) # to be adjust with TE
text(1.7,-0.95, bquote(R^2 == .(round(RsquareAdj(rda)$adj.r.squared, 2))), cex=0.9, pos=2)
text(1.7,-0.8, bquote(F == .(round(anova(rda, permutation=how(nperm=99999, block=data.std$covidence))$F[1], 2))), cex=0.9, pos=2)

dev.off()


#### Influence of traits and environmental factor on TE accumulation- Linear mix model (LMM) ####
# Example of a full model selection
lmer<-lmer(zn_ba ~ LA + SLA + LDMC + ph + country + AccSpeciesName_cor + (1|covidence), data=data_std)
anova(lmer)
# Assumptions verification
plot(resid(lmer),data_std$zn_ba )
plot(lmer)
qqmath(lmer, id=0.05)
shapiro.test(resid(lmer))

# model selection 
# test different models and interaction
# interactions (all non significant)
glm <- glm(HA ~ LA*LDMC*SLA, data=data3.tr, family = binomial )
summary(glm)
glm <- glm(HA ~ LA*LDMC, data=data3.tr, family = binomial )
summary(glm)
glm <- glm(HA ~ LA*SLA, data=data3.tr, family = binomial )
summary(glm)

# model creation of all options
glm3 <-glm(HA ~ LDMC+LA+SLA, data=data3.tr, family = binomial )
summary(glm3) 
glm21 <-glm(HA ~ LDMC+LA, data=data3.tr, family = binomial )
summary(glm21) 
glm22 <-glm(HA ~ SLA+LDMC, data=data3.tr, family = binomial )
summary(glm22) 
glm23 <-glm(HA ~ SLA+LA, data=data3.tr, family = binomial )
summary(glm23) 
glmLA <-glm(HA ~ LA, data=data3.tr, family = binomial )
summary(glmLA) 
glmLDMC <-glm(HA ~ LDMC, data=data3.tr, family = binomial )
summary(glmLDMC) 
glmSLA <-glm(HA ~ SLA, data=data3.tr, family = binomial )
summary(glmSLA) 

#comparing models
# LRT test between models
anova(glm3, glm21, glm22, glm23, glmLA, glmLDMC, glmSLA, test='LRT') 
# AIC table
aictab(cand.set = list(glm3, glm21, glm22, glm23, glmLA, glmLDMC, glmSLA), modnames = c('LDMC+LA+SLA', 'LDMC+LA', 'SLA+LDMC','SLA +LA','LA','LDMC','SLA'))
anova(glm3, glm21, test='LRT')
anova(glm21, glmLA, test='LRT')







#### Influence of traits and environmental factor on TE accumulation- Linear mix model (LMM) ####

# cu only database
data_cu <- data_std %>% filter(!is.na(cu_ba))


# check normality of cu_ba
par(mfrow = c(2,3))
hist(data_cu$cu_ba)
hist(log(data_cu$cu_ba)) # best transformation
hist(log10(data_cu$cu_ba))
hist(log2(data_cu$cu_ba))
hist(logit(data_cu$cu_ba))
hist(sqrt(data_cu$cu_ba))
hist(data_cu$cu_ba^(1/3))

# check normality of cu_s
par(mfrow = c(2,3))
hist(data_cu$cu_s)
hist(log(data_cu$cu_s))
hist(log10(data_cu$cu_s))# best transformation
hist(log2(data_cu$cu_s)) # second best
hist(logit(data_cu$cu_s))
hist(sqrt(data_cu$cu_s))
hist(data_cu$cu_s^(1/3))
hist(asin(sqrt(data_cu$cu_s)))
hist(decostand(data_cu$cu_s, method = 'log', MARGIN = 2))


# check normality of ph
par(mfrow = c(2,3))
hist(data_cu$ph)
hist(log(data_cu$ph)) # best transformation
hist(log10(data_cu$ph))
hist(log2(data_cu$ph))
hist(logit(data_cu$ph))
hist(sqrt(data_cu$ph))
hist(data_cu$ph^(1/3))
hist(asin(sqrt(data_cu$ph)))
hist(decostand(data_cu$ph, method = 'log', MARGIN = 2))


data_cu$cu_ba_log <- log(data_cu$cu_ba)
# For cu_ba
lmer.cu_ba <- lmerTest::lmer(data_cu$log(data_cu$cu_ba) ~ LA_log + SLA_log + LDMC_log  + (1|cu_s)  + (1|covidence), data = data_cu)
lmer.cu_ba <- lmerTest::lmer(log(cu_ba) ~ LA_log + SLA_log + LDMC_log  + (1|cu_s)  + (1|covidence), data = data_cu)
anova(lmer.cu_ba)
summary(lmer.cu_ba)

plot(data_cu$log(data_cu$cu_ba)~data_cu$SLA_log)
unique(data_cu$AccSpeciesName_cor)


# Assumptions verification
plot(resid(lmer.cu_ba),data_cu$cu_ba)
plot(lmer.cu_ba) # 
qqmath(lmer.cu_ba, id=0.05) 
shapiro.test(resid(lmer.cu_ba))

hist(resid(lmer.cu_ba))

# For cu_br
lmer.cu_br <- lmer(cu_br ~ LA_log + SLA_log + LDMC_log + cu_s + cec + ph + country + AccSpeciesName_cor + (1|covidence), data = data_std)
lmer.cu_br <- lmer(cu_br ~ LA_log + SLA_log + LDMC_log + cu_s + ph + country + AccSpeciesName_cor + (1|covidence), data = data_std)

anova(lmer.cu_br)
summary(lmer.cu_br)

