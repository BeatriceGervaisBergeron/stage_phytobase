# Analysis script template

### Step

# Import packages
# Import data
# Matrix of functional traits
  # Distribution and normality
  # Transformation
# Matrix of TE accumulation and environmental factors
  # Distribution and normality
  # Transformation
  # TE scores
# Illustrating the data
  # PCA of willow vs all species
  # PCA of willow traits
  # RDA of all factors
# Influence of traits and environmental factor on TE accumulation- Linear mix model (LMM)
  # model selection 



#### Import packages ####
library(vegan)
library(HH)
library('plotrix')
library(lme4)
library(dplyr)
library(tidyr)
library(stringr)
<<<<<<< HEAD
=======
library(lmerTest)
>>>>>>> e22fb95709d5d5ec8b755fc3b5f682baa4e2979f

#### Import data ####
data_std <- read.csv('./Pei yin/data_cleaning_final/data_std_cleaned.csv', sep=',',header = T, dec = '.')

## join the traits to your data 
traits <- readRDS('./complete_data.rds')
data_std <- left_join(data_std , traits, by=c('AccSpeciesName_cor'='sp'))

#### Matrix of functional traits ####
# Distribution and normality
# Transformation

#### Matrix of TE accumulation and environmental factors ####
# Distribution and normality
# Transformation

# matrix of useful variables
# modified all character variable into factorial
data_std <- data_std %>%
  mutate(
    AccSpeciesName_cor = as.factor(AccSpeciesName_cor)
    , country = as.factor(country))

# if need no NA
data_clean <-data_std[,c('as_ba',	'cd_ba',	'cu_ba',	'pb_ba',	'zn_ba',	'se_ba',	'ni_ba',	'co_ba',	'mn_ba',	'cr_ba',	'hg_ba', 'LA' , 'SLA' ,'LDMC' , 'ph' ,'AccSpeciesName_cor', 'covidence')]
data_clean <- na.omit(data_std[,c('cd_ba','zn_ba','LA' , 'SLA' ,'LDMC' , 'ph' ,'AccSpeciesName_cor', 'covidence')])
## Cd and Zn are th emost abundant metal tested (without NA)
TE_leaves <- data_clean[,c(	'cd_ba',	'zn_ba')]


# TE scores

### 1- score of TE accumulation

# make a matrix only of TE concentrations in leaves
TE_leaves <- data_std %>% 
  filter(organs_ba == c('leaves'))
TE_leaves <- TE_leaves[,c('as_ba',	'cd_ba',	'cu_ba',	'pb_ba',	'zn_ba',	'se_ba',	'ni_ba',	'co_ba',	'mn_ba',	'cr_ba',	'hg_ba')]
# make a score of all TE
TE_leaves_std <-decostand(TE_leaves, method='standardize', na.rm = T)
TE_leaves_std$ba_total <- apply(TE_leaves_std, MARGIN = 1, FUN = mean, )# need to omit the NA from the analysis


#### PCA of willow vs all species ####

# identify the willow on the global range
#willow
traits$Salix <-1
salix_complete <-traits %>% filter(str_detect(sp, 'Salix')) # 27 sp
traits$Salix[traits$sp %in% salix_complete$sp] <-2
traits <-traits %>% arrange(Salix)

#normalise the traits
data3.tr <- traits[,c('sp','Salix')]
data3.tr$LA <- log(traits$LA)
data3.tr$SLA <- log(traits$SLA)
data3.tr$LDMC <- logit(traits$LDMC)

# standardized the data to make them comparable
traits.s<-decostand(data3.tr[,c('LA','SLA','LDMC')], method='standardize', MARGIN=2)

# plot the pca
pca <- rda(traits.s)
library(RColorBrewer)
palette(brewer.pal(n = 4, name = "Set2"))
plot(pca
     , type = 'n'
     ,xlab='PC 1 (49%)'
     ,ylab='PC 2 (33%)'
     ,scaling= 2
     , xlim = c(-5.5, 5.5)
     , ylim = c(-1, 8)
     )
text(scores(pca, display="species", choices=c(1), scaling=2),
     scores(pca, display="species", choices=c(2), scaling=2),
     labels=rownames(scores(pca, display="species", scaling=2)),
     col="black", cex=0.8)       

fleche<-scores(pca,choices=1:2,scaling=2,display="sp")
arrows(0,0,fleche[,1],fleche[,2],length=0, col='black')
points(scores(pca, display="sites", choices=c(1), scaling=2),
       scores(pca, display="sites", choices=c(2), scaling=2),
       col=traits$Salix, cex=0.5, pch=c(1,15)[as.numeric(traits$Salix)])



##### PCA of willow traits ####

# normalise the traits
salix_traits <- salix_complete
salix_traits$LA <- log(salix_traits$LA)
salix_traits$SLA <- log(salix_traits$SLA)
salix_traits$LDMC <- logit(salix_traits$LDMC)

# standardized the data to make them comparable
salix_traits.s<-decostand(salix_traits[,c('LA','SLA','LDMC')], method='standardize', MARGIN=2)

# plot the pca
pca <- rda(salix_traits.s)
library(RColorBrewer)
palette(brewer.pal(n = 4, name = "Set2"))
png(filename = './Pei Yin/data_analysis/figures/pca_salix.png' )
plot(pca
     , type = 'n'
     ,xlab='PC 1 (49%)'
     ,ylab='PC 2 (33%)'
     ,scaling= 2
     , xlim = c(-2, 4)
     , ylim = c(-1.5, 1.5)
)
text(scores(pca, display="species", choices=c(1), scaling=2),
     scores(pca, display="species", choices=c(2), scaling=2),
     labels=rownames(scores(pca, display="species", scaling=2)),
     col="black", cex=0.8)       

fleche<-scores(pca,choices=1:2,scaling=2,display="sp")
arrows(0,0,fleche[,1],fleche[,2],length=0, col='black')
text(scores(pca, display="sites", choices=c(1), scaling=2),
       scores(pca, display="sites", choices=c(2), scaling=2) )
# table of species
salix_traits$nb <-1:27
salix_sp <- salix_traits[,c('nb','sp')]
addtable2plot(2.5,-2.7,salix_sp, bty = "o", display.colnames = F, title = 'Legend', display.rownames = F, hlines = F,
              vlines = F, cex= 0.7, xpad=.3, ypad = .7)

dev.off()

#### RDA of all factors #### 


# rda of the differnt TE accumulation in functions of traits, covariable and controlling for the covariability of each study (covidence ID)
rda<-rda(TE_leaves ~ LA + SLA +LDMC + ph + Condition(covidence), data = data_clean, na.action = na.omit, scale=TRUE)# significant ?
anova(rda, permutation=how(nperm=99999, block=data_clean$covidence)) # F= 9,14 , significatif
anova(rda, permutation=how(nperm=99999, block=data_clean$covidence), by='axis') # first axe significant
RsquareAdj(rda)$adj.r.squared # R2 = 0.55 
summary(rda)# axe 1= 23%, axe 2= 17% 


#plot RDA (En construction)
#png("figures/RDA_full.png", width=2300, height=2200, pointsize=72) # to save your figure in a figure folder
rdap<-rda(TE_leaves ~ LA + SLA +LDMC +ph  , data=data_clean, scale=T)
palette(brewer.pal(n = 8, name = "Set2"))# color choice
plot(rdap)


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


# Example ofa full model selection
lmer<-lmer(zn_ba ~ LA + SLA + LDMC + ph + country + AccSpeciesName_cor + (1|covidence), data=data_std)
anova(lmer)# is something significant? with *
# Assumptions verification
plot(resid(lmer),data_std$zn_ba )# mostly random points, but slightly in linear position
plot(lmer)# mostly random points so respected variance homogeneity
qqmath(lmer, id=0.05)# points are mostly in line so respected normality (outliers?)
shapiro.test(resid(lmer))# normal and not better with log transformation

# model selection 

# test different models and interaction
# interactions (all non significant)
glm <- glm(HA ~ LA*LDMC*SLA, data=data3.tr, family = binomial )#AIC 258.27
summary(glm)# nothing significant
glm <- glm(HA ~ LA*LDMC, data=data3.tr, family = binomial )#AIC 251.22
summary(glm)# nothing significant
glm <- glm(HA ~ LA*SLA, data=data3.tr, family = binomial )#AIC 253.51
summary(glm)# nothing significant

# model creation of all options
glm3 <-glm(HA ~ LDMC+LA+SLA, data=data3.tr, family = binomial )#AIC 251
summary(glm3) # LA sign and LDMC 0.11
glm21 <-glm(HA ~ LDMC+LA, data=data3.tr, family = binomial )#AIC 249.48
summary(glm21) # LA sign and LDMC 0.56
glm22 <-glm(HA ~ SLA+LDMC, data=data3.tr, family = binomial )#AIC 258
summary(glm22) # LDMC sign
glm23 <-glm(HA ~ SLA+LA, data=data3.tr, family = binomial )#AIC 258
summary(glm23) # LA sign
glmLA <-glm(HA ~ LA, data=data3.tr, family = binomial )#AIC 251
summary(glmLA) # LA sign 
glmLDMC <-glm(HA ~ LDMC, data=data3.tr, family = binomial )#AIC 256
summary(glmLDMC) # LDMC sign
glmSLA <-glm(HA ~ SLA, data=data3.tr, family = binomial )#AIC 260
summary(glmSLA) # SLA non sign

#comparing models
# LRT test between models
anova(glm3, glm21, glm22, glm23, glmLA, glmLDMC, glmSLA, test='LRT') 
# AIC table
aictab(cand.set = list(glm3, glm21, glm22, glm23, glmLA, glmLDMC, glmSLA), modnames = c('LDMC+LA+SLA', 'LDMC+LA', 'SLA+LDMC','SLA +LA','LA','LDMC','SLA'))
# significant difference between the best model and the full model
anova(glm3, glm21, test='LRT')# not significant
# significant difference between the two best model 
anova(glm21, glmLA, test='LRT')# 0.057 not significant, but close. Meaning LA alone can almost be as good




