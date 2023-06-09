# Analysis script

library(vegan)
library(HH)


#### call your data  ####
data_std <- read.csv('./Pei yin/soil_sp_database_Pei_Yin_std.csv', sep=',',header = T, dec = '.')

## join the traits to your data 
traits <- readRDS('./complete_data.rds')
data_std <- left_join(data_std , traits, by=c('AccSpeciesName_cor'='sp'))


##### PCA of willows vs all species ####


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
     , ylim = c(-2, 10)
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





#### RDA, correlations between multiple variables #### EN construction

# make a matrix only of Te concnetrations in leaves
TE_leaves <- data_std[,c('as_ba',	'cd_ba',	'cu_ba',	'pb_ba',	'zn_ba',	'se_ba',	'ni_ba',	'co_ba',	'mn_ba',	'cr_ba',	'hg_ba')]
# modified all caracter variable into factorial
data_std <- data_std %>%
  mutate(
    AccSpeciesName_cor = as.factor(AccSpeciesName_cor)
    , country = as.factor(country))

# if need no NA
data_clean <-data_std[,c('as_ba',	'cd_ba',	'cu_ba',	'pb_ba',	'zn_ba',	'se_ba',	'ni_ba',	'co_ba',	'mn_ba',	'cr_ba',	'hg_ba', 'LA' , 'SLA' ,'LDMC' , 'ph' , 'country' ,'AccSpeciesName_cor', 'covidence')]
data_clean <- na.omit(data_std[,c('cd_ba','zn_ba','LA' , 'SLA' ,'LDMC' , 'ph' , 'country' ,'AccSpeciesName_cor', 'covidence')])
TE_leaves <- data_clean[,c(	'cd_ba',	'zn_ba')]

# rda of the differnt TE accumulation in functions of traits, covariable and controlling for the covariability of each study (covidence ID)
rda<-rda(TE_leaves ~ LA + SLA +LDMC + ph + country + AccSpeciesName_cor + Condition(covidence), data = data_clean, na.action = na.omit, scale=TRUE)# significant ?
anova(rda, permutation=how(nperm=99999, block=data.std$covidence)) # F= 1.56
anova(rda, permutation=how(nperm=99999, block=data.std$covidence), by='axis') # first axe significant
RsquareAdj(rda)$adj.r.squared # R2 = 0.25 
summary(rda)# axe 1= 23%, axe 2= 17% 


#plot RDA (En construction)
#png("figures/RDA_full.png", width=2300, height=2200, pointsize=72) # to save your figure in a figure folder
rdap<-rda(TE_leaves ~ LA + SLA +LDMC  , data=data_clean, scale=TRUE)
palette(brewer.pal(n = 8, name = "Set2"))# color choice
plot(rdap
     ,type='n'
     ,scaling=2, display=c('sp','cn', 'sites')
     ,xlab=' RDA 1 (?%)'
     ,ylab='RDA 2 (?%)'
     ,ylim=c(-1,1)
     ,xlim=c(-0.5,1.2)
     ,cex.lab=1
)
col <- rep(c(4,1,5,6,2,3),each=8)
po <- rep(c(15,17,18,1,16,25),each=8)
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




#### linear models ####
# there is also the option to do alinear model
# With a linear model, only one variable is allowed, so we will do a TE index of all TE together

# Exemple of what it would be
lmer<-lmer(TE ~ LA + SLA +LDMC + pH + country + species + (1|covidence), data=data.std)
anova(lmer)# is something significant? with *
# Assumptions verification
plot(resid(lmer),data.std$TE )# mostly random points, but slightly in linear position
plot(lmer)# mostly random points so respected variance homogeneity
qqmath(lmer, id=0.05)# points are mostly in line so respected normality (outliers?)
shapiro.test(resid(lmer))# normal and not better with log transformation


