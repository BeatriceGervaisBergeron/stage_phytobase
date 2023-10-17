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
library(MuMIn)
library(corrplot)
library(pracma)
library(MASS)

#### Import data ####
data <- read.csv('./Amélie/data_cleaning_final/data_std_cu_cleaned.csv', sep="\t", header = TRUE, dec = '.')
# Transform data
data <- data %>%
  mutate(
    covidence = as.factor(covidence)
    , author = as.factor(author)
    , year = as.numeric(year)
    , country = as.factor(country)
    , location = as.factor(location)
    , latitude = as.factor(latitude)
    , longitude = as.factor(longitude)
    , climate = as.factor(climate)
    , expe_t = as.numeric(expe_t)
    , mat = as.numeric(mat)
    , map = as.numeric(map)
    , map_units = as.factor(map_units)
    , soil = as.factor(soil)
    , type = as.factor(type)
    , ph = as.numeric(ph)
    , om = as.numeric(om)
    , om_units = as.factor(om_units)
    , oc = as.numeric(oc)
    , oc_units = as.factor(oc_units)
    , texture = as.factor(texture)
    , clay = as.numeric(clay)
    , clay_units = as.factor(clay_units)
    , ec = as.numeric(ec)
    , ec_units = as.factor(ec_units)
    , sand = as.numeric(sand)
    , sand_units = as.factor(sand_units)
    , cec = as.numeric(cec)
    , cec_units = as.factor(cec_units)
    , N = as.numeric(N)
    , N_units = as.factor(N_units)
    , N_types = as.factor(N_types)
    , P = as.numeric(P)
    , P_units = as.factor(P_units)
    , P_type = as.factor(P_type)
    , as_s = as.numeric(as_s)
    , cd_s = as.numeric(cd_s)
    , cu_s = as.numeric(cu_s)
    , pb_s = as.numeric(pb_s)
    , zn_s = as.numeric(zn_s)
    , se_s = as.numeric(se_s)
    , ni_s = as.numeric(ni_s)
    , co_s = as.numeric(co_s)
    , mn_s = as.numeric(mn_s)
    , cr_s = as.numeric(cr_s)
    , hg_s = as.numeric(hg_s)
    , units_s = as.factor(units_s)
    , n_s = as.numeric(n_s)
    , fraction_s = as.factor(fraction_s)
    , Bioavailable = as.factor(Bioavailable)
    , sp = as.factor(sp)
    , name = as.factor(name)
    , cultivar = as.factor(cultivar)
    , p_density = as.numeric(p_density)
    , units_density = as.factor(units_density)
    , ba_total = as.numeric(ba_total)
    , ba_stem = as.numeric(ba_stem)
    , ba_leaf = as.numeric(ba_leaf)
    , br = as.numeric(br)
    , units_b = as.factor(units_b)
    , as_ba = as.numeric(as_ba)
    , cd_ba = as.numeric(cd_ba)
    , cu_ba = as.numeric(cu_ba)
    , pb_ba = as.numeric(pb_ba)
    , zn_ba = as.numeric(zn_ba)
    , se_ba = as.numeric(se_ba)
    , ni_ba = as.numeric(ni_ba)
    , co_ba = as.numeric(co_ba)
    , mn_ba = as.numeric(mn_ba)
    , cr_ba = as.numeric(cr_ba)
    , hg_ba = as.numeric(hg_ba)
    , units_te_ba = as.factor(units_te_ba)
    , n_te_ba = as.numeric(n_te_ba)
    , organs_ba = as.factor(organs_ba)
    , as_br = as.numeric(as_br)
    , cd_br = as.numeric(cd_br)
    , cu_br = as.numeric(cu_br)
    , pb_br = as.numeric(pb_br)
    , zn_br = as.numeric(zn_br)
    , se_br = as.numeric(se_br)
    , ni_br = as.numeric(ni_br)
    , co_br = as.numeric(co_br)
    , mn_br = as.numeric(mn_br)
    , cr_br = as.numeric(cr_br)
    , hg_br = as.numeric(hg_br)
    , units_te_br = as.factor(units_te_br)
    , n_te_br = as.numeric(n_te_br)
    , organs_br = as.factor(organs_br)
    , as_ba_1 = as.numeric(as_ba_1)
    , cd_ba_1 = as.numeric(cd_ba_1)
    , cu_ba_1 = as.numeric(cu_ba_1)
    , pb_ba_1 = as.numeric(pb_ba_1)
    , zn_ba_1 = as.numeric(zn_ba_1)
    , se_ba_1 = as.numeric(se_ba_1)
    , ni_ba_1 = as.numeric(ni_ba_1)
    , co_ba_1 = as.numeric(co_ba_1)
    , mn_ba_1 = as.numeric(mn_ba_1)
    , cr_ba_1 = as.numeric(cr_ba_1)
    , hg_ba_1 = as.numeric(hg_ba_1)
    , units_te_ba_1 = as.factor(units_te_ba_1)
    , n_te_ba_1 = as.numeric(n_te_ba_1)
    , organs_ba_1 = as.factor(organs_ba_1)
    , as_ba_2 = as.numeric(as_ba_2)
    , cd_ba_2 = as.numeric(cd_ba_2)
    , cu_ba_2 = as.numeric(cu_ba_2)
    , pb_ba_2 = as.numeric(pb_ba_2)
    , zn_ba_2 = as.numeric(zn_ba_2)
    , se_ba_2 = as.numeric(se_ba_2)
    , ni_ba_2 = as.numeric(ni_ba_2)
    , co_ba_2 = as.numeric(co_ba_2)
    , mn_ba_2 = as.numeric(mn_ba_2)
    , cr_ba_2 = as.numeric(cr_ba_2)
    , hg_ba_2 = as.numeric(hg_ba_2)
    , units_te_ba_2 = as.factor(units_te_ba_2)
    , n_te_ba_2 = as.numeric(n_te_ba_2)
    , organs_ba_2 = as.factor(organs_ba_2)
    , as_ba_3 = as.numeric(as_ba_3)
    , cd_ba_3 = as.numeric(cd_ba_3)
    , cu_ba_3 = as.numeric(cu_ba_3)
    , pb_ba_3 = as.numeric(pb_ba_3)
    , zn_ba_3 = as.numeric(zn_ba_3)
    , se_ba_3 = as.numeric(se_ba_3)
    , ni_ba_3 = as.numeric(ni_ba_3)
    , co_ba_3 = as.numeric(co_ba_3)
    , mn_ba_3 = as.numeric(mn_ba_3)
    , cr_ba_3 = as.numeric(cr_ba_3)
    , hg_ba_3 = as.numeric(hg_ba_3)
    , units_te_ba_3 = as.factor(units_te_ba_3)
    , n_te_ba_3 = as.numeric(n_te_ba_3)
    , organs_ba_3 = as.factor(organs_ba_3)
  )

seuil_hyperaccumulator <- 300
# Ajouter une nouvelle colonne 'Hyperaccumulator' dans data
data_cu <- data %>%
  mutate(Hyperaccumulator = ifelse(cu_ba > seuil_hyperaccumulator | cu_ba_1 > seuil_hyperaccumulator | cu_ba_2 > seuil_hyperaccumulator | cu_ba_3 > seuil_hyperaccumulator, "Oui", "Non"))
# Visualiser les hyperaccumulatrices
hyperaccumulators <- data_cu %>% filter(Hyperaccumulator == "Oui")
hyperaccumulators$AccSpeciesName_cor

##Standardisation
numeric_columns <- sapply(data, is.numeric)
data[numeric_columns] <- scale(data[numeric_columns])

# Analyse linéaire
model <- lm(cu_br ~ cu_s, data = data[numeric_columns])
model
summary(model)
ggplot(data, aes(x = cu_s, y = cu_ba)) +
  geom_point() +
  labs(x = "Cuivre dans le sol (cu_s)", y = "Cuivre dans les plantes (cu_ba)",
       title = "Relation entre les concentrations de cuivre dans les parties sous-terraines des plantes et dans le sol") +
  scale_x_continuous(limits = c(0, 10)) +   
  scale_y_continuous(limits = c(0, 5)) 

model <- lm(cu_s ~ P, data = data[numeric_columns])
model
summary(model)
ggplot(data, aes(x = P, y = cu_s)) +
  geom_point() +
  labs(x = "Phosphore dans le sol (P)", y = "Cuivre dans le sol (cu_s)",
       title = "Relation entre les concentrations de phosphore et de cuivre dans le sol")

model <- lm(cu_ba ~ cu_s, data = data[numeric_columns])
model
summary(model)
ggplot(data, aes(x = cu_s, y = cu_ba)) +
  geom_point() +
  labs(x = "Cuivre dans le sol (cu_s)", y = "Cuivre dans les plantes (cu_ba)",
       title = "Relation entre les concentrations de cuivre dans les parties aériennes des plantes et dans le sol") +
  scale_x_continuous(limits = c(0, 10)) +   
  scale_y_continuous(limits = c(0, 5)) 
geom_smooth(method = "lm", se = FALSE)

model <- lm(cu_ba ~ cu_s, data = data[numeric_columns])
model
summary(model)
ggplot(data, aes(x = cu_s, y = cu_br)) +
  geom_point() +
  labs(x = "Cuivre dans le sol (cu_s)", y = "Cuivre dans les plantes (cu_br)",
       title = "Relation entre les concentrations de cuivre dans les racines des plantes et dans le sol") +
  scale_x_continuous(limits = c(0, 10)) +   
  scale_y_continuous(limits = c(0, 5))

# Filtrer les organes que vous souhaitez inclure dans le boxplot
organes_inclus <- c("leaves", "shoots", "stems")
data_filtre <- data %>%
  filter(organs_ba %in% organes_inclus | 
           organs_ba_1 %in% organes_inclus | 
           organs_ba_2 %in% organes_inclus | 
           organs_ba_3 %in% organes_inclus)
p <- ggplot(data_filtre, aes(x = organs_ba, y = cu_ba)) +
  geom_boxplot() +
  labs(x = "Organes", y = "Concentration de cuivre (mg/kg)",
       title = "Comparaison des concentrations de cuivre dans les différentes parties aériennes") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 250)) # Ajuster les limites des axes x et y

p <- p + stat_summary(fun.data = mean_cl_normal, geom = "crossbar", width = 0.2, color = "blue") +
  stat_summary(fun = median, geom = "point", shape = 17, size = 3, color = "red")
summary_data <- aggregate(cu_ba ~ organs_ba, data = data_filtre, 
                          FUN = function(x) c(mean = mean(x), median = median(x)))
print(summary_data)
# Afficher le graphique avec les statistiques sommaires
print(p)

# Filtrer les données pour inclure uniquement les racines
data_roots <- data %>%
  filter(organs_br %in% c("roots"))
# Créer le graphique boxplot
p <- ggplot(data_roots, aes(x = "Racines", y = cu_br)) +
  geom_boxplot() +
  labs(x = "Organe", y = "Concentration de cuivre (mg/kg)",
       title = "Concentration de cuivre dans les racines") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 250)) # Ajuster les limites de l'axe y
# Afficher le graphique
print(p)

data_leaves <- data %>%
  filter(organs_ba %in% c("leaves"))
# Créer le graphique boxplot
p <- ggplot(data_leaves, aes(x = "Feuilles", y = cu_ba)) +
  geom_boxplot() +
  labs(x = "Organe", y = "Concentration de cuivre (mg/kg)",
       title = "Concentration de cuivre dans les feuilles") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 250)) # Ajuster les limites de l'axe y
# Afficher le graphique
print(p)

# Calculer les médianes et moyennes pour les feuilles (cu_ba)
summary_data_leaves <- aggregate(cu_ba ~ Organe, data = data_filtre[data_filtre$Organe == "Feuilles", ], 
                          FUN = function(x) c(median = median(x), mean = mean(x)))

# Calculer les médianes et moyennes pour les racines (cu_br)
summary_data_roots <- aggregate(cu_br ~ Organe, data = data_filtre[data_filtre$Organe == "Racines", ], 
                          FUN = function(x) c(median = median(x), mean = mean(x)))

# Afficher les tableaux résumés
print("Feuilles:")
print(summary_data_leaves)

print("Racines:")
print(summary_data_roots)





#RDA
env_vars <- data[numeric_columns][, c("ph", "clay", "sand", "om", "oc", "N", "P", "map", "mat", "ec", "cec")]
metal_conc_soil <- data[numeric_columns][, c("as_s", "cd_s", "cu_s", "pb_s", "zn_s", "ni_s", "se_s", "co_s", "mn_s", "cr_s", "hg_s")] 
metal_conc_plants <- data[numeric_columns][, c("as_ba", "cd_ba", "cu_ba", "pb_ba", "zn_ba", "ni_ba", "se_ba", "co_ba", "mn_ba", "cr_ba", "hg_ba")]
metal_conc_plants_imputed <- apply(metal_conc_plants, 2, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))
metal_conc_plants_imputed <- as.data.frame(metal_conc_plants_imputed)
data_complete <- cbind(metal_conc_plants_imputed, env_vars)
rda_result <- rda(data_complete)
plot(rda_result)
summary(rda_result)

rda_result <- rda(data_complete, na.action = na.exclude)

summary(rda_result)

#PCA
selected_columns <- (data[numeric_columns][,c("cu_s", "pb_s", "zn_s", "se_s", "ni_s", "co_s", "mn_s", "cr_s", "hg_s", "map", "ph", "om", "oc", "clay", "sand", "ec", "cec")])
selected_data <- data[, selected_columns]
pca_model <- prcomp(selected_data)
#problème avec les NA

#ANOVA
anova_model <- aov(cu_ba ~ country, data = data)
tukey_results <- glht(anova_model, linfct = mcp(country = "Tukey"))
summary(tukey_results)












## join the traits to your data 
traits <- readRDS("./complete_data.rds")
data_std <- left_join(data , traits, by=c('AccSpeciesName_cor' = 'sp'))

# remove the sp. that don't have LA, SLA and LDMC values 
# i.e. remove the lines in which LA values are 'NA'
data_std <- data_std %>%
  filter(!is.na(LA) & !is.na(SLA) & !is.na(LDMC))

# if need no NA
data_clean <-data_std[,c('as_ba',	'cd_ba',	'cu_ba',	'pb_ba',	'zn_ba',	'se_ba',	'ni_ba',	'co_ba',	'mn_ba',	'cr_ba',	'hg_ba', 'LA' , 'SLA' ,'LDMC' , 'ph' ,'AccSpeciesName_cor', 'covidence')]
data_clean <- na.omit(data_std[,c('cu_ba', 'cd_ba','zn_ba','LA' , 'SLA' ,'LDMC' , 'ph' ,'AccSpeciesName_cor', 'covidence')])

## Cu, Cd and Zn are the most abundant metal tested (without NA)
TE_leaves <- data_clean[,c(	'cd_ba',	'zn_ba', "cu_ba")]

# TE scores
### 1- score of TE accumulation
# make a matrix only of TE concentrations in leaves
TE_leaves <- data_std %>%
  filter(organs_ba == "leaves") %>%
  select(cd_ba, zn_ba, cu_ba)

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

#plot RDA
rdap<-rda(TE_leaves ~ LA + SLA + LDMC + ph, data=data_clean, scale=T)
scores_rda <- scores(rdap, display = "sites")
scores_df <- as.data.frame(scores_rda)
my_palette <- brewer.pal(n = 8, name = "Set2")
ordiplot(rdap, scaling=2, xlim = c(-10, 10), ylim = c(-10, 10), cex = 1, col = my_palette, display=c('sp','cn', 'sites'))

pca <- rda(TE_leaves, scale=T)
plot(pca)

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
text(1.7,-0.8, bquote(F == .(round(anova(rda, permutation=how(nperm=99999, block=data_std$covidence))$F[1], 2))), cex=0.9, pos=2)

dev.off()


#### Influence of traits and environmental factor on TE accumulation- Linear mix model (LMM) ####
# Example of a full model selection
#Spécifiez les noms des variables numériques à standardiser
vars_to_standardize <- c("LA","SLA","LDMC","ph","cu_s")
# Standardisation des variables
data_std[vars_to_standardize] <- scale(data_std[vars_to_standardize])

lmer<-lmer(cu_ba ~ om + cu_s+ LA + SLA + LDMC + ph + (1|covidence), data=data_std)
anova(lmer)

# Assumptions verification
plot(resid(lmer),data_std$cu_ba )
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
lmer.cu_br <- lmer(cu_br ~ log(LA) + log(SLA) + log(LDMC) + cu_s + cec + ph + country + AccSpeciesName_cor + (1|covidence), data = data_std)
lmer.cu_br <- lmer(cu_br ~ log(LA) + log(SLA) + log(LDMC) + cu_s + ph + country + AccSpeciesName_cor + (1|covidence), data = data_std)

anova(lmer.cu_br)
summary(lmer.cu_br)

