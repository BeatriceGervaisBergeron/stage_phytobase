setwd("C:/Users/User/Documents/Scolaire/UDEM/Maîtrise/Hiver 2023/Stage phyto/stage_phytobase")
# DATA CLEANING
## LIBRARY
library(dplyr)
library(stringr)
library(taxize)
library(vegan)
library(readr)

#### call your data  ####
data <- read.csv('./Amélie/soil_sp_database_Amelie.csv', sep=';',header = T, dec = '.')

#### decimals ####
# make sure there is not comma instead of points
# already with points
# make sure all column are in the correct forms (character or numerical)
str(data)

# transform variable that needed
# Transform data
data <- data %>%
  mutate(
    covidence = as.numeric(covidence)
    , author = as.character(author)
    , year = as.numeric(year)
    , country = as.character(country)
    , location = as.character(location)
    , latitude = as.character(latitude)
    , longitude = as.character(longitude)
    , climate = as.character(climate)
    , expe_t = as.numeric(expe_t)
    , mat = as.numeric(mat)
    , map = as.numeric(map)
    , map_units = as.character(map_units)
    , soil = as.character(soil)
    , type = as.character(type)
    , ph = as.numeric(ph)
    , om = as.numeric(om)
    , om_units = as.character(om_units)
    , oc = as.numeric(oc)
    , oc_units = as.character(oc_units)
    , texture = as.character(texture)
    , clay = as.numeric(clay)
    , clay_units = as.character(clay_units)
    , ec = as.numeric(ec)
    , ec_units = as.character(ec_units)
    , sand = as.numeric(sand)
    , sand_units = as.character(sand_units)
    , cec = as.numeric(cec)
    , cec_units = as.character(cec_units)
    , N = as.numeric(N)
    , N_units = as.character(N_units)
    , N_types = as.character(N_types)
    , P = as.numeric(P)
    , P_units = as.character(P_units)
    , P_type = as.character(P_type)
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
    , units_s = as.character(units_s)
    , n_s = as.numeric(n_s)
    , fraction_s = as.character(fraction_s)
    , Bioavailable = as.character(Bioavailable)
    , sp = as.character(sp)
    , name = as.character(name)
    , cultivar = as.character(cultivar)
    , p_density = as.numeric(p_density)
    , units_density = as.character(units_density)
    , ba_total = as.numeric(ba_total)
    , ba_stem = as.numeric(ba_stem)
    , ba_leaf = as.numeric(ba_leaf)
    , br = as.numeric(br)
    , units_b = as.character(units_b)
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
    , units_te_ba = as.character(units_te_ba)
    , n_te_ba = as.numeric(n_te_ba)
    , organs_ba = as.character(organs_ba)
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
    , units_te_br = as.character(units_te_br)
    , n_te_br = as.numeric(n_te_br)
    , organs_br = as.character(organs_br)
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
    , units_te_ba_1 = as.character(units_te_ba_1)
    , n_te_ba_1 = as.numeric(n_te_ba_1)
    , organs_ba_1 = as.character(organs_ba_1)
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
    , units_te_ba_2 = as.character(units_te_ba_2)
    , n_te_ba_2 = as.numeric(n_te_ba_2)
    , organs_ba_2 = as.character(organs_ba_2)
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
    , units_te_ba_3 = as.character(units_te_ba_3)
    , n_te_ba_3 = as.numeric(n_te_ba_3)
    , organs_ba_3 = as.character(organs_ba_3)
  )
# Na were introduced when values were not only numerical.

# Verify the mutation
str(data)# good

#### all white space to NA ####
data[data == ''] <- NA

#### species names cleaning  ####
# check unique sp list in your database
uni_sp<-as.data.frame(unique(data$name)) # 394 unique species
colnames(uni_sp) <- c('sp') 

# Correct the name according to the species name corrected from TRY
# list_sp_cor already corrected
list_sp_cor <-readRDS('list_sp_cor.rds')
# sp not present in the list
to.be.cor <- anti_join(uni_sp, list_sp_cor, by=c('sp'='user_supplied_name')) # 124 species not on the corrected list, so need to be corrected

# Resolve the unmatched name with the 4 databases selected:
# "The International Plant Names Index",'USDA NRCS PLANTS Database',"Tropicos - Missouri Botanical Garden", 'Catalogue of Life'
match.name <- to.be.cor$sp %>%
  gnr_resolve(data_source_ids = c(1,150,165,167), 
              with_canonical_ranks=T)

# provide a short summary table
matches <- match.name %>%
  select(user_supplied_name,submitted_name, matched_name2, score)%>% #add original value
  distinct()

# Are all species considered in the correction
uni_sp_2<- as.data.frame(unique(match.name$user_supplied_name)) # 121 sp
colnames(uni_sp_2) <- c('sp')

# which are not included
unmatch <- to.be.cor %>% filter(!sp %in% uni_sp_2$sp)
colnames(unmatch) <- c('user_supplied_name')

# add them to the end of the match list
matches.all<-bind_rows(matches, unmatch)

# Insert three new columns (change names as you like) and insert text:
# ‘implement’ - should the name suggested by GNR be used? (TRUE/FALSE)?
# ‘alternative’ - write an alternative name here
# ‘dupl’ - Is this entry a duplicate with other name in this list (TRUE/FALSE)?
matches.all$implement<-''
matches.all$alternative<-''
matches.all$dupl<-''

# write it back as a table for manual correction in Excel
write.table(matches.all,
            "./Amélie/uni_sp_match_names.txt", 
            sep="\t", row.names = F, quote = F)

# open the txt file in excel to make manual corrections
# For all hybrids (with x) both name should be keep
# Save the corrected names in a txt file name, adding _cor to the name of the document
# import back the data 
uni_sp_cor <- read.table("./Amélie/uni_sp_match_names_cor.txt", 
                         sep="\t", header=T, stringsAsFactors = F)

# eliminate duplicates
uni_sp_cor$dupl2 <- duplicated(uni_sp_cor$user_supplied_name)
uni_sp_cor<-uni_sp_cor %>%  filter(!dupl2 ==T)


# Save the final corrected list in an rds object
saveRDS(uni_sp_cor, file='Amélie/cu_sp_cor.rds')

#### Join list of corrected names ####
# call the newly corrected list
cu_sp_cor <- readRDS('Amélie/cu_sp_cor.rds')

# Convert the score column in cu_sp_cor to double
cu_sp_cor$score <- as.numeric(cu_sp_cor$score)

# Combine the corrected lists
list_sp_cor_cu <- bind_rows(cu_sp_cor, list_sp_cor)

# add the corrections to the data
data <- data %>% 
  left_join(list_sp_cor_cu, by=c('name'= 'user_supplied_name'))

### BEA: ici il faut enlever les colonnes submitted_name à dupl2, soit 131 a 137. 
#Tu as éliminé des colonnes de données ici, dont ta colonne organ_ba_1, d'ou ton problème de reconnaissance
### a remplacer par quelque chose comme ça si tu préfères:
data <- data %>% 
  dplyr::select(-c(submitted_name, matched_name2, score, alternative, dupl, dupl2))


# Check number of sp now
uni_cu_cor <-unique(data$name)

###BEA: j'en ai 376 maintenant. Est ce que ça te donne le meme resultats? ce serait bine de l,inclure au commentaires pour assurer la reproductibilité du script
###AME: j'en ai 394

#### standardized units ####
# Select all the units column
units <- data %>%
  select(contains('unit'))

# check unit conversion for every unit column
colnames(units)
#"om_units"
unique(units$om_units) # "%" "g kg-1" "g.O2.kg-1" "g.kg-1" "mg kg-1" "g dm-3" "dag kg-1" "g kg" "g kg-3"
# need to convert to %
data_std <- data %>%
  mutate(om = ifelse(om_units == 'g kg', om/10, om)
         ,om_units = ifelse(om_units == 'g kg', '%', om_units)
         ,om = ifelse(om_units == 'g.kg-1', om/10, om)
         ,om_units = ifelse(om_units == 'g.kg-1', '%', om_units)
         ,om = ifelse(om_units == 'g kg-1', om/10, om)
         ,om_units = ifelse(om_units == 'g kg-1', '%', om_units)
         ,om = ifelse(om_units == 'g.O2.kg-1', om/10, om)
         ,om_units = ifelse(om_units == 'g.O2.kg-1', '%', om_units)
         ,om = ifelse(om_units == 'mg kg-1', om/100, om)
         ,om_units = ifelse(om_units == 'mg kg-1', '%', om_units)
  )
# verify
unique(data_std$om_units) # need to convert "dag kg-1" in "%" 

### BEA: pour les unité qui te reste à convertir, il faudrait 1) que tu vérifie que ce sont bine les unités données (dag kg-1 ne me dis rien et g/kg3 me semble une erreur)
# 2) tu pourrais les ajouter au tableau de transformation des unités dans le document de prototcol et me proposer les conversions qui te semble adéquate selon tes recherches, 
# 3) ajoute des ? au tableau si tu ne sais vraiment pas, mais je pense que tu peux trouver comment conbvertir mg Kg-1
# 4) ajouter les sources pour la proposition de convertion si nécessaire
# 5) pour les unités avec des volumes, comme L ou dm3, c'est effectivement plus compliqué. Essai de voir ce qui t'es proposé avec une recheche et je vais regarder de mon cote aussi.


#"oc_units" 
unique(units$oc_units) # "%" "g kg-1" "mgL-1" "mg kg-1" "g kg"
# need to convert  to %
data_std <- data %>%
  mutate(
    oc = ifelse(oc_units == 'g kg', oc/10, oc)
    ,oc_units = ifelse(oc_units == 'g kg', '%', oc_units)
    ,oc = ifelse(oc_units == 'g kg-1', oc/10, oc)
    ,oc_units = ifelse(oc_units == 'g kg-1', '%', oc_units)
    ,oc = ifelse(oc_units == 'mg kg-1', oc/100, oc)
    ,oc_units = ifelse(oc_units == 'mg kg-1', '%', oc_units)
  )
# verify
unique(data_std$oc_units) # need to convert "mgL-1" in "%"

### BEA: attention, si tu ajoute des modifications a data_std, tu dois repartir de data_std, si tu reprend data seulement tu reparts de data avec 0 modifications

#"clay_units" 
unique(units$clay_units) #"%" "g kg-1" "mm" "mg kg-1" "g kg"
# need to convert  to %
data_std <- data %>%
  mutate(
    clay = ifelse(clay_units == 'g kg', clay/10, clay)
    ,clay_units = ifelse(clay_units == 'g kg', '%', clay_units)
    ,clay = ifelse(clay_units == 'g kg-1', clay/10, clay)
    ,clay_units = ifelse(clay_units == 'g kg-1', '%', clay_units)
    ,clay = ifelse(clay_units == 'mg kg-1', clay/100, clay)
    ,clay_units = ifelse(clay_units == 'mg kg-1', '%', clay_units)
  )
# verify
unique(data_std$clay_units) #only "%"


#"sand_units" 
unique(units$sand_units) #  "%" "g kg-1" "mm" "mg kg-1" "g kg"
# need to convert  to %
data_std <- data %>%
  mutate(
    sand = ifelse(sand_units == 'g kg', sand/10, sand)
    ,sand_units = ifelse(sand_units == 'g kg', '%', sand_units)
    ,sand = ifelse(sand_units == 'g kg-1', sand/10, sand)
    ,sand_units = ifelse(sand_units == 'g kg-1', '%', sand_units)
    ,sand = ifelse(sand_units == 'mg kg-1', sand/100, sand)
    ,sand_units = ifelse(sand_units == 'mg kg-1', '%', sand_units)
  )
# verify
unique(data_std$sand_units) #only "%"


#"ec_units"
unique(units$ec_units) # "uScm-1" "dSm-1" "ms cm-1" "mS/cm" "uS cm" "mS m-1" "uS cm-1" "us/m" "dS/m" "dS cm-1" "mS cm-1" "uS" "us cm" "us cm-1" "uS/cm" "dS m-1" "mS/m" "dS m"
#need to convert  to mS cm-1
convert <- c("ms cm-1","mS/cm","mS cm-1")
data_std <- data %>%
  mutate(
    ec_units = ifelse(ec_units %in% convert , 'mS cm−1', ec_units)
    ,ec = ifelse(ec_units == 'uScm-1', ec/1000, ec)
    ,ec_units = ifelse(ec_units == 'uScm-1', 'mS cm−1', ec_units)
    ,ec = ifelse(ec_units == 'uS cm-1', ec/1000, ec)
    ,ec_units = ifelse(ec_units == 'uS cm-1', 'mS cm−1', ec_units)
    ,ec = ifelse(ec_units == 'us cm-1', ec/1000, ec)
    ,ec_units = ifelse(ec_units == 'us cm-1', 'mS cm−1', ec_units)
    ,ec = ifelse(ec_units == 'us cm', ec/1000, ec)
    ,ec_units = ifelse(ec_units == 'us cm', 'mS cm−1', ec_units)
    ,ec = ifelse(ec_units == 'uS cm', ec/1000, ec)
    ,ec_units = ifelse(ec_units == 'uS cm', 'mS cm−1', ec_units)
    ,ec = ifelse(ec_units == 'uS/cm', ec/1000, ec)
    ,ec_units = ifelse(ec_units == 'uS/cm', 'mS cm−1', ec_units)
    ,ec = ifelse(ec_units == 'dSm-1', ec/10, ec)
    ,ec_units = ifelse(ec_units == 'dSm-1', 'mS cm−1', ec_units)
    ,ec = ifelse(ec_units == 'dS/m', ec/10, ec)
    ,ec_units = ifelse(ec_units == 'dS/m', 'mS cm−1', ec_units)
    ,ec = ifelse(ec_units == 'dS m', ec/10, ec)
    ,ec_units = ifelse(ec_units == 'dS m', 'mS cm−1', ec_units)
    ,ec = ifelse(ec_units == 'dS m-1', ec/10, ec)
    ,ec_units = ifelse(ec_units == 'dS m-1', 'mS cm−1', ec_units)
    ,ec = ifelse(ec_units == 'dS cm-1', ec/100, ec)
    ,ec_units = ifelse(ec_units == 'dS cm-1', 'mS cm−1', ec_units)
    ,ec = ifelse(ec_units == 'mS m-1', ec/10, ec)
    ,ec_units = ifelse(ec_units == 'mS m-1', 'mS cm−1', ec_units)
    ,ec = ifelse(ec_units == 'mS/m', ec/10, ec)
    ,ec_units = ifelse(ec_units == 'mS/m', 'mS cm−1', ec_units)
    ,ec = ifelse(ec_units == 'us/m', ec/10, ec)
    ,ec_units = ifelse(ec_units == 'us/m', 'mS cm−1', ec_units)
    ,ec = ifelse(ec_units == 'uS', ec/10, ec)
    ,ec_units = ifelse(ec_units == 'uS', 'mS cm−1', ec_units)
  )
# verify
unique(data_std$ec_units)# only "mS cm−1" 


#"cec_units"
unique(units$cec_units) # "cmol kg-1" "cmolc kg-1" "meq 100-1" "meq 100 g-1" "cmolc/kg" "cmol/kg" "meq 100g-1" "mmolc dm-3" "cmol+ kg-1" "cmol(+)kg-1" "meq/100g" "molc kg-1" "%" "mmol kg-1" "cmol kg" "cmol+ kg" "cmol/100g soil" "cmolc dm-3" "mM(+)/kg DM" 
# need to convert to cmolc kg-1
convert <- c("meq 100-1","meq 100g-1","meq/100g","meq 100 g−3","cmolc/kg","meq 100 g-1","cmol/kg","cmol kg","cmol kg-1","molc kg-1","cmol(+)kg-1","cmol+ kg-1","cmol+ kg","mM(+)/kg DM")
data_std <- data_std %>%
  mutate(
    cec_units = ifelse(cec_units %in% convert , 'cmolc kg-1', cec_units)
    ,cec = ifelse(cec_units == 'mmol kg-1', cec/10, cec)
    ,cec_units = ifelse(cec_units == 'mmol kg-1', 'cmolc kg-1', cec_units)
    ,cec = ifelse(cec_units == 'cmolc dm-3', cec*10, cec)
    ,cec_units = ifelse(cec_units == 'cmolc dm-3', 'cmolc kg-1', cec_units)
    ,cec = ifelse(cec_units == 'mmolc dm-3', cec/10, cec)
    ,cec_units = ifelse(cec_units == 'mmolc dm-3', 'cmolc kg-1', cec_units)
    ,cec = ifelse(cec_units == 'cmol/100g soil', cec/100, cec)
    ,cec_units = ifelse(cec_units == 'cmol/100g soil', 'cmolc kg-1', cec_units)
    ,cec = ifelse(cec_units == '%', cec*10, cec)
    ,cec_units = ifelse(cec_units == '%', 'cmolc kg-1', cec_units)
  )
#verify
unique(data_std$cec_units) # only "cmolc kg-1"


#"N_units" 
unique(units$N_units) # "mg kg-1" "g kg-1" "%" "mg/g" "g.kg-1" "mg/kg" "mg g-1" "g kg dw-1" "g kg" "kg ha-1" "mg kg"
# need to convert to mg kg−1
convert <- c("mg/kg","mg kg","mg kg-1")
data_std <- data %>%
  mutate(
    N_units = ifelse(N_units %in% convert , 'mg kg−1', N_units)
    ,N = ifelse(N_units == '%', N*10000, N)
    ,N_units = ifelse(N_units == '%', 'mg kg−1', N_units)
    ,N = ifelse(N_units == 'g kg-1', N*1000, N)
    ,N_units = ifelse(N_units == 'g kg-1', 'mg kg−1', N_units)
    ,N = ifelse(N_units == 'g.kg-1', N*1000, N)
    ,N_units = ifelse(N_units == 'g.kg-1', 'mg kg−1', N_units)
    ,N = ifelse(N_units == 'mg/g', N*1000, N)
    ,N_units = ifelse(N_units == 'mg/g', 'mg kg−1', N_units)
    ,N = ifelse(N_units == 'mg g-1', N*1000, N)
    ,N_units = ifelse(N_units == 'mg g-1', 'mg kg−1', N_units)
    ,N = ifelse(N_units == 'g kg', N*1000, N)
    ,N_units = ifelse(N_units == 'g kg', 'mg kg−1', N_units)
    ,N = ifelse(N_units == 'g kg dw-1', N*1000, N)
    ,N_units = ifelse(N_units == 'g kg dw-1', 'mg kg−1', N_units)
  )
#verify
unique(data_std$N_units) # need to convert "kg ha-1" in "mg kg-1"


#"P_units"   
unique(units$P_units) # "mg kg-1" "kg ha-1" "mg kg" "P2O5" "g.kg-1" "g kg-1" "mg/kg" "ug g-1" "g kg dw-1" "mg kg-1 (P2O5)" "mg 100 g-1" "%" "mg 100g-1" "mg dm-3" "mg g-1" "ppm" "meq/100g" "mg L-1" "g kg" "g P2O5 kg-1""mg P/kg
# need to convert to mg kg-1
convert <- c("mg/kg","mg kg","mg g-1","ppm","mg kg-1 (P2O5)","mg P/kg")
data_std <- data %>%
  mutate(
    P_units = ifelse(P_units %in% convert , 'mg kg-1', P_units)
    ,P = ifelse(P_units == '%', P*1000, P)
    ,P_units = ifelse(P_units == '%', 'mg kg-1', P_units)
    ,P = ifelse(P_units == 'g.kg-1', P*1000, P)
    ,P_units = ifelse(P_units == 'g.kg-1', 'mg kg-1', P_units)
    ,P = ifelse(P_units == 'g kg-1', P*1000, P)
    ,P_units = ifelse(P_units == 'g kg-1', 'mg kg-1', P_units)
    ,P = ifelse(P_units == 'ug g-1', P*1000, P)
    ,P_units = ifelse(P_units == 'ug g-1', 'mg kg-1', P_units)
    ,P = ifelse(P_units == 'g kg dw-1', P*1000, P)
    ,P_units = ifelse(P_units == 'g kg dw-1', 'mg kg-1', P_units)
    ,P = ifelse(P_units == 'mg 100 g-1', P/10, P)
    ,P_units = ifelse(P_units == 'mg 100 g-1', 'mg kg-1', P_units)
    ,P = ifelse(P_units == 'mg 100g-1', P/10, P)
    ,P_units = ifelse(P_units == 'mg 100g-1', 'mg kg-1', P_units)
    ,P = ifelse(P_units == 'mg dm-3', P*10, P)
    ,P_units = ifelse(P_units == 'mg dm-3', 'mg kg-1', P_units)
    ,P = ifelse(P_units == 'g kg', P*1000, P)
    ,P_units = ifelse(P_units == 'g kg', 'mg kg-1', P_units)
    ,P = ifelse(P_units == 'P2O5', P*0.4364, P)
    ,P_units = ifelse(P_units == 'P2O5', 'mg kg-1', P_units)
    ,P = ifelse(P_units == 'g P2O5 kg-1', P*1000, P)
    ,P_units = ifelse(P_units == 'g P2O5 kg-1', 'mg kg-1', P_units)
    ,P = ifelse(P_units == 'mg L-1', P*1000, P)
    ,P_units = ifelse(P_units == 'mg L-1', 'mg kg-1', P_units)
    ,P = ifelse(P_units == 'meq/100g', P*30.97*10, P)
    ,P_units = ifelse(P_units == 'meq/100g', 'mg kg-1', P_units)
  )
#verify
unique(data_std$P_units)# need to convert "kg ha-1" in "mg kg-1"


#"units_s"   
unique(units$units_s) # "mg kg-1" "g kg-1" "mg kg" "ppm" "ug.L-1" "ug g-1" "mg/kg" "ug g" "mg dm-3" "ug/g" "mg L-1" "uM/g"
# need to convert to mg kg-1
convert <- c("mg/kg","mg kg","ppm","ug.L-1", "ug g-1","ug g","ug/g","mg dm-3")
data_std <- data %>%
  mutate(
    units_s = ifelse(units_s %in% convert , 'mg kg-1', units_s)
    ,s = ifelse(units_s == 'g kg-1', as_s*1000, as_s)
    ,units_s = ifelse(units_s == 'g kg-1', 'mg kg-1', units_s)
    ,s = ifelse(units_s == 'g kg-1', cd_s*1000, cd_s)
    ,units_s = ifelse(units_s == 'g kg-1', 'mg kg-1', units_s)
    ,s = ifelse(units_s == 'g kg-1', cu_s*1000, cu_s)
    ,units_s = ifelse(units_s == 'g kg-1', 'mg kg-1', units_s)
    ,s = ifelse(units_s == 'g kg-1', pb_s*1000, pb_s)
    ,units_s = ifelse(units_s == 'g kg-1', 'mg kg-1', units_s)
    ,s = ifelse(units_s == 'g kg-1', zn_s*1000, zn_s)
    ,units_s = ifelse(units_s == 'g kg-1', 'mg kg-1', units_s)
    ,s = ifelse(units_s == 'g kg-1', se_s*1000, se_s)
    ,units_s = ifelse(units_s == 'g kg-1', 'mg kg-1', units_s)
    ,s = ifelse(units_s == 'g kg-1', ni_s*1000, ni_s)
    ,units_s = ifelse(units_s == 'g kg-1', 'mg kg-1', units_s)
    ,s = ifelse(units_s == 'g kg-1', co_s*1000, co_s)
    ,units_s = ifelse(units_s == 'g kg-1', 'mg kg-1', units_s)
    ,s = ifelse(units_s == 'g kg-1', mn_s*1000, mn_s)
    ,units_s = ifelse(units_s == 'g kg-1', 'mg kg-1', units_s)
    ,s = ifelse(units_s == 'g kg-1', cr_s*1000, cr_s)
    ,units_s = ifelse(units_s == 'g kg-1', 'mg kg-1', units_s)
    ,s = ifelse(units_s == 'g kg-1', hg_s*1000, hg_s)
    ,units_s = ifelse(units_s == 'g kg-1', 'mg kg-1', units_s)
  )
#verify
unique(data_std$units_s) # need to convert "mg L-1" and "uM/g" in "mg kg-1"

    
#"units_b" 
unique(units$units_b) #"g/plant" "g pot-1" "g per pot" "mg" "g" "mg plant-1" "g plant-1" "kg" "kg acre-1" "g pot -1" "g " "%" "mg ha-1" "g m-2" "g/pot" "g plant -1" "g plant" "t ha-1" "g FM" "kg ha-1" "g m2"
## need to convert to g
convert <- c("g/plant","g pot-1","g per pot","g plant-1", "g pot -1","g ","g/pot","g plant -1","g plant","g m-2","g FM","g m2")
data_std <- data %>%
  mutate(
    units_b = ifelse(units_b %in% convert , 'g', units_b)
    ,b = ifelse(units_b == 'mg', ba_total/1000, ba_total)
    ,units_b = ifelse(units_b == 'mg', 'g', units_b)
    ,b = ifelse(units_b == 'mg', ba_stem/1000, ba_stem)
    ,units_b = ifelse(units_b == 'mg', 'g', units_b)
    ,b = ifelse(units_b == 'mg', ba_leaf/1000, ba_leaf)
    ,units_b = ifelse(units_b == 'mg', 'g', units_b)
    ,b = ifelse(units_b == 'mg', br/1000, br)
    ,units_b = ifelse(units_b == 'mg', 'g', units_b)
    ,b = ifelse(units_b == 'mg plant-1', ba_total/1000, ba_total)
    ,units_b = ifelse(units_b == 'mg plant-1', 'g', units_b)
    ,b = ifelse(units_b == 'mg plant-1', ba_stem/1000, ba_stem)
    ,units_b = ifelse(units_b == 'mg plant-1', 'g', units_b)
    ,b = ifelse(units_b == 'mg plant-1', ba_leaf/1000, ba_leaf)
    ,units_b = ifelse(units_b == 'mg plant-1', 'g', units_b)
    ,b = ifelse(units_b == 'mg plant-1', br/1000, br)
    ,units_b = ifelse(units_b == 'mg plant-1', 'g', units_b)
    ,b = ifelse(units_b == 'kg', ba_total*1000, ba_total)
    ,units_b = ifelse(units_b == 'kg', 'g', units_b)
    ,b = ifelse(units_b == 'kg', ba_stem*1000, ba_stem)
    ,units_b = ifelse(units_b == 'kg', 'g', units_b)
    ,b = ifelse(units_b == 'kg', ba_leaf*1000, ba_leaf)
    ,units_b = ifelse(units_b == 'kg', 'g', units_b)
    ,b = ifelse(units_b == 'kg', br*1000, br)
    ,units_b = ifelse(units_b == 'kg', 'g', units_b)
  )

#verify
unique(data_std$units_b) # need to convert "kg acre-1","mg ha-1","t ha-1","kg ha-1" en "g"


#"units_te_ba" 
unique(units$units_te_ba) # "mg kg-1" "mg m2 year-1" "mg kg" "ug.g-1" "ppm" "ug g" "ug/g" "mg kg " "mg/kg" "mg plant-1" "mg kg-1 DW" "ug g-1" "mg m-2" "uM/g DW" "mg pot-1" "ug kg-1" "ppm/ppb" "kg ha-1"
# need to convert to mg kg-1 
convert <- c("mg kg","ppm","mg kg ","mg/kg", "mg kg-1 DW","ppm/ppb")
data_std <- data %>%
  mutate(
    units_te_ba = ifelse(units_te_ba %in% convert , 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug.g-1', as_ba/1000, as_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug.g-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug.g-1', cd_ba/1000, cd_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug.g-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug.g-1', cu_ba/1000, cu_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug.g-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug.g-1', pb_ba/1000, pb_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug.g-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug.g-1', zn_ba/1000, zn_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug.g-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug.g-1', se_ba/1000, se_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug.g-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug.g-1', ni_ba/1000, ni_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug.g-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug.g-1', co_ba/1000, co_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug.g-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug.g-1', mn_ba/1000, mn_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug.g-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug.g-1', cr_ba/1000, cr_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug.g-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug.g-1', hg_ba/1000, hg_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug.g-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug g', as_ba/1000, as_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug g', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug g', cd_ba/1000, cd_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug g', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug g', cu_ba/1000, cu_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug g', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug g', pb_ba/1000, pb_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug g', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug g', zn_ba/1000, zn_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug g', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug g', se_ba/1000, se_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug g', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug g', ni_ba/1000, ni_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug g', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug g', co_ba/1000, co_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug g', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug g', mn_ba/1000, mn_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug g', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug g', cr_ba/1000, cr_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug g', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug g', hg_ba/1000, hg_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug g', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug/g', as_ba/1000, as_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug/g', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug/g', cd_ba/1000, cd_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug/g', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug/g', cu_ba/1000, cu_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug/g', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug/g', pb_ba/1000, pb_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug/g', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug/g', zn_ba/1000, zn_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug/g', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug/g', se_ba/1000, se_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug/g', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug/g', ni_ba/1000, ni_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug/g', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug/g', co_ba/1000, co_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug/g', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug/g', mn_ba/1000, mn_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug/g', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug/g', cr_ba/1000, cr_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug/g', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug/g', hg_ba/1000, hg_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug/g', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug g-1', as_ba/1000, as_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug g-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug g-1', cd_ba/1000, cd_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug g-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug g-1', cu_ba/1000, cu_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug g-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug g-1', pb_ba/1000, pb_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug g-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug g-1', zn_ba/1000, zn_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug g-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug g-1', se_ba/1000, se_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug g-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug g-1', ni_ba/1000, ni_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug g-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug g-1', co_ba/1000, co_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug g-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug g-1', mn_ba/1000, mn_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug g-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug g-1', cr_ba/1000, cr_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug g-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug g-1', hg_ba/1000, hg_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug g-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug kg-1', as_ba/1000, as_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug kg-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug kg-1', cd_ba/1000, cd_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug kg-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug kg-1', cu_ba/1000, cu_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug kg-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug kg-1', pb_ba/1000, pb_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug kg-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug kg-1', zn_ba/1000, zn_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug kg-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug kg-1', se_ba/1000, se_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug kg-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug kg-1', ni_ba/1000, ni_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug kg-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug kg-1', co_ba/1000, co_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug kg-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug kg-1', mn_ba/1000, mn_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug kg-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug kg-1', cr_ba/1000, cr_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug kg-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug kg-1', hg_ba/1000, hg_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug kg-1', 'mg kg-1', units_te_ba)
  )
# verify
unique(data_std$units_te_ba) # Need to convert "kg ha-1",mg m-2","uM/g DW","mg m2 year-1","mg plant-1","mg pot-1" in "mg kg-1"

### BEA: tu pourrais inscrire toutes tes transformations d'unité dans le tableau des variables, quetsions que je les valide plus facilement?


#"units_te_br"   
unique(units$units_te_br) # "mg kg-1" "mg kg" "ug.g-1" "ug g" "ug/g" "mg plant-1" "mg kg-1 DW" "ug g-1" "mg m-2" "uM/g DW" "mg pot-1" 
# need to convert to mg kg-1 
convert <- c("mg kg", "mg kg-1 DW")
data_std <- data %>%
  mutate(
    units_te_br = ifelse(units_te_br %in% convert , 'mg kg-1', units_te_br)
    ,br = ifelse(units_te_br == 'ug.g-1', as_br*1000, as_br)
    ,units_te_br = ifelse(units_te_br == 'ug.g-1', 'mg kg-1', units_te_br)
    ,br = ifelse(units_te_br == 'ug.g-1', cd_br*1000, cd_br)
    ,units_te_br = ifelse(units_te_br == 'ug.g-1', 'mg kg-1', units_te_br)
    ,br = ifelse(units_te_br == 'ug.g-1', cu_br*1000, cu_br)
    ,units_te_br = ifelse(units_te_br == 'ug.g-1', 'mg kg-1', units_te_br)
    ,br = ifelse(units_te_br == 'ug.g-1', pb_br*1000, pb_br)
    ,units_te_br = ifelse(units_te_br == 'ug.g-1', 'mg kg-1', units_te_br)
    ,br = ifelse(units_te_br == 'ug.g-1', zn_br*1000, zn_br)
    ,units_te_br = ifelse(units_te_br == 'ug.g-1', 'mg kg-1', units_te_br)
    ,br = ifelse(units_te_br == 'ug.g-1', se_br*1000, se_br)
    ,units_te_br = ifelse(units_te_br == 'ug.g-1', 'mg kg-1', units_te_br)
    ,br = ifelse(units_te_br == 'ug.g-1', ni_br*1000, ni_br)
    ,units_te_br = ifelse(units_te_br == 'ug.g-1', 'mg kg-1', units_te_br)
    ,br = ifelse(units_te_br == 'ug.g-1', co_br*1000, co_br)
    ,units_te_br = ifelse(units_te_br == 'ug.g-1', 'mg kg-1', units_te_br)
    ,br = ifelse(units_te_br == 'ug.g-1', mn_br*1000, mn_br)
    ,units_te_br = ifelse(units_te_br == 'ug.g-1', 'mg kg-1', units_te_br)
    ,br = ifelse(units_te_br == 'ug.g-1', cr_br*1000, cr_br)
    ,units_te_br = ifelse(units_te_br == 'ug.g-1', 'mg kg-1', units_te_br)
    ,br = ifelse(units_te_br == 'ug.g-1', hg_br*1000, hg_br)
    ,units_te_br = ifelse(units_te_br == 'ug.g-1', 'mg kg-1', units_te_br)
    ,br = ifelse(units_te_br == 'ug g', as_br*1000, as_br)
    ,units_te_br = ifelse(units_te_br == 'ug g', 'mg kg-1', units_te_br)
    ,br = ifelse(units_te_br == 'ug g', cd_br*1000, cd_br)
    ,units_te_br = ifelse(units_te_br == 'ug g', 'mg kg-1', units_te_br)
    ,br = ifelse(units_te_br == 'ug g', cu_br*1000, cu_br)
    ,units_te_br = ifelse(units_te_br == 'ug g', 'mg kg-1', units_te_br)
    ,br = ifelse(units_te_br == 'ug g', pb_br*1000, pb_br)
    ,units_te_br = ifelse(units_te_br == 'ug g', 'mg kg-1', units_te_br)
    ,br = ifelse(units_te_br == 'ug g', zn_br*1000, zn_br)
    ,units_te_br = ifelse(units_te_br == 'ug g', 'mg kg-1', units_te_br)
    ,br = ifelse(units_te_br == 'ug g', se_br*1000, se_br)
    ,units_te_br = ifelse(units_te_br == 'ug g', 'mg kg-1', units_te_br)
    ,br = ifelse(units_te_br == 'ug g', ni_br*1000, ni_br)
    ,units_te_br = ifelse(units_te_br == 'ug g', 'mg kg-1', units_te_br)
    ,br = ifelse(units_te_br == 'ug g', co_br*1000, co_br)
    ,units_te_br = ifelse(units_te_br == 'ug g', 'mg kg-1', units_te_br)
    ,br = ifelse(units_te_br == 'ug g', mn_br*1000, mn_br)
    ,units_te_br = ifelse(units_te_br == 'ug g', 'mg kg-1', units_te_br)
    ,br = ifelse(units_te_br == 'ug g', cr_br*1000, cr_br)
    ,units_te_br = ifelse(units_te_br == 'ug g', 'mg kg-1', units_te_br)
    ,br = ifelse(units_te_br == 'ug g', hg_br*1000, hg_br)
    ,units_te_br = ifelse(units_te_br == 'ug g', 'mg kg-1', units_te_br)
    ,br = ifelse(units_te_br == 'ug/g', as_br*1000, as_br)
    ,units_te_br = ifelse(units_te_br == 'ug/g', 'mg kg-1', units_te_br)
    ,br = ifelse(units_te_br == 'ug/g', cd_br*1000, cd_br)
    ,units_te_br = ifelse(units_te_br == 'ug/g', 'mg kg-1', units_te_br)
    ,br = ifelse(units_te_br == 'ug/g', cu_br*1000, cu_br)
    ,units_te_br = ifelse(units_te_br == 'ug/g', 'mg kg-1', units_te_br)
    ,br = ifelse(units_te_br == 'ug/g', pb_br*1000, pb_br)
    ,units_te_br = ifelse(units_te_br == 'ug/g', 'mg kg-1', units_te_br)
    ,br = ifelse(units_te_br == 'ug/g', zn_br*1000, zn_br)
    ,units_te_br = ifelse(units_te_br == 'ug/g', 'mg kg-1', units_te_br)
    ,br = ifelse(units_te_br == 'ug/g', se_br*1000, se_br)
    ,units_te_br = ifelse(units_te_br == 'ug/g', 'mg kg-1', units_te_br)
    ,br = ifelse(units_te_br == 'ug/g', ni_br*1000, ni_br)
    ,units_te_br = ifelse(units_te_br == 'ug/g', 'mg kg-1', units_te_br)
    ,br = ifelse(units_te_br == 'ug/g', co_br*1000, co_br)
    ,units_te_br = ifelse(units_te_br == 'ug/g', 'mg kg-1', units_te_br)
    ,br = ifelse(units_te_br == 'ug/g', mn_br*1000, mn_br)
    ,units_te_br = ifelse(units_te_br == 'ug/g', 'mg kg-1', units_te_br)
    ,br = ifelse(units_te_br == 'ug/g', cr_br*1000, cr_br)
    ,units_te_br = ifelse(units_te_br == 'ug/g', 'mg kg-1', units_te_br)
    ,br = ifelse(units_te_br == 'ug/g', hg_br*1000, hg_br)
    ,units_te_br = ifelse(units_te_br == 'ug/g', 'mg kg-1', units_te_br)
    ,br = ifelse(units_te_br == 'ug g-1', as_br*1000, as_br)
    ,units_te_br = ifelse(units_te_br == 'ug g-1', 'mg kg-1', units_te_br)
    ,br = ifelse(units_te_br == 'ug g-1', cd_br*1000, cd_br)
    ,units_te_br = ifelse(units_te_br == 'ug g-1', 'mg kg-1', units_te_br)
    ,br = ifelse(units_te_br == 'ug g-1', cu_br*1000, cu_br)
    ,units_te_br = ifelse(units_te_br == 'ug g-1', 'mg kg-1', units_te_br)
    ,br = ifelse(units_te_br == 'ug g-1', pb_br*1000, pb_br)
    ,units_te_br = ifelse(units_te_br == 'ug g-1', 'mg kg-1', units_te_br)
    ,br = ifelse(units_te_br == 'ug g-1', zn_br*1000, zn_br)
    ,units_te_br = ifelse(units_te_br == 'ug g-1', 'mg kg-1', units_te_br)
    ,br = ifelse(units_te_br == 'ug g-1', se_br*1000, se_br)
    ,units_te_br = ifelse(units_te_br == 'ug g-1', 'mg kg-1', units_te_br)
    ,br = ifelse(units_te_br == 'ug g-1', ni_br*1000, ni_br)
    ,units_te_br = ifelse(units_te_br == 'ug g-1', 'mg kg-1', units_te_br)
    ,br = ifelse(units_te_br == 'ug g-1', co_br*1000, co_br)
    ,units_te_br = ifelse(units_te_br == 'ug g-1', 'mg kg-1', units_te_br)
    ,br = ifelse(units_te_br == 'ug g-1', mn_br*1000, mn_br)
    ,units_te_br = ifelse(units_te_br == 'ug g-1', 'mg kg-1', units_te_br)
    ,br = ifelse(units_te_br == 'ug g-1', cr_br*1000, cr_br)
    ,units_te_br = ifelse(units_te_br == 'ug g-1', 'mg kg-1', units_te_br)
    ,br = ifelse(units_te_br == 'ug g-1', hg_br*1000, hg_br)
    ,units_te_br = ifelse(units_te_br == 'ug g-1', 'mg kg-1', units_te_br)
  )
# verify
unique(data_std$units_te_br) # need to convert "mg plant-1","mg m-2",mg pot-1" in "mg kg-1"


#"units_te_ba_1"
unique(units$units_te_ba_1) # "mg kg-1" "ug.g-1" "ug g" "mg kg" "mg plant-1" "ppm" "uM/g DW" "ug/g" "ug kg-1" "ug g-1"
# need to convert to mg kg-1 
convert <- c("mg kg","ppm")
data_std <- data %>%
  mutate(
    units_te_ba_1 = ifelse(units_te_ba_1 %in% convert , 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug.g-1', as_ba_1*1000, as_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug.g-1', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug.g-1', cd_ba_1*1000, cd_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug.g-1', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug.g-1', cu_ba_1*1000, cu_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug.g-1', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug.g-1', pb_ba_1*1000, pb_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug.g-1', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug.g-1', zn_ba_1*1000, zn_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug.g-1', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug.g-1', se_ba_1*1000, se_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug.g-1', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug.g-1', ni_ba_1*1000, ni_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug.g-1', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug.g-1', co_ba_1*1000, co_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug.g-1', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug.g-1', mn_ba_1*1000, mn_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug.g-1', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug.g-1', cr_ba_1*1000, cr_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug.g-1', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug.g-1', hg_ba_1*1000, hg_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug.g-1', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug g', as_ba_1*1000, as_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug g', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug g', cd_ba_1*1000, cd_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug g', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug g', cu_ba_1*1000, cu_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug g', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug g', pb_ba_1*1000, pb_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug g', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug g', zn_ba_1*1000, zn_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug g', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug g', se_ba_1*1000, se_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug g', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug g', ni_ba_1*1000, ni_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug g', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug g', co_ba_1*1000, co_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug g', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug g', mn_ba_1*1000, mn_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug g', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug g', cr_ba_1*1000, cr_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug g', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug g', hg_ba_1*1000, hg_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug g', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug/g', as_ba_1*1000, as_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug/g', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug/g', cd_ba_1*1000, cd_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug/g', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug/g', cu_ba_1*1000, cu_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug/g', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug/g', pb_ba_1*1000, pb_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug/g', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug/g', zn_ba_1*1000, zn_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug/g', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug/g', se_ba_1*1000, se_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug/g', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug/g', ni_ba_1*1000, ni_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug/g', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug/g', co_ba_1*1000, co_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug/g', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug/g', mn_ba_1*1000, mn_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug/g', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug/g', cr_ba_1*1000, cr_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug/g', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug/g', hg_ba_1*1000, hg_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug/g', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug g-1', as_ba_1*1000, as_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug g-1', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug g-1', cd_ba_1*1000, cd_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug g-1', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug g-1', cu_ba_1*1000, cu_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug g-1', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug g-1', pb_ba_1*1000, pb_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug g-1', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug g-1', zn_ba_1*1000, zn_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug g-1', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug g-1', se_ba_1*1000, se_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug g-1', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug g-1', ni_ba_1*1000, ni_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug g-1', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug g-1', co_ba_1*1000, co_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug g-1', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug g-1', mn_ba_1*1000, mn_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug g-1', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug g-1', cr_ba_1*1000, cr_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug g-1', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug g-1', hg_ba_1*1000, hg_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug g-1', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug kg-1', as_ba_1/1000, as_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug kg-1', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug kg-1', cd_ba_1/1000, cd_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug kg-1', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug kg-1', cu_ba_1/1000, cu_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug kg-1', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug kg-1', pb_ba_1/1000, pb_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug kg-1', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug kg-1', zn_ba_1/1000, zn_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug kg-1', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug kg-1', se_ba_1/1000, se_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug kg-1', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug kg-1', ni_ba_1/1000, ni_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug kg-1', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug kg-1', co_ba_1/1000, co_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug kg-1', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug kg-1', mn_ba_1/1000, mn_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug kg-1', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug kg-1', cr_ba_1/1000, cr_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug kg-1', 'mg kg-1', units_te_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'ug kg-1', hg_ba_1/1000, hg_ba_1)
    ,units_te_ba_1 = ifelse(units_te_ba_1 == 'ug kg-1', 'mg kg-1', units_te_ba_1)
  )
# verify
unique(data_std$units_te_ba_1) # need to convert "uM/g DW", "mg plant-1" in mg kg-1


#"units_te_ba_2"
unique(units$units_te_ba_2) # "mg kg-1" "ug g"    "mg kg"
#need to convert to mg kg-1 
convert <- c("mg kg")
data_std <- data %>%
  mutate(
    units_te_ba_2 = ifelse(units_te_ba_2 %in% convert , 'mg kg-1', units_te_ba_2)
    ,ba_2 = ifelse(units_te_ba_2 == 'ug g', as_ba_2*1000, as_ba_2)
    ,units_te_ba_2 = ifelse(units_te_ba_2 == 'ug g', 'mg kg-1', units_te_ba_2)
    ,ba_2 = ifelse(units_te_ba_2 == 'ug g', cd_ba_2*1000, cd_ba_2)
    ,units_te_ba_2 = ifelse(units_te_ba_2 == 'ug g', 'mg kg-1', units_te_ba_2)
    ,ba_2 = ifelse(units_te_ba_2 == 'ug g', cu_ba_2*1000, cu_ba_2)
    ,units_te_ba_2 = ifelse(units_te_ba_2 == 'ug g', 'mg kg-1', units_te_ba_2)
    ,ba_2 = ifelse(units_te_ba_2 == 'ug g', pb_ba_2*1000, pb_ba_2)
    ,units_te_ba_2 = ifelse(units_te_ba_2 == 'ug g', 'mg kg-1', units_te_ba_2)
    ,ba_2 = ifelse(units_te_ba_2 == 'ug g', zn_ba_2*1000, zn_ba_2)
    ,units_te_ba_2 = ifelse(units_te_ba_2 == 'ug g', 'mg kg-1', units_te_ba_2)
    ,ba_2 = ifelse(units_te_ba_2 == 'ug g', se_ba_2*1000, se_ba_2)
    ,units_te_ba_2 = ifelse(units_te_ba_2 == 'ug g', 'mg kg-1', units_te_ba_2)
    ,ba_2 = ifelse(units_te_ba_2 == 'ug g', ni_ba_2*1000, ni_ba_2)
    ,units_te_ba_2 = ifelse(units_te_ba_2 == 'ug g', 'mg kg-1', units_te_ba_2)
    ,ba_2 = ifelse(units_te_ba_2 == 'ug g', co_ba_2*1000, co_ba_2)
    ,units_te_ba_2 = ifelse(units_te_ba_2 == 'ug g', 'mg kg-1', units_te_ba_2)
    ,ba_2 = ifelse(units_te_ba_2 == 'ug g', mn_ba_2*1000, mn_ba_2)
    ,units_te_ba_2 = ifelse(units_te_ba_2 == 'ug g', 'mg kg-1', units_te_ba_2)
    ,ba_2 = ifelse(units_te_ba_2 == 'ug g', cr_ba_2*1000, cr_ba_2)
    ,units_te_ba_2 = ifelse(units_te_ba_2 == 'ug g', 'mg kg-1', units_te_ba_2)
    ,ba_2 = ifelse(units_te_ba_2 == 'ug g', hg_ba_2*1000, hg_ba_2)
  )
# verify
unique(data_std$units_te_ba_2) # only "mg kg-1"    

#"units_te_ba_3"
unique(units$units_te_ba_3) # only "mg kg-1"

#"units_density"
unique(data$units_density)# "plants/pot" "plant/pot" "stems/acre" "g cm-3" "g/pot" "plants/plot" "seeds/0.75m2" "plants/rhizobox" "plants ha−1"
convert <- c("plant/pot")
data_std <- data %>%
  mutate(
    units_density = ifelse(units_density %in% convert , 'plants/pot', units_density)
 )
# verify
unique(data_std$units_density) # "plants/pot" "stems/acre" "g cm-3"  "g/pot" "plants/plot" "seeds/0.75m2" "plants/rhizobox" "plants ha−1"



#### standardize categories terms ####

# Climate
unique(data$climate) 
# 'on hold', to be unify if needed further on

# Texture
unique(data$texture) 

# p_density
unique(data$p_density)
# 'on hold', to be unify if needed further on

# organs_ba, organs_ba_1, organs_ba_2, organs_ba_3
unique(data$organs_ba)
unique(data$organs_ba_1)
unique(data$organs_ba_2)
unique(data$organs_ba_3)
# conversion
syn_shoots <- c("Shoots","shoots","Shoot","shoot","overground organs","leaves+stems", "stems + leaves", "stems+leaves", 
                "Stems, leaves and flowers", "Whole top part", "Leaves +stems","Above-ground parts", "Above ground parts", 
                "Aboveground parts", "Aerial part", "Aerial parts", "Stems and leaves"  )
syn_stems <- c("Stalks","Twigs","Stems","stems","stem","Stem","stalk","Twig","Branch","branch","branches","Branches",
               "Lower stems","Culms","Stubble","stalks")
syn_leaves <- c("leaf","Leaf","Leaves","leaves","Leafs","Foliage","Aciculum", "Unwashed leaves","Needle", "Lower leaves",
                "Washed leaves", "Leaf/Needle")
syn_flowers <- c("flowers","Heads","Head","Spikelets","head")
syn_fruits <- c("Berry","Edible parts")
syn_wood <- c("wood","Woody","Trunk wood", "Bark","Trunk" )
syn_whole<- c("leaves + roots","whole sample (leaves+stems+roots)", "whole plant" )

### BEA: you need to go back to have more information on those organs: NA, tissus, herbs,                

data_std <- data_std %>%
  mutate(organs_ba = ifelse(organs_ba %in% syn_shoots , 'shoots', organs_ba)) %>% 
  mutate(organs_ba = ifelse(organs_ba %in% syn_stems , 'stems', organs_ba)) %>%
  mutate(organs_ba = ifelse(organs_ba %in% syn_leaves , 'leaves', organs_ba)) %>%
  mutate(organs_ba = ifelse(organs_ba %in% syn_flowers , 'flowers', organs_ba)) %>%
  mutate(organs_ba = ifelse(organs_ba %in% syn_fruits , 'fruits', organs_ba)) %>%
  mutate(organs_ba = ifelse(organs_ba %in% syn_wood , 'wood', organs_ba)) %>%
  mutate(organs_ba = ifelse(organs_ba %in% syn_whole , 'whole', organs_ba)) %>%
  mutate(organs_ba_1 = ifelse(organs_ba_1 %in% syn_shoots , 'shoots', organs_ba_1)) %>%
  mutate(organs_ba_1 = ifelse(organs_ba_1 %in% syn_stems , 'stems', organs_ba_1)) %>%
  mutate(organs_ba_1 = ifelse(organs_ba_1 %in% syn_leaves , 'leaves', organs_ba_1)) %>%
  mutate(organs_ba_1 = ifelse(organs_ba_1 %in% syn_flowers , 'flowers', organs_ba_1)) %>%
  mutate(organs_ba_1 = ifelse(organs_ba_1 %in% syn_fruits , 'fruits', organs_ba_1)) %>%
  mutate(organs_ba_1 = ifelse(organs_ba_1 %in% syn_wood , 'wood', organs_ba_1)) %>%
  mutate(organs_ba_1 = ifelse(organs_ba_1 %in% syn_whole , 'whole', organs_ba_1)) %>%
  mutate(organs_ba_2 = ifelse(organs_ba_2 %in% syn_shoots , 'shoots', organs_ba_2)) %>%
  mutate(organs_ba_2 = ifelse(organs_ba_2 %in% syn_stems , 'stems', organs_ba_2)) %>%
  mutate(organs_ba_2 = ifelse(organs_ba_2 %in% syn_leaves , 'leaves', organs_ba_2)) %>%
  mutate(organs_ba_2 = ifelse(organs_ba_2 %in% syn_flowers , 'flowers', organs_ba_2)) %>%
  mutate(organs_ba_2 = ifelse(organs_ba_2 %in% syn_fruits , 'fruits', organs_ba_2)) %>%
  mutate(organs_ba_2 = ifelse(organs_ba_2 %in% syn_wood , 'wood', organs_ba_2)) %>%
  mutate(organs_ba_2 = ifelse(organs_ba_2 %in% syn_whole , 'whole', organs_ba_2)) %>%
  mutate(organs_ba_3 = ifelse(organs_ba_3 %in% syn_shoots , 'shoots', organs_ba_3)) %>%
  mutate(organs_ba_3 = ifelse(organs_ba_3 %in% syn_stems , 'stems', organs_ba_3)) %>%
  mutate(organs_ba_3 = ifelse(organs_ba_3 %in% syn_leaves , 'leaves', organs_ba_3)) %>%
  mutate(organs_ba_3 = ifelse(organs_ba_3 %in% syn_flowers , 'flowers', organs_ba_3)) %>%
  mutate(organs_ba_3 = ifelse(organs_ba_3 %in% syn_fruits , 'fruits', organs_ba_3)) %>% 
  mutate(organs_ba_3 = ifelse(organs_ba_3 %in% syn_wood , 'wood', organs_ba_3)) %>%
  mutate(organs_ba_3 = ifelse(organs_ba_3 %in% syn_whole , 'whole', organs_ba_3))

# verify
unique(data_std$organs_ba) # "leaves"  "shoots"  "whole"   "stems" "wood"    "fruits"
unique(data_std$organs_ba_1) # "stems"   "shoots"  "leaves"  "flowers" "wood"
unique(data_std$organs_ba_2) # "flowers" "stems"   "wood"   "fruits"
unique(data_std$organs_ba_3) # only "wood"


### BEA: il faudrait vérifier pourquoi tous les espaces sont remplacer. Nomralement, la fonction ne doit remplacer que s'il y a un nom d'organe.

# organs_br
unique(data$organs_br)
# conversion
syn_roots <- c("root","roots ","rhizomes")
data_std <- data_std %>%
  mutate(organs_br = ifelse(organs_br %in% syn_roots , 'roots', organs_br))
# verify
unique(data_std$organs_br) # only "roots"


#### outliers and errors in numerical data #### 

# see if duplicates data entries
unique_obs<- data_std[duplicated(data_std)] # 0 variables--> 0 duplicates to eliminate

# list of variables that need to be verify
num_cols <- unlist(lapply(data_std, is.numeric)) #identify numerical data 
data_num <- data_std[ , num_cols]  # 91 variables

###BEA: moi je vois 70 variables? et il y en aura peut-être car au début j'ai identifié des colonnes qui doivent être 'mutate'
###AME: est-ce que tu en vois 91 maintenant? 

# import the data normal range
num_range <- read.table("./numerical_range_variables.txt", 
                        sep="\t", header=T, stringsAsFactors = F)


## check outliers
# for each variable, isolate data that are outside the range
list <-colnames(data_num)
list

# isolate the outliers lines for the variable 'covidence'
outliers <- data_std %>% 
  filter(covidence < num_range$min_value[num_range$variables == 'covidence'] | covidence > num_range$max_value[num_range$variables == 'covidence'] )
outliers
# 0 lines --> no apparent outliers

# you can also write it with number from the list to save time, as follow with Covidence as number 1 in the list
outliers <- data_std %>% 
  filter(data_num[,1] < num_range$min_value[1] | data_num[,1] > num_range$max_value[1] )
outliers
# 0 lines --> no apparent outliers

# isolate the outliers lines for the variable 'year'
outliers <- data_std %>% 
  filter(year < num_range$min_value[num_range$variables == 'year'] | year > num_range$max_value[num_range$variables == 'year'] )
outliers
# 0 lines --> no apparent outliers

# isolate the outliers lines for the variable 'expe_t'
outliers <- data_std %>% 
  filter(expe_t < num_range$min_value[num_range$variables == 'expe_t'] | expe_t > num_range$max_value[num_range$variables == 'expe_t'] )
outliers
# 36 lines --> 36 outliers to verify
# no 2973 (expe_t=16.5--> glasshouse minimum temperature 8ºC; maximum temperature 25ºC,inférieur au range [17-27])
# no 2762 (expe_t=31--> greenhouse was monitored at an average of 31ºC,supérieur au range [17-27)
# no 2248 (expe_t=28.9--> in the growing environment, the average temperature was 28.9ºC,supérieur au range [17-27)
# no 218 (expe_t=28--> the chamber temperature was 28ºC,supérieur au range [17-27)
# no 4239 (expe_t=30-->the temperature was ambient 25–35ºC,supérieur au range [17-27)

# isolate the outliers lines for the variable 'mat'
outliers <- data_std %>% 
  filter(mat < num_range$min_value[num_range$variables == 'mat'] | mat > num_range$max_value[num_range$variables == 'mat'] )
outliers
# 9 lines --> 9 outliers to verify
# no 5829 (mat=35-->The summer (June to September) is a very hot, 35ºC on average,supérieur au range [2-25)

# isolate the outliers lines for the variable 'map'
outliers <- data_std %>% 
  filter(map < num_range$min_value[num_range$variables == 'map'] | map > num_range$max_value[num_range$variables == 'map'] )
outliers
# 21 lines --> 21 outliers to verify
# no 1195 (map=1.9--> mean annual rainfall was 1.67–2.13 mm/year,inférieur au range [50-3000])
# no 1510 (map=12-->average rainfall of 12 mm persist during the growth period (November to March) of crop in Faisalabad,inférieur au range [50-3000])
# no 3230 (map=3.014-->c'est la moyenne des précipitations de Janvier à Septembre 2009,inférieur au range [50-3000])

# isolate the outliers lines for the variable 'ph'
outliers <- data_std %>% 
  filter(ph < num_range$min_value[num_range$variables == 'ph'] | ph > num_range$max_value[num_range$variables == 'ph'] )
outliers
# 49 lines --> 49 outliers to verify
# no 8790 (ph=3.25-->Rz ph=2.9-3.6)
# no 5829 (ph=2.6, inférieur au range [3.5-8.5])
# no 8521 (ph=2.34, inférieur au range [3.5-8.5])
# no 5760 (ph=8.6, supérieur au range [3.5-8.5])
# no 2205 (ph=8.9, supérieur au range [3.5-8.5])
# no 6873 (ph=2.78 et 2.81-->dans les Supplementary data, inférieur au range [3.5-8.5])
# no 4193 (ph=8.8, supérieur au range [3.5-8.5])
# no 8198 (ph=8.65-->moyenne de 6.6 et 6.7 Table 1, supérieur au range [3.5-8.5] )
# no 7605 (ph=3.05-->inférieur au range [3.5-8.5])
# no 501 (ph=8.55, supérieur au range [3.5-8.5])
# no 1225 (ph=8.64 et 8.72, supérieur au range [3.5-8.5])
# no 1406 (ph=3.01-->Table 3,inférieur au range [3.5-8.5])
# no 8345 (ph=8.9, supérieur au range [3.5-8.5])
# no 3933 (ph=8.54, supérieur au range [3.5-8.5])

# isolate the outliers lines for the variable 'om'
outliers <- data_std %>% 
  filter(om < num_range$min_value[num_range$variables == 'om'] | om > num_range$max_value[num_range$variables == 'om'] )
outliers
# 90 lines --> 90 outliers to verify
# no 1955
# no 4088
# no 1560
# no 7489
# no 2248
# no 1675
# no 2890
# no 1264
# no 4063
# no 1249
# no 1357

# isolate the outliers lines for the variable 'oc'
outliers <- data_std %>% 
  filter(oc < num_range$min_value[num_range$variables == 'oc'] | oc > num_range$max_value[num_range$variables == 'oc'] )
outliers
# 258 lines --> 258 outliers to verify
# no 1783
# no 1663
# no 196
# no 249
# no 7473
# no 616
# no 2187
# no 4254
# no 6382
# no 1831
# no 6871
# no 2600
# no 2011
# no 4193
# no 133
# no 6544
# no 1356
# no 7504
# no 5338
# no 2864
# no 2289
# no 1675
# no 2695
# no 3510
# no 1700
# no 8669
# no 168
# no 3029
# no 8172
# no 2890
# no 6407
# no 7762
# no 91
# no 4248
# no 8509
# no 7129
# no 1398
# no 1599
# no 397
# no 3616
# no 8196
# no 1249
# no 2129
# no 1357
# no 5467
# no 177
# no 8079

# isolate the outliers lines for the variable 'clay'
outliers <- data_std %>% 
  filter(clay < num_range$min_value[num_range$variables == 'clay'] | clay > num_range$max_value[num_range$variables == 'clay'] )
outliers
# 60 lines --> 60 outliers to verify
# no 1955
# no 6871
# no 1239
# no 8172
# no 2181
# no 3032
# no 1599
# no 4063
# no 3616
# no 6607
# no 1249
# no 177
# no 8079
# no 8774

# isolate the outliers lines for the variable 'sand'
outliers <- data_std %>% 
  filter(sand < num_range$min_value[num_range$variables == 'sand'] | sand > num_range$max_value[num_range$variables == 'sand'] )
outliers
# 57 lines --> 57 outliers to verify
# no 3591
# no 1955
# no 4088
# no 1239
# no 8172
# no 2890
# no 3032
# no 4063
# no 3616
# no 1249
# no 8079
# no 8774

# isolate the outliers lines for the variable 'ec'
outliers <- data_std %>% 
  filter(ec < num_range$min_value[num_range$variables == 'ec'] | ec > num_range$max_value[num_range$variables == 'ec'] )
outliers
# 140 lines --> 140 outliers to verify
# no 6402
# no 4088
# no 7473
# no 616
# no 4254
# no 1884
# no 3067
# no 4193
# no 7974
# no 2532
# no 2695
# no 2632
# no 3510
# no 168
# no 2133
# no 5770
# no 6607
# no 8196
# no 354
# no 8345

# isolate the outliers lines for the variable 'cec'
outliers <- data_std %>% 
  filter(cec < num_range$min_value[num_range$variables == 'cec'] | cec > num_range$max_value[num_range$variables == 'cec'] )
outliers
# 35 lines --> 35 outliers to verify
# no 1955
# no 593
# no 2762
# no 7858
# no 1356
# no 1675
# no 168
# no 1239
# no 1599
# no 177

# isolate the outliers lines for the variable 'N'
outliers <- data_std %>% 
  filter(N < num_range$min_value[num_range$variables == 'n'] | N > num_range$max_value[num_range$variables == 'n'] )
outliers
# 248 lines --> 248 outliers to verify
# no 1663
# no 1955
# no 1146
# no 5829
# no 196
# no 4088
# no 7473
# no 2187
# no 2205
# no 6382
# no 4193
# no 7974
# no 133
# no 6544
# no 1356
# no 2218
# no 2864
# no 2289
# no 1675
# no 2695
# no 5614
# no 3013
# no 8669
# no 168
# no 1683
# no 1239
# no 2890
# no 2181
# no 6407
# no 3032
# no 501
# no 2154
# no 1011
# no 3396
# no 6607
# no 8196
# no 838
# no 1357
# no 5467
# no 177
# no 8012
# no 4239

# isolate the outliers lines for the variable 'P'
outliers <- data_std %>% 
  filter(P < num_range$min_value[num_range$variables == 'p'] | P > num_range$max_value[num_range$variables == 'p'] )
outliers
# 234 lines --> 234 outliers to verify
# no 1041
# no 3591
# no 8790
# no 5829
# no 4088
# no 249
# no 2187
# no 6382
# no 1831
# no 271
# no 2600
# no 7974
# no 2762
# no 6544
# no 7858
# no 1356
# no 218
# no 902
# no 5338
# no 2864
# no 1683
# no 2181
# no 96
# no 1264
# no 1011
# no 3396
# no 2089
# no 1249
# no 1357
# no 5467
# no 4239
# no 2351


# isolate the outliers lines for the variable 'as_s'
outliers <- data_std %>% 
  filter(as_s < num_range$min_value[num_range$variables == 'as_s'] | as_s > num_range$max_value[num_range$variables == 'as_s'] )
outliers
# 68 lines --> 68 outliers to verify
# no 5793
# no 1560
# no 6873
# no 2218
# no 3416
# no 1398
# no 5770
# no 1225
# no 1406
# no 428

# isolate the outliers lines for the variable 'cd_s'
outliers <- data_std %>% 
  filter(cd_s < num_range$min_value[num_range$variables == 'cd_s'] | cd_s > num_range$max_value[num_range$variables == 'cd_s'] )
outliers
# 15 lines --> 15 outliers to verify
# no 6873
# no 3067
# no 2289
# no 1683
# no 8509

# isolate the outliers lines for the variable 'cu_s'
outliers <- data_std %>% 
  filter(cu_s < num_range$min_value[num_range$variables == 'cu_s'] | cu_s > num_range$max_value[num_range$variables == 'cu_s'] )
outliers
# 43 lines --> 43 outliers to verify
# no 1663
# no 3036
# no 466
# no 1560
# no 1884
# no 2695
# no 3416
# no 8669
# no 1683
# no 1225
# no 1406

# isolate the outliers lines for the variable 'pb_s'
outliers <- data_std %>% 
  filter(pb_s < num_range$min_value[num_range$variables == 'pb_s'] | pb_s > num_range$max_value[num_range$variables == 'pb_s'] )
outliers
# 24 lines --> 24 outliers to verify
# no 5793
# no 1146
# no 8790
# no 3036
# no 6873
# no 3067
# no 2218
# no 3416
# no 3377

# isolate the outliers lines for the variable 'zn_s'
outliers <- data_std %>% 
  filter(zn_s < num_range$min_value[num_range$variables == 'zn_s'] | zn_s > num_range$max_value[num_range$variables == 'zn_s'] )
outliers
# 29 lines --> 29 outliers to verify
# no 5793
# no 3067
# no 2289
# no 3416
# no 1374
# no 3377

# isolate the outliers lines for the variable 'se_s'
outliers <- data_std %>% 
  filter(se_s < num_range$min_value[num_range$variables == 'se_s'] | se_s > num_range$max_value[num_range$variables == 'se_s'] )
outliers
# 11 lines --> 11 outliers to verify
# no 6873
# no 121

# isolate the outliers lines for the variable 'ni_s'
outliers <- data_std %>% 
  filter(ni_s < num_range$min_value[num_range$variables == 'ni_s'] | ni_s > num_range$max_value[num_range$variables == 'ni_s'] )
outliers
# 0 lines --> no apparent outliers

# isolate the outliers lines for the variable 'co_s'
outliers <- data_std %>% 
  filter(co_s < num_range$min_value[num_range$variables == 'co_s'] | co_s > num_range$max_value[num_range$variables == 'co_s'] )
outliers
# 0 lines --> no apparent outliers

# isolate the outliers lines for the variable 'mn_s'
outliers <- data_std %>% 
  filter(mn_s < num_range$min_value[num_range$variables == 'mn_s'] | mn_s > num_range$max_value[num_range$variables == 'mn_s'] )
outliers
# 1 line --> 1 outlier to verify
# no 8790

# isolate the outliers lines for the variable 'cr_s'
outliers <- data_std %>% 
  filter(cr_s < num_range$min_value[num_range$variables == 'cr_s'] | cr_s > num_range$max_value[num_range$variables == 'cr_s'] )
outliers
# 0 lines --> no apparent outliers

# isolate the outliers lines for the variable 'hg_s'
outliers <- data_std %>% 
  filter(hg_s < num_range$min_value[num_range$variables == 'hg_s'] | hg_s > num_range$max_value[num_range$variables == 'hg_s'] )
outliers
# 0 lines --> no apparent outliers

# isolate the outliers lines for the variable 'n_s'
outliers <- data_std %>% 
  filter(n_s < num_range$min_value[num_range$variables == 'n_s'] | n_s > num_range$max_value[num_range$variables == 'n_s'] )
outliers
# 177 lines --> 177 outliers to verify
# no 2011
# no 6544
# no 2532
# no 7504
# no 2632
# no 3029
# no 1599
# no 6519
# no 354
# no 428

# isolate the outliers lines for the variable 'ba_total'
outliers <- data_std %>% 
  filter(ba_total < num_range$min_value[num_range$variables == 'ba_total'] | ba_total > num_range$max_value[num_range$variables == 'ba_total'] )
outliers
# 0 lines --> no apparent outliers

# isolate the outliers lines for the variable 'ba_stem'
outliers <- data_std %>% 
  filter(ba_stem < num_range$min_value[num_range$variables == 'ba_stem'] | ba_stem > num_range$max_value[num_range$variables == 'ba_stem'] )
outliers
# 0 lines --> no apparent outliers

# isolate the outliers lines for the variable 'ba_leaf'
outliers <- data_std %>% 
  filter(ba_leaf < num_range$min_value[num_range$variables == 'ba_leaf'] | ba_leaf > num_range$max_value[num_range$variables == 'ba_leaf'] )
outliers
# 0 lines --> no apparent outliers

# isolate the outliers lines for the variable 'br'
outliers <- data_std %>% 
  filter(br < num_range$min_value[num_range$variables == 'br'] | br > num_range$max_value[num_range$variables == 'br'] )
outliers
# 0 lines --> no apparent outliers

# isolate the outliers lines for the variable 'as_ba'
outliers <- data_std %>% 
  filter(as_ba < num_range$min_value[num_range$variables == 'as_ba'] | as_ba > num_range$max_value[num_range$variables == 'as_ba'] )
outliers
# 1 line --> 1 outlier to verify
# no 1041

# isolate the outliers lines for the variable 'cd_ba'
outliers <- data_std %>% 
  filter(cd_ba < num_range$min_value[num_range$variables == 'cd_ba'] | cd_ba > num_range$max_value[num_range$variables == 'cd_ba'] )
outliers
# 3 lines --> 3 outliers to verify
# no 1195
# no 3067
# no 2289

# isolate the outliers lines for the variable 'cu_ba'
outliers <- data_std %>% 
  filter(cu_ba < num_range$min_value[num_range$variables == 'cu_ba'] | cu_ba > num_range$max_value[num_range$variables == 'cu_ba'] )
outliers
# 19 lines --> 19 outliers to verify
# no 57
# no 2972
# no 1560
# no 8669
# no 3029
# no 1264
# no 2857
# no 3230
# no 397
# no 1011

# isolate the outliers lines for the variable 'pb_ba'
outliers <- data_std %>% 
  filter(pb_ba < num_range$min_value[num_range$variables == 'pb_ba'] | pb_ba > num_range$max_value[num_range$variables == 'pb_ba'] )
outliers
# 4 lines --> 4 outliers to verify
# no 1146
# no 3416
# no 3029

# isolate the outliers lines for the variable 'zn_ba'
outliers <- data_std %>% 
  filter(zn_ba < num_range$min_value[num_range$variables == 'zn_ba'] | zn_ba > num_range$max_value[num_range$variables == 'zn_ba'] )
outliers
# 11 lines --> 11 outliers to verify
# no 1146
# no 2187
# no 3067
# no 2289
# no 3029

# isolate the outliers lines for the variable 'se_ba'
outliers <- data_std %>% 
  filter(se_ba < num_range$min_value[num_range$variables == 'se_ba'] | se_ba > num_range$max_value[num_range$variables == 'se_ba'] )
outliers
# 0 lines --> no apparent outliers

# isolate the outliers lines for the variable 'ni_ba'
outliers <- data_std %>% 
  filter(ni_ba < num_range$min_value[num_range$variables == 'ni_ba'] | ni_ba > num_range$max_value[num_range$variables == 'ni_ba'] )
outliers
# 0 lines --> no apparent outliers

# isolate the outliers lines for the variable 'co_ba'
outliers <- data_std %>% 
  filter(co_ba < num_range$min_value[num_range$variables == 'co_ba'] | co_ba > num_range$max_value[num_range$variables == 'co_ba'] )
outliers
# 0 lines --> no apparent outliers

# isolate the outliers lines for the variable 'mn_ba'
outliers <- data_std %>% 
  filter(mn_ba < num_range$min_value[num_range$variables == 'mn_ba'] | mn_ba > num_range$max_value[num_range$variables == 'mn_ba'] )
outliers
# 0 lines --> no apparent outliers

# isolate the outliers lines for the variable 'cr_ba'
outliers <- data_std %>% 
  filter(cr_ba < num_range$min_value[num_range$variables == 'cr_ba'] | cr_ba > num_range$max_value[num_range$variables == 'cr_ba'] )
outliers
# 0 lines --> no apparent outliers

# isolate the outliers lines for the variable 'hg_ba'
outliers <- data_std %>% 
  filter(hg_ba < num_range$min_value[num_range$variables == 'hg_ba'] | hg_ba > num_range$max_value[num_range$variables == 'hg_ba'] )
outliers
# 0 lines --> no apparent outliers
   
# isolate the outliers lines for the variable 'n_te_ba'
outliers <- data_std %>% 
  filter(n_te_ba < num_range$min_value[num_range$variables == 'n_te_ba'] | n_te_ba > num_range$max_value[num_range$variables == 'n_te_ba'] )
outliers
# 53 lines --> 53 outliers to verify
# no 2011
# no 7504
# no 3029
# no 6519

# isolate the outliers lines for the variable 'as_br'
outliers <- data_std %>% 
  filter(as_br < num_range$min_value[num_range$variables == 'as_br'] | as_br > num_range$max_value[num_range$variables == 'as_br'] )
outliers
# 1 line --> 1 outlier to verify
# no 1041

# isolate the outliers lines for the variable 'cd_br'
outliers <- data_std %>% 
  filter(cd_br < num_range$min_value[num_range$variables == 'cd_br'] | cd_br > num_range$max_value[num_range$variables == 'cd_br'] )
outliers
# 5 lines --> 5 outliers to verify
# no 1195
# no 3067
# no 4239

# isolate the outliers lines for the variable 'cu_br'
outliers <- data_std %>% 
  filter(cu_br < num_range$min_value[num_range$variables == 'cu_br'] | cu_br > num_range$max_value[num_range$variables == 'cu_br'] )
outliers
# 62 lines --> 62 outliers to verify
# no 6402
# no 1663
# no 3591
# no 2973
# no 8790
# no 466
# no 5760
# no 218
# no 7605
# no 1833
# no 2857
# no 3230
# no 8713
# no 1011
# no 6607
# no 3933
# no 4239


# isolate the outliers lines for the variable 'pb_br'
outliers <- data_std %>% 
  filter(pb_br < num_range$min_value[num_range$variables == 'pb_br'] | pb_br > num_range$max_value[num_range$variables == 'pb_br'] )
outliers
# 5 lines --> 5 outliers to verify
# no 5793
# no 3067
# no 3416

# isolate the outliers lines for the variable 'zn_br'
outliers <- data_std %>% 
  filter(zn_br < num_range$min_value[num_range$variables == 'zn_br'] | zn_br > num_range$max_value[num_range$variables == 'zn_br'] )
outliers
# 4 lines --> 4 outliers to verify
# no 2187
# no 3067

# isolate the outliers lines for the variable 'se_br'
outliers <- data_std %>% 
  filter(se_br < num_range$min_value[num_range$variables == 'se_br'] | se_br > num_range$max_value[num_range$variables == 'se_br'] )
outliers
# 0 lines --> no apparent outliers

# isolate the outliers lines for the variable 'ni_br'
outliers <- data_std %>% 
  filter(ni_br < num_range$min_value[num_range$variables == 'ni_br'] | ni_br > num_range$max_value[num_range$variables == 'ni_br'] )
outliers
# 0 lines --> no apparent outliers

# isolate the outliers lines for the variable 'co_br'
outliers <- data_std %>% 
  filter(co_br < num_range$min_value[num_range$variables == 'co_br'] | co_br > num_range$max_value[num_range$variables == 'co_br'] )
outliers
# 0 lines --> no apparent outliers

# isolate the outliers lines for the variable 'mn_br'
outliers <- data_std %>% 
  filter(mn_br < num_range$min_value[num_range$variables == 'mn_br'] | mn_br > num_range$max_value[num_range$variables == 'mn_br'] )
outliers
# 0 lines --> no apparent outliers

# isolate the outliers lines for the variable 'cr_br'
outliers <- data_std %>% 
  filter(cr_br < num_range$min_value[num_range$variables == 'cr_br'] | cr_br > num_range$max_value[num_range$variables == 'cr_br'] )
outliers
# 0 lines --> no apparent outliers

# isolate the outliers lines for the variable 'hg_br'
outliers <- data_std %>% 
  filter(hg_br < num_range$min_value[num_range$variables == 'hg_br'] | hg_br > num_range$max_value[num_range$variables == 'hg_br'] )
outliers
# 3 lines --> 3 outliers to verify
# no 1831
# no 3416

# isolate the outliers lines for the variable 'n_te_br'
outliers <- data_std %>% 
  filter(n_te_br < num_range$min_value[num_range$variables == 'n_te_br'] | n_te_br > num_range$max_value[num_range$variables == 'n_te_br'] )
outliers
# 9 lines --> 9 outliers to verify
# no 7504
# no 1599

# isolate the outliers lines for the variable 'as_ba_1'
outliers <- data_std %>% 
  filter(as_ba_1 < num_range$min_value[num_range$variables == 'as_ba'] | as_ba_1 > num_range$max_value[num_range$variables == 'as_ba'] )
outliers
# 0 lines --> no apparent outliers

# isolate the outliers lines for the variable 'cd_ba_1'
outliers <- data_std %>% 
  filter(cd_ba_1 < num_range$min_value[num_range$variables == 'cd_ba'] | cd_ba_1 > num_range$max_value[num_range$variables == 'cd_ba'] )
outliers
# 0 lines --> no apparent outliers

# isolate the outliers lines for the variable 'cu_ba_1'
outliers <- data_std %>% 
  filter(cu_ba_1 < num_range$min_value[num_range$variables == 'cu_ba'] | cu_ba_1 > num_range$max_value[num_range$variables == 'cu_ba'] )
outliers
# 22 lines --> 22 outliers to verify
# no 57
# no 3490
# no 3029
# no 2857
# no 3230
# no 1011
# no 4239

# isolate the outliers lines for the variable 'pb_ba_1'
outliers <- data_std %>% 
  filter(pb_ba_1 < num_range$min_value[num_range$variables == 'pb_ba'] | pb_ba_1 > num_range$max_value[num_range$variables == 'pb_ba'] )
outliers
# 6 lines --> 6 outliers to verify
# no 3490
# no 3029

# isolate the outliers lines for the variable 'zn_ba_1'
outliers <- data_std %>% 
  filter(zn_ba_1 < num_range$min_value[num_range$variables == 'zn_ba'] | zn_ba_1 > num_range$max_value[num_range$variables == 'zn_ba'] )
outliers
# 6 lines --> 6 outliers to verify
# no 2187
# no 3029

# isolate the outliers lines for the variable 'se_ba_1'
outliers <- data_std %>% 
  filter(se_ba_1 < num_range$min_value[num_range$variables == 'se_ba'] | se_ba_1 > num_range$max_value[num_range$variables == 'se_ba'] )
outliers
# 0 lines --> no apparent outliers

# isolate the outliers lines for the variable 'ni_ba_1'
outliers <- data_std %>% 
  filter(ni_ba_1 < num_range$min_value[num_range$variables == 'ni_ba'] | ni_ba_1 > num_range$max_value[num_range$variables == 'ni_ba'] )
outliers
# 0 lines --> no apparent outliers

# isolate the outliers lines for the variable 'co_ba_1'
outliers <- data_std %>% 
  filter(co_ba_1 < num_range$min_value[num_range$variables == 'co_ba'] | co_ba_1 > num_range$max_value[num_range$variables == 'co_ba'] )
outliers
# 0 lines --> no apparent outliers

# isolate the outliers lines for the variable 'mn_ba_1'
outliers <- data_std %>% 
  filter(mn_ba_1 < num_range$min_value[num_range$variables == 'mn_ba'] | mn_ba_1 > num_range$max_value[num_range$variables == 'mn_ba'] )
outliers
# 0 lines --> no apparent outliers

# isolate the outliers lines for the variable 'cr_ba_1'
outliers <- data_std %>% 
  filter(cr_ba_1 < num_range$min_value[num_range$variables == 'cr_ba'] | cr_ba_1 > num_range$max_value[num_range$variables == 'cr_ba'] )
outliers
# 0 lines --> no apparent outliers

# isolate the outliers lines for the variable 'hg_ba_1'
outliers <- data_std %>% 
  filter(hg_ba_1 < num_range$min_value[num_range$variables == 'hg_ba'] | hg_ba_1 > num_range$max_value[num_range$variables == 'hg_ba'] )
outliers
# 2 lines --> 2 outliers to verify
# no 7974

# isolate the outliers lines for the variable 'n_te_ba_1'
outliers <- data_std %>% 
  filter(n_te_ba_1 < num_range$min_value[num_range$variables == 'n_te_ba'] | n_te_ba_1 > num_range$max_value[num_range$variables == 'n_te_ba'] )
outliers
# 5 lines --> 5 outliers to verify
# no 3029

# isolate the outliers lines for the variable 'as_ba_2'
outliers <- data_std %>% 
  filter(as_ba_2 < num_range$min_value[num_range$variables == 'as_ba'] | as_ba_2 > num_range$max_value[num_range$variables == 'as_ba'] )
outliers
# 0 lines --> no apparent outliers

# isolate the outliers lines for the variable 'cd_ba_2'
outliers <- data_std %>% 
  filter(cd_ba_2 < num_range$min_value[num_range$variables == 'cd_ba'] | cd_ba_2 > num_range$max_value[num_range$variables == 'cd_ba'] )
outliers
# 0 lines --> no apparent outliers

# isolate the outliers lines for the variable 'cu_ba_2'
outliers <- data_std %>% 
  filter(cu_ba_2 < num_range$min_value[num_range$variables == 'cu_ba'] | cu_ba_2 > num_range$max_value[num_range$variables == 'cu_ba'] )
outliers
# 3 lines --> 3 outliers to verify
# no 8713

# isolate the outliers lines for the variable 'pb_ba_2'
outliers <- data_std %>% 
  filter(pb_ba_2 < num_range$min_value[num_range$variables == 'pb_ba'] | pb_ba_2 > num_range$max_value[num_range$variables == 'pb_ba'] )
outliers
# 0 lines --> no apparent outliers

# isolate the outliers lines for the variable 'zn_ba_2'
outliers <- data_std %>% 
  filter(zn_ba_2 < num_range$min_value[num_range$variables == 'zn_ba'] | zn_ba_2 > num_range$max_value[num_range$variables == 'zn_ba'] )
outliers
# 2 lines --> 2 outliers to verify
# no 2187
# no 8713

# isolate the outliers lines for the variable 'se_ba_2'
outliers <- data_std %>% 
  filter(se_ba_2 < num_range$min_value[num_range$variables == 'se_ba'] | se_ba_2 > num_range$max_value[num_range$variables == 'se_ba'] )
outliers
# 0 lines --> no apparent outliers

# isolate the outliers lines for the variable 'ni_ba_2'
outliers <- data_std %>% 
  filter(ni_ba_2 < num_range$min_value[num_range$variables == 'ni_ba'] | ni_ba_2 > num_range$max_value[num_range$variables == 'ni_ba'] )
outliers
# 0 lines --> no apparent outliers

# isolate the outliers lines for the variable 'co_ba_2'
outliers <- data_std %>% 
  filter(co_ba_2 < num_range$min_value[num_range$variables == 'co_ba'] | co_ba_2 > num_range$max_value[num_range$variables == 'co_ba'] )
outliers
# 0 lines --> no apparent outliers

# isolate the outliers lines for the variable 'mn_ba_2'
outliers <- data_std %>% 
  filter(mn_ba_2 < num_range$min_value[num_range$variables == 'mn_ba'] | mn_ba_2 > num_range$max_value[num_range$variables == 'mn_ba'] )
outliers
# 0 lines --> no apparent outliers

# isolate the outliers lines for the variable 'cr_ba_2'
outliers <- data_std %>% 
  filter(cr_ba_2 < num_range$min_value[num_range$variables == 'cr_ba'] | cr_ba_2 > num_range$max_value[num_range$variables == 'cr_ba'] )
outliers
# 0 lines --> no apparent outliers

# isolate the outliers lines for the variable 'hg_ba_2'
outliers <- data_std %>% 
  filter(hg_ba_2 < num_range$min_value[num_range$variables == 'hg_ba'] | hg_ba_2 > num_range$max_value[num_range$variables == 'hg_ba'] )
outliers
# 0 lines --> no apparent outliers

# isolate the outliers lines for the variable 'n_te_ba_2'
outliers <- data_std %>% 
  filter(n_te_ba_2 < num_range$min_value[num_range$variables == 'n_te_ba'] | n_te_ba_2 > num_range$max_value[num_range$variables == 'n_te_ba'] )
outliers
# 0 lines --> no apparent outliers

# isolate the outliers lines for the variable 'as_ba_3'
outliers <- data_std %>% 
  filter(as_ba_3 < num_range$min_value[num_range$variables == 'as_ba'] | as_ba_3 > num_range$max_value[num_range$variables == 'as_ba'] )
outliers
# 0 lines --> no apparent outliers

# isolate the outliers lines for the variable 'cd_ba_3'
outliers <- data_std %>% 
  filter(cd_ba_3 < num_range$min_value[num_range$variables == 'cd_ba'] | cd_ba_3 > num_range$max_value[num_range$variables == 'cd_ba'] )
outliers
# 0 lines --> no apparent outliers

# isolate the outliers lines for the variable 'cu_ba_3'
outliers <- data_std %>% 
  filter(cu_ba_3 < num_range$min_value[num_range$variables == 'cu_ba'] | cu_ba_3 > num_range$max_value[num_range$variables == 'cu_ba'] )
outliers
# 0 lines --> no apparent outliers

# isolate the outliers lines for the variable 'pb_ba_3'
outliers <- data_std %>% 
  filter(pb_ba_3 < num_range$min_value[num_range$variables == 'pb_ba'] | pb_ba_3 > num_range$max_value[num_range$variables == 'pb_ba'] )
outliers
# 0 lines --> no apparent outliers

# isolate the outliers lines for the variable 'zn_ba_3'
outliers <- data_std %>% 
  filter(zn_ba_3 < num_range$min_value[num_range$variables == 'zn_ba'] | zn_ba_3 > num_range$max_value[num_range$variables == 'zn_ba'] )
outliers
# 0 lines --> no apparent outliers

# isolate the outliers lines for the variable 'se_ba_3'
outliers <- data_std %>% 
  filter(se_ba_3 < num_range$min_value[num_range$variables == 'se_ba'] | se_ba_3 > num_range$max_value[num_range$variables == 'se_ba'] )
outliers
# 0 lines --> no apparent outliers

# isolate the outliers lines for the variable 'ni_ba_3'
outliers <- data_std %>% 
  filter(ni_ba_3 < num_range$min_value[num_range$variables == 'ni_ba'] | ni_ba_3 > num_range$max_value[num_range$variables == 'ni_ba'] )
outliers
# 0 lines --> no apparent outliers

# isolate the outliers lines for the variable 'co_ba_3'
outliers <- data_std %>% 
  filter(co_ba_3 < num_range$min_value[num_range$variables == 'co_ba'] | co_ba_3 > num_range$max_value[num_range$variables == 'co_ba'] )
outliers
# 0 lines --> no apparent outliers

# isolate the outliers lines for the variable 'mn_ba_3'
outliers <- data_std %>% 
  filter(mn_ba_3 < num_range$min_value[num_range$variables == 'mn_ba'] | mn_ba_3 > num_range$max_value[num_range$variables == 'mn_ba'] )
outliers
# 0 lines --> no apparent outliers

# isolate the outliers lines for the variable 'cr_ba_3'
outliers <- data_std %>% 
  filter(cr_ba_3 < num_range$min_value[num_range$variables == 'cr_ba'] | cr_ba_3 > num_range$max_value[num_range$variables == 'cr_ba'] )
outliers
# 0 lines --> no apparent outliers

# isolate the outliers lines for the variable 'hg_ba_3'
outliers <- data_std %>% 
  filter(hg_ba_3 < num_range$min_value[num_range$variables == 'hg_ba'] | hg_ba_3 > num_range$max_value[num_range$variables == 'hg_ba'] )
outliers
# 0 lines --> no apparent outliers

# isolate the outliers lines for the variable 'n_te_ba_3'
outliers <- data_std %>% 
  filter(n_te_ba_3 < num_range$min_value[num_range$variables == 'n_te_ba'] | n_te_ba_3 > num_range$max_value[num_range$variables == 'n_te_ba'] )
outliers
# 0 lines --> no apparent outliers




#### Add clay and sand % according to textural class of soils###

# check for unique terms in 'texture'
unique(data$texture) 

# standardize textural terms
data_std <- data_std %>%
  mutate(texture = ifelse(texture == 'Clayey silt loam' , 'Silty clay loam', texture)) %>%
  mutate(texture = ifelse(texture == 'Loamy','Loam', texture)) %>%
  mutate(texture = ifelse(texture == 'Sandy','Sand', texture)) %>%
  mutate(texture = ifelse(texture == 'Sandy ','Sand', texture)) %>%
  mutate(texture = ifelse(texture == 'Clayey silt ','Silty clay', texture)) %>%
  mutate(texture = ifelse(texture == 'Clayey sand ','Sandy clay', texture))

# verify
unique(data_std$texture) 

# call conversion table
txt_table <- read.table("./textural_class_average.txt", 
                        sep="\t", header=T, stringsAsFactors = F)
#textural class list
txt_list <-txt_table$texture

# Add the % if needed
data_std_texture <- data_std %>%
  filter(is.na(clay)|is.na(sand)) %>% 
  mutate(clay = replace(clay, texture == txt_table$texture[1], txt_table$clay[1])) %>%
  mutate(sand = replace(sand, texture == txt_table$texture[1], txt_table$sand[1])) %>% 
  mutate(clay = replace(clay, texture == txt_table$texture[2], txt_table$clay[2])) %>%
  mutate(sand = replace(sand, texture == txt_table$texture[2], txt_table$sand[2])) %>%
  mutate(clay = replace(clay, texture == txt_table$texture[20], txt_table$clay[20])) %>% 
  mutate(sand = replace(sand, texture == txt_table$texture[20], txt_table$sand[20])) %>%
  mutate(clay = replace(clay, texture == txt_table$texture[21], txt_table$clay[21])) %>% 
  mutate(sand = replace(sand, texture == txt_table$texture[21], txt_table$sand[21])) %>%
  mutate(clay = replace(clay, texture == txt_table$texture[22], txt_table$clay[22])) %>% 
  mutate(sand = replace(sand, texture == txt_table$texture[22], txt_table$sand[22])) %>%
  mutate(clay = replace(clay, texture == txt_table$texture[23], txt_table$clay[23])) %>% 
  mutate(sand = replace(sand, texture == txt_table$texture[23], txt_table$sand[23])) %>%
  mutate(clay = replace(clay, texture == txt_table$texture[24], txt_table$clay[24])) %>% 
  mutate(sand = replace(sand, texture == txt_table$texture[24], txt_table$sand[24])) %>%
  mutate(clay = replace(clay, texture == txt_table$texture[30], txt_table$clay[30])) %>%
  mutate(sand = replace(sand, texture == txt_table$texture[30], txt_table$sand[30]))
  
# Add the % unit if needed
data_std_texture <- data_std_texture %>%
  mutate(clay_units = replace(clay_units, texture == "Sandy loam", "%")) %>% # add "%" unit to clay_units where texture is "Sandy loam"
  mutate(sand_units = replace(sand_units, texture == "Sandy loam", "%")) %>% # add "%" unit to sand_units where texture is "Sandy loam"
  mutate(clay_units = replace(clay_units, texture == "Silty clay loam", "%")) %>% # add "%" unit to clay_units where texture is "Silty clay loam"
  mutate(sand_units = replace(sand_units, texture == "Silty clay loam", "%")) %>% # add "%" unit to sand_units where texture is "Silty clay loam"
  mutate(clay_units = replace(clay_units, texture == "Silty loam", "%")) %>% # add "%" unit to clay_units where texture is "Silty loam"
  mutate(sand_units = replace(sand_units, texture == "Silty loam", "%")) %>% # add "%" unit to sand_units where texture is "Silty loam"
  mutate(clay_units = replace(clay_units, texture == "Clay loam", "%")) %>% # add "%" unit to clay_units where texture is "Clay loam"
  mutate(sand_units = replace(sand_units, texture == "Clay loam", "%")) %>% # add "%" unit to sand_units where texture is "Clay loam"
  mutate(clay_units = replace(clay_units, texture == "Sand", "%")) %>% # add "%" unit to clay_units where texture is "Sand"
  mutate(sand_units = replace(sand_units, texture == "Sand", "%")) %>% # add "%" unit to sand_units where texture is "Sand"
  mutate(clay_units = replace(clay_units, texture == "Clayey silt", "%")) %>% # add "%" unit to clay_units where texture is "Clayey silt"
  mutate(sand_units = replace(sand_units, texture == "Clayey silt", "%")) %>% # add "%" unit to sand_units where texture is "Clayey silt"
  mutate(clay_units = replace(clay_units, texture == "Loamy sand", "%")) %>% # add "%" unit to clay_units where texture is "Loamy sand"
  mutate(sand_units = replace(sand_units, texture == "Loamy sand", "%")) %>% # add "%" unit to sand_units where texture is "Loamy sand"
  mutate(clay_units = replace(clay_units, texture == "Sandy clay loam", "%")) %>% # add "%" unit to clay_units where texture is "Sandy clay loam"
  mutate(sand_units = replace(sand_units, texture == "Sandy clay loam", "%")) %>% # add "%" unit to sand_units where texture is "Sandy clay loam"
  mutate(clay_units = replace(clay_units, texture == "Silty clay", "%")) %>% # add "%" unit to clay_units where texture is "Silty clay"
  mutate(sand_units = replace(sand_units, texture == "Silty clay", "%")) %>% # add "%" unit to sand_units where texture is "Silty clay"
  mutate(clay_units = replace(clay_units, texture == "Clayey sand", "%")) %>% # add "%" unit to clay_units where texture is "Clayey sand"
  mutate(sand_units = replace(sand_units, texture == "Clayey sand", "%")) %>% # add "%" unit to sand_units where texture is "Clayey sand"
  mutate(clay_units = replace(clay_units, texture == "Sandy clay", "%")) %>% # add "%" unit to clay_units where texture is "Sandy clay"
  mutate(sand_units = replace(sand_units, texture == "Sandy clay", "%")) %>% # add "%" unit to sand_units where texture is "Sandy clay"
  mutate(clay_units = replace(clay_units, texture == "Clay", "%")) %>% # add "%" unit to clay_units where texture is "Clay"
  mutate(sand_units = replace(sand_units, texture == "Clay", "%")) %>% # add "%" unit to sand_units where texture is "Clay"
  mutate(clay_units = replace(clay_units, texture == "Loam", "%")) %>% # add "%" unit to clay_units where texture is "Loam"
  mutate(sand_units = replace(sand_units, texture == "Loam", "%")) # add "%" unit to sand_units where texture is "Loam"

# now all the textural class should be add in % in the clay and sand column

# verify for clay_units
unique(data_std_texture$clay_units[4:18]) # "%"
unique(data_std_texture$clay_units[20:87]) # "%"

# verify for sand_units
unique(data_std_texture$sand_units[4:18]) # "%"
unique(data_std_texture$clay_units[20:87]) # "%"

#### Save the final corrected file ####

# Save the final corrected file with textural classes in an rds object
saveRDS(data_std_texture, file = 'Amélie/data_cleaning_final/data_std_cleaned.rds')

# save the final corrected file as txt file
write.table(data_std_texture,
            "./Amélie/data_cleaning_final/data_std_cleaned.txt", 
            sep="\t", row.names = F, quote = F)

#### join the traits to your data ####

traits <- readRDS('./complete_data.rds')
data_std <- left_join(data_std,traits)

##### visualization of the data ##### EN construction

#### normality ######

# configuring graphics settings
par(mfrow = c(4, 2))
par(mar = c(2, 2, 1, 1))

# histograms of database
hist(data_std$ph, breaks = "FD", main = "pH")
hist(data_std$om, breaks = "FD", main = "OM")
hist(data_std$oc, breaks = "FD", main = "OC")
hist(data_std$SLA, breaks = "FD", main = "SLA") #Error in if (h == 0) { : missing value where TRUE/FALSE needed
hist(data_std$LDMC, breaks = "FD", main = "LDMC") #Error in if (h == 0) { : missing value where TRUE/FALSE needed
hist(data_std$LA, breaks = "FD", main = "LA") #Error in if (h == 0) { : missing value where TRUE/FALSE needed
hist(data_std$cd_ba, breaks = "FD", main = "cd_ba")
hist(data_std$zn_ba, breaks = "FD", main = "zn_ba") #Error in hist.default(data_std$zn_ba, breaks = "FD", main = "zn_ba") : 'x' doit être numérique


#### colinarity ######
# is there colinearity between some variables
HH::vif( ph ~ om +oc + LA +SLA + LDMC + cd_ba + zn_ba, data=data_std)


# normality of the traits

# is LA normally distributed ad is there a transformation that make it more normal?
par(mfrow=c(4,2))
par(mar=c(2,2,1,1)) # Ajuster les marges
hist(traits$LA)
hist(log(traits$LA))
hist(log10(traits$LA))
hist(sqrt(traits$LA))
hist(decostand(traits$LA, method='log', MARGIN=2))
hist(traits$LA^(1/3))
hist(decostand(traits$LA, method='standardize', MARGIN=2))
hist(log2(traits$LA))

# is SLA normally distributed ad is there a transformation that make it more normal?
par(mfrow=c(4,2))
par(mar=c(2,2,1,1)) # Ajuster les marges
hist(traits$SLA)
hist(log(traits$SLA))
hist(log10(traits$SLA))
hist(sqrt(traits$SLA))
hist(decostand(traits$SLA, method='log', MARGIN=2))
hist(traits$SLA^(1/3))
hist(decostand(traits$SLA, method='standardize', MARGIN=2))
hist(log2(traits$SLA))

# is LDMC normally distributed ad is there a transformation that make it more normal?
par(mfrow=c(4,2))
par(mar=c(2,2,1,1)) # Ajuster les marges
hist(traits$LDMC)
hist(log(traits$LDMC)) 
hist(log10(traits$LDMC))
hist(sqrt(traits$LDMC))
hist(decostand(traits$LDMC, method='log', MARGIN=2))
hist(traits$LDMC^(1/3))
hist(decostand(traits$LDMC, method='standardize', MARGIN=2))
hist(logit(traits$LDMC))

#so we can log transform LA and SLA and logit transform LDMC


# visualize your sp with your traits
sp_traits <- na.omit(data_std[,c('LA' , 'SLA' ,'LDMC' )])
# standardized the data to make them comparable
sp_traits.s<-decostand(sp_traits[,c('LA','SLA','LDMC')], method='standardize', MARGIN=2)

pca <-rda(sp_traits.s)
plot(pca)

#### save your corrected and standardize data ####

write.table(data_std,
            "./Amélie/data_std.txt", 
            sep=",", row.names = F, quote = F)