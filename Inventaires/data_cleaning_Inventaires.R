setwd("C:/Users/User/Documents/Scolaire/UDEM/Maîtrise/Hiver 2023/Stage phyto/stage_phytobase")
# DATA CLEANING
## LIBRARY
library(dplyr)
library(stringr)
library(taxize)
library(vegan)
library(readr)

#### call your data  ####
data <- read.csv('./Inventaires/soil_sp_database_Inventaires.csv', sep=';',header = T, dec = '.')

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
    , N_type = as.character(N_type)
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
str(data)

#### all white space to NA ####
data[data == ''] <- NA

#### species names cleaning  ####
# check unique sp list in your database
uni_sp<-as.data.frame(unique(data$name)) #1182 unique species
colnames(uni_sp) <- c('sp') 


# Correct the name according to the species name corrected from TRY
# list_sp_cor already corrected
list_sp_cor <-readRDS('list_sp_cor.rds')
# sp not present in the list
to.be.cor <- anti_join(uni_sp, list_sp_cor, by=c('sp'='user_supplied_name')) # 636 species not on the corrected list, so need to be corrected

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
uni_sp_2<- as.data.frame(unique(match.name$user_supplied_name)) # 606 sp
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
            "./Inventaires/uni_sp_match_names.txt", 
            sep="\t", row.names = F, quote = F)

# open the txt file in excel to make manual corrections
# For all hybrids (with x) both name should be keep
# Save the corrected names in a txt file name, adding _cor to the name of the document
# import back the data 
uni_sp_cor <- read.table("./Inventaires/uni_sp_match_names_cor.txt", 
                         sep="\t", header=T, stringsAsFactors = F)

# eliminate duplicates
uni_sp_cor$dupl2 <- duplicated(uni_sp_cor$user_supplied_name)
uni_sp_cor<-uni_sp_cor %>%  filter(!dupl2 ==T)


# Save the final corrected list in an rds object
saveRDS(uni_sp_cor, file='Inventaires/inv_sp_cor.rds')

#### Join list of corrected names ####
# call the newly corrected list
inv_sp_cor <- readRDS('Inventaires/inv_sp_cor.rds')

# Convert the score column in inv_sp_cor to double
inv_sp_cor$score <- as.numeric(inv_sp_cor$score)

# Combine the corrected lists
list_sp_cor_inv <- bind_rows(inv_sp_cor, list_sp_cor)

# add the corrections to the data
data <- data %>% 
  left_join(list_sp_cor_inv, by=c('name'= 'user_supplied_name'))

data <- data %>% 
  mutate(AccSpeciesName_cor = ifelse(implement == T, alternative, submitted_name))

data <- data %>% 
  dplyr::select(-c(submitted_name, matched_name2, score, alternative, dupl, dupl2))


# Check number of sp now
uni_inv_cor <-unique(data$name)










#### standardized units ####
# Select all the units column
units <- data %>%
  select(contains('unit'))

# check unit conversion for every unit column
colnames(units)
#"om_units"
unique(units$om_units) # "%" "g kg-1" "g kg"
# need to convert to %
data_std <- data %>%
  mutate(om = ifelse(om_units == 'g kg', om/10, om)
         ,om_units = ifelse(om_units == 'g kg', '%', om_units)
         ,om = ifelse(om_units == 'g kg-1', om/10, om)
         ,om_units = ifelse(om_units == 'g kg-1', '%', om_units)
  )
# verify
unique(data_std$om_units) # only "%" 

#"oc_units" 
unique(units$oc_units) # "%" "g kg-1" "mg g-1" "g 100 g-1"
# need to convert  to %
data_std <- data_std %>%
  mutate(
    oc = ifelse(oc_units == 'g kg-1', oc/10, oc)
    ,oc_units = ifelse(oc_units == 'g kg-1', '%', oc_units)
    ,oc = ifelse(oc_units == 'mg g-1', oc/10, oc)
    ,oc_units = ifelse(oc_units == 'mg g-1', '%', oc_units)
    ,oc = ifelse(oc_units == 'g 100 g-1', oc*100, oc)
    ,oc_units = ifelse(oc_units == 'g 100 g-1', '%', oc_units)
  )
# verify
unique(data_std$oc_units) # only "%"


#"clay_units" 
unique(units$clay_units) #"%" "g kg-1"
# need to convert  to %
data_std <- data_std %>%
  mutate(
    clay = ifelse(clay_units == 'g kg-1', clay/10, clay)
    ,clay_units = ifelse(clay_units == 'g kg-1', '%', clay_units)
    ,clay_units = ifelse(clay == 'NA', 'NA', clay_units)
  )
# verify
unique(data_std$clay_units) # only "%"


#"sand_units" 
unique(units$sand_units) # only "%"

#"ec_units"
unique(units$ec_units) # "ds m-1" "us cm" "us cm-1" "ds m" "us/cm" "dS m-1" "dS/m" "%" "mmh os/cm" "ms m-1" "ucm/cm" "ms cm" "us" "dsm-1" "umohs/cm " "ms cm-1"
#need to convert  to mS cm-1
convert <- c("ms cm","mS/cm","ms cm-1")
data_std <- data_std %>%
  mutate(
    ec_units = ifelse(ec_units %in% convert , 'mS cm−1', ec_units)
    ,ec = ifelse(ec_units == 'us cm-1', ec/1000, ec)
    ,ec_units = ifelse(ec_units == 'us cm-1', 'mS cm−1', ec_units)
    ,ec = ifelse(ec_units == 'us/cm', ec/1000, ec)
    ,ec_units = ifelse(ec_units == 'us/cm', 'mS cm−1', ec_units)
    ,ec = ifelse(ec_units == 'us cm', ec/1000, ec)
    ,ec_units = ifelse(ec_units == 'us cm', 'mS cm−1', ec_units)
    ,ec = ifelse(ec_units == 'ds m-1', ec, ec)
    ,ec_units = ifelse(ec_units == 'ds m-1', 'mS cm−1', ec_units)
    ,ec = ifelse(ec_units == 'dS/m', ec, ec)
    ,ec_units = ifelse(ec_units == 'dS/m', 'mS cm−1', ec_units)
    ,ec = ifelse(ec_units == 'dS m-1', ec, ec)
    ,ec_units = ifelse(ec_units == 'dS m-1', 'mS cm−1', ec_units)
    ,ec = ifelse(ec_units == 'ds m', ec, ec)
    ,ec_units = ifelse(ec_units == 'ds m', 'mS cm−1', ec_units)
    ,ec = ifelse(ec_units == 'dsm-1', ec, ec)
    ,ec_units = ifelse(ec_units == 'dsm-1', 'mS cm−1', ec_units)
    ,ec = ifelse(ec_units == 'mmh os/cm', ec, ec)
    ,ec_units = ifelse(ec_units == 'mmh os/cm', 'mS cm−1', ec_units)
    ,ec = ifelse(ec_units == 'umohs/cm ', ec*1000, ec)
    ,ec_units = ifelse(ec_units == 'umohs/cm ', 'mS cm−1', ec_units)
    ,ec = ifelse(ec_units == 'ms m-1', ec/100, ec)
    ,ec_units = ifelse(ec_units == 'ms m-1', 'mS cm−1', ec_units)
    ,ec = ifelse(ec_units == 'mS/m', ec/100, ec)
    ,ec_units = ifelse(ec_units == 'mS/m', 'mS cm−1', ec_units)
    ,ec = ifelse(ec_units == 'us/m', ec/100000, ec)
    ,ec_units = ifelse(ec_units == 'us/m', 'mS cm−1', ec_units)
    ,ec = ifelse(ec_units == 'us', ec/1000, ec)# we assume the unit is uS/cm to make sense of the data
    ,ec_units = ifelse(ec_units == 'us', 'mS cm−1', ec_units)
  )
# verify
unique(data_std$ec_units) # "mS cm−1" "%" "ucm/cm"

#"cec_units"
unique(units$cec_units) # "cmol kg-1" "cmolc kg-1" "meq 100-1" "meq 100 g-1" "cmolc/kg" "cmol/kg" "meq 100g-1" "mmolc dm-3" "cmol+ kg-1" "cmol(+)kg-1" "meq/100g" "molc kg-1" "%" "mmol kg-1" "cmol kg" "cmol+ kg" "cmol/100g soil" "cmolc dm-3" "mM(+)/kg DM" 
# need to convert to cmolc kg-1
convert <- c("meq 100-1","meq 100g-1","meq/100g","meq 100 g−3","cmolc/kg","meq 100 g-1","me/100 g","cmol/kg","cmol kg","cmol kg-1","molc kg-1","cmol(+)kg-1","cmol+ kg-1","cmol+ kg","cmol(+)kg","cmol+/kg")
data_std <- data_std %>%
  mutate(
    cec_units = ifelse(cec_units %in% convert , 'cmolc kg-1', cec_units)
    ,cec = ifelse(cec_units == 'mmol kg-1', cec/10, cec)
    ,cec_units = ifelse(cec_units == 'mmol kg-1', 'cmolc kg-1', cec_units)
    ,cec = ifelse(cec_units == 'mmol(+) kg-1', cec/10, cec)
    ,cec_units = ifelse(cec_units == 'mmol(+) kg-1', 'cmolc kg-1', cec_units)
    ,cec = ifelse(cec_units == 'cmolc dm-3', cec*10, cec)
    ,cec_units = ifelse(cec_units == 'cmolc dm-3', 'cmolc kg-1', cec_units)
    ,cec = ifelse(cec_units == 'mmolc dm-3', cec/10, cec)
    ,cec_units = ifelse(cec_units == 'mmolc dm-3', 'cmolc kg-1', cec_units)
    ,cec = ifelse(cec_units == 'cmol/100g soil', cec/100, cec)
    ,cec_units = ifelse(cec_units == 'cmol/100g soil', 'cmolc kg-1', cec_units)
    ,cec = ifelse(cec_units == "mM(+)/kg DM", cec/10, cec)
    ,cec_units = ifelse(cec_units == "mM(+)/kg DM", 'cmolc kg-1', cec_units)
    ,cec = ifelse(cec_units == '%', cec, cec)
    ,cec_units = ifelse(cec_units == '%', 'cmolc kg-1', cec_units)
    
    
  )
#verify
unique(data_std$cec_units) # "cmolc kg-1", "mmolc/100"


#"N_units" 
unique(units$N_units) # "mg kg-1" "g kg-1" "%" "mg/g" "g.kg-1" "mg/kg" "mg g-1" "g kg dw-1" "g kg" "kg ha-1" "mg kg"
# need to convert to mg kg−1
convert <- c("mg/kg","mg kg","mg kg-1","ppm")
data_std <- data_std %>%
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
    ,N = ifelse(N_units == 'kg/ha', NA, N)
    ,N_units = ifelse(N == 'NA', 'NA', N_units)
  )
#verify
unique(data_std$N_units) # "mg kg-1","mg l-1","g 100 g-1","mg/100g"


#"P_units"   
unique(units$P_units) # "mg kg-1" "kg ha-1" "mg kg" "P2O5" "g.kg-1" "g kg-1" "mg/kg" "ug g-1" "g kg dw-1" "mg kg-1 (P2O5)" "mg 100 g-1" "%" "mg 100g-1" "mg dm-3" "mg g-1" "ppm" "meq/100g" "mg L-1" "g kg" "g P2O5 kg-1""mg P/kg
# need to convert to mg kg-1
convert <- c("mg/kg","mg kg","mg g-1","ppm","mg kg-1 (P2O5)","mg P/kg", 'ug g-1')
data_std <- data_std %>%
  mutate(
    P_units = ifelse(P_units %in% convert , 'mg kg-1', P_units)
    ,P = ifelse(P_units == '%', P*1000, P)
    ,P_units = ifelse(P_units == '%', 'mg kg-1', P_units)
    ,P = ifelse(P_units == 'g.kg-1', P*1000, P)
    ,P_units = ifelse(P_units == 'g.kg-1', 'mg kg-1', P_units)
    ,P = ifelse(P_units == 'g kg-1', P*1000, P)
    ,P_units = ifelse(P_units == 'g kg-1', 'mg kg-1', P_units)
    ,P = ifelse(P_units == 'g kg dw-1', P*1000, P)
    ,P_units = ifelse(P_units == 'g kg dw-1', 'mg kg-1', P_units)
    ,P = ifelse(P_units == 'mg 100 g-1', P/10, P)
    ,P_units = ifelse(P_units == 'mg 100 g-1', 'mg kg-1', P_units)
    ,P = ifelse(P_units == 'mg/100g', P/10, P)
    ,P_units = ifelse(P_units == 'mg/100g', 'mg kg-1', P_units)
    ,P = ifelse(P_units == 'mg dm-3', P*10, P)
    ,P_units = ifelse(P_units == 'mg dm-3', 'mg kg-1', P_units)
    ,P = ifelse(P_units == 'g kg', P*1000, P)
    ,P_units = ifelse(P_units == 'g kg', 'mg kg-1', P_units)
    ,P = ifelse(P_units == 'P2O5', P*0.4364, P)
    ,P_units = ifelse(P_units == 'P2O5', 'mg kg-1', P_units)
    ,P = ifelse(P_units == 'g P2O5 kg-1', P*1000, P)
    ,P_units = ifelse(P_units == 'g P2O5 kg-1', 'mg kg-1', P_units)
    ,P = ifelse(P_units == 'meq/100g', P*(30.974/5)*10, P)
    ,P_units = ifelse(P_units == 'meq/100g', 'mg kg-1', P_units)
    ,P = ifelse(P_units == 'kg/ha', NA, P)
    ,P = ifelse(P_units == 'mg L-1', NA, P)
    ,P_units = ifelse(P == 'NA', 'NA', P_units)
  )
#verify
unique(data_std$P_units)# "mg kg-1","g 100 g-1"


#"units_s"   
unique(units$units_s) # "mg kg-1" "g kg-1" "mg kg" "ppm" "ug.L-1" "ug g-1" "mg/kg" "ug g" "mg dm-3" "ug/g" "mg L-1" "uM/g"
# need to convert to mg kg-1
convert <- c("mg/kg","mg kg","ppm","ug.L-1", "ug g-1","ug g","ug/g","mg dm-3")
data_std <- data_std %>%
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
    ,s = ifelse(units_s == 'uM/g', NA, cu_s)
    ,units_s = ifelse(s == 'NA', 'NA', units_s)
  )
#verify
unique(data_std$units_s) # only "mg kg-1"


#"units_b" 
unique(units$units_b) #"g/plant" "g pot-1" "g per pot" "mg" "g" "mg plant-1" "g plant-1" "kg" "kg acre-1" "g pot -1" "g " "%" "mg ha-1" "g m-2" "g/pot" "g plant -1" "g plant" "t ha-1" "g FM" "kg ha-1" "g m2"
## need to convert to g
convert <- c("g/plant","g pot-1","g per pot","g plant-1", "g pot -1","g ","g/pot","g plant -1","g plant","g m-2","g FM","g m2")
data_std <- data_std %>%
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
    ,units_b = ifelse(b == 'NA', 'NA', units_b)
  )

#verify
unique(data_std$units_b) # only "g"


#"units_te_ba" 
unique(units$units_te_ba) # "mg kg-1" "mg m2 year-1" "mg kg" "ug.g-1" "ppm" "ug g" "ug/g" "mg kg " "mg/kg" "mg plant-1" "mg kg-1 DW" "ug g-1" "mg m-2" "uM/g DW" "mg pot-1" "ug kg-1" "ppm/ppb" "kg ha-1"
# need to convert to mg kg-1 
convert <- c("mg kg","ppm","mg kg ","mg/kg", "mg kg-1 DW","ppm/ppb")
data_std <- data_std %>%
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
    ,ba = ifelse(units_te_ba == 'mg plant-1', cu_ba/ba_total*1000, cu_ba)
    ,units_te_ba = ifelse(units_te_ba == 'mg plant-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'mg pot-1', cu_ba/ba_total*1000, cu_ba)
    ,ba = ifelse(units_te_ba == 'mg pot-1', pb_ba/ba_total*1000, pb_ba)
    ,units_te_ba = ifelse(units_te_ba == 'mg pot-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'kg ha-1', as_ba/ba_leaf*1000000, as_ba)
    ,ba = ifelse(units_te_ba == 'kg ha-1', cu_ba/ba_leaf*1000000, cu_ba)
    ,ba = ifelse(units_te_ba == 'kg ha-1', pb_ba/ba_leaf*1000000, pb_ba)
    ,ba = ifelse(units_te_ba == 'kg ha-1', zn_ba/ba_leaf*1000000, zn_ba)
    ,ba = ifelse(units_te_ba == 'kg ha-1', mn_ba/ba_leaf*1000000, mn_ba)
    ,ba = ifelse(units_te_ba == 'kg ha-1', cr_ba/ba_leaf*1000000, cr_ba)
    ,units_te_ba = ifelse(units_te_ba == 'kg ha-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'mg m-2', as_ba/ba_leaf*1000, as_ba)
    ,ba = ifelse(units_te_ba == 'mg m-2', cd_ba/ba_leaf*1000, cd_ba)
    ,ba = ifelse(units_te_ba == 'mg m-2', cu_ba/ba_leaf*1000, cu_ba)
    ,ba = ifelse(units_te_ba == 'mg m-2', pb_ba/ba_leaf*1000, pb_ba)
    ,ba = ifelse(units_te_ba == 'mg m-2', zn_ba/ba_leaf*1000, zn_ba)
    ,ba = ifelse(units_te_ba == 'mg m-2', se_ba/ba_leaf*1000, se_ba)
    ,units_te_ba = ifelse(units_te_ba == 'mg m-2', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'uM/g DW', NA, as_ba)
    ,ba = ifelse(units_te_ba == 'uM/g DW', NA, cd_ba)
    ,ba = ifelse(units_te_ba == 'uM/g DW', NA, cu_ba)
    ,ba = ifelse(units_te_ba == 'uM/g DW', NA, pb_ba)
    ,ba = ifelse(units_te_ba == 'uM/g DW', NA, zn_ba)
    ,ba = ifelse(units_te_ba == 'uM/g DW', NA, se_ba)
    ,ba = ifelse(units_te_ba == 'uM/g DW', NA, ni_ba)
    ,ba = ifelse(units_te_ba == 'uM/g DW', NA, co_ba)
    ,ba = ifelse(units_te_ba == 'uM/g DW', NA, mn_ba)
    ,ba = ifelse(units_te_ba == 'uM/g DW', NA, cr_ba)
    ,ba = ifelse(units_te_ba == 'uM/g DW', NA, hg_ba)
    ,units_te_ba = ifelse(ba == 'NA', 'NA', units_te_ba)
  )
# verify
unique(data_std$units_te_ba) # only "mg kg-1"


#"units_te_br"   
unique(units$units_te_br) # "mg kg-1" "mg kg" "ug.g-1" "ug g" "ug/g" "mg plant-1" "mg kg-1 DW" "ug g-1" "mg m-2" "uM/g DW" "mg pot-1" 
# need to convert to mg kg-1 
convert <- c("mg kg", "mg kg-1 DW")
data_std <- data_std %>%
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
    ,br = ifelse(units_te_br == 'uM/g DW', NA, as_br)
    ,br = ifelse(units_te_br == 'uM/g DW', NA, cd_br)
    ,br = ifelse(units_te_br == 'uM/g DW', NA, cu_br)
    ,br = ifelse(units_te_br == 'uM/g DW', NA, pb_br)
    ,br = ifelse(units_te_br == 'uM/g DW', NA, zn_br)
    ,br = ifelse(units_te_br == 'uM/g DW', NA, se_br)
    ,br = ifelse(units_te_br == 'uM/g DW', NA, ni_br)
    ,br = ifelse(units_te_br == 'uM/g DW', NA, co_br)
    ,br = ifelse(units_te_br == 'uM/g DW', NA, mn_br)
    ,br = ifelse(units_te_br == 'uM/g DW', NA, cr_br)
    ,br = ifelse(units_te_br == 'uM/g DW', NA, hg_br)
    ,units_te_br = ifelse(br == 'NA', 'NA', units_te_br)
  )
# verify
unique(data_std$units_te_br) # only "mg kg-1"


#"units_te_ba_1"
unique(units$units_te_ba_1) # "mg kg-1" "ug.g-1" "ug g" "mg kg" "mg plant-1" "ppm" "uM/g DW" "ug/g" "ug kg-1" "ug g-1"
# need to convert to mg kg-1 
convert <- c("mg kg","ppm")
data_std <- data_std %>%
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
    ,ba_1 = ifelse(units_te_ba_1 == 'uM/g DW', NA, as_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'uM/g DW', NA, cd_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'uM/g DW', NA, cu_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'uM/g DW', NA, pb_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'uM/g DW', NA, zn_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'uM/g DW', NA, se_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'uM/g DW', NA, ni_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'uM/g DW', NA, co_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'uM/g DW', NA, mn_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'uM/g DW', NA, cr_ba_1)
    ,ba_1 = ifelse(units_te_ba_1 == 'uM/g DW', NA, hg_ba_1)
    ,units_te_ba_1 = ifelse(ba_1 == 'NA', 'NA', units_te_ba_1)
  )
# verify
unique(data_std$units_te_ba_1) # only "mg kg-1"


#"units_te_ba_2"
unique(units$units_te_ba_2) # "mg kg-1" "ug g"    "mg kg"
#need to convert to mg kg-1 
convert <- c("mg kg")
data_std <- data_std %>%
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
unique(data$units_density)# "g/m3"


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
syn_shoots <- c("Shoots","flowering shoots","shoots","Shoot","shoot","Shoot ","Shoots ","overground organs","leaves+stems", "stems + leaves", "stems+leaves", 
                "Stems, leaves and flowers", "Whole top part", "Leaves +stems","Above-ground parts", "Above ground parts", 
                "Aboveground parts","Shoots/leaves", "Aerial part", "Aerial parts", "Stems and leaves", "Leaves and stems"  )
syn_stems <- c("Stalks","Twigs","Stems","stems","stem","Stem","stalk","Twig","Branch","branch","branches","Branches",
               "Lower stems","Culms","Stubble","stalks","stem and flowers")
syn_leaves <- c("leaf","Leaf","Leaves","leaves","Leave","Leafs","Foliage","Aciculum", "Unwashed leaves","Needle", "Lower leaves",
                "Washed leaves", "Leaf/Needle")
syn_flowers <- c("flowers","Heads","Head","Spikelets","head","Flowers","Inflorescence")
syn_fruits <- c("Berry","Edible parts", "infructescence","Fruit")
syn_wood <- c("wood","Woody","Trunk wood", "Bark","Trunk" )
syn_whole<- c("leaves + roots","whole sample (leaves+stems+roots)", "whole plant","Whole plant" )                

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
unique(data_std$organs_ba) # "leaves"  "shoots"  "whole"   "stems" "wood" "fruits" "flowers" ###"Bulb"
unique(data_std$organs_ba_1) # "stems"   "shoots"  "leaves"  "fruits" "wood"
unique(data_std$organs_ba_2) # "flowers" fruits"
unique(data_std$organs_ba_3) # only "fruits"


# organs_br
unique(data$organs_br)
# conversion
syn_roots <- c("root","roots ","rhizomes","Root","Roots")
data_std <- data_std %>%
  mutate(organs_br = ifelse(organs_br %in% syn_roots , 'roots', organs_br))
# verify
unique(data_std$organs_br) # only "roots"


#### outliers and errors in numerical data #### 

# see if duplicates data entries
unique_obs<- data_std[duplicated(data_std)] # 0 variables--> 0 duplicates to eliminate

# list of variables that need to be verify
num_cols <- unlist(lapply(data_std, is.numeric)) #identify numerical data 
data_num <- data_std[ , num_cols]  # 96 variables

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

# you can also write it with number from the list to save time, as follow with Covidence as number 1 in the list
outliers <- data_std %>% 
  filter(data_num[,1] < num_range$min_value[1] | data_num[,1] > num_range$max_value[1] )
outliers

# isolate the outliers lines for the variable 'year'
outliers <- data_std %>% 
  filter(year < num_range$min_value[num_range$variables == 'year'] | year > num_range$max_value[num_range$variables == 'year'] )
outliers

# isolate the outliers lines for the variable 'expe_t'
outliers <- data_std %>% 
  filter(expe_t < num_range$min_value[num_range$variables == 'expe_t'] | expe_t > num_range$max_value[num_range$variables == 'expe_t'] )
outliers

# isolate the outliers lines for the variable 'mat'
outliers <- data_std %>% 
  filter(mat < num_range$min_value[num_range$variables == 'mat'] | mat > num_range$max_value[num_range$variables == 'mat'] )
outliers

# isolate the outliers lines for the variable 'map'
outliers <- data_std %>% 
  filter(map < num_range$min_value[num_range$variables == 'map'] | map > num_range$max_value[num_range$variables == 'map'] )
outliers

# isolate the outliers lines for the variable 'ph'
outliers <- data_std %>% 
  filter(ph < num_range$min_value[num_range$variables == 'ph'] | ph > num_range$max_value[num_range$variables == 'ph'] )
outliers

# isolate the outliers lines for the variable 'om'
outliers <- data_std %>% 
  filter(om < num_range$min_value[num_range$variables == 'om'] | om > num_range$max_value[num_range$variables == 'om'] )
outliers

# isolate the outliers lines for the variable 'oc'
outliers <- data_std %>% 
  filter(oc < num_range$min_value[num_range$variables == 'oc'] | oc > num_range$max_value[num_range$variables == 'oc'] )
outliers

# isolate the outliers lines for the variable 'clay'
outliers <- data_std %>% 
  filter(clay < num_range$min_value[num_range$variables == 'clay'] | clay > num_range$max_value[num_range$variables == 'clay'] )
outliers

# isolate the outliers lines for the variable 'sand'
outliers <- data_std %>% 
  filter(sand < num_range$min_value[num_range$variables == 'sand'] | sand > num_range$max_value[num_range$variables == 'sand'] )
outliers

# isolate the outliers lines for the variable 'ec'
outliers <- data_std %>% 
  filter(ec < num_range$min_value[num_range$variables == 'ec'] | ec > num_range$max_value[num_range$variables == 'ec'] )
outliers

# isolate the outliers lines for the variable 'cec'
outliers <- data_std %>% 
  filter(cec < num_range$min_value[num_range$variables == 'cec'] | cec > num_range$max_value[num_range$variables == 'cec'] )
outliers

# isolate the outliers lines for the variable 'N'
outliers <- data_std %>% 
  filter(N < num_range$min_value[num_range$variables == 'n'] | N > num_range$max_value[num_range$variables == 'n'] )
outliers

# isolate the outliers lines for the variable 'P'
outliers <- data_std %>% 
  filter(P < num_range$min_value[num_range$variables == 'p'] | P > num_range$max_value[num_range$variables == 'p'] )
outliers

# isolate the outliers lines for the variable 'as_s'
outliers <- data_std %>% 
  filter(as_s < num_range$min_value[num_range$variables == 'as_s'] | as_s > num_range$max_value[num_range$variables == 'as_s'] )
outliers

# isolate the outliers lines for the variable 'cd_s'
outliers <- data_std %>% 
  filter(cd_s < num_range$min_value[num_range$variables == 'cd_s'] | cd_s > num_range$max_value[num_range$variables == 'cd_s'] )
outliers

# isolate the outliers lines for the variable 'cu_s'
outliers <- data_std %>% 
  filter(cu_s < num_range$min_value[num_range$variables == 'cu_s'] | cu_s > num_range$max_value[num_range$variables == 'cu_s'] )
outliers

# isolate the outliers lines for the variable 'pb_s'
outliers <- data_std %>% 
  filter(pb_s < num_range$min_value[num_range$variables == 'pb_s'] | pb_s > num_range$max_value[num_range$variables == 'pb_s'] )
outliers

# isolate the outliers lines for the variable 'zn_s'
outliers <- data_std %>% 
  filter(zn_s < num_range$min_value[num_range$variables == 'zn_s'] | zn_s > num_range$max_value[num_range$variables == 'zn_s'] )
outliers

# isolate the outliers lines for the variable 'se_s'
outliers <- data_std %>% 
  filter(se_s < num_range$min_value[num_range$variables == 'se_s'] | se_s > num_range$max_value[num_range$variables == 'se_s'] )
outliers

# isolate the outliers lines for the variable 'ni_s'
outliers <- data_std %>% 
  filter(ni_s < num_range$min_value[num_range$variables == 'ni_s'] | ni_s > num_range$max_value[num_range$variables == 'ni_s'] )
outliers

# isolate the outliers lines for the variable 'co_s'
outliers <- data_std %>% 
  filter(co_s < num_range$min_value[num_range$variables == 'co_s'] | co_s > num_range$max_value[num_range$variables == 'co_s'] )
outliers

# isolate the outliers lines for the variable 'mn_s'
outliers <- data_std %>% 
  filter(mn_s < num_range$min_value[num_range$variables == 'mn_s'] | mn_s > num_range$max_value[num_range$variables == 'mn_s'] )
outliers

# isolate the outliers lines for the variable 'cr_s'
outliers <- data_std %>% 
  filter(cr_s < num_range$min_value[num_range$variables == 'cr_s'] | cr_s > num_range$max_value[num_range$variables == 'cr_s'] )
outliers

# isolate the outliers lines for the variable 'hg_s'
outliers <- data_std %>% 
  filter(hg_s < num_range$min_value[num_range$variables == 'hg_s'] | hg_s > num_range$max_value[num_range$variables == 'hg_s'] )
outliers

# isolate the outliers lines for the variable 'n_s'
outliers <- data_std %>% 
  filter(n_s < num_range$min_value[num_range$variables == 'n_s'] | n_s > num_range$max_value[num_range$variables == 'n_s'] )
outliers

# isolate the outliers lines for the variable 'ba_total'
outliers <- data_std %>% 
  filter(ba_total < num_range$min_value[num_range$variables == 'ba_total'] | ba_total > num_range$max_value[num_range$variables == 'ba_total'] )
outliers

# isolate the outliers lines for the variable 'ba_stem'
outliers <- data_std %>% 
  filter(ba_stem < num_range$min_value[num_range$variables == 'ba_stem'] | ba_stem > num_range$max_value[num_range$variables == 'ba_stem'] )
outliers

# isolate the outliers lines for the variable 'ba_leaf'
outliers <- data_std %>% 
  filter(ba_leaf < num_range$min_value[num_range$variables == 'ba_leaf'] | ba_leaf > num_range$max_value[num_range$variables == 'ba_leaf'] )
outliers

# isolate the outliers lines for the variable 'br'
outliers <- data_std %>% 
  filter(br < num_range$min_value[num_range$variables == 'br'] | br > num_range$max_value[num_range$variables == 'br'] )
outliers

# isolate the outliers lines for the variable 'as_ba'
outliers <- data_std %>% 
  filter(as_ba < num_range$min_value[num_range$variables == 'as_ba'] | as_ba > num_range$max_value[num_range$variables == 'as_ba'] )
outliers

# isolate the outliers lines for the variable 'cd_ba'
outliers <- data_std %>% 
  filter(cd_ba < num_range$min_value[num_range$variables == 'cd_ba'] | cd_ba > num_range$max_value[num_range$variables == 'cd_ba'] )
outliers

# isolate the outliers lines for the variable 'cu_ba'
outliers <- data_std %>% 
  filter(cu_ba < num_range$min_value[num_range$variables == 'cu_ba'] | cu_ba > num_range$max_value[num_range$variables == 'cu_ba'] )
outliers

# isolate the outliers lines for the variable 'pb_ba'
outliers <- data_std %>% 
  filter(pb_ba < num_range$min_value[num_range$variables == 'pb_ba'] | pb_ba > num_range$max_value[num_range$variables == 'pb_ba'] )
outliers

# isolate the outliers lines for the variable 'zn_ba'
outliers <- data_std %>% 
  filter(zn_ba < num_range$min_value[num_range$variables == 'zn_ba'] | zn_ba > num_range$max_value[num_range$variables == 'zn_ba'] )
outliers

# isolate the outliers lines for the variable 'se_ba'
outliers <- data_std %>% 
  filter(se_ba < num_range$min_value[num_range$variables == 'se_ba'] | se_ba > num_range$max_value[num_range$variables == 'se_ba'] )
outliers

# isolate the outliers lines for the variable 'ni_ba'
outliers <- data_std %>% 
  filter(ni_ba < num_range$min_value[num_range$variables == 'ni_ba'] | ni_ba > num_range$max_value[num_range$variables == 'ni_ba'] )
outliers

# isolate the outliers lines for the variable 'co_ba'
outliers <- data_std %>% 
  filter(co_ba < num_range$min_value[num_range$variables == 'co_ba'] | co_ba > num_range$max_value[num_range$variables == 'co_ba'] )
outliers

# isolate the outliers lines for the variable 'mn_ba'
outliers <- data_std %>% 
  filter(mn_ba < num_range$min_value[num_range$variables == 'mn_ba'] | mn_ba > num_range$max_value[num_range$variables == 'mn_ba'] )
outliers

# isolate the outliers lines for the variable 'cr_ba'
outliers <- data_std %>% 
  filter(cr_ba < num_range$min_value[num_range$variables == 'cr_ba'] | cr_ba > num_range$max_value[num_range$variables == 'cr_ba'] )
outliers

# isolate the outliers lines for the variable 'hg_ba'
outliers <- data_std %>% 
  filter(hg_ba < num_range$min_value[num_range$variables == 'hg_ba'] | hg_ba > num_range$max_value[num_range$variables == 'hg_ba'] )
outliers

# isolate the outliers lines for the variable 'n_te_ba'
outliers <- data_std %>% 
  filter(n_te_ba < num_range$min_value[num_range$variables == 'n_te_ba'] | n_te_ba > num_range$max_value[num_range$variables == 'n_te_ba'] )
outliers

# isolate the outliers lines for the variable 'as_br'
outliers <- data_std %>% 
  filter(as_br < num_range$min_value[num_range$variables == 'as_br'] | as_br > num_range$max_value[num_range$variables == 'as_br'] )
outliers

# isolate the outliers lines for the variable 'cd_br'
outliers <- data_std %>% 
  filter(cd_br < num_range$min_value[num_range$variables == 'cd_br'] | cd_br > num_range$max_value[num_range$variables == 'cd_br'] )
outliers

# isolate the outliers lines for the variable 'cu_br'
outliers <- data_std %>% 
  filter(cu_br < num_range$min_value[num_range$variables == 'cu_br'] | cu_br > num_range$max_value[num_range$variables == 'cu_br'] )
outliers

# isolate the outliers lines for the variable 'pb_br'
outliers <- data_std %>% 
  filter(pb_br < num_range$min_value[num_range$variables == 'pb_br'] | pb_br > num_range$max_value[num_range$variables == 'pb_br'] )
outliers

# isolate the outliers lines for the variable 'zn_br'
outliers <- data_std %>% 
  filter(zn_br < num_range$min_value[num_range$variables == 'zn_br'] | zn_br > num_range$max_value[num_range$variables == 'zn_br'] )
outliers

# isolate the outliers lines for the variable 'se_br'
outliers <- data_std %>% 
  filter(se_br < num_range$min_value[num_range$variables == 'se_br'] | se_br > num_range$max_value[num_range$variables == 'se_br'] )
outliers

# isolate the outliers lines for the variable 'ni_br'
outliers <- data_std %>% 
  filter(ni_br < num_range$min_value[num_range$variables == 'ni_br'] | ni_br > num_range$max_value[num_range$variables == 'ni_br'] )
outliers

# isolate the outliers lines for the variable 'co_br'
outliers <- data_std %>% 
  filter(co_br < num_range$min_value[num_range$variables == 'co_br'] | co_br > num_range$max_value[num_range$variables == 'co_br'] )
outliers

# isolate the outliers lines for the variable 'mn_br'
outliers <- data_std %>% 
  filter(mn_br < num_range$min_value[num_range$variables == 'mn_br'] | mn_br > num_range$max_value[num_range$variables == 'mn_br'] )
outliers

# isolate the outliers lines for the variable 'cr_br'
outliers <- data_std %>% 
  filter(cr_br < num_range$min_value[num_range$variables == 'cr_br'] | cr_br > num_range$max_value[num_range$variables == 'cr_br'] )
outliers

# isolate the outliers lines for the variable 'hg_br'
outliers <- data_std %>% 
  filter(hg_br < num_range$min_value[num_range$variables == 'hg_br'] | hg_br > num_range$max_value[num_range$variables == 'hg_br'] )
outliers

# isolate the outliers lines for the variable 'n_te_br'
outliers <- data_std %>% 
  filter(n_te_br < num_range$min_value[num_range$variables == 'n_te_br'] | n_te_br > num_range$max_value[num_range$variables == 'n_te_br'] )
outliers

# isolate the outliers lines for the variable 'as_ba_1'
outliers <- data_std %>% 
  filter(as_ba_1 < num_range$min_value[num_range$variables == 'as_ba'] | as_ba_1 > num_range$max_value[num_range$variables == 'as_ba'] )
outliers

# isolate the outliers lines for the variable 'cd_ba_1'
outliers <- data_std %>% 
  filter(cd_ba_1 < num_range$min_value[num_range$variables == 'cd_ba'] | cd_ba_1 > num_range$max_value[num_range$variables == 'cd_ba'] )
outliers

# isolate the outliers lines for the variable 'cu_ba_1'
outliers <- data_std %>% 
  filter(cu_ba_1 < num_range$min_value[num_range$variables == 'cu_ba'] | cu_ba_1 > num_range$max_value[num_range$variables == 'cu_ba'] )
outliers

# isolate the outliers lines for the variable 'pb_ba_1'
outliers <- data_std %>% 
  filter(pb_ba_1 < num_range$min_value[num_range$variables == 'pb_ba'] | pb_ba_1 > num_range$max_value[num_range$variables == 'pb_ba'] )
outliers

# isolate the outliers lines for the variable 'zn_ba_1'
outliers <- data_std %>% 
  filter(zn_ba_1 < num_range$min_value[num_range$variables == 'zn_ba'] | zn_ba_1 > num_range$max_value[num_range$variables == 'zn_ba'] )
outliers

# isolate the outliers lines for the variable 'se_ba_1'
outliers <- data_std %>% 
  filter(se_ba_1 < num_range$min_value[num_range$variables == 'se_ba'] | se_ba_1 > num_range$max_value[num_range$variables == 'se_ba'] )
outliers

# isolate the outliers lines for the variable 'ni_ba_1'
outliers <- data_std %>% 
  filter(ni_ba_1 < num_range$min_value[num_range$variables == 'ni_ba'] | ni_ba_1 > num_range$max_value[num_range$variables == 'ni_ba'] )
outliers

# isolate the outliers lines for the variable 'co_ba_1'
outliers <- data_std %>% 
  filter(co_ba_1 < num_range$min_value[num_range$variables == 'co_ba'] | co_ba_1 > num_range$max_value[num_range$variables == 'co_ba'] )
outliers

# isolate the outliers lines for the variable 'mn_ba_1'
outliers <- data_std %>% 
  filter(mn_ba_1 < num_range$min_value[num_range$variables == 'mn_ba'] | mn_ba_1 > num_range$max_value[num_range$variables == 'mn_ba'] )
outliers

# isolate the outliers lines for the variable 'cr_ba_1'
outliers <- data_std %>% 
  filter(cr_ba_1 < num_range$min_value[num_range$variables == 'cr_ba'] | cr_ba_1 > num_range$max_value[num_range$variables == 'cr_ba'] )
outliers

# isolate the outliers lines for the variable 'hg_ba_1'
outliers <- data_std %>% 
  filter(hg_ba_1 < num_range$min_value[num_range$variables == 'hg_ba'] | hg_ba_1 > num_range$max_value[num_range$variables == 'hg_ba'] )
outliers

# isolate the outliers lines for the variable 'n_te_ba_1'
outliers <- data_std %>% 
  filter(n_te_ba_1 < num_range$min_value[num_range$variables == 'n_te_ba'] | n_te_ba_1 > num_range$max_value[num_range$variables == 'n_te_ba'] )
outliers

# isolate the outliers lines for the variable 'as_ba_2'
outliers <- data_std %>% 
  filter(as_ba_2 < num_range$min_value[num_range$variables == 'as_ba'] | as_ba_2 > num_range$max_value[num_range$variables == 'as_ba'] )
outliers

# isolate the outliers lines for the variable 'cd_ba_2'
outliers <- data_std %>% 
  filter(cd_ba_2 < num_range$min_value[num_range$variables == 'cd_ba'] | cd_ba_2 > num_range$max_value[num_range$variables == 'cd_ba'] )
outliers

# isolate the outliers lines for the variable 'cu_ba_2'
outliers <- data_std %>% 
  filter(cu_ba_2 < num_range$min_value[num_range$variables == 'cu_ba'] | cu_ba_2 > num_range$max_value[num_range$variables == 'cu_ba'] )
outliers

# isolate the outliers lines for the variable 'pb_ba_2'
outliers <- data_std %>% 
  filter(pb_ba_2 < num_range$min_value[num_range$variables == 'pb_ba'] | pb_ba_2 > num_range$max_value[num_range$variables == 'pb_ba'] )
outliers

# isolate the outliers lines for the variable 'zn_ba_2'
outliers <- data_std %>% 
  filter(zn_ba_2 < num_range$min_value[num_range$variables == 'zn_ba'] | zn_ba_2 > num_range$max_value[num_range$variables == 'zn_ba'] )
outliers

# isolate the outliers lines for the variable 'se_ba_2'
outliers <- data_std %>% 
  filter(se_ba_2 < num_range$min_value[num_range$variables == 'se_ba'] | se_ba_2 > num_range$max_value[num_range$variables == 'se_ba'] )
outliers

# isolate the outliers lines for the variable 'ni_ba_2'
outliers <- data_std %>% 
  filter(ni_ba_2 < num_range$min_value[num_range$variables == 'ni_ba'] | ni_ba_2 > num_range$max_value[num_range$variables == 'ni_ba'] )
outliers

# isolate the outliers lines for the variable 'co_ba_2'
outliers <- data_std %>% 
  filter(co_ba_2 < num_range$min_value[num_range$variables == 'co_ba'] | co_ba_2 > num_range$max_value[num_range$variables == 'co_ba'] )
outliers

# isolate the outliers lines for the variable 'mn_ba_2'
outliers <- data_std %>% 
  filter(mn_ba_2 < num_range$min_value[num_range$variables == 'mn_ba'] | mn_ba_2 > num_range$max_value[num_range$variables == 'mn_ba'] )
outliers

# isolate the outliers lines for the variable 'cr_ba_2'
outliers <- data_std %>% 
  filter(cr_ba_2 < num_range$min_value[num_range$variables == 'cr_ba'] | cr_ba_2 > num_range$max_value[num_range$variables == 'cr_ba'] )
outliers

# isolate the outliers lines for the variable 'hg_ba_2'
outliers <- data_std %>% 
  filter(hg_ba_2 < num_range$min_value[num_range$variables == 'hg_ba'] | hg_ba_2 > num_range$max_value[num_range$variables == 'hg_ba'] )
outliers

# isolate the outliers lines for the variable 'n_te_ba_2'
outliers <- data_std %>% 
  filter(n_te_ba_2 < num_range$min_value[num_range$variables == 'n_te_ba'] | n_te_ba_2 > num_range$max_value[num_range$variables == 'n_te_ba'] )
outliers

# isolate the outliers lines for the variable 'as_ba_3'
outliers <- data_std %>% 
  filter(as_ba_3 < num_range$min_value[num_range$variables == 'as_ba'] | as_ba_3 > num_range$max_value[num_range$variables == 'as_ba'] )
outliers

# isolate the outliers lines for the variable 'cd_ba_3'
outliers <- data_std %>% 
  filter(cd_ba_3 < num_range$min_value[num_range$variables == 'cd_ba'] | cd_ba_3 > num_range$max_value[num_range$variables == 'cd_ba'] )
outliers

# isolate the outliers lines for the variable 'cu_ba_3'
outliers <- data_std %>% 
  filter(cu_ba_3 < num_range$min_value[num_range$variables == 'cu_ba'] | cu_ba_3 > num_range$max_value[num_range$variables == 'cu_ba'] )
outliers

# isolate the outliers lines for the variable 'pb_ba_3'
outliers <- data_std %>% 
  filter(pb_ba_3 < num_range$min_value[num_range$variables == 'pb_ba'] | pb_ba_3 > num_range$max_value[num_range$variables == 'pb_ba'] )
outliers

# isolate the outliers lines for the variable 'zn_ba_3'
outliers <- data_std %>% 
  filter(zn_ba_3 < num_range$min_value[num_range$variables == 'zn_ba'] | zn_ba_3 > num_range$max_value[num_range$variables == 'zn_ba'] )
outliers

# isolate the outliers lines for the variable 'se_ba_3'
outliers <- data_std %>% 
  filter(se_ba_3 < num_range$min_value[num_range$variables == 'se_ba'] | se_ba_3 > num_range$max_value[num_range$variables == 'se_ba'] )
outliers

# isolate the outliers lines for the variable 'ni_ba_3'
outliers <- data_std %>% 
  filter(ni_ba_3 < num_range$min_value[num_range$variables == 'ni_ba'] | ni_ba_3 > num_range$max_value[num_range$variables == 'ni_ba'] )
outliers

# isolate the outliers lines for the variable 'co_ba_3'
outliers <- data_std %>% 
  filter(co_ba_3 < num_range$min_value[num_range$variables == 'co_ba'] | co_ba_3 > num_range$max_value[num_range$variables == 'co_ba'] )
outliers

# isolate the outliers lines for the variable 'mn_ba_3'
outliers <- data_std %>% 
  filter(mn_ba_3 < num_range$min_value[num_range$variables == 'mn_ba'] | mn_ba_3 > num_range$max_value[num_range$variables == 'mn_ba'] )
outliers

# isolate the outliers lines for the variable 'cr_ba_3'
outliers <- data_std %>% 
  filter(cr_ba_3 < num_range$min_value[num_range$variables == 'cr_ba'] | cr_ba_3 > num_range$max_value[num_range$variables == 'cr_ba'] )
outliers

# isolate the outliers lines for the variable 'hg_ba_3'
outliers <- data_std %>% 
  filter(hg_ba_3 < num_range$min_value[num_range$variables == 'hg_ba'] | hg_ba_3 > num_range$max_value[num_range$variables == 'hg_ba'] )
outliers

# isolate the outliers lines for the variable 'n_te_ba_3'
outliers <- data_std %>% 
  filter(n_te_ba_3 < num_range$min_value[num_range$variables == 'n_te_ba'] | n_te_ba_3 > num_range$max_value[num_range$variables == 'n_te_ba'] )
outliers



#### Add clay and sand % according to textural class of soils###

# check for unique terms in 'texture'
unique(data$texture) 

# standardize textural terms
data_std <- data_std %>%
  mutate(texture = ifelse(texture == 'Clayey silt loam' , 'Silty clay loam', texture)) %>%
  mutate(texture = ifelse(texture == 'Loamy','Loam', texture)) %>%
  mutate(texture = ifelse(texture == 'Sandy','Sand', texture)) %>%
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
data_std <- data_std %>%
  filter(is.na(clay)|is.na(sand)) %>% 
  mutate(clay = ifelse(texture == txt_table$texture[1] , txt_table$clay[1], clay)) %>% 
  mutate(sand = ifelse(texture == txt_table$texture[1] , txt_table$sand[1], sand)) %>% 
  mutate(clay = ifelse(texture == txt_table$texture[2] , txt_table$clay[2], clay)) %>% 
  mutate(sand = ifelse(texture == txt_table$texture[2] , txt_table$sand[2], sand)) %>%
  mutate(clay = ifelse(texture == txt_table$texture[3] , txt_table$clay[3], clay)) %>% 
  mutate(sand = ifelse(texture == txt_table$texture[3] , txt_table$sand[3], sand)) %>%
  mutate(clay = ifelse(texture == txt_table$texture[4] , txt_table$clay[4], clay)) %>% 
  mutate(sand = ifelse(texture == txt_table$texture[4] , txt_table$sand[4], sand)) %>%
  mutate(clay = ifelse(texture == txt_table$texture[5] , txt_table$clay[5], clay)) %>% 
  mutate(sand = ifelse(texture == txt_table$texture[5] , txt_table$sand[5], sand)) %>%
  mutate(clay = ifelse(texture == txt_table$texture[6] , txt_table$clay[6], clay)) %>% 
  mutate(sand = ifelse(texture == txt_table$texture[6] , txt_table$sand[6], sand)) %>%
  mutate(clay = ifelse(texture == txt_table$texture[7] , txt_table$clay[7], clay)) %>% 
  mutate(sand = ifelse(texture == txt_table$texture[7] , txt_table$sand[7], sand)) %>%
  mutate(clay = ifelse(texture == txt_table$texture[8] , txt_table$clay[8], clay)) %>% 
  mutate(sand = ifelse(texture == txt_table$texture[8] , txt_table$sand[8], sand)) %>%
  mutate(clay = ifelse(texture == txt_table$texture[9] , txt_table$clay[9], clay)) %>% 
  mutate(sand = ifelse(texture == txt_table$texture[9] , txt_table$sand[9], sand)) %>%
  mutate(clay = ifelse(texture == txt_table$texture[10] , txt_table$clay[10], clay)) %>% 
  mutate(sand = ifelse(texture == txt_table$texture[10] , txt_table$sand[10], sand)) %>%
  mutate(clay = ifelse(texture == txt_table$texture[11] , txt_table$clay[11], clay)) %>% 
  mutate(sand = ifelse(texture == txt_table$texture[11] , txt_table$sand[11], sand)) %>%
  mutate(clay = ifelse(texture == txt_table$texture[12] , txt_table$clay[12], clay)) %>% 
  mutate(sand = ifelse(texture == txt_table$texture[12] , txt_table$sand[12], sand)) %>%
  mutate(clay = ifelse(texture == txt_table$texture[13] , txt_table$clay[13], clay)) %>% 
  mutate(sand = ifelse(texture == txt_table$texture[13] , txt_table$sand[13], sand)) %>%
  mutate(clay = ifelse(texture == txt_table$texture[14] , txt_table$clay[14], clay)) %>% 
  mutate(sand = ifelse(texture == txt_table$texture[14] , txt_table$sand[14], sand)) %>%
  mutate(clay = ifelse(texture == txt_table$texture[15] , txt_table$clay[15], clay)) %>% 
  mutate(sand = ifelse(texture == txt_table$texture[15] , txt_table$sand[15], sand)) %>%
  mutate(clay = ifelse(texture == txt_table$texture[16] , txt_table$clay[16], clay)) %>% 
  mutate(sand = ifelse(texture == txt_table$texture[16] , txt_table$sand[16], sand)) %>%
  mutate(clay = ifelse(texture == txt_table$texture[17] , txt_table$clay[17], clay)) %>% 
  mutate(sand = ifelse(texture == txt_table$texture[17] , txt_table$sand[17], sand))


# Add the % unit if needed
data_std <- data_std %>%
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
unique(data_std$clay_units) # "%"

# verify for sand_units
unique(data_std$sand_units) # "%"

#### Save the final corrected file ####

# Save the final corrected file with textural classes in an rds object
saveRDS(data_std, file = 'Inventaires/data_cleaning_final/data_std_inv_cleaned.rds')

# save the final corrected file as csv file and txt file
write.table(data_std,
            "./Inventaires/data_cleaning_final/data_std_inv_cleaned.csv", 
            sep="\t", row.names = F, quote = F)
write.table(data_std,
            "./Inventaires/data_cleaning_final/data_std_inv_cleaned.txt", 
            sep="\t", row.names = F, quote = F)
