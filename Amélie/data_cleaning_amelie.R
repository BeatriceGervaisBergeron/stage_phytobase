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
    expe_t = as.numeric(expe_t)
    , country = as.factor(country)
    , ph = as.numeric(ph)
    , oc = as.numeric(oc)
    , ec = as.numeric(ec)
    , sand = as.numeric(sand)
    , clay = as.numeric(clay)
    , as_s = as.numeric(as_s)
    , co_s = as.numeric(co_s)
    , mn_s = as.numeric(mn_s)
    , hg_s = as.numeric(hg_s)
    , ba_total = as.numeric(ba_total)
    , ba_stem = as.numeric(ba_stem)
    , ba_leaf = as.numeric(ba_leaf)
    , br = as.numeric(br)
    , as_ba = as.numeric(as_ba)
    , co_ba = as.numeric(co_ba)
    , hg_ba = as.numeric(hg_ba)
    , co_br = as.numeric(co_br)
    , mn_br = as.numeric(mn_br)
    , hg_br = as.numeric(hg_br)
    , as_ba_1 = as.numeric(as_ba_1)
    , zn_ba_1 = as.numeric(zn_ba_1)
    , se_ba_1 = as.numeric(se_ba_1)
    , co_ba_1 = as.numeric(co_ba_1)
    , mn_ba_1 = as.numeric(mn_ba_1)
    , hg_ba_1 = as.numeric(hg_ba_1))
# all value for expe_t, ph, clay, co_s, mn_s, hg_s, co_ba, hg_ba, mn_br are not only numerical, so Na were introduced.
# Verify the mutation
str(data)#good
#### all white space to NA ####
data[data == ''] <- NA
#### species names cleaning  ####
# check unique sp list in your database
uni_sp<-as.data.frame(unique(data$name)) # 395 unique species
colnames(uni_sp) <- c('sp') 
# Correct the name according to the species name corrected from TRY
# list_sp_cor already corrected
list_sp_cor <-readRDS('list_sp_cor.rds')
# sp not present in the list
to.be.cor <- anti_join(uni_sp, list_sp_cor, by=c('sp'='user_supplied_name')) # 125 species not on the corrected list, so need to be corrected
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
uni_sp_cor <-uni_sp_cor %>%  filter(!dupl2 ==T)
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
# make a new column of the correct names
data <- data %>% 
  mutate(AccSpeciesName_cor = ifelse(implement == T, alternative, submitted_name)) 
# Keep the corrected column
data <- data[,-c(100:106)]
# Check number of sp now
uni_cu_cor <-unique(data$AccSpeciesName_cor)
#### standardized units ####
# Select all the units column
units <- data %>%
  select(contains('unit'))


# check unit conversion for every unit column
colnames(units)
#"om_units"
unique(units$om_units) # "%" "g kg-1" "g.O2.kg-1" "g.kg-1" "mg kg-1" "g dm-3" "dag kg-1" "g kg" "g kg-3"
# need to convert to % (/10)
data_std <- data %>%
  mutate(om = ifelse(om_units == 'g kg', om/10, om)
         ,om_units = ifelse(om_units == 'g kg', '%', om_units)
         ,om = ifelse(om_units == 'g.kg-1', om/10, om)
         ,om_units = ifelse(om_units == 'g.kg-1', '%', om_units)
         ,om = ifelse(om_units == 'g kg-1', om/10, om)
         ,om_units = ifelse(om_units == 'g kg-1', '%', om_units)
         ,om = ifelse(om_units == 'g.O2.kg-1', om/10, om)
         ,om_units = ifelse(om_units == 'g.O2.kg-1', '%', om_units)
)
# verify
unique(data_std$om_units) # need to convert "mg kg-1""g dm-3""dag kg-1""g kg-3" in "%" 


#"oc_units" 
unique(units$oc_units) # "%" "g kg-1" "mgL-1" "mg kg-1" "g kg"
# need to convert  to % (/10)
data_std <- data %>%
  mutate(
    oc = ifelse(oc_units == 'g kg', oc/10, oc)
         ,oc_units = ifelse(oc_units == 'g kg', '%', oc_units)
         ,oc = ifelse(oc_units == 'g kg-1', oc/10, oc)
         ,oc_units = ifelse(oc_units == 'g kg-1', '%', oc_units)
)
# verify
unique(data_std$oc_units) # need to convert "mgL-1""mg kg-1" in "%"


#"clay_units" 
unique(units$clay_units) #"%" "g kg-1" "mm" "mg kg-1" "g kg"
# need to convert  to % (/10)
data_std <- data %>%
  mutate(
    clay = ifelse(clay_units == 'g kg', clay/10, clay)
    ,clay_units = ifelse(clay_units == 'g kg', '%', clay_units)
    ,clay = ifelse(clay_units == 'g kg-1', clay/10, clay)
    ,clay_units = ifelse(clay_units == 'g kg-1', '%', clay_units)
)
# verify
unique(data_std$clay_units) # need to convert "mm""mg kg-1" in "%"


#"sand_units" 
unique(units$sand_units) #  "%" "g kg-1" "mm" "mg kg-1" "g kg"
# need to convert  to % (/10)
data_std <- data %>%
  mutate(
    sand = ifelse(sand_units == 'g kg', sand/10, sand)
    ,sand_units = ifelse(sand_units == 'g kg', '%', sand_units)
    ,sand = ifelse(sand_units == 'g kg-1', sand/10, sand)
    ,sand_units = ifelse(sand_units == 'g kg-1', '%', sand_units)
  )
# verify
unique(data_std$sand_units) # need to convert "mm""mg kg-1" in "%"


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
convert <- c("meq 100-1","meq 100g-1","meq/100g","meq 100 g−3","cmolc/kg")
data_std <- data_std %>%
  mutate(
    cec_units = ifelse(cec_units %in% convert , 'cmolc kg-1', cec_units)
    ,cec = ifelse(cec_units == 'mmol kg-1', cec/10, cec)
    ,cec_units = ifelse(cec_units == 'mmol kg-1', 'cmolc kg-1', cec_units)
    ,cec = ifelse(cec_units == 'cmolc dm-3', cec*10, cec)
    ,cec_units = ifelse(cec_units == 'cmolc dm-3', 'cmolc kg-1', cec_units)
    ,cec = ifelse(cec_units == 'mmolc dm-3', cec/10, cec)
    ,cec_units = ifelse(cec_units == 'mmolc dm-3', 'cmolc kg-1', cec_units)
)
#verify
unique(data_std$cec_units) # need to convert "cmol kg-1""meq 100 g-1""cmol/kg""cmol+ kg-1""cmol(+)kg-1""molc kg-1""%""cmol kg""cmol+ kg""cmol/100g soil" "mM(+)/kg DM" en cmolc kg-1


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
    ,N = ifelse(N_units == 'kg ha-1', N*10, N)
    ,N_units = ifelse(N_units == 'kg ha-1', 'mg kg−1', N_units)
    ,N = ifelse(N_units == 'g kg dw-1', N*1000, N)
    ,N_units = ifelse(N_units == 'g kg dw-1', 'mg kg−1', N_units)
)
#verify
unique(data_std$N_units) # only "mg kg−1"

#"P_units"   
unique(units$P_units) # "mg kg-1" "kg ha-1" "mg kg" "P2O5" "g.kg-1" "g kg-1" "mg/kg" "ug g-1" "g kg dw-1" "mg kg-1 (P2O5)" "mg 100 g-1" "%" "mg 100g-1" "mg dm-3" "mg g-1" "ppm" "meq/100g" "mg L-1" "g kg" "g P2O5 kg-1""mg P/kg
# need to convert to mg kg-1
convert <- c("mg/kg","mg kg","mg g-1","ppm","mg kg-1 (P2O5)","mg P/kg")
data_std <- data %>%
  mutate(
    P_units = ifelse(P_units %in% convert , 'mg kg-1', P_units)
    ,P = ifelse(P_units == '%', P*10000, P)
    ,P_units = ifelse(P_units == '%', 'mg kg-1', P_units)
    ,P = ifelse(P_units == 'kg ha-1', P*10, P)
    ,P_units = ifelse(P_units == 'kg ha-1', 'mg kg-1', P_units)
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
    
)
#verify
unique(data_std$P_units)# need to convert meq/100g in mg kg-1


#"units_s"   
unique(units$units_s) # "mg kg-1" "g kg-1" "mg kg" "ppm" "ug.L-1" "ug g-1" "mg/kg" "ug g" "mg dm-3" "ug/g" "mg L-1" "uM/g"
# need to convert to mg kg-1
convert <- c("mg/kg","mg kg","ppm","ug.L-1", "ug g-1","ug g","ug/g")
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
unique(data_std$units_s) # need to convert "mg dm-3""mg L-1""uM/g" in "mg kg-1"

    
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
    ,b = ifelse(units_b == 'kg acre-1', ba_total*1000/4046.86, ba_total)
    ,units_b = ifelse(units_b == 'kg acre-1', 'g', units_b)
    ,b = ifelse(units_b == 'kg acre-1', ba_stem*1000/4046.86, ba_stem)
    ,units_b = ifelse(units_b == 'kg acre-1', 'g', units_b)
    ,b = ifelse(units_b == 'kg acre-1', ba_leaf*1000/4046.86, ba_leaf)
    ,units_b = ifelse(units_b == 'kg acre-1', 'g', units_b)
    ,b = ifelse(units_b == 'kg acre-1', br*1000/4046.86, br)
    ,units_b = ifelse(units_b == 'kg acre-1', 'g', units_b)
    ,b = ifelse(units_b == 'mg ha-1', ba_total/10000, ba_total)
    ,units_b = ifelse(units_b == 'mg ha-1', 'g', units_b)
    ,b = ifelse(units_b == 'mg ha-1', ba_stem/10000, ba_stem)
    ,units_b = ifelse(units_b == 'mg ha-1', 'g', units_b)
    ,b = ifelse(units_b == 'mg ha-1', ba_leaf/10000, ba_leaf)
    ,units_b = ifelse(units_b == 'mg ha-1', 'g', units_b)
    ,b = ifelse(units_b == 'mg ha-1', br/10000, br)
    ,units_b = ifelse(units_b == 'mg ha-1', 'g', units_b)
    ,b = ifelse(units_b == 't ha-1', ba_total*1000000, ba_total)
    ,units_b = ifelse(units_b == 't ha-1', 'g', units_b)
    ,b = ifelse(units_b == 't ha-1', ba_stem*1000000, ba_stem)
    ,units_b = ifelse(units_b == 't ha-1', 'g', units_b)
    ,b = ifelse(units_b == 't ha-1', ba_leaf*1000000, ba_leaf)
    ,units_b = ifelse(units_b == 't ha-1', 'g', units_b)
    ,b = ifelse(units_b == 't ha-1', br*1000000, br)
    ,units_b = ifelse(units_b == 't ha-1', 'g', units_b)
    ,b = ifelse(units_b == 'kg ha-1', ba_total*10000, ba_total)
    ,units_b = ifelse(units_b == 'kg ha-1', 'g', units_b)
    ,b = ifelse(units_b == 'kg ha-1', ba_stem*10000, ba_stem)
    ,units_b = ifelse(units_b == 'kg ha-1', 'g', units_b)
    ,b = ifelse(units_b == 'kg ha-1', ba_leaf*10000, ba_leaf)
    ,units_b = ifelse(units_b == 'kg ha-1', 'g', units_b)
    ,b = ifelse(units_b == 'kg ha-1', br*10000, br)
    ,units_b = ifelse(units_b == 'kg ha-1', 'g', units_b)
)

#verify
unique(data_std$units_b) # need to convert "%" in "g"


#"units_te_ba" 
unique(units$units_te_ba) # "mg kg-1" "mg m2 year-1" "mg kg" "ug.g-1" "ppm" "ug g" "ug/g" "mg kg " "mg/kg" "mg plant-1" "mg kg-1 DW" "ug g-1" "mg m-2" "uM/g DW" "mg pot-1" "ug kg-1" "ppm/ppb" "kg ha-1"
# need to convert to mg kg-1 
convert <- c("mg kg","ppm","mg kg ","mg/kg", "mg kg-1 DW")
data_std <- data %>%
  mutate(
    units_te_ba = ifelse(units_te_ba %in% convert , 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug.g-1', as_ba*1000, as_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug.g-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug.g-1', cd_ba*1000, cd_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug.g-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug.g-1', cu_ba*1000, cu_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug.g-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug.g-1', pb_ba*1000, pb_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug.g-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug.g-1', zn_ba*1000, zn_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug.g-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug.g-1', se_ba*1000, se_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug.g-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug.g-1', ni_ba*1000, ni_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug.g-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug.g-1', co_ba*1000, co_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug.g-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug.g-1', mn_ba*1000, mn_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug.g-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug.g-1', cr_ba*1000, cr_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug.g-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug.g-1', hg_ba*1000, hg_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug.g-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug g', as_ba*1000, as_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug g', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug g', cd_ba*1000, cd_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug g', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug g', cu_ba*1000, cu_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug g', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug g', pb_ba*1000, pb_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug g', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug g', zn_ba*1000, zn_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug g', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug g', se_ba*1000, se_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug g', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug g', ni_ba*1000, ni_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug g', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug g', co_ba*1000, co_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug g', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug g', mn_ba*1000, mn_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug g', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug g', cr_ba*1000, cr_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug g', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug g', hg_ba*1000, hg_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug g', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug/g', as_ba*1000, as_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug/g', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug/g', cd_ba*1000, cd_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug/g', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug/g', cu_ba*1000, cu_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug/g', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug/g', pb_ba*1000, pb_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug/g', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug/g', zn_ba*1000, zn_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug/g', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug/g', se_ba*1000, se_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug/g', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug/g', ni_ba*1000, ni_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug/g', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug/g', co_ba*1000, co_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug/g', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug/g', mn_ba*1000, mn_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug/g', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug/g', cr_ba*1000, cr_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug/g', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug/g', hg_ba*1000, hg_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug/g', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug g-1', as_ba*1000, as_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug g-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug g-1', cd_ba*1000, cd_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug g-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug g-1', cu_ba*1000, cu_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug g-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug g-1', pb_ba*1000, pb_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug g-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug g-1', zn_ba*1000, zn_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug g-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug g-1', se_ba*1000, se_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug g-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug g-1', ni_ba*1000, ni_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug g-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug g-1', co_ba*1000, co_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug g-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug g-1', mn_ba*1000, mn_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug g-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug g-1', cr_ba*1000, cr_ba)
    ,units_te_ba = ifelse(units_te_ba == 'ug g-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'ug g-1', hg_ba*1000, hg_ba)
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
    ,ba = ifelse(units_te_ba == 'kg ha-1', as_ba/10, as_ba)
    ,units_te_ba = ifelse(units_te_ba == 'kg ha-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'kg ha-1', cd_ba/10, cd_ba)
    ,units_te_ba = ifelse(units_te_ba == 'kg ha-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'kg ha-1', cu_ba/10, cu_ba)
    ,units_te_ba = ifelse(units_te_ba == 'kg ha-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'kg ha-1', pb_ba/10, pb_ba)
    ,units_te_ba = ifelse(units_te_ba == 'kg ha-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'kg ha-1', zn_ba/10, zn_ba)
    ,units_te_ba = ifelse(units_te_ba == 'kg ha-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'kg ha-1', se_ba/10, se_ba)
    ,units_te_ba = ifelse(units_te_ba == 'kg ha-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'kg ha-1', ni_ba/10, ni_ba)
    ,units_te_ba = ifelse(units_te_ba == 'kg ha-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'kg ha-1', co_ba/10, co_ba)
    ,units_te_ba = ifelse(units_te_ba == 'kg ha-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'kg ha-1', mn_ba/10, mn_ba)
    ,units_te_ba = ifelse(units_te_ba == 'kg ha-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'kg ha-1', cr_ba/10, cr_ba)
    ,units_te_ba = ifelse(units_te_ba == 'kg ha-1', 'mg kg-1', units_te_ba)
    ,ba = ifelse(units_te_ba == 'kg ha-1', hg_ba/10, hg_ba)
    ,units_te_ba = ifelse(units_te_ba == 'kg ha-1', 'mg kg-1', units_te_ba)
)
# verify
unique(data_std$units_te_ba) # Need to convert "mg m2 year-1""mg plant-1""mg m-2""uM/g DW""mg pot-1""ppm/ppb" in "mg kg-1"


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
    ,br = ifelse(units_te_br == 'ug g1', co_br*1000, co_br)
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
unique(data_std$units_te_br) # need to convert "mg plant-1""mg m-2""uM/g DW""mg pot-1" in mg kg-1


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
unique(data_std$units_te_ba_1) # need to convert "mg plant-1" "uM/g DW" in mg kg-1




#### standardize categories terms ####

# Climate
unique(data$climate) 
# 'on hold', to be unify if needed further on

# Texture
unique(data$texture) 

# p_density
unique(data$p_density)
# 'on hold', to be unify if needed further on

# organs_ba, organs_ba_1
unique(data$organs_ba)
unique(data$organs_ba_1)
unique(data$organs_ba_2)
unique(data$organs_ba_3)
# conversion
syn_shoots <- c("Shoots","shoots","Shoot","shoot")
syn_stems <- c("Stalks","Twigs","Stems","stems","stem","Stem","stalk","Twig","Branch","branch","branches","Branches","Lower stems","Culms","Bark","Stubble")
syn_leaves <- c("leaf","Leaf","Leaves","leaves","Leafs","Foliage","Aciculum")
syn_flowers <- c("flowers","Heads","Head","Spikelets")
syn_fruits <- c("Berry","Edible parts")                
                 

data_std <- data_std %>%
  mutate(organs_ba = ifelse(organs_ba %in% syn_shoots , 'shoots', organs_ba)) %>% 
  mutate(organs_ba = ifelse(organs_ba %in% syn_stems , 'stems', organs_ba)) %>%
  mutate(organs_ba = ifelse(organs_ba %in% syn_leaves , 'leaves', organs_ba)) %>%
  mutate(organs_ba = ifelse(organs_ba %in% syn_flowers , 'flowers', organs_ba)) %>%
  mutate(organs_ba = ifelse(organs_ba %in% syn_fruits , 'fruits', organs_ba)) %>%
  mutate(organs_ba_1 = ifelse(organs_ba_1 %in% syn_shoots , 'shoots', organs_ba_1)) %>%
  mutate(organs_ba_1 = ifelse(organs_ba_1 %in% syn_stems , 'stems', organs_ba_1)) %>%
  mutate(organs_ba_1 = ifelse(organs_ba_1 %in% syn_leaves , 'leaves', organs_ba_1)) %>%
  mutate(organs_ba_1 = ifelse(organs_ba_1 %in% syn_flowers , 'flowers', organs_ba_1)) %>%
  mutate(organs_ba_1 = ifelse(organs_ba_1 %in% syn_fruits , 'fruits', organs_ba_1)) %>%
  mutate(organs_ba_2 = ifelse(organs_ba_2 %in% syn_shoots , 'shoots', organs_ba_2)) %>%
  mutate(organs_ba_2 = ifelse(organs_ba_2 %in% syn_stems , 'stems', organs_ba_2)) %>%
  mutate(organs_ba_2 = ifelse(organs_ba_2 %in% syn_leaves , 'leaves', organs_ba_2)) %>%
  mutate(organs_ba_2 = ifelse(organs_ba_2 %in% syn_flowers , 'flowers', organs_ba_2)) %>%
  mutate(organs_ba_2 = ifelse(organs_ba_2 %in% syn_fruits , 'fruits', organs_ba_2)) %>%
  mutate(organs_ba_3 = ifelse(organs_ba_3 %in% syn_shoots , 'shoots', organs_ba_3)) %>%
  mutate(organs_ba_3 = ifelse(organs_ba_3 %in% syn_stems , 'stems', organs_ba_3)) %>%
  mutate(organs_ba_3 = ifelse(organs_ba_3 %in% syn_leaves , 'leaves', organs_ba_3)) %>%
  mutate(organs_ba_3 = ifelse(organs_ba_3 %in% syn_flowers , 'flowers', organs_ba_3)) %>%
  mutate(organs_ba_3 = ifelse(organs_ba_3 %in% syn_fruits , 'fruits', organs_ba_3))

# verify
unique(data_std$organs_ba) 
unique(data_std$organs_ba_1)
unique(data_std$organs_ba_2)
unique(data_std$organs_ba_3)

# organs_br
# make same thing as for organ_ba



#### outliers and errors in numerical data #### 

# see if duplicates data entries
unique_obs<- data_std[duplicated(data_std)] # 0 variables means 0 duplicates to eliminate

# list of variables that need to be verify
num_cols <- unlist(lapply(data_std, is.numeric)) #identify numerical data 
data_num <- data_std[ , num_cols]  # 55 variables

# import the data normal range
num_range <- read.table("./numerical_range_variables.txt", 
                        sep="\t", header=T, stringsAsFactors = F)

# for each of the 55 variables, isolate data that are outside the range
# here are the 55 variables
list <-colnames(data_num)

# isolate the outliers lines for the variable 'covidence'
outliers <- data_std %>% 
  filter(covidence < num_range$min_value[num_range$variables == 'covidence'] | covidence > num_range$max_value[num_range$variables == 'covidence'] )
# if the outliers has 0 lines, it indicate not apparent outliers

# you can also write it with number from the list to save time, as follow with Covidence as number 1 in the list
outliers <- data_std %>% 
  filter(data_num[,1] < num_range$min_value[1] | data_num[,1] > num_range$max_value[1] )


# outliers for list[2] = year 
outliers <- data_std %>% 
  filter(data_num[,2] < num_range$min_value[2] | data_num[,2] > num_range$max_value[2] )
# 0 line, so no outliers

# if some lines appear, go see the data and verify in the literature if it is a typo, or if it is the exact number from the literature
# if the data still appear high, Write a note in the 'journal de bord'

## CONTINUE with all the 66 variables




#### Add clay and sand % according to textural class of soils###

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
# now all the textural class should be add in % in the clay and sand column


#### join the traits to your data ####

traits <- readRDS('./complete_data.rds')
data_std <- left_join(data_std , traits, by=c('AccSpeciesName_cor'='sp'))



##### visualization of the data ##### EN construction

#### normality ######

# histograms of database

par(mfrow=c(4,2))
par(mar=c(2,2,1,1)) # Ajuster les marges
hist(data_std$ph)
hist(data_std$om)
hist(data_std$oc)
hist(data_std$SLA)
hist(data_std$LDMC)
hist(data_std$LA)
hist(data_std$cd_ba)
hist(data_std$zn_ba)

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
hist(traits$SLA)
hist(log(traits$SLA)) #best
hist(log10(traits$SLA))
hist(sqrt(traits$SLA))
hist(decostand(traits$SLA, method='log', MARGIN=2))
hist(traits$SLA^(1/3))
hist(decostand(traits$SLA, method='standardize', MARGIN=2))
hist(log2(traits$SLA))

# is LDMC normally distributed ad is there a transformation that make it more normal?
par(mfrow=c(4,2))
hist(traits$LDMC)
hist(log(traits$LDMC)) 
hist(log10(traits$LDMC))
hist(sqrt(traits$LDMC))
hist(decostand(traits$LDMC, method='log', MARGIN=2))
hist(traits$LDMC^(1/3))
hist(decostand(traits$LDMC, method='standardize', MARGIN=2))
hist(logit(traits$LDMC)) #best

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