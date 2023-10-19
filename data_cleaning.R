# DATA CLEANING

## LIBRARY

library(dplyr)
library(stringr)
library(taxize)




#### call your data  ####
data <- read.csv('./Pei yin/data_cleaning_test/soil_sp_database_Pei_Yin.csv', sep=',',header = T, dec = '.')



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
    , clay = as.numeric(clay)
    , co_s = as.numeric(co_s)
    , mn_s = as.numeric(mn_s)
    , hg_s = as.numeric(hg_s)
    , co_ba = as.numeric(co_ba)
    , hg_ba = as.numeric(hg_ba)
    , co_br = as.numeric(co_br)
    , mn_br = as.numeric(mn_br)
    , hg_br = as.numeric(hg_br)
    , as_ba.1 = as.numeric(as_ba.1)
    , zn_ba.1 = as.numeric(zn_ba.1)
    , se_ba.1 = as.numeric(se_ba.1)
    , co_ba.1 = as.numeric(co_ba.1)
    , mn_ba.1 = as.numeric(mn_ba.1)
    , hg_ba.1 = as.numeric(hg_ba.1))
# here all value for ph and clay are not only numerical, so Na were introduced. Those column need to be adjusted

# Verify the mutation
str(data) # good


#### all white space to NA ####
data[data == ''] <- NA



#### species names cleaning  ####

# check unique sp list in your database
uni_sp<-as.data.frame(unique(data$name)) # 36 unique species
colnames(uni_sp) <- c('sp')

# Correct the name according to the species name corrected from TRY

# list_sp_cor already corrected
list_sp_cor <-readRDS('list_sp_cor.rds')

# sp not present in the list
to.be.cor <- anti_join(uni_sp, list_sp_cor, by=c('sp'='user_supplied_name')) # 22 species not on the corrected list, so need to be corrected

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
uni_sp_2<- as.data.frame(unique(match.name$user_supplied_name)) # 20 sp, so 2 sp were not taken into acount
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
            "./Pei yin/uni_sp_match_names.txt", 
            sep="\t", row.names = F, quote = F)

# open the txt file in excel to make manual corrections
  # All in () content should be remove
  # For all hybrids (with x) both name should be keep

# Save the corrected names in a txt file name, adding _cor to the name of the document

# import back the data 
uni_sp_cor <- read.table("./Pei yin/uni_sp_match_names_cor.txt", 
                         sep="\t", header=T, stringsAsFactors = F)

# eliminate duplicates
uni_sp_cor$dupl2 <- duplicated(uni_sp_cor$user_supplied_name)
uni_sp_cor <-uni_sp_cor %>%  filter(!dupl2 ==T) # no duplicates in the first column

# Save the final corrected list in an rds object
saveRDS(uni_sp_cor, file='Pei yin/salix_sp_cor.rds')

#### Join list of corrected names ####

# call the newly corrected list
salix_sp_cor <- readRDS('Pei yin/salix_sp_cor.rds')

# Add the new corrected list to the complete on
list_sp_cor_salix<- bind_rows( salix_sp_cor, list_sp_cor)

# add the corrections to the data
data <- data %>% 
  left_join(list_sp_cor_salix, by=c('name'= 'user_supplied_name'))
# make a new column of the correct names
data <- data %>% 
  mutate(AccSpeciesName_cor = ifelse(implement == T, alternative, submitted_name)) 
# Keep the corrected column
data <- data[,-c(100:106)] # shoudl have 100 columns

# Check number of sp now
uni_salix_cor <-unique(data$AccSpeciesName_cor)# 33 sp
# 3 species less, if you return to the uni_sp with 36 sp, you see indeed 3 lines with redundant names



#### standardized units ####

# Select all the units column
units <- data %>%
  select(contains('unit'))
# check unit conversion for every unit column
colnames(units)

#"om_units"
unique(units$om_units) # "%" "" 
# no need for conversion if only %

#"oc_units" 
unique(units$oc_units) # ""     "g/kg" "%"  
# need to convert g/Kg to % (/10)
data_std <- data %>%
  mutate(oc = ifelse(oc_units == 'g/kg', oc/10, oc)) %>% # divide per 10 all the oc data that have g/kg units
  mutate(oc_units = ifelse(oc_units == 'g/kg', '%', oc_units)) # replace all g/kg units per %
# verify
unique(data_std$oc_units) # only "%" 

#"clay_units" 
unique(units$clay_units) # "%" "" 
# no need for conversion if only % 

#"sand_units" 
unique(units$sand_units) #  "%" "" 
# no need for conversion if only % 

#"ec_units"
unique(units$ec_units) # ""  "mS cm−1"
# no need for conversion if only mS cm−1 

#"cec_units"
unique(units$cec_units) # "meq 100 g−1" "meq 100 g−2" "meq 100 g−3" "cmolc/kg"    "cmolc kg-1"  ""  "mmol kg−1"  
# no need for conversion for all of these:"meq 100 g−1" "meq 100 g−2" "meq 100 g−3" "cmolc/kg", they all mean "cmolc kg-1"
convert <- c("meq 100 g−1","meq 100 g−2","meq 100 g−3","cmolc/kg")
data_std <- data_std %>%
  mutate(cec_units = ifelse(cec_units %in% convert , 'cmolc kg-1', cec_units)) # replace all g/kg units per %
# need to convert mmol kg−1 to cmolc kg-1 (/10)
data_std <- data_std %>%
  mutate(cec = ifelse(cec_units == 'mmol kg−1', cec/10, cec)) %>% # divide per 10 all the cec data that have g/kg units
  mutate(cec_units = ifelse(cec_units == 'mmol kg−1', 'cmolc kg-1', cec_units)) # replace all g/kg units per %
#verify
unique(data_std$cec_units)# only cmolc kg-1

#"n_units" 
unique(units$n_units) # "%" "" 
# need to convert % to mg kg-1 (*10000)
data_std <- data_std %>%
  mutate(n = ifelse(n_units == '%', n*10000, n)) %>% # multiply per 10000 all the n data that have % units
  mutate(n_units = ifelse(n_units == '%', 'mg kg−1', n_units)) # replace all % per g/kg units 
#verify
unique(data_std$n_units)# only mg kg-1

#"p_units"   
unique(units$p_units) # "%"  ""  "mg kg−1"
# need to convert % to mg kg-1 (*10000)
data_std <- data_std %>%
  mutate(p = ifelse(p_units == '%', p*10000, p)) %>% # multiply per 10000 all the p data that have % units
  mutate(p_units = ifelse(p_units == '%', 'mg kg−1', p_units)) # replace all % per g/kg units 
#verify
unique(data_std$p_units)# only mg kg-1

#"units_s"   
unique(units$units_s) # "mg kg−1"
# no need for conversion if only mg kg−1 

#"units_b" 
unique(units$units_b) # "g m−2"       "g"           ""            "mg kg−1"     "g/plant"     "t ha−1 yr−1"
# the mg mg-1 unit do not make sense, it should be verified for potential mistake
# since biomass is measure really differently, for the moment we'll let it like that
# 'on hold', to be unify if needed further on

#"units_te_ba" 
unique(units$units_te_ba) # "mg kg−1" "mg Kg-1"
# conversion of only mg kg−1 
data_std <- data_std %>%
  mutate(units_te_ba = ifelse(units_te_ba == 'mg Kg-1' , 'mg kg−1', units_te_ba)) # replace all mg Kg-1 to mg kg−1
# verify
unique(data_std$units_te_ba) # only "mg kg−1" "" 

#"units_te_br"   
unique(units$units_te_br) # "mg kg−1" "mg Kg-1"
# conversion of only mg kg−1 
data_std <- data_std %>%
  mutate(units_te_br = ifelse(units_te_br == 'mg Kg-1' , 'mg kg−1', units_te_br)) # replace all mg Kg-1 to mg kg−1
# verify
unique(data_std$units_te_br) # only "mg kg−1" "" 

#"units_te_ba.1"
unique(units$units_te_ba.1) # "mg Kg-1"
# conversion of only mg kg−1 
data_std <- data_std %>%
  mutate(units_te_ba.1 = ifelse(units_te_ba.1 == 'mg Kg-1' , 'mg kg−1', units_te_ba.1)) # replace all mg Kg-1 to mg kg−1
# verify
unique(data_std$units_te_ba.1) # only "mg kg−1" "" 


#### standardize categories terms ####

# Climate
unique(data$climate) 
# 'on hold', to be unify if needed further on

# Texture
unique(data$texture) # NA                "fine sandy loam" "Clay sand silt"  "Loamy"           "Coarse-textured"
# some categories need to be check and adjust as they are not texture
# adjust the name according to the tables in the protocol

# p_density
unique(data$p_density)
# 'on hold', to be unify if needed further on

# organs_ba and organs_ba.1
unique(data_std$organs_ba) # "Shoots" "leaf"   "leaves" "Stems"  "Leaves" NA
unique(data_std$organs_ba.1)# NA       "Stalks" "Twigs"  "Stems" 
# conversion for only shoots or leaves or stems
syn_shoots <- c("Shoots")
syn_stems <- c("Stalks", "Twigs" , "Stems")
syn_leaves <- c("leaf", "Leaves")

data_std <- data_std %>%
  mutate(organs_ba = ifelse(organs_ba %in% syn_shoots , 'shoots', organs_ba)) %>% # replace all by shoots 
  mutate(organs_ba = ifelse(organs_ba %in% syn_stems , 'stems', organs_ba)) %>% # replace all by stems
  mutate(organs_ba = ifelse(organs_ba %in% syn_leaves , 'leaves', organs_ba)) %>% # replace all by leaves
  mutate(organs_ba.1 = ifelse(organs_ba.1 %in% syn_shoots , 'shoots', organs_ba.1)) %>% # replace all by shoots 
  mutate(organs_ba.1 = ifelse(organs_ba.1 %in% syn_stems , 'stems', organs_ba.1)) %>% # replace all by stems
  mutate(organs_ba.1 = ifelse(organs_ba.1 %in% syn_leaves , 'leaves', organs_ba.1)) # replace all by leaves

# verify
unique(data_std$organs_ba) # only shoots or leaves or stems
unique(data_std$organs_ba.1) # only stems

# organs_br
# make same thing as for organ_ba



#### outliers and errors in numerical data #### 

# see if duplicates data entries
unique_obs<- data_std[duplicated(data_std)] # 0 variables means 0 duplicates to eliminate

# list of variables that need to be verify
num_cols <- unlist(lapply(data_std, is.numeric)) #identify numerical data 
data_num <- data_std[ , num_cols]  # keep only numerical data, so 66 variables

# import the data normal range
num_range <- read.table("./numerical_range_variables.txt", 
                         sep="\t", header=T, stringsAsFactors = F)

# for each of the 66 variables, isolate data that are outside the range
# here are the 66 variables
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

## CONTINU with all the 66 variables


#### geolocalization ####


# For unknown coordinate, extract it form location or country
# access google map via API
#install.packages('ggmap')
library('ggmap')
register_google('AIzaSyCHpQ0BAGzCaqEpu2aGgN5wVK8nPXu6PVw') # Beatrice key, tell me if it does not work for you

# get the country list with no locations
no_loc <- data_std_cleaned %>% 
  filter(is.na(location))
country <- unique(no_loc$country)
# replace the location by country names
data_std_cleaned$location[data_std_cleaned$country %in% country & is.na(data_std_cleaned$location)] <- country[country %in% data_std_cleaned$country]

# get the location list with no coordinates
no_coord <- data_std_cleaned %>% 
  filter(is.na(latitude))
location <- unique(no_coord$location)

# get table of coordinates and address 
coord <-geocode(location, output = 'latlona')
# remove typos and uniform the name of the locations that does not work
#replace the old name for the corrected name
data_std_cleaned$location[data_std_cleaned$location == 'Tavsanli District'] <- c('Tavsanli')
# more changes if necessary

# Once all corrected, get the location coordinated again
no_coord <- data_std_cleaned %>% 
  filter(is.na(latitude))
location <- unique(no_coord$location)
# get table of coordinates and address to verify later the data
coord <-geocode(location, output = 'latlona')
coord$location <- location

# merge the coordinates to the dataframe
data_std_cleaned <- merge(data_std_cleaned, coord, by='location', all.x = TRUE)
# relocate the column to see if the replacement is good
data_std_cleaned <-data_std_cleaned %>% relocate(location, .after=country)
data_std_cleaned <-data_std_cleaned %>% relocate(c('lon','lat','address'), .after=longitude)


### Longitude and latitude ###
# Uniform the longitude and latitude format to be '## ## ###.##'
# replace ' and " by empty space
data_std_cleaned$latitude <- str_replace_all(data_std_cleaned$latitude, "'", " ")
data_std_cleaned$latitude <- str_replace_all(data_std_cleaned$latitude, '"', " ")
data_std_cleaned$longitude<- str_replace_all(data_std_cleaned$longitude, "'", " ")
data_std_cleaned$longitude <- str_replace_all(data_std_cleaned$longitude, '"', " ")
# Add - to S and W 
data_std_cleaned[grep('S',data_std_cleaned$latitude),c('latitude')] <-paste0('-',data_std_cleaned[grep('S',data_std_cleaned$latitude),c('latitude')])
data_std_cleaned[grep('W',data_std_cleaned$longitude),c('longitude')] <-paste0('-',data_std_cleaned[grep('W',data_std_cleaned$longitude),c('longitude')])
# Delete N and E
data_std_cleaned$latitude <- str_replace_all(data_std_cleaned$latitude, 'N', "")
data_std_cleaned$latitude <- str_replace_all(data_std_cleaned$latitude, 'S', "")
data_std_cleaned$longitude <- str_replace_all(data_std_cleaned$longitude, 'E', "")
data_std_cleaned$longitude <- str_replace_all(data_std_cleaned$longitude, 'W', "")
# remove space at the end
data_std_cleaned$latitude <- str_replace_all(data_std_cleaned$latitude, ' $', "")
data_std_cleaned$latitude <- str_replace_all(data_std_cleaned$latitude, ' $', "")
data_std_cleaned$longitude <- str_replace_all(data_std_cleaned$longitude, ' $', "")
data_std_cleaned$longitude <- str_replace_all(data_std_cleaned$longitude, ' $', "")

# Convert units
#install.packages("measurements") 
library(measurements)
##LATITUDE
# isolate the DMS units
DMS <- unique(data_std_cleaned[grepl('\\d \\d',data_std_cleaned$latitude),c('latitude') ])
# convert to decimals
DEC <-lapply(DMS,function(x) conv_unit(x,from = "deg_min_sec", to = "dec_deg" ) )
# put them together in a dataframe
latitude <- as.data.frame(DMS)
latitude$lat_decimal <-unlist(DEC)
#join all the decimal together
data_std_cleaned$lat[data_std_cleaned$latitude %in% DMS] <- DEC[match(data_std_cleaned$latitude, DMS, nomatch = 0)]

##LONGITUDE
# isolate the DMS
DMSlong <- unique(data_std_cleaned[grepl('\\d \\d',data_std_cleaned$longitude),c('longitude') ])
# convert to decimals
DEClong <-lapply(DMSlong,function(x) conv_unit(x,from = "deg_min_sec", to = "dec_deg" ) )
# put them together in a dataframe
longitude <- as.data.frame(DMSlong)
longitude$lat_decimal <-unlist(DEClong)
#join all the decimal together
data_std_cleaned$lon[data_std_cleaned$longitude %in% DMSlong] <- DEClong[match(data_std_cleaned$longitude, DMSlong, nomatch = 0)]
# if want to put them in the original longitude column
#data_std_cleaned$longitude <- ifelse(is.na(data_std_cleaned$lon), data_std_cleaned$longitude, data_std_cleaned$lon)

# copy the original decimal coordinate in lat and lon column
data_std_cleaned$lon <- ifelse(is.na(data_std_cleaned$lon), data_std_cleaned$longitude, data_std_cleaned$lon)
data_std_cleaned$lat <- ifelse(is.na(data_std_cleaned$lat), data_std_cleaned$latitude, data_std_cleaned$lat)
# now the lon and lat should be all decimal and ready for uniform uses


#### MAP and MAT ####
# Extract the MAP and MAT from the lon and lat 

# install.packages('terra')
# install.packages('sp')
# install.packages('raster', dependencies=TRUE, repos='https://CRAN.R-project.org/')
library(raster)
library('sp')

# download the climate dataset
r <- getData("worldclim",var="bio",res=2.5)# can adjust the resolution between 10, 5, 2.5, 30 sec
r <- r[[c(1,12)]]
names(r) <- c("Temp","Prec")
## THIS DATABASE TAKE A LOT OF SPACE, SO DELETE IT ONCE FINISH TO AVOID PUSHING IT ON GITHUB

# extract the longitude and latitude of your data
coords <- data_std_cleaned[,c('lon','lat')]
coords <- coords %>%
  mutate(
    lon = as.numeric(lon),
    lat = as.numeric(lat))

points <- SpatialPoints(coords, proj4string = r@crs)
values <- extract(r,points)
climate <- cbind.data.frame(coordinates(points),values)
# carefull, the temperature are with a factor of 10
climate$MAT_cor <- climate$Temp/10

# add column for the worldclim MAP and MAT in the dataset
data_std_cleaned$MAP_wc <- climate$Prec
data_std_cleaned$MAT_wc <- climate$MAT_cor
data_std_cleaned <- data_std_cleaned %>% relocate(c('MAT_wc','MAP_wc'), .after = map..mm.)
# Correct with the location if needed, like here would be for Hokkaido, japan, as the coordinate seems no precise enought?



#### Add clay and sand % according to textural class of soils####

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
hist(traits$LA)
hist(log(traits$LA)) #best
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
            "./Pei yin/data_std.txt", 
            sep=",", row.names = F, quote = F)

