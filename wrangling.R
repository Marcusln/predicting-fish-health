library(dplyr)
library(magrittr)

#
## Data wrangling
###

# import dataset (from BarentsWatch)
lakselus_per_fisk <- read.csv2("lakselus_per_fisk2.csv", encoding="ISO-8859-1")

# create a copy
lice = lakselus_per_fisk

# rename columns to avoid problems with special characters
colnames(lice)[1] = "week"
colnames(lice)[2] = "year"
colnames(lice)[3] = "location.id"
colnames(lice)[4] = "location.name"
colnames(lice)[5] = "adult.female.lice"
colnames(lice)[6] = "moving.lice"
colnames(lice)[7] = "stuck.lice"
colnames(lice)[8] = "brakklagt"
colnames(lice)[9] = "municipality.id"
colnames(lice)[10] = "municipality.name"
colnames(lice)[11] = "county.id"
colnames(lice)[12] = "county.name"
colnames(lice)[13] = "latitude"
colnames(lice)[14] = "longitude"
colnames(lice)[15] = "lice.limit"
colnames(lice)[16] = "lice.above.limit"
colnames(lice)[17] = "sea.temp"
colnames(lice)[18] = "production.id"
colnames(lice)[19] = "production.name"

# create new variable to be used in a later join
lice$year.week = paste0(lice$year, lice$week)

# removing rows without location name, these are inactive locations ref documentation
lice = lice[!lice$production.name == '',]

# import dataset (from BarentsWatch)
ila_pd <- read_csv("ila_pd.csv")

# create a copy
disease = ila_pd

# rename columns to avoid problems with special characters
colnames(disease)[1] = "week"
colnames(disease)[2] = "year"
colnames(disease)[3] = "location.id"
colnames(disease)[4] = "location.name"
colnames(disease)[5] = "disease"
colnames(disease)[6] = "status"
colnames(disease)[7] = "from.date"
colnames(disease)[8] = "to.date"
colnames(disease)[9] = "municipality.id"
colnames(disease)[10] = "municipality.name"
colnames(disease)[11] = "county.id"
colnames(disease)[12] = "county.name"
colnames(disease)[13] = "latitude"
colnames(disease)[14] = "longitude"
colnames(disease)[15] = "production.id"
colnames(disease)[16] = "production.name"

# create new variable to be used in a later join
disease$year.week = paste0(disease$year,disease$week)

# remove unnecessary variables and those already in the lice dataframe
disease = disease[,-c(1,2,4,7,8,9,10,11,12,13,14,15,16)]

# add disease info to lice
salmon = left_join(lice, disease, by = c("year.week","location.id"))

# import dataset (from BarentsWatch)
tiltak_mot_lakselus <- read_csv("tiltak_mot_lakselus.csv")

# create a copy
treatment = tiltak_mot_lakselus

# rename columns to avoid problems with special characters
colnames(treatment)[1] = "week"
colnames(treatment)[2] = "year"
colnames(treatment)[3] = "location.id"
colnames(treatment)[4] = "location.name"
colnames(treatment)[5] = "treatment"
colnames(treatment)[6] = "treatment.type"
colnames(treatment)[7] = "chemical"
colnames(treatment)[8] = "cleaner.fish.id"
colnames(treatment)[9] = "cleaner.fish"
colnames(treatment)[10] = "no.of.cleaner.fish"
colnames(treatment)[11] = "extent"
colnames(treatment)[12] = "municipality.id"
colnames(treatment)[13] = "municipality.name"
colnames(treatment)[14] = "county.id"
colnames(treatment)[15] = "county.name"
colnames(treatment)[16] = "longitude"
colnames(treatment)[17] = "latitude"
colnames(treatment)[18] = "production.id"
colnames(treatment)[19] = "production.name"

# create new variable to be used in a later join
treatment$year.week = paste0(treatment$year, treatment$week)

treatment = treatment[,-c(1,2,4,12,13,14,15,16,17,18,19)]

# add treatment info to salmon
salmon = left_join(salmon, treatment, by = c("year.week", "location.id"))

# remove variables unnecessary variables
salmon$week = NULL
salmon$week.y = NULL
salmon$year = NULL
salmon$year.y = NULL
salmon$loca

# replace NAs with 0
salmon$adult.female.lice[is.na(salmon$adult.female.lice)] = 0

salmon %<>% 
  mutate(lice.above.limit = ifelse(salmon$adult.female.lice > 0.5, 1, 0))

