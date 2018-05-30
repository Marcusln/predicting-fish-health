library(dplyr)
library(magrittr)
library(readr)

#
## Data wrangling
###

# import dataset (from BarentsWatch)
lakselus_per_fisk <- read.csv2("lakselus_per_fisk2.csv", encoding="ISO-8859-1", dec = ",", sep = ";")

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

# replace NA with 0, assuming NA means no lice
salmon[is.na(salmon$adult.female.lice),'adult.female.lice'] = 0
salmon[is.na(salmon$moving.lice),'moving.lice'] = 0
salmon[is.na(salmon$stuck.lice),'stuck.lice'] = 0

# replace yes/no with 1/0
salmon$brakklagt = as.character(salmon$brakklagt)
salmon[salmon$brakklagt == 'Ja','brakklagt'] = 1
salmon[salmon$brakklagt == 'Nei','brakklagt'] = 0
salmon$brakklagt = as.factor(salmon$brakklagt)

salmon$lice.above.limit = as.character(salmon$lice.above.limit)
salmon[salmon$lice.above.limit == 'Ja','lice.above.limit'] = 'yes'
salmon[salmon$lice.above.limit == 'Nei','lice.above.limit'] = 'no'
salmon[salmon$lice.above.limit == '','lice.above.limit'] = 'no'
salmon$lice.above.limit = as.factor(salmon$lice.above.limit)

# drop unused level
salmon$lice.limit = factor(salmon$lice.limit)

# set NA from disease to none because the fish is healthy
salmon[is.na(salmon$disease),'disease'] = 'none'
salmon[is.na(salmon$status),'status'] = 'healthy'
salmon[is.na(salmon$treatment),'treatment'] = 'none'
salmon[is.na(salmon$treatment.type),'treatment.type'] = 'none'
salmon[is.na(salmon$chemical),'chemical'] = 'none'
salmon[is.na(salmon$cleaner.fish.id),'cleaner.fish.id'] = 0
salmon[is.na(salmon$no.of.cleaner.fish),'no.of.cleaner.fish'] = 0
salmon[is.na(salmon$extent),'extent'] = 'none'

# format as factor
# salmon$municipality.id = as.factor(salmon$municipality.id)
# salmon$county.id = as.factor(salmon$county.id)
salmon$production.id = as.factor(salmon$production.id)
# salmon$week = as.factor(salmon$week)
# salmon$year = as.factor(salmon$year)
salmon$disease = as.factor(salmon$disease)
salmon$status = as.factor(salmon$status)
salmon$treatment = as.factor(salmon$treatment)
salmon$treatment.type = as.factor(salmon$treatment.type)
salmon$chemical = as.factor(salmon$chemical)
# salmon$cleaner.fish.id = as.factor(salmon$cleaner.fish.id)
salmon$no.of.cleaner.fish = as.integer(salmon$no.of.cleaner.fish)
salmon$extent = as.factor(salmon$extent)

# drop name of the cleaner fish as its not needed
salmon = salmon[,-27]
# remove variables with no/duplicated signal
salmon = salmon[,-c(3,4,10,12,19,20)]

# check NAs: 48,6% of sea.temp is missing, rest is complete
round(sort(sapply(salmon, function (x) mean(is.na(x)))), digits=3)
