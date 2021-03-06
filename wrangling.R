library(dplyr)
library(magrittr)
library(readr)

#
## Data wrangling
###

# import dataset (from BarentsWatch)
lakselus_per_fisk <- read_excel("lakselus_per_fisk.xlsx")

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

lice$week = as.Date(lice$week, '%V')
lice$year = as.integer(lice$year)
lice$year = as.Date(lice$year, '%Y')

# add leading zero for week
lice$week = ifelse(nchar(lice$week) == 1, formatC(lice$week, width = 2, format = "d", flag = "0"), lice$week)

# format as date
lice$year.week2 = as.Date(paste(lice$year, lice$week, 7, sep = "-"), "%Y-%V-%u")

# removing rows without location name, these are inactive locations ref documentation
lice = lice[!is.na(lice$location.name),]

# import dataset (from BarentsWatch)
ila_pd <- read_excel("ila_pd.xlsx")

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
disease = disease[,-c(1,2,4,9,10,11,12,13,14,15,16)]

# add disease info to lice
salmon = left_join(lice, disease, by = c("year.week","location.id"))

# import dataset (from BarentsWatch)
tiltak_mot_lakselus <- read_excel("tiltak_mot_lakselus.xlsx")

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

treatment = treatment[,-c(1,2,4,9,12,13,14,15,16,17,18,19)]

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

# drop unused level
salmon$lice.limit = factor(salmon$lice.limit)

# set NA from disease etc to none because the fish is healthy
salmon[is.na(salmon$disease),'disease'] = 'none'
salmon[is.na(salmon$status),'status'] = 'healthy'
salmon[is.na(salmon$treatment),'treatment'] = 'none'
salmon[is.na(salmon$treatment.type),'treatment.type'] = 'none'
salmon[is.na(salmon$chemical),'chemical'] = 'none'
salmon[is.na(salmon$cleaner.fish.id),'cleaner.fish.id'] = 0
salmon[is.na(salmon$no.of.cleaner.fish),'no.of.cleaner.fish'] = 0
salmon[is.na(salmon$extent),'extent'] = 'none'

# format variables
# salmon$municipality.id = as.factor(salmon$municipality.id)
# salmon$location.id = as.factor(salmon$location.id)
salmon$county.id = as.factor(salmon$county.id)
salmon$production.id = as.factor(salmon$production.id)
# salmon$week = as.factor(salmon$week)
# salmon$year = as.factor(salmon$year)
salmon$disease = as.factor(salmon$disease)
salmon$status = as.factor(salmon$status)
salmon$treatment = as.factor(salmon$treatment)
salmon$treatment.type = as.factor(salmon$treatment.type)
salmon$chemical = as.factor(salmon$chemical)
salmon$cleaner.fish.id = as.factor(salmon$cleaner.fish.id)
salmon$no.of.cleaner.fish = as.integer(salmon$no.of.cleaner.fish)
salmon$extent = as.factor(salmon$extent)
salmon$year.week = as.integer(salmon$year.week)
salmon$year = as.factor(salmon$year)
salmon$location.id = as.character(salmon$location.id)

# remove variables with no/duplicated signal
salmon = salmon[,-c(4,10,12,16,19)]

#
## Format dates
###

salmon$year.week2 = salmon$year.week
salmon$year.week2 = as.Date(salmon$year.week2, format = "%Y%W")

# create 8 lag variables (8 weeks), and 2 lead variables for adult.female.lice
salmon %<>% 
  group_by(location.id) %>% 
  mutate(lag1lice = dplyr::lag(adult.female.lice, n = 1, default = NA, order_by = year.week),
         lag2lice = dplyr::lag(adult.female.lice, n = 2, default = NA, order_by = year.week),
         lag3lice = dplyr::lag(adult.female.lice, n = 3, default = NA, order_by = year.week),
         lag4lice = dplyr::lag(adult.female.lice, n = 4, default = NA, order_by = year.week),
         lag5lice = dplyr::lag(adult.female.lice, n = 5, default = NA, order_by = year.week),
         lag6lice = dplyr::lag(adult.female.lice, n = 6, default = NA, order_by = year.week),
         lag7lice = dplyr::lag(adult.female.lice, n = 7, default = NA, order_by = year.week),
         lag8lice = dplyr::lag(adult.female.lice, n = 8, default = NA, order_by = year.week),
         lead1lice = dplyr::lead(adult.female.lice, n = 1, default = NA, order_by = year.week),
         lead2lice = dplyr::lead(adult.female.lice, n = 2, default = NA, order_by = year.week)
         )

# double check its correct
salmon %>% 
  filter(location.id == 12260) %>% # check tail with year == 2012
  group_by(location.id) %>% 
  View()

# create dataset for modelling
salmon.cleaned = salmon

# check NAs: sea.temp NAs could be imputed but for now we delete them.
# other NAs are in lag/lead and should be removed.
round(sort(sapply(salmon.cleaned, function (x) mean(is.na(x)))), digits=3)

# remove NAs
salmon.cleaned = na.omit(salmon.cleaned)
round(sort(sapply(salmon.cleaned, function (x) mean(is.na(x)))), digits=3)
