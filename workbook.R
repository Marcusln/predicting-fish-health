library(dplyr)

#
## Data wrangling
###

# import dataset (from BarentsWatch)
lakselus_per_fisk <- read_csv("lakselus_per_fisk.csv")

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
colnames(lice)[8] = "municipality.id"
colnames(lice)[9] = "municipality.name"
colnames(lice)[10] = "latitude"
colnames(lice)[11] = "longitude"
colnames(lice)[12] = "sea.temp"
colnames(lice)[13] = "production.id"
colnames(lice)[14] = "production.name"

# create new variable to be used in a later join
lice$year.week = paste0(lice$year,lakselus$week)

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
treatment$year.week = paste0(treatment$year,treatment$week)

# import list of county id with corresponding county id
DimPostnummer <- read_delim("DimPostnummer.csv",";", escape_double = FALSE, trim_ws = TRUE)

# remove variables not needed
DimPostnummer$Postnummer = NULL
DimPostnummer$Poststed = NULL
DimPostnummer$Kommune = NULL
DimPostnummer$PostnummerKategoriKode = NULL
DimPostnummer$PostnummerKategori = NULL
DimPostnummer$Latitude = NULL
DimPostnummer$Longitude = NULL

# rename to make join easier
DimPostnummer$county.id = DimPostnummer$FylkeKode
DimPostnummer$county = DimPostnummer$Fylke
DimPostnummer$municipality.id = DimPostnummer$KommuneKode

# remove old variables
DimPostnummer$FylkeKode = NULL
DimPostnummer$KommuneKode = NULL
DimPostnummer$Fylke = NULL


salmon = left_join(lice, disease, by = c("year.week","location.id"))
