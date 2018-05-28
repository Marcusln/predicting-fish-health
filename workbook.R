ila_pd <- read_csv("ila_pd.csv")
lakselus_per_fisk <- read_csv("lakselus_per_fisk.csv")
tiltak_mot_lakselus <- read_csv("tiltak_mot_lakselus.csv")

#
## Data wrangling
###

library(dplyr)

# translate columns

lakselus = lakselus_per_fisk

colnames(lakselus)[1] = "week"
colnames(lakselus)[2] = "year"
colnames(lakselus)[3] = "location.id"
colnames(lakselus)[4] = "location.name"
colnames(lakselus)[5] = "adult.female.lice"
colnames(lakselus)[6] = "moving.lice"
colnames(lakselus)[7] = "stuck.lice"
colnames(lakselus)[8] = "municipality.id"
colnames(lakselus)[9] = "municipality.name"
colnames(lakselus)[10] = "latitude"
colnames(lakselus)[11] = "longitude"
colnames(lakselus)[12] = "sea.temp"
colnames(lakselus)[13] = "production.id"
colnames(lakselus)[14] = "production.name"

lakselus$year.week = paste0(lakselus$year,lakselus$week)
