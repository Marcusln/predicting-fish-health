
# import list of county id with corresponding municipality id
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
DimPostnummer %<>% rename("county.id" = "FylkeKode",
                          "county.name" = "Fylke",
                          "municipality.id.x" = "KommuneKode")

# remove duplicate rows
DimPostnummer = unique(DimPostnummer)

# add county data by merging lice and DimPostNumber
salmon = left_join(salmon, DimPostnummer, by = "municipality.id.x")

# replace NA in county.id from lice with county.id from DimPostNumber
salmon$county.id.x[is.na(salmon$county.id.x)] = salmon$county.id.y[is.na(salmon$county.id.x)]
