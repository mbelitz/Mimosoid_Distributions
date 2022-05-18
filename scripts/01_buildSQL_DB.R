library(data.table)
library(dbplyr)
library(RSQLite)

## read in the data
t <- fread("Mimosoid/sernec_mimosoid_occurrences_with_georefs000000000000.csv",
           select = c("id","decimalLatitude", "decimalLongitude",
                      "eventDate", "year", "institutionCode", "occurrenceID",
                      "scientificName", "genus", "specificEpithet", "recordedBy",
                      "verbatimEventDate", "locality"))


myDB <- "data/Mimosoid_Occs.db"
myConn <- dbConnect(drv = SQLite(), dbname= myDB)
dbListTables(myConn)

dbWriteTable(myConn,"sernec",t, overwrite = T)
dbListTables(myConn)

# now idigbio
t <- fread("Mimosoid/idigbio_mimosoid_occurrences_with_georefs000000000000.csv",
           select = c("coreid", "decimalLatitude", "decimalLongitude",
                      "month", "day", "year", "institutionCode", "occurrenceID",
                      "scientificName", "genus", "specificEpithet", "recordedBy",
                      "verbatimEventDate", "locality"))

dbWriteTable(myConn,"iDigBio",t)
dbListTables(myConn)


## now gbif
t <- fread("Mimosoid/gbif_mimosoid_occurrences_with_georefs000000000000.csv",
           select = c("gbifid","decimalLatitude", "decimalLongitude",
                      "eventDate", "year", "institutionCode", "occurrenceID",
                      "scientificName", "genus", "species", "recordedBy",
                      "v_verbatimEventDate", "locality"))

dbWriteTable(myConn,"gbif",t)
dbListTables(myConn)

t2 <- fread("Mimosoid/gbif_mimosoid_occurrences_with_georefs000000000001.csv",
            select = c("gbifid","decimalLatitude", "decimalLongitude",
                       "eventDate", "year", "institutionCode", "occurrenceID",
                       "scientificName", "genus", "species", "recordedBy",
                       "v_verbatimEventDate", "locality"))

dbWriteTable(myConn,"gbif",t2, append = T)
dbListTables(myConn)