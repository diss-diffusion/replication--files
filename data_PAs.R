#### Data on protected areas ####

#### 1. Creation of a data set listing all the protected areas on earth ####

# load the data

# Africa #
af0a <- readShapePoints("~/R/UCL/Dissertation/WDPA_WDOECM_Mar2023_Public_AF_shp/WDPA_WDOECM_Mar2023_Public_AF_shp_0/WDPA_WDOECM_Mar2023_Public_AF_shp-points.shp")

af0b <- readOGR(dsn = "~/R/UCL/Dissertation/WDPA_WDOECM_Mar2023_Public_AF_shp/WDPA_WDOECM_Mar2023_Public_AF_shp_0" ,layer = "WDPA_WDOECM_Mar2023_Public_AF_shp-polygons")

af1a <- readShapePoints("~/R/UCL/Dissertation/WDPA_WDOECM_Mar2023_Public_AF_shp/WDPA_WDOECM_Mar2023_Public_AF_shp_1/WDPA_WDOECM_Mar2023_Public_AF_shp-points.shp")

af1b <- readOGR(dsn = "~/R/UCL/Dissertation/WDPA_WDOECM_Mar2023_Public_AF_shp/WDPA_WDOECM_Mar2023_Public_AF_shp_1" ,layer = "WDPA_WDOECM_Mar2023_Public_AF_shp-polygons")

af2a <- read_sf(dsn = "~/R/UCL/Dissertation/WDPA_WDOECM_Mar2023_Public_AF_shp/WDPA_WDOECM_Mar2023_Public_AF_shp_2", layer = "WDPA_WDOECM_Mar2023_Public_AF_shp-points")

af2b <- readOGR(dsn = "~/R/UCL/Dissertation/WDPA_WDOECM_Mar2023_Public_AF_shp/WDPA_WDOECM_Mar2023_Public_AF_shp_2" ,layer = "WDPA_WDOECM_Mar2023_Public_AF_shp-polygons")


# West Asia #
wa0a <- readShapePoints("~/R/UCL/Dissertation/WDPA_WDOECM_Mar2023_Public_WA_shp/WDPA_WDOECM_Mar2023_Public_WA_shp_0/WDPA_WDOECM_Mar2023_Public_WA_shp-points.shp")

wa0b <- readOGR(dsn = "~/R/UCL/Dissertation/WDPA_WDOECM_Mar2023_Public_WA_shp/WDPA_WDOECM_Mar2023_Public_WA_shp_0" ,layer = "WDPA_WDOECM_Mar2023_Public_WA_shp-polygons")

wa1a <- readShapePoints("~/R/UCL/Dissertation/WDPA_WDOECM_Mar2023_Public_WA_shp/WDPA_WDOECM_Mar2023_Public_WA_shp_1/WDPA_WDOECM_Mar2023_Public_WA_shp-points.shp")

wa1b <- readOGR(dsn = "~/R/UCL/Dissertation/WDPA_WDOECM_Mar2023_Public_WA_shp/WDPA_WDOECM_Mar2023_Public_WA_shp_1" ,layer = "WDPA_WDOECM_Mar2023_Public_WA_shp-polygons")

wa2a <- readShapePoints("~/R/UCL/Dissertation/WDPA_WDOECM_Mar2023_Public_WA_shp/WDPA_WDOECM_Mar2023_Public_WA_shp_2/WDPA_WDOECM_Mar2023_Public_WA_shp-points.shp")

wa2b <- readOGR(dsn = "~/R/UCL/Dissertation/WDPA_WDOECM_Mar2023_Public_WA_shp/WDPA_WDOECM_Mar2023_Public_WA_shp_2" ,layer = "WDPA_WDOECM_Mar2023_Public_WA_shp-polygons")


# East Asia #
as0a <- readShapePoints("~/R/UCL/Dissertation/WDPA_WDOECM_Mar2023_Public_AS_shp/WDPA_WDOECM_Mar2023_Public_AS_shp_0/WDPA_WDOECM_Mar2023_Public_AS_shp-points.shp")

as0b <- readOGR(dsn = "~/R/UCL/Dissertation/WDPA_WDOECM_Mar2023_Public_AS_shp/WDPA_WDOECM_Mar2023_Public_AS_shp_0" ,layer = "WDPA_WDOECM_Mar2023_Public_AS_shp-polygons")

as1a <- readShapePoints("~/R/UCL/Dissertation/WDPA_WDOECM_Mar2023_Public_AS_shp/WDPA_WDOECM_Mar2023_Public_AS_shp_1/WDPA_WDOECM_Mar2023_Public_AS_shp-points.shp")

as1b <- readOGR(dsn = "~/R/UCL/Dissertation/WDPA_WDOECM_Mar2023_Public_AS_shp/WDPA_WDOECM_Mar2023_Public_AS_shp_1" ,layer = "WDPA_WDOECM_Mar2023_Public_AS_shp-polygons")

as2a <- readShapePoints("~/R/UCL/Dissertation/WDPA_WDOECM_Mar2023_Public_AS_shp/WDPA_WDOECM_Mar2023_Public_AS_shp_2/WDPA_WDOECM_Mar2023_Public_AS_shp-points.shp")

as2b <- readOGR(dsn = "~/R/UCL/Dissertation/WDPA_WDOECM_Mar2023_Public_AS_shp/WDPA_WDOECM_Mar2023_Public_AS_shp_2" ,layer = "WDPA_WDOECM_Mar2023_Public_AS_shp-polygons")


# North America #
na0a <- readShapePoints("~/R/UCL/Dissertation/WDPA_WDOECM_Mar2023_Public_NA_shp/WDPA_WDOECM_Mar2023_Public_NA_shp_0/WDPA_WDOECM_Mar2023_Public_NA_shp-points.shp")

na0b <- readOGR(dsn = "~/R/UCL/Dissertation/WDPA_WDOECM_Mar2023_Public_NA_shp/WDPA_WDOECM_Mar2023_Public_NA_shp_0" ,layer = "WDPA_WDOECM_Mar2023_Public_NA_shp-polygons")

na1a <- readShapePoints("~/R/UCL/Dissertation/WDPA_WDOECM_Mar2023_Public_NA_shp/WDPA_WDOECM_Mar2023_Public_NA_shp_1/WDPA_WDOECM_Mar2023_Public_NA_shp-points.shp")

na1b <- readOGR(dsn = "~/R/UCL/Dissertation/WDPA_WDOECM_Mar2023_Public_NA_shp/WDPA_WDOECM_Mar2023_Public_NA_shp_1" ,layer = "WDPA_WDOECM_Mar2023_Public_NA_shp-polygons")

na2a <- readShapePoints("~/R/UCL/Dissertation/WDPA_WDOECM_Mar2023_Public_NA_shp/WDPA_WDOECM_Mar2023_Public_NA_shp_2/WDPA_WDOECM_Mar2023_Public_NA_shp-points.shp")

na2b <- readOGR(dsn = "~/R/UCL/Dissertation/WDPA_WDOECM_Mar2023_Public_NA_shp/WDPA_WDOECM_Mar2023_Public_NA_shp_2" ,layer = "WDPA_WDOECM_Mar2023_Public_NA_shp-polygons")


# South America #
sa0a <- readShapePoints("~/R/UCL/Dissertation/WDPA_WDOECM_Mar2023_Public_SA_shp/WDPA_WDOECM_Mar2023_Public_SA_shp_0/WDPA_WDOECM_Mar2023_Public_SA_shp-points.shp")

sa0b <- readOGR(dsn = "~/R/UCL/Dissertation/WDPA_WDOECM_Mar2023_Public_SA_shp/WDPA_WDOECM_Mar2023_Public_SA_shp_0" ,layer = "WDPA_WDOECM_Mar2023_Public_SA_shp-polygons")

sa1a <- readShapePoints("~/R/UCL/Dissertation/WDPA_WDOECM_Mar2023_Public_SA_shp/WDPA_WDOECM_Mar2023_Public_SA_shp_1/WDPA_WDOECM_Mar2023_Public_SA_shp-points.shp")

sa1b <- readOGR(dsn = "~/R/UCL/Dissertation/WDPA_WDOECM_Mar2023_Public_SA_shp/WDPA_WDOECM_Mar2023_Public_SA_shp_1" ,layer = "WDPA_WDOECM_Mar2023_Public_SA_shp-polygons")

sa2a <- readShapePoints("~/R/UCL/Dissertation/WDPA_WDOECM_Mar2023_Public_SA_shp/WDPA_WDOECM_Mar2023_Public_SA_shp_2/WDPA_WDOECM_Mar2023_Public_SA_shp-points.shp")

sa2b <- readOGR(dsn = "~/R/UCL/Dissertation/WDPA_WDOECM_Mar2023_Public_SA_shp/WDPA_WDOECM_Mar2023_Public_SA_shp_2" ,layer = "WDPA_WDOECM_Mar2023_Public_SA_shp-polygons")


# Polar Areas #
po0a <- readShapePoints("~/R/UCL/Dissertation/WDPA_WDOECM_Mar2023_Public_PO_shp/WDPA_WDOECM_Mar2023_Public_PO_shp_0/WDPA_WDOECM_Mar2023_Public_PO_shp-points.shp")

po0b <- readOGR(dsn = "~/R/UCL/Dissertation/WDPA_WDOECM_Mar2023_Public_PO_shp/WDPA_WDOECM_Mar2023_Public_PO_shp_0" ,layer = "WDPA_WDOECM_Mar2023_Public_PO_shp-polygons")

po1b <- readOGR(dsn = "~/R/UCL/Dissertation/WDPA_WDOECM_Mar2023_Public_PO_shp/WDPA_WDOECM_Mar2023_Public_PO_shp_1" ,layer = "WDPA_WDOECM_Mar2023_Public_PO_shp-polygons")

po2b <- readOGR(dsn = "~/R/UCL/Dissertation/WDPA_WDOECM_Mar2023_Public_PO_shp/WDPA_WDOECM_Mar2023_Public_PO_shp_2" ,layer = "WDPA_WDOECM_Mar2023_Public_PO_shp-polygons")


# Europe #
eu0a <- readShapePoints("~/R/UCL/Dissertation/WDPA_WDOECM_Mar2023_Public_EU_shp/WDPA_WDOECM_Mar2023_Public_EU_shp_0/WDPA_WDOECM_Mar2023_Public_EU_shp-points.shp")

eu0b <- readOGR(dsn = "~/R/UCL/Dissertation/WDPA_WDOECM_Mar2023_Public_EU_shp/WDPA_WDOECM_Mar2023_Public_EU_shp_0" ,layer = "WDPA_WDOECM_Mar2023_Public_EU_shp-polygons")

eu1a <- readShapePoints("~/R/UCL/Dissertation/WDPA_WDOECM_Mar2023_Public_EU_shp/WDPA_WDOECM_Mar2023_Public_EU_shp_1/WDPA_WDOECM_Mar2023_Public_EU_shp-points.shp")

eu1b <- readOGR(dsn = "~/R/UCL/Dissertation/WDPA_WDOECM_Mar2023_Public_EU_shp/WDPA_WDOECM_Mar2023_Public_EU_shp_1" ,layer = "WDPA_WDOECM_Mar2023_Public_EU_shp-polygons")

eu2a <- read_sf("~/R/UCL/Dissertation/WDPA_WDOECM_Mar2023_Public_EU_shp/WDPA_WDOECM_Mar2023_Public_EU_shp_2/WDPA_WDOECM_Mar2023_Public_EU_shp-points.shp")

eu2b <- readOGR(dsn = "~/R/UCL/Dissertation/WDPA_WDOECM_Mar2023_Public_EU_shp/WDPA_WDOECM_Mar2023_Public_EU_shp_2" ,layer = "WDPA_WDOECM_Mar2023_Public_EU_shp-polygons")


# turn the data into data frames and remove some variables
af2a <- as.data.frame(af2a)
af2a <- af2a[, !names(af2a) == "geometry"]

eu2a <- as.data.frame(eu2a)
eu2a <- eu2a[,!names(eu2a) == "geometry"]

af0b <- af0b[, !names(af0b) == "GIS_M_AREA"]
af0b <- af0b[, !names(af0b) == "GIS_AREA"]

af1b <- af1b[, !names(af1b) == "GIS_M_AREA"]
af1b <- af1b[, !names(af1b) == "GIS_AREA"]

af2b <- af2b[, !names(af2b) == "GIS_M_AREA"]
af2b <- af2b[, !names(af2b) == "GIS_AREA"]

na0b <- na0b[, !names(na0b) == "GIS_M_AREA"]
na0b <- na0b[, !names(na0b) == "GIS_AREA"]

na1b <- na1b[, !names(na1b) == "GIS_M_AREA"]
na1b <- na1b[, !names(na1b) == "GIS_AREA"]

na2b <- na2b[, !names(na2b) == "GIS_M_AREA"]
na2b <- na2b[, !names(na2b) == "GIS_AREA"]

sa0b <- sa0b[, !names(sa0b) == "GIS_M_AREA"]
sa0b <- sa0b[, !names(sa0b) == "GIS_AREA"]

sa1b <- sa1b[, !names(sa1b) == "GIS_M_AREA"]
sa1b <- sa1b[, !names(sa1b) == "GIS_AREA"]

sa2b <- sa2b[, !names(sa2b) == "GIS_M_AREA"]
sa2b <- sa2b[, !names(sa2b) == "GIS_AREA"]

wa0b <- wa0b[, !names(wa0b) == "GIS_M_AREA"]
wa0b <- wa0b[, !names(wa0b) == "GIS_AREA"]

wa1b <- wa1b[, !names(wa1b) == "GIS_M_AREA"]
wa1b <- wa1b[, !names(wa1b) == "GIS_AREA"]

wa2b <- wa2b[, !names(wa2b) == "GIS_M_AREA"]
wa2b <- wa2b[, !names(wa2b) == "GIS_AREA"]

as0b <- as0b[, !names(as0b) == "GIS_M_AREA"]
as0b <- as0b[, !names(as0b) == "GIS_AREA"]

as1b <- as1b[, !names(as1b) == "GIS_M_AREA"]
as1b <- as1b[, !names(as1b) == "GIS_AREA"]

as2b <- as2b[, !names(as2b) == "GIS_M_AREA"]
as2b <- as2b[, !names(as2b) == "GIS_AREA"]

po0b <- po0b[, !names(po0b) == "GIS_M_AREA"]
po0b <- po0b[, !names(po0b) == "GIS_AREA"]

po1b <- po1b[, !names(po1b) == "GIS_M_AREA"]
po1b <- po1b[, !names(po1b) == "GIS_AREA"]

po2b <- po2b[, !names(po2b) == "GIS_M_AREA"]
po2b <- po2b[, !names(po2b) == "GIS_AREA"]

eu0b <- eu0b[, !names(eu0b) == "GIS_M_AREA"]
eu0b <- eu0b[, !names(eu0b) == "GIS_AREA"]

eu1b <- eu1b[, !names(eu1b) == "GIS_M_AREA"]
eu1b <- eu1b[, !names(eu1b) == "GIS_AREA"]

eu2b <- eu2b[, !names(eu2b) == "GIS_M_AREA"]
eu2b <- eu2b[, !names(eu2b) == "GIS_AREA"]


# merge all the data into one data frame
names(af2a)
a <- rbind(af0a@data, af0b@data, af1a@data, af1b@data, af2a, af2b@data, na0a@data, na0b@data)

data <- rbind(af0a@data, af0b@data, af1a@data, af1b@data, af2a, af2b@data, na0a@data, na0b@data, na1a@data, na1b@data, na2a@data, na2b@data, sa0a@data, sa0b@data, sa1a@data, sa1b@data, sa2a@data, sa2b@data, wa0a@data, wa0b@data, wa1a@data, wa1b@data, wa2a@data, wa2b@data, as0a@data, as0b@data, as1a@data, as1b@data, as2a@data, as2b@data, po0a@data, po0b@data, po1b@data, po2b@data, eu0a@data, eu0b@data, eu1a@data, eu1b@data, eu2a, eu2b@data)

# write a file
write.csv(data, "pa_data.csv")


#### 2. Add information about PADDD events ####


## Load and clean PA data

# load data
data <- read.csv("pa_data.csv")

# remove areas which do not correspond to the definition of a protected area
datac <- data[data$PA_DEF == 1, ]

# remove areas which are not national
datac <- datac[datac$DESIG_TYPE == "National", ]

# remove some variables
datac <- datac[, !names(datac) == "MANG_PLAN"]
datac <- datac[, !names(datac) == "SUPP_INFO"]
datac <- datac[, !names(datac) == "SUB_LOC"]

# remove the rows on protected areas that are mostly marine
datac <- datac[datac$MARINE != 2,]

# replace China by Taiwan in PARENT_ISO column
datac$PARENT_ISO[datac$PARENT_ISO == "CHN" & datac$ISO3 == "TWN"] <- "TWN"



## Clean and transform data about PADDD events

# load data
paddd_events <- read.csv("PADDD events.csv")
paddd_reversals <- read.csv("PADDD reversals.csv")

# keep only downnsize and degazette events
paddd_events <- paddd_events[paddd_events$EventType != "Downgrade", ]
paddd_reversals <- paddd_reversals[paddd_reversals$EventType != "Downgrade", ]

# keep only enacted events
paddd_events <- paddd_events[paddd_events$EnactedPro == "Enacted", ]
paddd_reversals <- paddd_reversals[paddd_reversals$EnactedPro == "Enacted", ]

# create records for "unknown" WDPAIDS
unk <- paddd_events[paddd_events$WDPAID == "unk", ]

# create vectors of names without the "classic " words found in PA names
classic <- c("Reserve", "Natural", "Park", "Nature", "Forest", "Area", 
             "Protected", "Features", "National", "Conservation", "Management", 
             "Agreement", "Hills", "Recreation", "Mount", "State",
             "Hill", "Resources", "Indigenous", "Wildlife", "Hunting",
             "Range", "Mountain", "Swamp")

for (i in classic) {
  
  unk$allnames <- gsub(i, "", unk$allnames)
  
}

# create a vector for each datasets with the list of words that appear in PA names in each data set
stringsA <- list()
stringsB <- list()

for (i in 1:nrow(unk)) {
  stringsA <- c(stringsA, strsplit(unk$allnames[i], split = " "))
}

for (j in 1:nrow(datac)) {
  stringsB <- c(stringsB, strsplit(datac$NAME[j], split = " "))
}

# make the two lists vectors
stringsA <- unlist(stringsA)
stringsB <- unlist(stringsB)

# find the intersection between PA names in "unk" and PA names in "datac"
inter <- intersect(stringsA, stringsB)

# remove words that do not identify unique areas (general words) from the intersection
to_remove <- c("", "-", "2", "de", "et", "la", "Del", "dos", "do", "d", "#", 
               "y", "s", "del", ";", ")", "A", "Of", "De", "&", ",",
               "/", "V", "C", "B", "e", "St", "Dam", "Great", "of", "and", 
               "Farm", "Reserva", "Rd", "Greater", "Springs", "Unnamed", "Rocks",
               "Desert", "das", "Santuario", "sur", "Montagne", "Resource", "Conservacion",
               "Development", "Station", "Zona", "Controlled", "Islands", "Pier", "Miscellaneous",
               "Geological", "Historical", "The", "Special", "Biosphere", "Community",
               "Sanctuary", "Regional", "Protection", "les", "Private", "forest", "Wilderness")

inter <- inter[!inter %in% to_remove]

# modify a name to ensure R can run the code
inter[inter == "(No."] <- "(No.)"

# create a list of row numbers corresponding to rows in which at least one word matches between the two datasets
matches <- list()

for (i in 1:length(inter)) {
  
  inter1 <- datac[grep(inter[i], datac$NAME),]
  
  inter2 <- unk[grep(inter[i], unk$allnames),]
  
  for (j in 1:length(inter1)) {
    
    for (k in 1:length(inter2)) {
      
      if (!is.na(inter1$PARENT_ISO[j] == inter2$ISO3166[k])) {
        
        if (inter1$PARENT_ISO[j] == inter2$ISO3166[k]) {
          
          b <- rownames(unk[grep(inter[i], unk$allnames),])
          
          matches <- c(matches, b)
        }
      }
    }
  }
}

# keep each number only once
matches <- unique(matches)

# filter "unk" to keep only matches
unk_filtered <- unk[rownames(unk) %in% matches,]

write.csv(unk_filtered, "unk_filtered.csv")

unk_filtered <- read.csv("unk_filtered.csv")

# assign WDPAIDs for each match between names
unk$WDPAID[unk$allnames == "Kate Reed   "] <- 126436
unk$WDPAID[unk$allnames == "Kuranda  "] <- 555625188
unk$WDPAID[unk$allnames == "Deen Maar   "] <- 314866
unk$WDPAID[unk$allnames == "Mapleton  "] <- 555625193
unk$WDPAID[unk$allnames == "Wilby    - Bushland "] <- 309122
unk$WDPAID[unk$allnames == "Tumoulin  "] <- 555625202
unk$WDPAID[unk$allnames == "Doma Mungi Creek    - Streamside "] <- 354617
unk$WDPAID[unk$allnames == "Briagolong    - Bushland "] <- 314150
unk$WDPAID[unk$allnames == "Dulcie "] <- 313833
unk$WDPAID[unk$allnames == "Danbulla  "] <- 555625177
unk$WDPAID[unk$allnames == "Connors  "] <- 555625175
unk$WDPAID[unk$allnames == "Lexton   "] <- 356578
unk$WDPAID[unk$allnames == "Dandenong s  "] <- 24633
unk$WDPAID[unk$allnames == "Rifle   "] <- 357108
unk$WDPAID[unk$allnames == "Sandhurst Reference "] <- 64292
unk$WDPAID[unk$allnames == "Organ Pipes  "] <- 24787
unk$WDPAID[unk$allnames == "Althorpe Islands  "] <- 24285
unk$WDPAID[unk$allnames == "Gawler s  "] <- 126313
unk$WDPAID[unk$allnames == "Cape Liptrap Coastal  "] <- 127244
unk$WDPAID[unk$allnames == "O'Halloran   "] <- 314003
unk$WDPAID[unk$allnames == "Coffin Bay  "] <- 10606
unk$WDPAID[unk$allnames == "Warramate       "] <- 314709
unk$WDPAID[unk$allnames == "Long Reach    "] <- 309512
unk$WDPAID[unk$allnames == "Nullarbor  "] <- 2665
unk$WDPAID[unk$allnames == "Beethang I29 B.R.   "] <- 127386
unk$WDPAID[unk$allnames == "Neerabup  "] <- 64123
unk$WDPAID[unk$allnames == "Fairy Dell    - Flora "] <- 126283
unk$WDPAID[unk$allnames == "Tam O'Shanter  "] <- 555625200
unk$WDPAID[unk$allnames == "Iron   "] <- 555577147
unk$WDPAID[unk$allnames == "Gadgarra  "] <- 555625183
unk$WDPAID[unk$allnames == "Malbon Thompson  "] <- 555625192
unk$WDPAID[unk$allnames == " Stanley 1  "] <- 555625197
unk$WDPAID[unk$allnames == "Pier Millan    - Bushland "] <- 308881
unk$WDPAID[unk$allnames == "Spring Gully  "] <- 357224
unk$WDPAID[unk$allnames == "Newland Head  "] <- 24420
unk$WDPAID[unk$allnames == "Patchewollock I117A    - Bushland "] <- 308873
unk$WDPAID[unk$allnames == "Ravenshoe 1  "] <- 555625199
unk$WDPAID[unk$allnames == "Coimadai    - Bushland "] <- 311404
unk$WDPAID[unk$allnames == "Greater Bendigo  "] <- 354841
unk$WDPAID[unk$allnames == "Gregory  "] <- 313836
unk$WDPAID[unk$allnames == "Olinda G168 Bushland     - Bushland "] <- 305362
unk$WDPAID[unk$allnames == "Manton Dam   "] <- 313857
unk$WDPAID[unk$allnames == "Warrandyte - Kinglake   "] <- 308976
unk$WDPAID[unk$allnames == "Bungeet Creek    - Bushland "] <- 127264
unk$WDPAID[unk$allnames == "Yarra s  "] <- 102573
unk$WDPAID[unk$allnames == "Walpole-Nornalup  "] <- 24133
unk$WDPAID[unk$allnames == "Toltol    - Flora And Fauna "] <- 314763
unk$WDPAID[unk$allnames == "Percy Isles  "] <- 309944
unk$WDPAID[unk$allnames == "Bromley    - Bushland "] <- 127381
unk$WDPAID[unk$allnames == "Cameron Regional  Regional "] <- 314073
unk$WDPAID[unk$allnames == "Great Otway  "] <- 354835
unk$WDPAID[unk$allnames == "Skipton    - Streamside "] <- 308945
unk$WDPAID[unk$allnames == " Tanner   "] <- 356820
unk$WDPAID[unk$allnames == "Yabba 2  "] <- 555625203
unk$WDPAID[unk$allnames == "Mysia N.C.R.   "] <- 356884
unk$WDPAID[unk$allnames == " Bullfight   "] <- 306763
unk$WDPAID[unk$allnames == "Alcock  "] <- 555625171
unk$WDPAID[unk$allnames == " Ridley N.C.R.   "] <- 126665
unk$WDPAID[unk$allnames == "Bungalally I49    - Bushland "] <- 314157
unk$WDPAID[unk$allnames == "Wayatinah  "] <- 314059
unk$WDPAID[unk$allnames == "Lake Purrumbete W.R   "] <- 314830
unk$WDPAID[unk$allnames == "Glenelg River (5)    - Streamside "] <- 314521
unk$WDPAID[unk$allnames == " Manning   "] <- 314891
unk$WDPAID[unk$allnames == "Kuranda West  "] <- 555625189
unk$WDPAID[unk$allnames == "Stringybark Creek Reference  (Outside PA)"] <- 127015
unk$WDPAID[unk$allnames == "Toolangi    - Bushland "] <- 314403
unk$WDPAID[unk$allnames == "Danbulla South  "] <- 555625177
unk$WDPAID[unk$allnames == "Bundjalung  "] <- 9465
unk$WDPAID[unk$allnames == "Pilchers Bridge   "] <- 314754
unk$WDPAID[unk$allnames == "Panitya    - Bushland "] <- 308871
unk$WDPAID[unk$allnames == "Lerderderg  "] <- 24738
unk$WDPAID[unk$allnames == " Farrell Regional  Regional "] <- 314081
unk$WDPAID[unk$allnames == "Port Cygnet  "] <- 5333
unk$WDPAID[unk$allnames == "Chiltern-Mt Pilot "] <- 354464
unk$WDPAID[unk$allnames == "Howick Group  "] <- 63057
unk$WDPAID[unk$allnames == "Kilmore East    - Bushland "] <- 306626
unk$WDPAID[unk$allnames == "Tragowel     -   (no hunti"] <- 314855
unk$WDPAID[unk$allnames == "Yering Gorge B.R.   "] <- 309216
unk$WDPAID[unk$allnames == "Arthurs Seat  "] <- 24574
unk$WDPAID[unk$allnames == "Pinkawillinie Reservoir  "] <- 309818
unk$WDPAID[unk$allnames == "Toogimbie   "] <- 357378
unk$WDPAID[unk$allnames == "Dandenong s  "] <- 24633
unk$WDPAID[unk$allnames == "Witjira  "] <- 24496
unk$WDPAID[unk$allnames == "Nhill       -   (NCR classifi"] <- 356906
unk$WDPAID[unk$allnames == "Grampians    "] <- 10607
unk$WDPAID[unk$allnames == "Croajingolong  "] <- 2029
unk$WDPAID[unk$allnames == "Kinglake  "] <- 543
unk$WDPAID[unk$allnames == "Bumbang I39    - Bushland "] <- 314156
unk$WDPAID[unk$allnames == "Beerwah  "] <- 555625174
unk$WDPAID[unk$allnames == "Macalister   "] <- 555625191
unk$WDPAID[unk$allnames == "Kooyoora    "] <- 24694
unk$WDPAID[unk$allnames == "Dunach   "] <- 354644
unk$WDPAID[unk$allnames == "Bargo   "] <- 354189
unk$WDPAID[unk$allnames == "Kinglake  "] <- 543
unk$WDPAID[unk$allnames == "Vale of Belvoir  "] <- 309712
unk$WDPAID[unk$allnames == "Wilkin    - Flora And Fauna "] <- 24876
unk$WDPAID[unk$allnames == "Southern Beekeepers  "] <- 126986
unk$WDPAID[unk$allnames == "Nullawarre    - Flora "] <- 308853
unk$WDPAID[unk$allnames == "Heathcote-Graytown  "] <- 354890
unk$WDPAID[unk$allnames == "Calliope  "] <- 555624984
unk$WDPAID[unk$allnames == "Baldy ain  "] <- 555625172
unk$WDPAID[unk$allnames == "Tidbinbilla  "] <- 1127
unk$WDPAID[unk$allnames == "Dhimurru   "] <- 313868
unk$WDPAID[unk$allnames == "Beekeepers  "] <- 126004
unk$WDPAID[unk$allnames == "Yarra s  "] <- 102573
unk$WDPAID[unk$allnames == " Charlie    - Flora "] <- 126637
unk$WDPAID[unk$allnames == "Great Otway  "] <- 354835
unk$WDPAID[unk$allnames == "Dooen     - Bushland "] <- 311600
unk$WDPAID[unk$allnames == "Gnarr I237    - Bushland "] <- 127256
unk$WDPAID[unk$allnames == "Strzelecki Regional "] <- 314009
unk$WDPAID[unk$allnames == "Flinders Group  "] <- 2632
unk$WDPAID[unk$allnames == "Alpine    "] <- 62941
unk$WDPAID[unk$allnames == "Goulburn River, Killingworth Rd Streamside     - Streamside "] <- 127499
unk$WDPAID[unk$allnames == "Lexton H6    - Bushland "] <- 306676
unk$WDPAID[unk$allnames == "Maria Creek  "] <- 24002
unk$WDPAID[unk$allnames == "Cape Melville  "] <- 410
unk$WDPAID[unk$allnames == "Japoon  "] <- 555625185
unk$WDPAID[unk$allnames == "Ravenhall   "] <- 357070
unk$WDPAID[unk$allnames == "Maiden Gully    - Bushland "] <- 555543499
unk$WDPAID[unk$allnames == "Danbulla West  "] <- 555625179
unk$WDPAID[unk$allnames == "Dunolly Bushland     - Bushland "] <- 354649
unk$WDPAID[unk$allnames == "Lake Rowan    - Bushland "] <- 306653
unk$WDPAID[unk$allnames == "Unnamed WA46940 5(1)(h) "] <- 357482
unk$WDPAID[unk$allnames == "Benger   "] <- 63926
unk$WDPAID[unk$allnames == "Lawrence Rocks    -   (No )"] <- 314834
unk$WDPAID[unk$allnames == "Danbulla South 2  "] <- 555625178
unk$WDPAID[unk$allnames == "Onkaparinga River  "] <- 314004
unk$WDPAID[unk$allnames == "Tarra - Bulga  "] <- 24845
unk$WDPAID[unk$allnames == "Monkey Mia Miscellaneous  "] <- 126604
unk$WDPAID[unk$allnames == "Silk Grass  ; Mayflower Bocawina  "] <- 301934
unk$WDPAID[unk$allnames == "Commerce Bight  ; Commerce Bight A & B; Mayflower Bocawina"] <- 301934
unk$WDPAID[unk$allnames == "Hamber Provincial "] <- 10452
unk$WDPAID[unk$allnames == "Emory Creek Class A ;Emory Creek Provincial "] <- 65186
unk$WDPAID[unk$allnames == "Los Haitises  "] <- 181
unk$WDPAID[unk$allnames == "Jaragua  "] <- 6673
unk$WDPAID[unk$allnames == "Tanjung Puting  "] <- 1490
unk$WDPAID[unk$allnames == "Batang Gadis  "] <- 34161
unk$WDPAID[unk$allnames == "Pidaung  Sanctuary"] <- 8005
unk$WDPAID[unk$allnames == "San Rafael Managed Resource ;San Rafael  "] <- 21003
unk$WDPAID[unk$allnames == "Defensores del Chaco  "] <- 242
unk$WDPAID[unk$allnames == "Cerro Cor?<ed>  "] <- 246
unk$WDPAID[unk$allnames == "Paso Bravo  "] <- 317128
unk$WDPAID[unk$allnames == "Selous Game "] <- 1399
unk$WDPAID[unk$allnames == "Malawa  Sanctuary"] <- 7932
unk$WDPAID[unk$allnames == "Echuya  "] <- 40371
unk$WDPAID[unk$allnames == "Bukaleba  "] <- 40417
unk$WDPAID[unk$primarynam == "North Karamoja Controlled Hunting Area"] <- 1436
unk$WDPAID[unk$allnames == "Semliki Controlled  ; Toro-Semliki  "] <- 1440
unk$WDPAID[unk$allnames == "Bears Ears  Monument"] <- 555670017
unk$WDPAID[unk$allnames == "Yosemite  "] <- 975
unk$WDPAID[unk$allnames == "Joshua Tree  ; Josha Tree  Monument"] <- 370321
unk$WDPAID[unk$allnames == " Rainier  ; Rainier; Rainier Wilderness"] <- 372151
unk$WDPAID[unk$primarynam == "Hoang Lien National Park"] <- 10357
unk$WDPAID[unk$allnames == "  No.26"] <- 301750
unk$WDPAID[unk$allnames == "  No.55"] <- 301778

# create two different datasets, one with known WDPAIDs and one with unknown WDPAIDs
kn <- paddd_events[paddd_events$WDPAID != "unk",]
kn <- rbind(kn, unk[unk$WDPAID != "unk",])

unk <- unk[unk$WDPAID == "unk",]

## Create one record per year when a protected area was created/downsized/degazetted (unknown WDPAIDs)

# make variables numeric
unk$Size_Pre <- as.numeric(unk$Size_Pre)
unk$Size_Post <- as.numeric(unk$Size_Post)
unk$Areaaffect <- as.numeric(unk$Areaaffect)

# create a new dataframe
rec_unk <- data.frame(matrix(ncol = 7))
colnames(rec_unk) <- c("WDPAID", "NAME", "ISO3", "STATUS_YR", "REP_AREA", "EVENT_TYPE", "area_affected")

# create one record per year when a protected area was created/downsized/degazetted
for (i in unique(unk$primarynam)) {
  
  area <- unk[unk$primarynam == i, ]
  
  row_original <- c("unk", i, area$ISO3166[1], area$YearPAGaze[1], area$Size_Pre[1], "Gazettement", NA)
  
  rec_unk <- rbind(rec_unk, row_original)
  
  for (j in 1:nrow(area)) {
    
    if (!is.na(area$Size_Pre[1]) & !is.na(area$Areaaffect[j])) {
      
      area_ch <- area$Areaaffect[j] * (-1)
      
    } else {
      
      area_ch <- area$Size_Post[j] - area$Size_Pre[j]
    }
    new_row <- c("unk", i, area$ISO3166[1], area$YearPADDD[j], area_ch, area$EventType[j], area$Areaaffect[j])
    
    rec_unk <- rbind(rec_unk, new_row)
    
    if (area$Reversal[j] == "Y") {
      
      if (area$Rev_Area[j] != "unk") {
        
        area_ch <- area$Rev_Area[j]
        
      } else {
        
        if (area$Rev_Type[j] == "Full" & !is.na(area$Rev_Type[j])) {
          
          area_ch <- area$Size_Pre[j]
          
        } else {
          
          area_ch <- NA
        }
      }
      
      new_row <- c("unk", i, area$ISO3166[1], area$YR_Reverse[j], area_ch, "Reversal", NA)
      
      rec_unk <- rbind(rec_unk, new_row)
    }
    
    
  }
  
  
}


# make variables numeric
kn$Size_Pre <- as.numeric(kn$Size_Pre)
kn$Size_Post <- as.numeric(kn$Size_Post)
kn$Areaaffect <- as.numeric(kn$Areaaffect)

# create a new dataframe
rec_kn <- data.frame(matrix(ncol = 7))
colnames(rec_kn) <- c("WDPAID", "NAME", "REP_AREA", "STATUS_YR", "ISO3", "event_type", "area_affected")


## Create one record for each PA

# create one record by PADDD event
for (i in unique(kn$primarynam)) {
  
  area <- kn[kn$primarynam == i, ]
  
  if(!is.na(area$YearPAGaze[1]) & !is.na((datac$STATUS_YR[datac$WDPAID == area$WDPAID[1]])[1])) {
    
    # if the original year of enactment cited in the PADDD database is lower than that cited in the WDPA, replace the year by the PADDD database year
    if((datac$STATUS_YR[datac$WDPAID == area$WDPAID[1]])[1] > area$YearPAGaze[1]) {
      
      year <- area$YearPAGaze[1]
      
    } else {
      
      year <- datac$STATUS_YR[datac$WDPAID == area$WDPAID[1]][1]
      
      year <- year[1]
    }
  }
  
  if (is.na(area$Size_Pre[1])) {
    
    area_ch <- datac$REP_AREA[datac$WDPAID == area$WDPAID[1]][1]
    
  } else {
    
    area_ch <- area$Size_Pre[1]
    
  }
  
  if (is.null(area_ch)) {
    
    area_ch <- NA
  }
  
  new_row <- c(area$WDPAID[1], i, area_ch, year, area$ISO3166[1], "Gazettement", NA)
  
  rec_kn <- rbind(rec_kn, new_row)
  
  for (j in 1:nrow(area)) {
    
    if (!is.na(area$Size_Pre[1]) & !is.na(area$Areaaffect[j])) {
      
      area_ch <- area$Areaaffect[j] * (-1)
      
    } else {
      
      area_ch <- area$Size_Post[j] - area$Size_Pre[j]
    }
    
    new_row <- c(area$WDPAID[j], i, area_ch, area$YearPADDD[j], area$ISO3166[j], area$EventType[j], area$Areaaffect[j])
    
    rec_kn <- rbind(rec_kn, new_row)
    
    if (area$Reversal[j] == "Y") {
      
      if (area$Rev_Area[j] != "unk") {
        
        area_ch <- area$Rev_Area[j]
        
      } else {
        
        if (area$Rev_Type[j] == "Full" & !is.na(area$Rev_Type[j])) {
          
          area_ch <- area$Size_Pre[j]
        }
      }
      
      new_row <- c(area$WDPAID[j], i, area_ch, area$YR_Reverse[j], area$ISO3166[1], "Reversal", NA)
      
      rec_kn <- rbind(rec_kn, new_row)
    }
  }
}





# remove a record that does not match the WDPA database record
rec_kn <- rec_kn[rec_kn$WDPAID != "801", ]

# code the area "NA" if the gazettement year is 0 as it means the area was probably not established by the state

for (i in 1:nrow(rec_unk)) {
  
  if (any(rec_unk$STATUS_YR[rec_unk$WDPAID == rec_unk$WDPAID[i]] == "0" | is.na(rec_unk$STATUS_YR[rec_unk$WDPAID == rec_unk$WDPAID[i]]))) {
    
    rec_unk$REP_AREA[i] <- NA
  }
  
}

for (i in 1:nrow(rec_kn)) {
  
  if (any(rec_kn$STATUS_YR[rec_kn$WDPAID == rec_kn$WDPAID[i]] == "0" | is.na(rec_kn$STATUS_YR[rec_kn$WDPAID == rec_kn$WDPAID[i]]))) {
    
    rec_kn$REP_AREA[i] <- NA
  }
  
}


# save the objects
write.csv(rec_unk, "rec_unk.csv")
write.csv(rec_kn, "rec_kn.csv")

rec_unk <- read.csv("rec_unk.csv")
rec_kn <- read.csv("rec_kn.csv")




# merge the datasets
`%notin%` <- Negate(`%in%`)

all_rec <- datac[datac$WDPAID %notin% rec_kn$WDPAID, c(2, 3, 5, 14, 18, 24)]

colnames(all_rec)[6] <- "ISO3"

all_rec <- merge(all_rec, rec_unk, by = c("WDPAID", "NAME", "STATUS_YR", "ISO3", "REP_AREA"), all = T)

all_rec <- merge(all_rec, rec_kn, by = c("WDPAID", "NAME", "STATUS_YR", "ISO3", "REP_AREA"), all = T)






#### 3. Creation of a binary (policy adoption) outcome variable #### 


## Clean the data on PA surface in each country each year

# make the area variable numeric
all_rec$REP_AREA <- as.numeric(all_rec$REP_AREA)

# create a dataframe with the total area enacted by each country each year
pa_area <- aggregate(REP_AREA ~ ISO3 + STATUS_YR, data = all_rec, sum)

# remove the figures where STATUS_YR is 0
pa_area <- pa_area[pa_area$STATUS_YR != 0, ]

# make the dataset include each year for each country
all_years <- 1800:2022
all_yc <- expand.grid(ISO3 = unique(pa_area$ISO3), STATUS_YR = all_years)

pa_area <- merge(pa_area, all_yc, by = c("ISO3", "STATUS_YR"), all = T)

# change the column names
colnames(pa_area) <- c("country", "year", "area")

# assign 0 to all NAs in "area"
pa_area$area[is.na(pa_area$area)] <- 0

# transform the dataframe into a large dataset
pa_area <- pa_area %>% 
  pivot_wider(names_from = year, values_from = area)

# add up the numbers so that every cell indicates the total area protected in force during that year
for (i in 2:(length(colnames(pa_area))-1)) {
  pa_area[,(i+1)] <- pa_area[,i] + pa_area[,(i+1)]
}

# transform the dataset into a long dataset
pa_area <- pa_area %>% 
  pivot_longer(cols = !country, names_to = "year", values_to = "pa_area")

# keep years 1961 to 2022
pa_area$year <- as.numeric(pa_area$year)
pa_area <- pa_area[pa_area$year %in% 1961:2022,]

# add year-country dyads for the countries that exist but are not in the dataset
pa_area <- merge(pa_area, country_ex, by = c("country", "year"), all.y = T)

# assign 0 to all NAs in pa_area
pa_area$pa_area[is.na(pa_area$pa_area)] <- 0


## Create a dataset with a binary outcome variable

pa_bin <- pa_area

# code the outcome variable as binary
pa_bin$pa_binary <- pa_bin$pa_area

pa_bin$pa_binary <- ifelse(pa_bin$pa_binary >0, 1, 0)

# make the dataframe wide again
pa_bin <- pa_bin[, -3]

# put ABW at the end to have all the years when pivoting
abw <- pa_bin[pa_bin$country == "ABW", ]
pa_bin <- pa_bin[pa_bin$country != "ABW", ]
pa_bin <- rbind(pa_bin, abw)

# pivot dataset
large_pa_binary <- pivot_wider(pa_bin, names_from = year, values_from = pa_binary, values_fn = mean)

large_pa_binary <- as.data.frame(large_pa_binary)

# replace NAs by 0s
for (i in 1:ncol(large_pa_binary)) {
  
  large_pa_binary[is.na(large_pa_binary[i]), i] <- "0"
}


# attribute row names
large_pa_binary <- as.data.frame(large_pa_binary)
rownames(large_pa_binary) <- large_pa_binary$country

large_pa_binary <- large_pa_binary[,-1]


# save the object
write.csv(large_pa_binary, "large_pa_binary.csv")


#### 4. Creation of a continuous (surface share of PA compared to country area) outcome variable ####

## Clean the data on land use

# create a dataframe with all years and countries
all_fyears <- 1961:2020

all_yc <- expand.grid(country = unique(datac$ISO3), year = all_fyears)

# load data
fao <- read.csv("Land use FAO data.csv")

# remove countries which do not exist anymore
fao <- fao[fao$Area != "Yugoslav SFR",]
fao <- fao[fao$Area != "Pacific Islands Trust Territory",]
fao <- fao[fao$Area != "Serbia and Montenegro",]
fao <- fao[fao$Area != "Türkiye",]
fao <- fao[fao$Area != "Netherlands Antilles (former)",]
fao <- fao[fao$Area != "Czechoslovakia",]
fao <- fao[fao$Area != "Belgium-Luxembourg",]
fao <- fao[fao$Area != "Channel Islands",]

# create a data frame with each country's surface
country_area <- data.frame()
country_area <- expand.grid(country = unique(fao$Area), year = unique(fao$Year))

# subset fao data to keep only the variable of interest
sub <- subset(fao, Item == "Country area", select = c(Area, Year, Value))

# add the area of Greenland to that of Denmark as it is a territory of the country
for (y in unique(sub$Year)) {
  
  dnk <- sub$Value[sub$Area == "Denmark" & sub$Year == y]
  gld <- sub$Value[sub$Area == "Greenland" & sub$Year == y]
  
  sub$Value[sub$Area == "Denmark" & sub$Year == y] <- dnk + gld
}

# merge the two data frames
country_area <- merge(country_area, sub, by.x = c("country", "year"), by.y = c("Area", "Year"), all.x = TRUE)

# make country names ISO3 character codes
country_area$country <- countrycode(country_area$country, origin = "country.name", destination = "iso3c", origin_regex = T)


## Merge the datasets on PA area and land use

# merge
pa_area <- merge(pa_area, country_area, by = c("country", "year"), all = F)

# change column names
colnames(pa_area)[4] <- "country_area"


## Compute the outcome variable: proportion of land that is protected

# make the variables numeric
pa_area$country_area <- as.numeric(pa_area$country_area)
pa_area$pa_area <- as.numeric(pa_area$pa_area)

# convert thousand hectares to square kilometres (FAO data)
pa_area$country_area <- pa_area$country_area * 10

# compute the outcome variable
pa_area$area_protected_prop <- pa_area$pa_area / pa_area$country_area

# remove the rows where the value is NA
pa_area <- pa_area[!is.na(pa_area$area_protected_prop),]

# look at countries with values below 0 or over 1
unique(pa_area$country[pa_area$area_protected_prop >1])
unique(pa_area$country[pa_area$area_protected_prop <0])

# remove the countries with ratio of protected areas higher than 1
pa_area <- pa_area[pa_area$country != "VCT",]
pa_area <- pa_area[pa_area$country != "DNK",]

# make it a wide data frame again 
large_pa_area <- pa_area[, -c(3:4)]

# but "ABW" at the end so as to have all the years when pivoting the data frame
abw <- large_pa_area[large_pa_area$country == "ABW",]
large_pa_area <- large_pa_area[large_pa_area$country != "ABW",]
large_pa_area <- rbind(large_pa_area, abw)

# pivot the data frame to a wider data frame
large_pa_area <- pivot_wider(large_pa_area, names_from = year, values_from = area_protected_prop, values_fn = mean)

large_pa_area <- as.data.frame(large_pa_area)

# make NAs 0s
for (i in 1:ncol(large_pa_area)) {
  
  large_pa_area[is.na(large_pa_area[i]), i] <- 0
}

# make "Inf" 0s
for (i in 1:ncol(large_pa_area)) {
  
  large_pa_area[!is.na(large_pa_area[i]) & large_pa_area[i] == Inf, i] <- 0
}

# assign row names
rownames(large_pa_area) <- large_pa_area$country
large_pa_area <- large_pa_area[, -1]

# save the objects
write.csv(large_pa_area, "large_pa_area.csv")



#### 5. Creation of an additional outcome variable (percentage point change in area proportion) ####

## Percentage point change in area proportion

# transform the dataframe into a large dataset
large_pa_area_pp <- pa_area[, -c(3, 4)]

# make area column numeric
large_pa_area_pp$area_protected_prop <- as.numeric(large_pa_area_pp$area_protected_prop)

# put "ABW" at the end so as to have all the years when pivoting the data frame
abw <- large_pa_area_pp[large_pa_area_pp$country == "ABW",]
large_pa_area_pp <- large_pa_area_pp[large_pa_area_pp$country != "ABW",]
large_pa_area_pp <- rbind(large_pa_area_pp, abw)


# make the data frame wide
large_pa_area_pp <- large_pa_area_pp %>% 
  pivot_wider(names_from = year, values_from = area_protected_prop, values_fn = mean)

# make it so that the outcome variable represents the percentage change between one year and the other
for (i in (length(colnames(large_pa_area_pp))-1):2) {
  large_pa_area_pp[,(i+1)] <- large_pa_area_pp[,(i+1)] - large_pa_area_pp[,i]
}


# make NAs 0s
for (i in 2:ncol(large_pa_area_pp)) {
  
  large_pa_area_pp[is.na(large_pa_area_pp[i]), i] <- 0
}

# make "Inf" 0s
for (i in 2:ncol(large_pa_area_pp)) {
  
  large_pa_area_pp[!is.na(large_pa_area_pp[i]) & large_pa_area_pp[i] == Inf, i] <- 0
}

# make 1961 values 0
large_pa_area_pp$`1961` <- 0

# assign row names
large_pa_area_pp <- as.data.frame(large_pa_area_pp)
rownames(large_pa_area_pp) <- large_pa_area_pp$country
large_pa_area_pp <- large_pa_area_pp[, -1]

# save the objects
write.csv(large_pa_area_pp, "large_pa_area_pp.csv")





#### 6. Create a long data set with all the values for each country-year dyad ####

# create a "country" column
large_pa_binary$country <- rownames(large_pa_binary)
large_pa_area$country <- rownames(large_pa_area)
large_pa_area_pp$country <- rownames(large_pa_area_pp)

# create a long version of each dataset
long_pa_area_b <- large_pa_binary %>% 
  pivot_longer(cols = -country, names_to = "year", values_to = "pa_b")

long_pa_area_p <- large_pa_area %>% 
  pivot_longer(cols = -country, names_to = "year", values_to = "pa_p")

long_pa_area_pc <- large_pa_area_pc %>% 
  pivot_longer(cols = -country, names_to = "year", values_to = "pa_pc")

long_pa_area_pp <- large_pa_area_pp %>% 
  pivot_longer(cols = -country, names_to = "year", values_to = "pa_pp")


# merge datasets
long_pa <- merge(long_pa_area_b, long_pa_area_p, by = c("year", "country"))
long_pa <- merge(long_pa, long_pa_area_pp, by = c("year", "country"))

# save the dataset
write.csv(long_pa, "long_pa.csv")


