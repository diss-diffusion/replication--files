#### Controls ####

#### GDP per capita ####

# load data
gdppc <- read.csv("./WB GDPpc.csv")

# remove useless variables
gdppc <- gdppc[, -c(1, 3, 4)]

# make it a long dataframe
gdppc <- gdppc %>% pivot_longer(cols = !Country.Code, names_to = "year", values_to = "gdppc")

# transform the year column into years
gdppc$year <- gsub("X", "", gdppc$year)

# make year column numeric
gdppc$year <- as.numeric(gdppc$year)

# lag variable
gdppc$year <- gdppc$year + 1

# change column names
colnames(gdppc)[1] <- "country"


#### GDP growth ####

# load data
gdpg <- read.csv("./WB GDP growth.csv")

# remove useless variables
gdpg <- gdpg[, -c(1, 3, 4)]

# make it a long data frame
gdpg <- gdpg %>% pivot_longer(cols = !Country.Code, names_to = "year", values_to = "gdpg")

# transform the year column into years
gdpg$year <- gsub("X", "", gdpg$year)

# make year column numeric
gdpg$year <- as.numeric(gdpg$year)

# lag variable
gdpg$year <- gdpg$year + 1

# change column names
colnames(gdpg)[1] <- "country"




#### Trade openness ####

# load data
tradeo <- read.csv("WB trade openness.csv")

# remove useless variables
tradeo <- tradeo[, -c(1, 3, 4)]

# make it a long dataframe
tradeo <- tradeo %>% pivot_longer(cols = !Country.Code, names_to = "year", values_to = "tradeo")

# transform the year column into years
tradeo$year <- gsub("X", "", tradeo$year)

# make year column numeric
tradeo$year <- as.numeric(tradeo$year)

# lag variable
tradeo$year <- tradeo$year + 1

# change column names
colnames(tradeo)[1] <- "country"


#### FDI as share of GDP ####

# load data
fdi <- read.csv("WB FDI USD.csv")
gdp <- read.csv("WB GDP USD.csv")

# remove useless variables
fdi <- fdi[, -c(1, 3, 4)]
gdp <- gdp[, -c(1, 3, 4)]

# make them long data frames
fdi <- fdi %>% pivot_longer(cols = !Country.Code, names_to = "year", values_to = "fdi")
gdp <- gdp %>% pivot_longer(cols = !Country.Code, names_to = "year", values_to = "gdp")

# transform the year column into years
fdi$year <- gsub("X", "", fdi$year)
gdp$year <- gsub("X", "", gdp$year)

# make year column numeric
fdi$year <- as.numeric(fdi$year)
gdp$year <- as.numeric(gdp$year)

# change column names
colnames(fdi)[1] <- "country"
colnames(gdp)[1] <- "country"

# merge
fdiprop <- merge(fdi, gdp, by = c("country", "year"), all = F)

# compute FDI as proportion of GDP
fdiprop$fdiprop <- fdiprop$fdi / fdiprop$gdp

# remove useless columns
fdiprop <- fdiprop[, c(1:2, 5)]

# lag variable
fdiprop$year <- fdiprop$year + 1


#### ODA as share of GDP ####

# load data
oda <- read.csv("WB ODA received USD.csv")

# remove useless variables
oda <- oda[, -c(1, 3, 4)]

# make them long data frames
oda <- oda %>% pivot_longer(cols = !Country.Code, names_to = "year", values_to = "oda")

# transform the year column into years
oda$year <- gsub("X", "", oda$year)

# make year column numeric
oda$year <- as.numeric(oda$year)

# change column names
colnames(oda)[1] <- "country"

# merge
odaprop <- merge(oda, gdp, by = c("country", "year"), all = F)

# make variables numeric
odaprop$oda <- as.numeric(odaprop$oda)
odaprop$gdp <- as.numeric(odaprop$gdp)

# compute ODA as proportion of GDP
odaprop$odaprop <- odaprop$oda / odaprop$gdp

# remove useless columns
odaprop <- odaprop[, c(1:2, 5)]

# lag variable
odaprop$year <- odaprop$year + 1


#### Natural resource rents as percentage of GDP ####

# load data
nrr <- read.csv("WB natural resource rents percentage of GDP.csv")

# remove useless variables
nrr <- nrr[, -c(1, 3, 4)]

# make it a long dataframe
nrr <- nrr %>% pivot_longer(cols = !Country.Code, names_to = "year", values_to = "nrr")

# transform the year column into years
nrr$year <- gsub("X", "", nrr$year)

# make year column numeric
nrr$year <- as.numeric(nrr$year)

# change column names
colnames(nrr)[1] <- "country"

# lag variable
nrr$year <- nrr$year + 1


#### Population density ####

# load data
popd <- read.csv("WB population density.csv")

# remove useless variables
popd <- popd[, -c(1, 3, 4)]

# make it a long dataframe
popd <- popd %>% pivot_longer(cols = !Country.Code, names_to = "year", values_to = "popd")

# transform the year column into years
popd$year <- gsub("X", "", popd$year)

# make year column numeric
popd$year <- as.numeric(popd$year)

# change column names
colnames(popd)[1] <- "country"

# lag variable
popd$year <- popd$year + 1


#### Freedom House Index Political Rights ####

# load data
fhipr <- read.csv("FHI scores PR.csv")

# rename country column
colnames(fhipr)[1] <- "country"

# remove blank countries
fhipr <- fhipr[fhipr$country != "", ]

# remove countries which do not exist anymore
fhipr <- fhipr[fhipr$country != "Czechoslovakia", ]
fhipr <- fhipr[fhipr$country != "Micronesia", ]
fhipr <- fhipr[fhipr$country != "Serbia and Montenegro", ]
fhipr <- fhipr[fhipr$country != "Yugoslavia", ]
fhipr <- fhipr[fhipr$country != "Vietnam, N.", ]
fhipr <- fhipr[fhipr$country != "Vietnam, S.", ]
fhipr <- fhipr[fhipr$country != "Yemen, N.", ]
fhipr <- fhipr[fhipr$country != "Yemen, S.", ]
fhipr <- fhipr[fhipr$country != "Germany, E. ", ]

# make a column character
fhipr$X1982 <- as.character(fhipr$X1982)

# make it a long dataframe
fhipr <- fhipr %>% pivot_longer(cols = !country, names_to = "year", values_to = "fhipr")

# transform the year column into years
fhipr$year <- gsub("X", "", fhipr$year)

# make year column numeric
fhipr$year <- as.numeric(fhipr$year)

# keep West Germany until 1989 + Germany from 1990
fhipr <- fhipr[fhipr$country != "Germany, W. " | (fhipr$country == "Germany, W. " & fhipr$year < 1990), ]
fhipr <- fhipr[fhipr$country != "Germany" | (fhipr$country == "Germany" & fhipr$year >= 1990), ]

# keep the USSR until 1990 + Russia from 1991
fhipr <- fhipr[fhipr$country != "USSR" | (fhipr$country == "USSR" & fhipr$year < 1991), ]
fhipr <- fhipr[fhipr$country != "Russia" | (fhipr$country == "Russia" & fhipr$year > 1990), ]

# change "country" variable from country names to country code
fhipr$country <- countrycode(fhipr$country, origin = "country.name", destination = "iso3c",
                             origin_regex = T)

# remove rows for which the countrycode function returned "NA"
fhipr <- fhipr[!is.na(fhipr$country), ]

# lag variable
fhipr$year <- fhipr$year + 1

# make variable numeric
fhipr$fhipr <- as.numeric(fhipr$fhipr)


