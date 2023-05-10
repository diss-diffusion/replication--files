#### Load data ####

# pa data
paa <- read.csv("large_pa_area.csv", row.names=1)
pab <- read.csv("large_pa_binary.csv", row.names=1)
long_pa <- read.csv("long_pa.csv", row.names=1)

paapc <- read.csv("large_pa_area_pc.csv", row.names=1)
paapp <- read.csv("large_pa_area_pp.csv", row.names=1)

# remove the "X" in front of the column names in paa, pab, paapc and paapp
for (i in 1:ncol(paa)){
  colnames(paa)[i] <- gsub('X', '', colnames(paa)[i])
}

for (i in 1:ncol(pab)){
  colnames(pab)[i] <- gsub('X', '', colnames(pab)[i])
}

for (i in 1:ncol(paapc)){
  colnames(paapc)[i] <- gsub('X', '', colnames(paapc)[i])
}

for (i in 1:ncol(paapp)){
  colnames(paapp)[i] <- gsub('X', '', colnames(paapp)[i])
}


# distance matrices
for (y in 1960:2016) {
  
  # load the matrices
  matb <- read.csv(paste0("dist_adj", y, ".csv"), row.names=1)

  # row-standardise the matrices
  matb <- matb/rowSums(matb, na.rm = T)

  # assign to object
  assign(paste0("matb", y), matb, env = .GlobalEnv)

}

# religion matrices
for (y in 1943:2012) {
  
  # load the matrices
  rely <- read.csv(paste0("rel_", y, ".csv"), row.names=1)
  
  # row-standardise the matrices
  rely <- rely/rowSums(rely, na.rm = T)
  
  # assign to object
  assign(paste0("rel_", y), rely, env = .GlobalEnv)
}

# trade equivalence matrices
for (y in 1962:2020) {
  
  cory <- read.csv(paste0("cor_", y, ".csv"), row.names=1)
  
  # row-standardise the matrices
  cory <- cory/rowSums(cory, na.rm = T)
  
  assign(paste0("cor_", y), cory, env = .GlobalEnv)
}


# trade equivalence matrices (imputed)
for (y in 1963:2020) {
  
  cory <- read.csv(paste0("cor_", y, "i.csv"), row.names=1)
  
  # row-standardise the matrices
  cory <- cory/rowSums(cory, na.rm = T)
  
  assign(paste0("cor_", y, "i"), cory, env = .GlobalEnv)
}


# trade equivalence matrices (limited influences)
for (y in 1963:2019) {
  
  cory <- read.csv(paste0("cor_", y, "_20.csv"), row.names=1)
  
  # row-standardise the matrices
  cory <- cory/rowSums(cory, na.rm = T)
  
  assign(paste0("cor_", y, "_20"), cory, env = .GlobalEnv)
}


# igo matrices
for (y in 1960:2014) {
  
  igo_adj <- read.csv(paste0("igo_adj", y, ".csv"), row.names=1)
  
  # row-standardise the matrices
  igo_adj <- igo_adj/rowSums(igo_adj, na.rm = T)
  
  assign(paste0("igo_adj", y), igo_adj, env = .GlobalEnv)
}


# igo matrices (limited influences)
for (y in 1960:2014) {
  
  igo_adj <- read.csv(paste0("igo_adj", y, "_20.csv"), row.names=1)
  
  # row-standardise the matrices
  igo_adj <- igo_adj/rowSums(igo_adj, na.rm = T)
  
  assign(paste0("igo_adj", y, "_20"), igo_adj, env = .GlobalEnv)
}


# environmental igo matrices
for (y in 1960:2014) {
  
  env_igo_adj <- read.csv(paste0("env_igo_adj", y, ".csv"), row.names=1)
  
  # row-standardise the matrices
  env_igo_adj <- env_igo_adj/rowSums(env_igo_adj, na.rm = T)
  
  assign(paste0("env_igo_adj", y), env_igo_adj, env = .GlobalEnv)
}


# environmental igo matrices (limited influences)
for (y in 1960:2014) {
  
  env_igo_adj <- read.csv(paste0("env_igo_adj", y, "_20.csv"), row.names=1)
  
  # row-standardise the matrices
  env_igo_adj <- env_igo_adj/rowSums(env_igo_adj, na.rm = T)
  
  assign(paste0("env_igo_adj", y, "_20"), env_igo_adj, env = .GlobalEnv)
}



# colonial past matrices
for(y in 1948:2021) {
  
  col_adj <- read.csv(paste0("coladj", y, ".csv"), row.names = 1)
  
  # row-standardise the matrices
  col_adj <- col_adj/rowSums(col_adj, na.rm = T)
  
  assign(paste0("coladj", y), col_adj, env = .GlobalEnv)
  
}

# language matrices
for (y in 1948:2021) {
  
  lg_adj <- read.csv(paste0("lg_adj", y, ".csv"), row.names=1)
  
  # row-standardise the matrices
  lg_adj <- lg_adj/rowSums(lg_adj, na.rm = T)
  
  assign(paste0("lg_adj", y), lg_adj, env = .GlobalEnv)
}

# legal system matrices
for (y in 1948:2021) {
  
  ls_adj <- read.csv(paste0("ls_adj", y, ".csv"), row.names=1)
  
  # row-standardise the matrices
  ls_adj <- ls_adj/rowSums(ls_adj, na.rm = T)
  
  assign(paste0("ls_adj", y), ls_adj, env = .GlobalEnv)
}

# country existence data
country_ex <- read.csv("country_ex.csv", row.names=1)


#### PA data matrices for the construction of the lags ####

# replace missing values by 0s
paa_m <- as.data.frame(paa)

for (i in 1:ncol(paa_m)) {
  
  paa_m[is.na(paa_m[i]),i] <- 0
  
}

# turn paa into a matrix
paa_m <- as.matrix(paa_m)
paa_m <- as.numeric(paa_m)
paa_m <- matrix(paa_m, ncol = ncol(paa))

# column + row names
colnames(paa_m) <- colnames(paa)
rownames(paa_m) <- rownames(paa)

# replace missing values by 0s
pab_m <- as.data.frame(pab)

for (i in 1:ncol(pab_m)) {
  
  pab_m[is.na(pab_m[i]),i] <- 0
  
}

# turn pab into a matrix
pab_m <- as.data.frame(pab)
pab_m <- as.matrix(pab_m)
pab_m <- as.numeric(pab_m)
pab_m <- matrix(pab_m, ncol = ncol(pab))

# column + row names
colnames(pab_m) <- colnames(pab)
rownames(pab_m) <- rownames(pab)


# replace missing values by 0s
paapp_m <- as.data.frame(paapp)

for (i in 1:ncol(paapp_m)) {
  
  paapp_m[is.na(paapp_m[i]),i] <- 0
  
}

# turn paa into a matrix
paapp_m <- as.matrix(paapp_m)
paapp_m <- as.numeric(paapp_m)
paapp_m <- matrix(paapp_m, ncol = ncol(paapp))

# column + row names
colnames(paapp_m) <- colnames(paapp)
rownames(paapp_m) <- rownames(paapp)


# replace missing values by 0s
paapc_m <- as.data.frame(paapc)

for (i in 1:ncol(paapc_m)) {
  
  paapc_m[is.na(paapc_m[i]),i] <- 0
  
}

# turn paa into a matrix
paapc_m <- as.matrix(paapc_m)
paapc_m <- as.numeric(paapc_m)
paapc_m <- matrix(paapc_m, ncol = ncol(paapc))

# column + row names
colnames(paapc_m) <- colnames(paapc)
rownames(paapc_m) <- rownames(paapc)



#### Cultural variables ####

#### Religion lag ####


## Binary outcome variable ##

yrel <- 1961:2012

for(y in yrel) {
  rely <- get(paste0("rel_", y))
  
  # turn into a numeric matrix
  rely_m <- as.matrix(rely)
  rely_m <- as.numeric(rely_m)
  rely_m <- matrix(rely_m, ncol = length(colnames(rely)))
  
  # assign column + row names
  colnames(rely_m) <- rownames(rely)
  rownames(rely_m) <- colnames(rely_m)
  
  # subset pa to keep only the rows corresponding to the countries listed in the distance matrix
  pab_mc <- subset(pab_m, rownames(pab_m) %in% rownames(rely_m))
  
  # subset distance matrix to keep only the rows corresponding to the countries listed in pa
  rely_m <- subset(rely_m, rownames(rely_m) %in% rownames(pab_mc))
  rely_m <- rely_m[, colnames(rely_m) %in% rownames(pab_mc)]
  
  # subset pa to keep only the year of interest
  pab_mc <- pab_mc[, paste0(y)]
  
  # replace missing values by zeros to be able to perform the matrix multiplication
  pab_mc[is.na(pab_mc)] <- 0
  
  # matrix multiplication - building the spatial lag
  rellagb <- as.data.frame(rely_m %*% pab_mc)
  
  # add column for country
  rellagb$country <- rownames(rellagb)
  
  # add column for year
  rellagb$year <- y
  
  # assign to object
  assign(paste0("pa_rellagb", y), rellagb, env = .GlobalEnv)
  
}

rlistb <- list()

# create a list of data frames to combine
for (i in 1:length(yrel)) {
  
  r <- get(paste0("pa_rellagb", yrel[i]))
  
  rlistb[[i]] <- r
  
  assign("rlistb", rlistb, env = .GlobalEnv)
}

# combine the datasets into a long dataset
long_pa_rellagb <- do.call(rbind, rlistb)

# display each country for each year
all_ryears <- 1961:2012
all_rycb <- expand.grid(country = unique(long_pa_rellagb$country), year = all_ryears)

long_pa_rellagb <- merge(long_pa_rellagb, all_rycb, by.x = c("year", "country"), by.y = c("year", "country"), all = T)

long_pa_rellagb <- as.data.frame(long_pa_rellagb)

# create column for the values lagged by a year
long_pa_rellagb$ylag <- long_pa_rellagb$value

# make year variable numeric
long_pa_rellagb$year <- as.numeric(long_pa_rellagb$year)

# lag the values
for (i in 1:length(long_pa_rellagb$country)) {
  if(long_pa_rellagb$year[i] == min(all_ryears)) {
    long_pa_rellagb$ylag[i] <- NA
  } else {
    long_pa_rellagb$ylag[i] <- long_pa_rellagb$V1[long_pa_rellagb$country == long_pa_rellagb$country[i] & long_pa_rellagb$year == ((long_pa_rellagb$year[i]) - 1)] }
}

# change column names
colnames(long_pa_rellagb)[3:4] <- c("rel_lag", "time_rel_lag")

# save the object
write.csv(long_pa_rellagb, "long_pa_rellagb.csv")

# check for missing values
long_pa_rellagb_ex <- merge(long_pa_rellagb, country_ex, by =  c("year", "country"), all.y = T)

table(is.na(long_pa_rellagb_ex$spatial_lag))

# save the object
write.csv(long_pa_rellagb_ex, "long_pa_rellagb_ex.csv")


## Proportion of PA outcome variable ##
yrel <- 1961:2012

for(y in yrel) {
  rely <- get(paste0("rel_", y))
  
  # turn into a numeric matrix
  rely_m <- as.matrix(rely)
  rely_m <- as.numeric(rely_m)
  rely_m <- matrix(rely_m, ncol = length(colnames(rely)))
  
  # assign column + row names
  colnames(rely_m) <- rownames(rely)
  rownames(rely_m) <- colnames(rely_m)
  
  # subset pa to keep only the rows corresponding to the countries listed in the distance matrix
  paa_mc <- subset(paa_m, rownames(paa_m) %in% rownames(rely_m))
  
  # subset distance matrix to keep only the rows corresponding to the countries listed in pa
  rely_m <- subset(rely_m, rownames(rely_m) %in% rownames(paa_mc))
  rely_m <- rely_m[, colnames(rely_m) %in% rownames(paa_mc)]
  
  # subset pa to keep only the year of interest
  paa_mc <- paa_mc[, paste0(y)]
  
  # replace missing values by zeros to be able to perform the matrix multiplication
  paa_mc[is.na(paa_mc)] <- 0
  
  # matrix multiplication - building the spatial lag
  rellag <- as.data.frame(rely_m %*% paa_mc)
  
  # add column for country
  rellag$country <- rownames(rellag)
  
  # add column for year
  rellag$year <- y
  
  # assign to object
  assign(paste0("pa_rellag", y), rellag, env = .GlobalEnv)
  
}

rlist <- list()

# create a list of data frames to combine
for (i in 1:length(yrel)) {
  
  r <- get(paste0("pa_rellag", yrel[i]))
  
  rlist[[i]] <- r
  
  assign("rlist", rlist, env = .GlobalEnv)
}

# combine the datasets into a long dataset
long_pa_rellag <- do.call(rbind, rlist)

# display each country for each year
all_ryears <- 1961:2012
all_ryc <- expand.grid(country = unique(long_pa_rellag$country), year = all_ryears)

long_pa_rellag <- merge(long_pa_rellag, all_ryc, by.x = c("year", "country"), by.y = c("year", "country"), all = T)

long_pa_rellag <- as.data.frame(long_pa_rellag)

# create column for the values lagged by a year
long_pa_rellag$ylag <- long_pa_rellag$value

# make year variable numeric
long_pa_rellag$year <- as.numeric(long_pa_rellag$year)

# lag the values
for (i in 1:length(long_pa_rellag$country)) {
  if(long_pa_rellag$year[i] == min(all_ryears)) {
    long_pa_rellag$ylag[i] <- NA
  } else {
    long_pa_rellag$ylag[i] <- long_pa_rellag$V1[long_pa_rellag$country == long_pa_rellag$country[i] & long_pa_rellag$year == ((long_pa_rellag$year[i]) - 1)] }
}

# change column names
colnames(long_pa_rellag)[3:4] <- c("rel_lag", "time_rel_lag")

# save the object
write.csv(long_pa_rellag, "long_pa_rellag.csv")

# check for missing values
long_pa_rellag_ex <- merge(long_pa_rellag, country_ex, by =  c("year", "country"), all.y = T)

table(is.na(long_pa_rellag_ex$rel_lag))

# save the object
write.csv(long_pa_rellag_ex, "long_pa_rellag_ex.csv")

## Percentage change in area protected outcome variable ##
yrel <- 1961:2012

for(y in yrel) {
  rely <- get(paste0("rel_", y))
  
  # turn into a numeric matrix
  rely_m <- as.matrix(rely)
  rely_m <- as.numeric(rely_m)
  rely_m <- matrix(rely_m, ncol = length(colnames(rely)))
  
  # assign column + row names
  colnames(rely_m) <- rownames(rely)
  rownames(rely_m) <- colnames(rely_m)
  
  # subset pa to keep only the rows corresponding to the countries listed in the distance matrix
  paa_mc <- subset(paapc_m, rownames(paapc_m) %in% rownames(rely_m))
  
  # subset distance matrix to keep only the rows corresponding to the countries listed in pa
  rely_m <- subset(rely_m, rownames(rely_m) %in% rownames(paa_mc))
  rely_m <- rely_m[, colnames(rely_m) %in% rownames(paa_mc)]
  
  # subset pa to keep only the year of interest
  paa_mc <- paa_mc[, paste0(y)]
  
  # replace missing values by zeros to be able to perform the matrix multiplication
  paa_mc[is.na(paa_mc)] <- 0
  
  # matrix multiplication - building the spatial lag
  rellag <- as.data.frame(rely_m %*% paa_mc)
  
  # add column for country
  rellag$country <- rownames(rellag)
  
  # add column for year
  rellag$year <- y
  
  # assign to object
  assign(paste0("papc_rellag", y), rellag, env = .GlobalEnv)
  
}

rlist <- list()

# create a list of data frames to combine
for (i in 1:length(yrel)) {
  
  r <- get(paste0("papc_rellag", yrel[i]))
  
  rlist[[i]] <- r
  
  assign("rlist", rlist, env = .GlobalEnv)
}

# combine the datasets into a long dataset
long_papc_rellag <- do.call(rbind, rlist)

# display each country for each year
all_ryears <- 1961:2012
all_ryc <- expand.grid(country = unique(long_papc_rellag$country), year = all_ryears)

long_papc_rellag <- merge(long_papc_rellag, all_ryc, by.x = c("year", "country"), by.y = c("year", "country"), all = T)

long_papc_rellag <- as.data.frame(long_papc_rellag)

# create column for the values lagged by a year
long_papc_rellag$ylag <- long_papc_rellag$value

# make year variable numeric
long_papc_rellag$year <- as.numeric(long_papc_rellag$year)

# lag the values
for (i in 1:length(long_papc_rellag$country)) {
  if(long_papc_rellag$year[i] == min(all_ryears)) {
    long_papc_rellag$ylag[i] <- NA
  } else {
    long_papc_rellag$ylag[i] <- long_papc_rellag$V1[long_papc_rellag$country == long_papc_rellag$country[i] & long_papc_rellag$year == ((long_papc_rellag$year[i]) - 1)] }
}

# change column names
colnames(long_papc_rellag)[3:4] <- c("rel_lag_pc", "time_rel_lag_pc")

# save the object
write.csv(long_papc_rellag, "long_papc_rellag.csv")

# check for missing values
long_papc_rellag_ex <- merge(long_papc_rellag, country_ex, by =  c("year", "country"), all.y = T)

table(is.na(long_papc_rellag_ex$rel_lag_pc))

# save the object
write.csv(long_papc_rellag_ex, "long_papc_rellag_ex.csv")


## Percentage point change in area protected outcome variable ##
yrel <- 1961:2012

for(y in yrel) {
  rely <- get(paste0("rel_", y))
  
  # turn into a numeric matrix
  rely_m <- as.matrix(rely)
  rely_m <- as.numeric(rely_m)
  rely_m <- matrix(rely_m, ncol = length(colnames(rely)))
  
  # assign column + row names
  colnames(rely_m) <- rownames(rely)
  rownames(rely_m) <- colnames(rely_m)
  
  # subset pa to keep only the rows corresponding to the countries listed in the distance matrix
  paa_mc <- subset(paapp_m, rownames(paapp_m) %in% rownames(rely_m))
  
  # subset distance matrix to keep only the rows corresponding to the countries listed in pa
  rely_m <- subset(rely_m, rownames(rely_m) %in% rownames(paa_mc))
  rely_m <- rely_m[, colnames(rely_m) %in% rownames(paa_mc)]
  
  # subset pa to keep only the year of interest
  paa_mc <- paa_mc[, paste0(y)]
  
  # replace missing values by zeros to be able to perform the matrix multiplication
  paa_mc[is.na(paa_mc)] <- 0
  
  # matrix multiplication - building the spatial lag
  rellag <- as.data.frame(rely_m %*% paa_mc)
  
  # add column for country
  rellag$country <- rownames(rellag)
  
  # add column for year
  rellag$year <- y
  
  # assign to object
  assign(paste0("papp_rellag", y), rellag, env = .GlobalEnv)
  
}

rlist <- list()

# create a list of data frames to combine
for (i in 1:length(yrel)) {
  
  r <- get(paste0("papp_rellag", yrel[i]))
  
  rlist[[i]] <- r
  
  assign("rlist", rlist, env = .GlobalEnv)
}

# combine the datasets into a long dataset
long_papp_rellag <- do.call(rbind, rlist)

# display each country for each year
all_ryears <- 1961:2012
all_ryc <- expand.grid(country = unique(long_papp_rellag$country), year = all_ryears)

long_papp_rellag <- merge(long_papp_rellag, all_ryc, by.x = c("year", "country"), by.y = c("year", "country"), all = T)

long_papp_rellag <- as.data.frame(long_papp_rellag)

# create column for the values lagged by a year
long_papp_rellag$ylag <- long_papp_rellag$value

# make year variable numeric
long_papp_rellag$year <- as.numeric(long_papp_rellag$year)

# lag the values
for (i in 1:length(long_papp_rellag$country)) {
  if(long_papp_rellag$year[i] == min(all_ryears)) {
    long_papp_rellag$ylag[i] <- NA
  } else {
    long_papp_rellag$ylag[i] <- long_papp_rellag$V1[long_papp_rellag$country == long_papp_rellag$country[i] & long_papp_rellag$year == ((long_papp_rellag$year[i]) - 1)] }
}

# change column names
colnames(long_papp_rellag)[3:4] <- c("rel_lag_pp", "time_rel_lag_pp")

# save the object
write.csv(long_papp_rellag, "long_papp_rellag.csv")

# check for missing values
long_papp_rellag_ex <- merge(long_papp_rellag, country_ex, by =  c("year", "country"), all.y = T)

table(is.na(long_papp_rellag_ex$rel_lag_pp))

# save the object
write.csv(long_papp_rellag_ex, "long_papp_rellag_ex.csv")



#### Language lag ####

## Binary outcome variable ##

all_gyears <- 1961:2020

for(y in all_gyears) {
  matlg <- get(paste0("lg_adj", y))
  
  # turn into a numeric matrix
  matlg_m <- as.matrix(matlg)
  matlg_m <- as.numeric(matlg_m)
  matlg_m <- matrix(matlg_m, ncol = length(colnames(matlg)))
  
  # assign column + row names
  colnames(matlg_m) <- rownames(matlg)
  rownames(matlg_m) <- colnames(matlg_m)
  
  # subset pa to keep only the rows corresponding to the countries listed in the distance matrix
  pab_mc <- as.data.frame(pab_m)
  pab_mc <- subset(pab_m, rownames(pab_m) %in% rownames(matlg_m))
  
  # subset distance matrix to keep only the rows corresponding to the countries listed in pa
  matlg_m <- as.data.frame(matlg_m)
  matlg_m <- subset(matlg_m, rownames(matlg_m) %in% rownames(pab_mc))
  matlg_m <- matlg_m[, colnames(matlg_m) %in% rownames(pab_mc)]
  
  # subset pa to keep only the year of interest
  pab_mc <- pab_mc[, colnames(pab_mc) == as.character(y)]
  
  # replace missing values by zeros to be able to perform the matrix multiplication
  pab_mc[is.na(pab_mc)] <- 0
  
  # matrix multiplication - building the spatial lag
  pab_mc <- as.matrix(pab_mc)
  matlg_m <- as.matrix(matlg_m)
  lglagb <- as.data.frame(matlg_m %*% pab_mc)
  
  # add column for country
  lglagb$country <- rownames(lglagb)
  
  # add column for year
  lglagb$year <- y
  
  # assign to object
  assign(paste0("pa_lglagb", y), lglagb, env = .GlobalEnv)
  
}

llistb <- list()

# create a list of data frames to combine
for (i in 1:length(all_gyears)) {
  
  lg <- get(paste0("pa_lglagb", all_gyears[i]))
  
  llistb[[i]] <- lg
  
  assign("llistb", llistb, env = .GlobalEnv)
}

# combine the datasets into a long dataset
long_pa_lglagb <- do.call(rbind, llistb)

# display each country each year
all_ycb <- expand.grid(country = unique(long_pa_lglagb$country), year = all_gyears)

long_pa_lglagb <- merge(long_pa_lglagb, all_ycb, by.x = c("year", "country"), by.y = c("year", "country"), all = T)

long_pa_lglagb <- as.data.frame(long_pa_lglagb)

# create column for the values lagged by a year
long_pa_lglagb$ylag <- long_pa_lglagb$V1

# make year variable numeric
long_pa_lglagb$year <- as.numeric(long_pa_lglagb$year)

# lag the values
for (i in 1:length(long_pa_lglagb$country)) {
  if(long_pa_lglagb$year[i] == min(long_pa_lglagb$year)) {
    long_pa_lglagb$ylag[i] <- NA
  } 
  else  {
    long_pa_lglagb$ylag[i] <- long_pa_lglagb$V1[long_pa_lglagb$country == long_pa_lglagb$country[i] & long_pa_lglagb$year == ((long_pa_lglagb$year[i]) - 1)] }
  
}

# change column names
colnames(long_pa_lglagb)[3:4] <- c("lang_lag", "time_lang_lag")

write.csv(long_pa_lglagb, "long_pa_lglagb.csv")

# check for missing variables
long_pa_lglagb_ex <- merge(long_pa_lglagb, country_ex, by =  c("year", "country"), all.y = T)

table(is.na(long_pa_lglagb_ex$lang_lag))

# save file
write.csv(long_pa_lglagb_ex, "long_pa_lglagb_ex.csv")


## Proportion of land area protected outcome variable ##

all_gyears <- 1961:2020

for(y in all_gyears) {
  matlg <- get(paste0("lg_adj", y))
  
  # turn into a numeric matrix
  matlg_m <- as.matrix(matlg)
  matlg_m <- as.numeric(matlg_m)
  matlg_m <- matrix(matlg_m, ncol = length(colnames(matlg)))
  
  # assign column + row names
  colnames(matlg_m) <- rownames(matlg)
  rownames(matlg_m) <- colnames(matlg_m)
  
  # subset pa to keep only the rows corresponding to the countries listed in the distance matrix
  paa_mc <- as.data.frame(paa_m)
  paa_mc <- subset(paa_m, rownames(paa_m) %in% rownames(matlg_m))
  
  # subset distance matrix to keep only the rows corresponding to the countries listed in pa
  matlg_m <- as.data.frame(matlg_m)
  matlg_m <- subset(matlg_m, rownames(matlg_m) %in% rownames(paa_mc))
  matlg_m <- matlg_m[, colnames(matlg_m) %in% rownames(paa_mc)]
  
  # subset pa to keep only the year of interest
  paa_mc <- paa_mc[, colnames(paa_mc) == as.character(y)]
  
  # replace missing values by zeros to be able to perform the matrix multiplication
  paa_mc[is.na(paa_mc)] <- 0
  
  # matrix multiplication - building the spatial lag
  paa_mc <- as.matrix(paa_mc)
  matlg_m <- as.matrix(matlg_m)
  lglag <- as.data.frame(matlg_m %*% paa_mc)
  
  # add column for country
  lglag$country <- rownames(lglag)
  
  # add column for year
  lglag$year <- y
  
  # assign to object
  assign(paste0("pa_lglag", y), lglag, env = .GlobalEnv)
  
}

llist <- list()

# create a list of data frames to combine
for (i in 1:length(all_gyears)) {
  
  lg <- get(paste0("pa_lglag", all_gyears[i]))
  
  llist[[i]] <- lg
  
  assign("llist", llist, env = .GlobalEnv)
}

# combine the datasets into a long dataset
long_pa_lglag <- do.call(rbind, llist)

# display each country each year
all_yc <- expand.grid(country = unique(long_pa_lglag$country), year = all_gyears)

long_pa_lglag <- merge(long_pa_lglag, all_yc, by.x = c("year", "country"), by.y = c("year", "country"), all = T)

long_pa_lglag <- as.data.frame(long_pa_lglag)

# create column for the values lagged by a year
long_pa_lglag$ylag <- long_pa_lglag$V1

# make year variable numeric
long_pa_lglag$year <- as.numeric(long_pa_lglag$year)

# lag the values
for (i in 1:length(long_pa_lglag$country)) {
  if(long_pa_lglag$year[i] == min(long_pa_lglag$year)) {
    long_pa_lglag$ylag[i] <- NA
  } 
  else  {
    long_pa_lglag$ylag[i] <- long_pa_lglag$V1[long_pa_lglag$country == long_pa_lglag$country[i] & long_pa_lglag$year == ((long_pa_lglag$year[i]) - 1)] }
  
}

# change column names
colnames(long_pa_lglag)[3:4] <- c("lang_lag", "time_lang_lag")

write.csv(long_pa_lglag, "long_pa_lglag.csv")


# check for missing variables
long_pa_lglag_ex <- merge(long_pa_lglag, country_ex, by =  c("year", "country"), all.y = T)

table(is.na(long_pa_lglag_ex$lang_lag))

# save file
write.csv(long_pa_lglag_ex, "long_pa_lglag_ex.csv")



## Percentage change area protected outcome variable ##

all_gyears <- 1961:2020

for(y in all_gyears) {
  matlg <- get(paste0("lg_adj", y))
  
  # turn into a numeric matrix
  matlg_m <- as.matrix(matlg)
  matlg_m <- as.numeric(matlg_m)
  matlg_m <- matrix(matlg_m, ncol = length(colnames(matlg)))
  
  # assign column + row names
  colnames(matlg_m) <- rownames(matlg)
  rownames(matlg_m) <- colnames(matlg_m)
  
  # subset pa to keep only the rows corresponding to the countries listed in the distance matrix
  paa_mc <- subset(paapc_m, rownames(paapc_m) %in% rownames(matlg_m))
  
  # subset distance matrix to keep only the rows corresponding to the countries listed in pa
  matlg_m <- as.data.frame(matlg_m)
  matlg_m <- subset(matlg_m, rownames(matlg_m) %in% rownames(paa_mc))
  matlg_m <- matlg_m[, colnames(matlg_m) %in% rownames(paa_mc)]
  
  # subset pa to keep only the year of interest
  paa_mc <- paa_mc[, colnames(paa_mc) == as.character(y)]
  
  # replace missing values by zeros to be able to perform the matrix multiplication
  paa_mc[is.na(paa_mc)] <- 0
  
  # matrix multiplication - building the spatial lag
  paa_mc <- as.matrix(paa_mc)
  matlg_m <- as.matrix(matlg_m)
  lglag <- as.data.frame(matlg_m %*% paa_mc)
  
  # add column for country
  lglag$country <- rownames(lglag)
  
  # add column for year
  lglag$year <- y
  
  # assign to object
  assign(paste0("papc_lglag", y), lglag, env = .GlobalEnv)
  
}

llist <- list()

# create a list of data frames to combine
for (i in 1:length(all_gyears)) {
  
  lg <- get(paste0("papc_lglag", all_gyears[i]))
  
  llist[[i]] <- lg
  
  assign("llist", llist, env = .GlobalEnv)
}

# combine the datasets into a long dataset
long_papc_lglag <- do.call(rbind, llist)

# display each country each year
all_yc <- expand.grid(country = unique(long_papc_lglag$country), year = all_gyears)

long_papc_lglag <- merge(long_papc_lglag, all_yc, by.x = c("year", "country"), by.y = c("year", "country"), all = T)

long_papc_lglag <- as.data.frame(long_papc_lglag)

# create column for the values lagged by a year
long_papc_lglag$ylag <- long_papc_lglag$V1

# make year variable numeric
long_papc_lglag$year <- as.numeric(long_papc_lglag$year)

# lag the values
for (i in 1:length(long_papc_lglag$country)) {
  if(long_papc_lglag$year[i] == min(long_papc_lglag$year)) {
    long_papc_lglag$ylag[i] <- NA
  } 
  else  {
    long_papc_lglag$ylag[i] <- long_papc_lglag$V1[long_papc_lglag$country == long_papc_lglag$country[i] & long_papc_lglag$year == ((long_papc_lglag$year[i]) - 1)] }
  
}

# change column names
colnames(long_papc_lglag)[3:4] <- c("lang_lag_pc", "time_lang_lag_pc")

write.csv(long_papc_lglag, "long_papc_lglag.csv")


# check for missing variables
long_papc_lglag_ex <- merge(long_papc_lglag, country_ex, by =  c("year", "country"), all.y = T)

table(is.na(long_papc_lglag_ex$lang_lag_pc))

# save file
write.csv(long_papc_lglag_ex, "long_papc_lglag_ex.csv")



## Percentage point change area protected outcome variable ##

all_gyears <- 1961:2020

for(y in all_gyears) {
  matlg <- get(paste0("lg_adj", y))
  
  # turn into a numeric matrix
  matlg_m <- as.matrix(matlg)
  matlg_m <- as.numeric(matlg_m)
  matlg_m <- matrix(matlg_m, ncol = length(colnames(matlg)))
  
  # assign column + row names
  colnames(matlg_m) <- rownames(matlg)
  rownames(matlg_m) <- colnames(matlg_m)
  
  # subset pa to keep only the rows corresponding to the countries listed in the distance matrix
  paa_mc <- subset(paapp_m, rownames(paapp_m) %in% rownames(matlg_m))
  
  # subset distance matrix to keep only the rows corresponding to the countries listed in pa
  matlg_m <- as.data.frame(matlg_m)
  matlg_m <- subset(matlg_m, rownames(matlg_m) %in% rownames(paa_mc))
  matlg_m <- matlg_m[, colnames(matlg_m) %in% rownames(paa_mc)]
  
  # subset pa to keep only the year of interest
  paa_mc <- paa_mc[, colnames(paa_mc) == as.character(y)]
  
  # replace missing values by zeros to be able to perform the matrix multiplication
  paa_mc[is.na(paa_mc)] <- 0
  
  # matrix multiplication - building the spatial lag
  paa_mc <- as.matrix(paa_mc)
  matlg_m <- as.matrix(matlg_m)
  lglag <- as.data.frame(matlg_m %*% paa_mc)
  
  # add column for country
  lglag$country <- rownames(lglag)
  
  # add column for year
  lglag$year <- y
  
  # assign to object
  assign(paste0("papp_lglag", y), lglag, env = .GlobalEnv)
  
}

llist <- list()

# create a list of data frames to combine
for (i in 1:length(all_gyears)) {
  
  lg <- get(paste0("papp_lglag", all_gyears[i]))
  
  llist[[i]] <- lg
  
  assign("llist", llist, env = .GlobalEnv)
}

# combine the datasets into a long dataset
long_papp_lglag <- do.call(rbind, llist)

# display each country each year
all_yc <- expand.grid(country = unique(long_papp_lglag$country), year = all_gyears)

long_papp_lglag <- merge(long_papp_lglag, all_yc, by.x = c("year", "country"), by.y = c("year", "country"), all = T)

long_papp_lglag <- as.data.frame(long_papp_lglag)

# create column for the values lagged by a year
long_papp_lglag$ylag <- long_papp_lglag$V1

# make year variable numeric
long_papp_lglag$year <- as.numeric(long_papp_lglag$year)

# lag the values
for (i in 1:length(long_papp_lglag$country)) {
  if(long_papp_lglag$year[i] == min(long_papp_lglag$year)) {
    long_papp_lglag$ylag[i] <- NA
  } 
  else  {
    long_papp_lglag$ylag[i] <- long_papp_lglag$V1[long_papp_lglag$country == long_papp_lglag$country[i] & long_papp_lglag$year == ((long_papp_lglag$year[i]) - 1)] }
  
}

# change column names
colnames(long_papp_lglag)[3:4] <- c("lang_lag_pp", "time_lang_lag_pp")

write.csv(long_papp_lglag, "long_papp_lglag.csv")


# check for missing variables
long_papp_lglag_ex <- merge(long_papp_lglag, country_ex, by =  c("year", "country"), all.y = T)

table(is.na(long_papp_lglag_ex$lang_lag_pp))

# save file
write.csv(long_papp_lglag_ex, "long_papp_lglag_ex.csv")




#### Trade competition lag ####


## Binary outcome variable ##

for(y in 1962:2020) {
  cor <- get(paste0("cor_", y))
  
  # in correlation matrices, in lines where there aren't only NAs: replace NAs with 0s
  cor <- as.data.frame(cor)
  
  for (i in 1:nrow(cor)) {
    
    if (any(!is.na(cor[i,]))) {
      
      for (j in 1:nrow(cor)) {
        
        if(is.na(cor[i,j])) {
          cor[i,j] <- 0
        }
      }}}
  
  assign(paste0("cor_", y), cor, env = .GlobalEnv)
  
  # turn into a numeric matrix
  cor_m <- as.matrix(cor)
  cor_m <- as.numeric(cor_m)
  cor_m <- matrix(cor_m, ncol = length(colnames(cor)))
  
  # assign column + row names
  colnames(cor_m) <- rownames(cor)
  rownames(cor_m) <- colnames(cor_m)
  
  # subset pa to keep only the rows corresponding to the countries listed in the distance matrix
  pab_mc <- subset(pab_m, rownames(pab_m) %in% rownames(cor_m))
  
  # subset distance matrix to keep only the rows corresponding to the countries listed in pa
  cor_m <- subset(cor_m, rownames(cor_m) %in% rownames(pab_mc))
  cor_m <- cor_m[, colnames(cor_m) %in% rownames(pab_mc)]
  
  # subset pa to keep only the year of interest
  pab_mc <- pab_mc[, paste0(y)]
  
  # replace missing values by zeros to be able to perform the matrix multiplication
  pab_mc[is.na(pab_mc)] <- 0
  
  # matrix multiplication - building the spatial lag
  tradelagb <- as.data.frame(cor_m %*% pab_mc)
  
  # add column for country
  tradelagb$country <- rownames(tradelagb)
  
  # add column for year
  tradelagb$year <- y
  
  # assign to object
  assign(paste0("pa_tradelagb", y), tradelagb, env = .GlobalEnv)
  
}

tlistb <- list()
all_tyears <- 1962:2020

# create a list of data frames to combine
for (i in 1:length(all_tyears)) {
  
  t <- get(paste0("pa_tradelagb", all_tyears[i]))
  
  tlistb[[i]] <- t
  
  assign("tlistb", tlistb, env = .GlobalEnv)
}

# combine the datasets into a long dataset
long_pa_tradelagb <- do.call(rbind, tlistb)

# display each country each year in the dataset
all_tycb <- expand.grid(country = unique(long_pa_tradelagb$country), year = all_tyears)

long_pa_tradelagb <- merge(long_pa_tradelagb, all_tycb, by.x = c("year", "country"), by.y = c("year", "country"), all = T)

long_pa_tradelagb <- as.data.frame(long_pa_tradelagb)

# create column for the values lagged by a year
long_pa_tradelagb$ylag <- long_pa_tradelagb$value

# make year variable numeric
long_pa_tradelagb$year <- as.numeric(long_pa_tradelagb$year)

# lag the values
for (i in 1:length(long_pa_tradelagb$country)) {
  if(long_pa_tradelagb$year[i] == min(all_tyears)) {
    long_pa_tradelagb$ylag[i] <- NA
  } else {
    long_pa_tradelagb$ylag[i] <- long_pa_tradelagb$V1[long_pa_tradelagb$country == long_pa_tradelagb$country[i] & long_pa_tradelagb$year == ((long_pa_tradelagb$year[i]) - 1)] }
}

# change column names
colnames(long_pa_tradelagb)[3:4] <- c("trade_lag", "time_trade_lag")

# save the object
write.csv(long_pa_tradelagb, "long_pa_tradelagb.csv")

# check for missing values
long_pa_tradelagb_ex <- merge(long_pa_tradelagb, country_ex, by =  c("year", "country"), all.y = T)

table(is.na(long_pa_tradelagb_ex$trade_lag))

# save the object
write.csv(long_pa_tradelagb_ex, "long_pa_tradelagb_ex.csv")


## Proportion of land area protected outcome variable ##

for(y in 1962:2020) {
  cor <- get(paste0("cor_", y))
  
  # in correlation matrices, in lines where there aren't only NAs: replace NAs with 0s
  cor <- as.data.frame(cor)
  
  for (i in 1:nrow(cor)) {
    
    if (any(!is.na(cor[i,]))) {
      
      for (j in 1:nrow(cor)) {
        
        if(is.na(cor[i,j])) {
          cor[i,j] <- 0
        }
      }}}
  
  assign(paste0("cor_", y), cor, env = .GlobalEnv)
  
  # turn into a numeric matrix
  cor_m <- as.matrix(cor)
  cor_m <- as.numeric(cor_m)
  cor_m <- matrix(cor_m, ncol = length(colnames(cor)))
  
  # assign column + row names
  colnames(cor_m) <- rownames(cor)
  rownames(cor_m) <- colnames(cor_m)
  
  # subset pa to keep only the rows corresponding to the countries listed in the distance matrix
  paa_mc <- subset(paa_m, rownames(paa_m) %in% rownames(cor_m))
  
  # subset distance matrix to keep only the rows corresponding to the countries listed in pa
  cor_m <- subset(cor_m, rownames(cor_m) %in% rownames(paa_mc))
  cor_m <- cor_m[, colnames(cor_m) %in% rownames(paa_mc)]
  
  # subset pa to keep only the year of interest
  paa_mc <- paa_mc[, paste0(y)]
  
  # replace missing values by zeros to be able to perform the matrix multiplication
  paa_mc[is.na(paa_mc)] <- 0
  
  # matrix multiplication - building the spatial lag
  tradelag <- as.data.frame(cor_m %*% paa_mc)
  
  # add column for country
  tradelag$country <- rownames(tradelag)
  
  # add column for year
  tradelag$year <- y
  
  # assign to object
  assign(paste0("pa_tradelag", y), tradelag, env = .GlobalEnv)
  
}

tlist <- list()
all_tyears <- 1962:2020

# create a list of data frames to combine
for (i in 1:length(all_tyears)) {
  
  t <- get(paste0("pa_tradelag", all_tyears[i]))
  
  tlist[[i]] <- t
  
  assign("tlist", tlist, env = .GlobalEnv)
}

# combine the datasets into a long dataset
long_pa_tradelag <- do.call(rbind, tlist)

# display each country each year in the dataset
all_tyc <- expand.grid(country = unique(long_pa_tradelag$country), year = all_tyears)

long_pa_tradelag <- merge(long_pa_tradelag, all_tyc, by.x = c("year", "country"), by.y = c("year", "country"), all = T)

long_pa_tradelag <- as.data.frame(long_pa_tradelag)

# create column for the values lagged by a year
long_pa_tradelag$ylag <- long_pa_tradelag$value

# make year variable numeric
long_pa_tradelag$year <- as.numeric(long_pa_tradelag$year)

# lag the values
for (i in 1:length(long_pa_tradelag$country)) {
  if(long_pa_tradelag$year[i] == min(all_tyears)) {
    long_pa_tradelag$ylag[i] <- NA
  } else {
    long_pa_tradelag$ylag[i] <- long_pa_tradelag$V1[long_pa_tradelag$country == long_pa_tradelag$country[i] & long_pa_tradelag$year == ((long_pa_tradelag$year[i]) - 1)] }
}

# change column names
colnames(long_pa_tradelag)[3:4] <- c("trade_lag", "time_trade_lag")

# save the object
write.csv(long_pa_tradelag, "long_pa_tradelag.csv")

# check for missing values
long_pa_tradelag_ex <- merge(long_pa_tradelag, country_ex, by =  c("year", "country"), all.y = T)

table(is.na(long_pa_tradelag_ex$trade_lag))

# save the object
write.csv(long_pa_tradelag_ex, "long_pa_tradelag_ex.csv")


## Percentage change in land area protected outcome variable ##

for(y in 1962:2020) {
  cor <- get(paste0("cor_", y))
  
  # in correlation matrices, in lines where there aren't only NAs: replace NAs with 0s
  cor <- as.data.frame(cor)
  
  for (i in 1:nrow(cor)) {
    
    if (any(!is.na(cor[i,]))) {
      
      for (j in 1:nrow(cor)) {
        
        if(is.na(cor[i,j])) {
          cor[i,j] <- 0
        }
      }}}
  
  assign(paste0("cor_", y), cor, env = .GlobalEnv)
  
  # turn into a numeric matrix
  cor_m <- as.matrix(cor)
  cor_m <- as.numeric(cor_m)
  cor_m <- matrix(cor_m, ncol = length(colnames(cor)))
  
  # assign column + row names
  colnames(cor_m) <- rownames(cor)
  rownames(cor_m) <- colnames(cor_m)
  
  # subset pa to keep only the rows corresponding to the countries listed in the distance matrix
  paa_mc <- subset(paapc_m, rownames(paapc_m) %in% rownames(cor_m))
  
  # subset distance matrix to keep only the rows corresponding to the countries listed in pa
  cor_m <- subset(cor_m, rownames(cor_m) %in% rownames(paa_mc))
  cor_m <- cor_m[, colnames(cor_m) %in% rownames(paa_mc)]
  
  # subset pa to keep only the year of interest
  paa_mc <- paa_mc[, paste0(y)]
  
  # replace missing values by zeros to be able to perform the matrix multiplication
  paa_mc[is.na(paa_mc)] <- 0
  
  # matrix multiplication - building the spatial lag
  tradelag <- as.data.frame(cor_m %*% paa_mc)
  
  # add column for country
  tradelag$country <- rownames(tradelag)
  
  # add column for year
  tradelag$year <- y
  
  # assign to object
  assign(paste0("papc_tradelag", y), tradelag, env = .GlobalEnv)
  
}

tlist <- list()
all_tyears <- 1962:2020

# create a list of data frames to combine
for (i in 1:length(all_tyears)) {
  
  t <- get(paste0("papc_tradelag", all_tyears[i]))
  
  tlist[[i]] <- t
  
  assign("tlist", tlist, env = .GlobalEnv)
}

# combine the datasets into a long dataset
long_papc_tradelag <- do.call(rbind, tlist)

# display each country each year in the dataset
all_tyc <- expand.grid(country = unique(long_papc_tradelag$country), year = all_tyears)

long_papc_tradelag <- merge(long_papc_tradelag, all_tyc, by.x = c("year", "country"), by.y = c("year", "country"), all = T)

long_papc_tradelag <- as.data.frame(long_papc_tradelag)

# create column for the values lagged by a year
long_papc_tradelag$ylag <- long_papc_tradelag$value

# make year variable numeric
long_papc_tradelag$year <- as.numeric(long_papc_tradelag$year)

# lag the values
for (i in 1:length(long_papc_tradelag$country)) {
  if(long_papc_tradelag$year[i] == min(all_tyears)) {
    long_papc_tradelag$ylag[i] <- NA
  } else {
    long_papc_tradelag$ylag[i] <- long_papc_tradelag$V1[long_papc_tradelag$country == long_papc_tradelag$country[i] & long_papc_tradelag$year == ((long_papc_tradelag$year[i]) - 1)] }
}

# change column names
colnames(long_papc_tradelag)[3:4] <- c("trade_lag_pc", "time_trade_lag_pc")

# save the object
write.csv(long_papc_tradelag, "long_papc_tradelag.csv")

# check for missing values
long_papc_tradelag_ex <- merge(long_papc_tradelag, country_ex, by =  c("year", "country"), all.y = T)

table(is.na(long_papc_tradelag_ex$trade_lag_pc))

# save the object
write.csv(long_papc_tradelag_ex, "long_papc_tradelag_ex.csv")



## Percentage change in land area protected outcome variable ##

for(y in 1962:2020) {
  cor <- get(paste0("cor_", y))
  
  # in correlation matrices, in lines where there aren't only NAs: replace NAs with 0s
  cor <- as.data.frame(cor)
  
  for (i in 1:nrow(cor)) {
    
    if (any(!is.na(cor[i,]))) {
      
      for (j in 1:nrow(cor)) {
        
        if(is.na(cor[i,j])) {
          cor[i,j] <- 0
        }
      }}}
  
  assign(paste0("cor_", y), cor, env = .GlobalEnv)
  
  # turn into a numeric matrix
  cor_m <- as.matrix(cor)
  cor_m <- as.numeric(cor_m)
  cor_m <- matrix(cor_m, ncol = length(colnames(cor)))
  
  # assign column + row names
  colnames(cor_m) <- rownames(cor)
  rownames(cor_m) <- colnames(cor_m)
  
  # subset pa to keep only the rows corresponding to the countries listed in the distance matrix
  paa_mc <- subset(paapp_m, rownames(paapp_m) %in% rownames(cor_m))
  
  # subset distance matrix to keep only the rows corresponding to the countries listed in pa
  cor_m <- subset(cor_m, rownames(cor_m) %in% rownames(paa_mc))
  cor_m <- cor_m[, colnames(cor_m) %in% rownames(paa_mc)]
  
  # subset pa to keep only the year of interest
  paa_mc <- paa_mc[, paste0(y)]
  
  # replace missing values by zeros to be able to perform the matrix multiplication
  paa_mc[is.na(paa_mc)] <- 0
  
  # matrix multiplication - building the spatial lag
  tradelag <- as.data.frame(cor_m %*% paa_mc)
  
  # add column for country
  tradelag$country <- rownames(tradelag)
  
  # add column for year
  tradelag$year <- y
  
  # assign to object
  assign(paste0("papp_tradelag", y), tradelag, env = .GlobalEnv)
  
}

tlist <- list()
all_tyears <- 1962:2020

# create a list of data frames to combine
for (i in 1:length(all_tyears)) {
  
  t <- get(paste0("papp_tradelag", all_tyears[i]))
  
  tlist[[i]] <- t
  
  assign("tlist", tlist, env = .GlobalEnv)
}

# combine the datasets into a long dataset
long_papp_tradelag <- do.call(rbind, tlist)

# display each country each year in the dataset
all_tyc <- expand.grid(country = unique(long_papp_tradelag$country), year = all_tyears)

long_papp_tradelag <- merge(long_papp_tradelag, all_tyc, by.x = c("year", "country"), by.y = c("year", "country"), all = T)

long_papp_tradelag <- as.data.frame(long_papp_tradelag)

# create column for the values lagged by a year
long_papp_tradelag$ylag <- long_papp_tradelag$value

# make year variable numeric
long_papp_tradelag$year <- as.numeric(long_papp_tradelag$year)

# lag the values
for (i in 1:length(long_papp_tradelag$country)) {
  if(long_papp_tradelag$year[i] == min(all_tyears)) {
    long_papp_tradelag$ylag[i] <- NA
  } else {
    long_papp_tradelag$ylag[i] <- long_papp_tradelag$V1[long_papp_tradelag$country == long_papp_tradelag$country[i] & long_papp_tradelag$year == ((long_papp_tradelag$year[i]) - 1)] }
}

# change column names
colnames(long_papp_tradelag)[3:4] <- c("trade_lag_pp", "time_trade_lag_pp")

# save the object
write.csv(long_papp_tradelag, "long_papp_tradelag.csv")

# check for missing values
long_papp_tradelag_ex <- merge(long_papp_tradelag, country_ex, by =  c("year", "country"), all.y = T)

table(is.na(long_papp_tradelag_ex$trade_lag))

# save the object
write.csv(long_papp_tradelag_ex, "long_papp_tradelag_ex.csv")




#### Trade competition lag - limited influences ####


## Binary outcome variable ##

for(y in 1962:2019) {
  cor <- get(paste0("cor_", y, "_20"))
  
  # in correlation matrices, in lines where there aren't only NAs: replace NAs with 0s
  cor <- as.data.frame(cor)
  
  for (i in 1:nrow(cor)) {
    
    if (any(!is.na(cor[i,]))) {
      
      for (j in 1:nrow(cor)) {
        
        if(is.na(cor[i,j])) {
          cor[i,j] <- 0
        }
      }}}
  
  assign(paste0("cor_", y, "_20"), cor, env = .GlobalEnv)
  
  # turn into a numeric matrix
  cor_m <- as.matrix(cor)
  cor_m <- as.numeric(cor_m)
  cor_m <- matrix(cor_m, ncol = length(colnames(cor)))
  
  # assign column + row names
  colnames(cor_m) <- rownames(cor)
  rownames(cor_m) <- colnames(cor_m)
  
  # subset pa to keep only the rows corresponding to the countries listed in the distance matrix
  pab_mc <- subset(pab_m, rownames(pab_m) %in% rownames(cor_m))
  
  # subset distance matrix to keep only the rows corresponding to the countries listed in pa
  cor_m <- subset(cor_m, rownames(cor_m) %in% rownames(pab_mc))
  cor_m <- cor_m[, colnames(cor_m) %in% rownames(pab_mc)]
  
  # subset pa to keep only the year of interest
  pab_mc <- pab_mc[, paste0(y)]
  
  # replace missing values by zeros to be able to perform the matrix multiplication
  pab_mc[is.na(pab_mc)] <- 0
  
  # matrix multiplication - building the spatial lag
  tradelagb <- as.data.frame(cor_m %*% pab_mc)
  
  # add column for country
  tradelagb$country <- rownames(tradelagb)
  
  # add column for year
  tradelagb$year <- y
  
  # assign to object
  assign(paste0("pa_tradelagb", y, "_20"), tradelagb, env = .GlobalEnv)
  
}

tlistb <- list()
all_tyears <- 1962:2019

# create a list of data frames to combine
for (i in 1:length(all_tyears)) {
  
  t <- get(paste0("pa_tradelagb", all_tyears[i], "_20"))
  
  tlistb[[i]] <- t
  
  assign("tlistb", tlistb, env = .GlobalEnv)
}

# combine the datasets into a long dataset
long_pa_tradelagb_20 <- do.call(rbind, tlistb)

# display each country each year in the dataset
all_tycb <- expand.grid(country = unique(long_pa_tradelagb_20$country), year = all_tyears)

long_pa_tradelagb_20 <- merge(long_pa_tradelagb_20, all_tycb, by.x = c("year", "country"), by.y = c("year", "country"), all = T)

long_pa_tradelagb_20 <- as.data.frame(long_pa_tradelagb_20)

# create column for the values lagged by a year
long_pa_tradelagb_20$ylag <- long_pa_tradelagb_20$value

# make year variable numeric
long_pa_tradelagb_20$year <- as.numeric(long_pa_tradelagb_20$year)

# lag the values
for (i in 1:length(long_pa_tradelagb_20$country)) {
  if(long_pa_tradelagb_20$year[i] == min(all_tyears)) {
    long_pa_tradelagb_20$ylag[i] <- NA
  } else {
    long_pa_tradelagb_20$ylag[i] <- long_pa_tradelagb_20$V1[long_pa_tradelagb_20$country == long_pa_tradelagb_20$country[i] & long_pa_tradelagb_20$year == ((long_pa_tradelagb_20$year[i]) - 1)] }
}

# change column names
colnames(long_pa_tradelagb_20)[3:4] <- c("trade_lag_20", "time_trade_lag_20")

# save the object
write.csv(long_pa_tradelagb_20, "long_pa_tradelagb_20.csv")

# check for missing values
long_pa_tradelagb_ex_20 <- merge(long_pa_tradelagb_20, country_ex, by =  c("year", "country"), all.y = T)

table(is.na(long_pa_tradelagb_ex_20$trade_lag_20))

# save the object
write.csv(long_pa_tradelagb_ex_20, "long_pa_tradelagb_ex_20.csv")


## Proportion of land area protected outcome variable ##

for(y in 1962:2019) {
  cor <- get(paste0("cor_", y, "_20"))
  
  # in correlation matrices, in lines where there aren't only NAs: replace NAs with 0s
  cor <- as.data.frame(cor)
  
  for (i in 1:nrow(cor)) {
    
    if (any(!is.na(cor[i,]))) {
      
      for (j in 1:nrow(cor)) {
        
        if(is.na(cor[i,j])) {
          cor[i,j] <- 0
        }
      }}}
  
  assign(paste0("cor_", y, "_20"), cor, env = .GlobalEnv)
  
  # turn into a numeric matrix
  cor_m <- as.matrix(cor)
  cor_m <- as.numeric(cor_m)
  cor_m <- matrix(cor_m, ncol = length(colnames(cor)))
  
  # assign column + row names
  colnames(cor_m) <- rownames(cor)
  rownames(cor_m) <- colnames(cor_m)
  
  # subset pa to keep only the rows corresponding to the countries listed in the distance matrix
  paa_mc <- subset(paa_m, rownames(paa_m) %in% rownames(cor_m))
  
  # subset distance matrix to keep only the rows corresponding to the countries listed in pa
  cor_m <- subset(cor_m, rownames(cor_m) %in% rownames(paa_mc))
  cor_m <- cor_m[, colnames(cor_m) %in% rownames(paa_mc)]
  
  # subset pa to keep only the year of interest
  paa_mc <- paa_mc[, paste0(y)]
  
  # replace missing values by zeros to be able to perform the matrix multiplication
  paa_mc[is.na(paa_mc)] <- 0
  
  # matrix multiplication - building the spatial lag
  tradelag <- as.data.frame(cor_m %*% paa_mc)
  
  # add column for country
  tradelag$country <- rownames(tradelag)
  
  # add column for year
  tradelag$year <- y
  
  # assign to object
  assign(paste0("pa_tradelag", y, "_20"), tradelag, env = .GlobalEnv)
  
}

tlist <- list()
all_tyears <- 1962:2019

# create a list of data frames to combine
for (i in 1:length(all_tyears)) {
  
  t <- get(paste0("pa_tradelag", all_tyears[i], "_20"))
  
  tlist[[i]] <- t
  
  assign("tlist", tlist, env = .GlobalEnv)
}

# combine the datasets into a long dataset
long_pa_tradelag_20 <- do.call(rbind, tlist)

# display each country each year in the dataset
all_tyc <- expand.grid(country = unique(long_pa_tradelag_20$country), year = all_tyears)

long_pa_tradelag_20 <- merge(long_pa_tradelag_20, all_tyc, by.x = c("year", "country"), by.y = c("year", "country"), all = T)

long_pa_tradelag_20 <- as.data.frame(long_pa_tradelag_20)

# create column for the values lagged by a year
long_pa_tradelag_20$ylag <- long_pa_tradelag_20$value

# make year variable numeric
long_pa_tradelag_20$year <- as.numeric(long_pa_tradelag_20$year)

# lag the values
for (i in 1:length(long_pa_tradelag_20$country)) {
  if(long_pa_tradelag_20$year[i] == min(all_tyears)) {
    long_pa_tradelag_20$ylag[i] <- NA
  } else {
    long_pa_tradelag_20$ylag[i] <- long_pa_tradelag_20$V1[long_pa_tradelag_20$country == long_pa_tradelag_20$country[i] & long_pa_tradelag_20$year == ((long_pa_tradelag_20$year[i]) - 1)] }
}

# change column names
colnames(long_pa_tradelag_20)[3:4] <- c("trade_lag_20", "time_trade_lag_20")

# save the object
write.csv(long_pa_tradelag_20, "long_pa_tradelag_20.csv")

# check for missing values
long_pa_tradelag_ex_20 <- merge(long_pa_tradelag_20, country_ex, by =  c("year", "country"), all.y = T)

table(is.na(long_pa_tradelag_ex_20$trade_lag_20))

# save the object
write.csv(long_pa_tradelag_ex_20, "long_pa_tradelag_ex_20.csv")


## Percentage change in land area protected outcome variable ##

for(y in 1962:2019) {
  cor <- get(paste0("cor_", y, "_20"))
  
  # in correlation matrices, in lines where there aren't only NAs: replace NAs with 0s
  cor <- as.data.frame(cor)
  
  for (i in 1:nrow(cor)) {
    
    if (any(!is.na(cor[i,]))) {
      
      for (j in 1:nrow(cor)) {
        
        if(is.na(cor[i,j])) {
          cor[i,j] <- 0
        }
      }}}
  
  assign(paste0("cor_", y, "_20"), cor, env = .GlobalEnv)
  
  # turn into a numeric matrix
  cor_m <- as.matrix(cor)
  cor_m <- as.numeric(cor_m)
  cor_m <- matrix(cor_m, ncol = length(colnames(cor)))
  
  # assign column + row names
  colnames(cor_m) <- rownames(cor)
  rownames(cor_m) <- colnames(cor_m)
  
  # subset pa to keep only the rows corresponding to the countries listed in the distance matrix
  paa_mc <- subset(paapc_m, rownames(paapc_m) %in% rownames(cor_m))
  
  # subset distance matrix to keep only the rows corresponding to the countries listed in pa
  cor_m <- subset(cor_m, rownames(cor_m) %in% rownames(paa_mc))
  cor_m <- cor_m[, colnames(cor_m) %in% rownames(paa_mc)]
  
  # subset pa to keep only the year of interest
  paa_mc <- paa_mc[, paste0(y)]
  
  # replace missing values by zeros to be able to perform the matrix multiplication
  paa_mc[is.na(paa_mc)] <- 0
  
  # matrix multiplication - building the spatial lag
  tradelag <- as.data.frame(cor_m %*% paa_mc)
  
  # add column for country
  tradelag$country <- rownames(tradelag)
  
  # add column for year
  tradelag$year <- y
  
  # assign to object
  assign(paste0("papc_tradelag", y, "_20"), tradelag, env = .GlobalEnv)
  
}

tlist <- list()
all_tyears <- 1962:2019

# create a list of data frames to combine
for (i in 1:length(all_tyears)) {
  
  t <- get(paste0("papc_tradelag", all_tyears[i], "_20"))
  
  tlist[[i]] <- t
  
  assign("tlist", tlist, env = .GlobalEnv)
}

# combine the datasets into a long dataset
long_papc_tradelag_20 <- do.call(rbind, tlist)

# display each country each year in the dataset
all_tyc <- expand.grid(country = unique(long_papc_tradelag_20$country), year = all_tyears)

long_papc_tradelag_20 <- merge(long_papc_tradelag_20, all_tyc, by.x = c("year", "country"), by.y = c("year", "country"), all = T)

long_papc_tradelag_20 <- as.data.frame(long_papc_tradelag_20)

# create column for the values lagged by a year
long_papc_tradelag_20$ylag <- long_papc_tradelag_20$value

# make year variable numeric
long_papc_tradelag_20$year <- as.numeric(long_papc_tradelag_20$year)

# lag the values
for (i in 1:length(long_papc_tradelag_20$country)) {
  if(long_papc_tradelag_20$year[i] == min(all_tyears)) {
    long_papc_tradelag_20$ylag[i] <- NA
  } else {
    long_papc_tradelag_20$ylag[i] <- long_papc_tradelag_20$V1[long_papc_tradelag_20$country == long_papc_tradelag_20$country[i] & long_papc_tradelag_20$year == ((long_papc_tradelag_20$year[i]) - 1)] }
}

# change column names
colnames(long_papc_tradelag_20)[3:4] <- c("trade_lag_pc_20", "time_trade_lag_pc_20")

# save the object
write.csv(long_papc_tradelag_20, "long_papc_tradelag_20.csv")

# check for missing values
long_papc_tradelag_ex_20 <- merge(long_papc_tradelag_20, country_ex, by =  c("year", "country"), all.y = T)

table(is.na(long_papc_tradelag_ex_20$trade_lag_pc_20))

# save the object
write.csv(long_papc_tradelag_ex_20, "long_papc_tradelag_ex_20.csv")



## Percentage change in land area protected outcome variable ##

for(y in 1962:2019) {
  cor <- get(paste0("cor_", y, "_20"))
  
  # in correlation matrices, in lines where there aren't only NAs: replace NAs with 0s
  cor <- as.data.frame(cor)
  
  for (i in 1:nrow(cor)) {
    
    if (any(!is.na(cor[i,]))) {
      
      for (j in 1:nrow(cor)) {
        
        if(is.na(cor[i,j])) {
          cor[i,j] <- 0
        }
      }}}
  
  assign(paste0("cor_", y, "_20"), cor, env = .GlobalEnv)
  
  # turn into a numeric matrix
  cor_m <- as.matrix(cor)
  cor_m <- as.numeric(cor_m)
  cor_m <- matrix(cor_m, ncol = length(colnames(cor)))
  
  # assign column + row names
  colnames(cor_m) <- rownames(cor)
  rownames(cor_m) <- colnames(cor_m)
  
  # subset pa to keep only the rows corresponding to the countries listed in the distance matrix
  paa_mc <- subset(paapp_m, rownames(paapp_m) %in% rownames(cor_m))
  
  # subset distance matrix to keep only the rows corresponding to the countries listed in pa
  cor_m <- subset(cor_m, rownames(cor_m) %in% rownames(paa_mc))
  cor_m <- cor_m[, colnames(cor_m) %in% rownames(paa_mc)]
  
  # subset pa to keep only the year of interest
  paa_mc <- paa_mc[, paste0(y)]
  
  # replace missing values by zeros to be able to perform the matrix multiplication
  paa_mc[is.na(paa_mc)] <- 0
  
  # matrix multiplication - building the spatial lag
  tradelag <- as.data.frame(cor_m %*% paa_mc)
  
  # add column for country
  tradelag$country <- rownames(tradelag)
  
  # add column for year
  tradelag$year <- y
  
  # assign to object
  assign(paste0("papp_tradelag", y, "_20"), tradelag, env = .GlobalEnv)
  
}

tlist <- list()
all_tyears <- 1962:2019

# create a list of data frames to combine
for (i in 1:length(all_tyears)) {
  
  t <- get(paste0("papp_tradelag", all_tyears[i], "_20"))
  
  tlist[[i]] <- t
  
  assign("tlist", tlist, env = .GlobalEnv)
}

# combine the datasets into a long dataset
long_papp_tradelag_20 <- do.call(rbind, tlist)

# display each country each year in the dataset
all_tyc <- expand.grid(country = unique(long_papp_tradelag_20$country), year = all_tyears)

long_papp_tradelag_20 <- merge(long_papp_tradelag_20, all_tyc, by.x = c("year", "country"), by.y = c("year", "country"), all = T)

long_papp_tradelag_20 <- as.data.frame(long_papp_tradelag_20)

# create column for the values lagged by a year
long_papp_tradelag_20$ylag <- long_papp_tradelag_20$value

# make year variable numeric
long_papp_tradelag_20$year <- as.numeric(long_papp_tradelag_20$year)

# lag the values
for (i in 1:length(long_papp_tradelag_20$country)) {
  if(long_papp_tradelag_20$year[i] == min(all_tyears)) {
    long_papp_tradelag_20$ylag[i] <- NA
  } else {
    long_papp_tradelag_20$ylag[i] <- long_papp_tradelag_20$V1[long_papp_tradelag_20$country == long_papp_tradelag_20$country[i] & long_papp_tradelag_20$year == ((long_papp_tradelag_20$year[i]) - 1)] }
}

# change column names
colnames(long_papp_tradelag_20)[3:4] <- c("trade_lag_pp_20", "time_trade_lag_pp_20")

# save the object
write.csv(long_papp_tradelag_20, "long_papp_tradelag_20.csv")

# check for missing values
long_papp_tradelag_ex_20 <- merge(long_papp_tradelag_20, country_ex, by =  c("year", "country"), all.y = T)

table(is.na(long_papp_tradelag_ex_20$trade_lag_20))

# save the object
write.csv(long_papp_tradelag_ex_20, "long_papp_tradelag_ex_20.csv")









#### Environmental IGO network lag ####


## Binary outcome variable ##

all_iyears <- 1961:2014

for(y in all_iyears) {
  matenv_igo <- get(paste0("env_igo_adj", y))
  
  # turn into a numeric matrix
  matenv_igo_m <- as.matrix(matenv_igo)
  matenv_igo_m <- as.numeric(matenv_igo_m)
  matenv_igo_m <- matrix(matenv_igo_m, ncol = length(colnames(matenv_igo)))
  
  # assign column + row names
  colnames(matenv_igo_m) <- rownames(matenv_igo)
  rownames(matenv_igo_m) <- colnames(matenv_igo_m)
  
  # subset pa to keep only the rows corresponding to the countries listed in the distance matrix
  pab_mc <- as.data.frame(pab_m)
  pab_mc <- subset(pab_m, rownames(pab_m) %in% rownames(matenv_igo_m))
  
  # subset distance matrix to keep only the rows corresponding to the countries listed in pa
  matenv_igo_m <- as.data.frame(matenv_igo_m)
  matenv_igo_m <- subset(matenv_igo_m, rownames(matenv_igo_m) %in% rownames(pab_mc))
  matenv_igo_m <- matenv_igo_m[, colnames(matenv_igo_m) %in% rownames(pab_mc)]
  
  # subset pa to keep only the year of interest
  pab_mc <- pab_mc[, colnames(pab_mc) == as.character(y)]
  
  # replace missing values by zeros to be able to perform the matrix multiplication
  pab_mc[is.na(pab_mc)] <- 0
  
  # matrix multiplication - building the spatial lag
  pab_mc <- data.matrix(pab_mc)
  matenv_igo_m <- as.matrix(matenv_igo_m)
  env_igolagb <- as.data.frame(matenv_igo_m %*% pab_mc)
  
  # add column for country
  env_igolagb$country <- rownames(env_igolagb)
  
  # add column for year
  if(nrow(env_igolagb) != 0) {
    env_igolagb$year <- y}
  
  # assign to object
  assign(paste0("pa_env_igolagb", y), env_igolagb, env = .GlobalEnv)
  
}



ilistb <- list()

# create a list of data frames to combine
for (i in 1:length(all_iyears)) {
  
  io <- get(paste0("pa_env_igolagb", all_iyears[i]))
  
  ilistb[[i]] <- io
  
  assign("ilistb", ilistb, env = .GlobalEnv)
}

# combine the datasets into a long dataset
long_pa_env_igolagb <- do.call(rbind, ilistb)

# display each country each year
all_ycb <- expand.grid(country = unique(long_pa_env_igolagb$country), year = all_iyears)

long_pa_env_igolagb <- merge(long_pa_env_igolagb, all_ycb, by.x = c("year", "country"), by.y = c("year", "country"), all = T)

long_pa_env_igolagb <- as.data.frame(long_pa_env_igolagb)

# create column for the values lagged by a year
long_pa_env_igolagb$ylag <- long_pa_env_igolagb$V1

# make year variable numeric
long_pa_env_igolagb$year <- as.numeric(long_pa_env_igolagb$year)

# lag the values
for (i in 1:length(long_pa_env_igolagb$country)) {
  if(long_pa_env_igolagb$year[i] == min(long_pa_env_igolagb$year)) {
    long_pa_env_igolagb$ylag[i] <- NA
  } 
  else  {
    long_pa_env_igolagb$ylag[i] <- long_pa_env_igolagb$V1[long_pa_env_igolagb$country == long_pa_env_igolagb$country[i] & long_pa_env_igolagb$year == ((long_pa_env_igolagb$year[i]) - 1)] }
  
}

# change column names
colnames(long_pa_env_igolagb)[3:4] <- c("env_igo_lag", "time_env_igo_lag")

write.csv(long_pa_env_igolagb, "long_pa_env_igolagb.csv")


# check for missing variables
long_pa_env_igolagb_ex <- merge(long_pa_env_igolagb, country_ex, by =  c("year", "country"), all.y = T)

table(is.na(long_pa_env_igolagb_ex$env_igo_lag))

# save file
write.csv(long_pa_env_igolagb_ex, "long_pa_env_igolagb_ex.csv")



## Proportion of land area protected outcome variable ##

all_iyears <- 1961:2014

for(y in all_iyears) {
  matenv_igo <- get(paste0("env_igo_adj", y))
  
  # turn into a numeric matrix
  matenv_igo_m <- as.matrix(matenv_igo)
  matenv_igo_m <- as.numeric(matenv_igo_m)
  matenv_igo_m <- matrix(matenv_igo_m, ncol = length(colnames(matenv_igo)))
  
  # assign column + row names
  colnames(matenv_igo_m) <- rownames(matenv_igo)
  rownames(matenv_igo_m) <- colnames(matenv_igo_m)
  
  # subset pa to keep only the rows corresponding to the countries listed in the distance matrix
  paa_mc <- as.data.frame(paa_m)
  paa_mc <- subset(paa_m, rownames(paa_m) %in% rownames(matenv_igo_m))
  
  # subset distance matrix to keep only the rows corresponding to the countries listed in pa
  matenv_igo_m <- as.data.frame(matenv_igo_m)
  matenv_igo_m <- subset(matenv_igo_m, rownames(matenv_igo_m) %in% rownames(paa_mc))
  matenv_igo_m <- matenv_igo_m[, colnames(matenv_igo_m) %in% rownames(paa_mc)]
  
  # subset pa to keep only the year of interest
  paa_mc <- paa_mc[, colnames(paa_mc) == as.character(y)]
  
  # replace missing values by zeros to be able to perform the matrix multiplication
  paa_mc[is.na(paa_mc)] <- 0
  
  # matrix multiplication - building the spatial lag
  paa_mc <- data.matrix(paa_mc)
  matenv_igo_m <- as.matrix(matenv_igo_m)
  env_igolag <- as.data.frame(matenv_igo_m %*% paa_mc)
  
  # add column for country
  env_igolag$country <- rownames(env_igolag)
  
  # add column for year
  if(nrow(env_igolag) != 0) {
    env_igolag$year <- y}
  
  # assign to object
  assign(paste0("pa_env_igolag", y), env_igolag, env = .GlobalEnv)
  
}



ilist <- list()

# create a list of data frames to combine
for (i in 1:length(all_iyears)) {
  
  io <- get(paste0("pa_env_igolag", all_iyears[i]))
  
  ilist[[i]] <- io
  
  assign("ilist", ilist, env = .GlobalEnv)
}

# combine the datasets into a long dataset
long_pa_env_igolag <- do.call(rbind, ilist)

# display each country each year
all_yc <- expand.grid(country = unique(long_pa_env_igolag$country), year = all_iyears)

long_pa_env_igolag <- merge(long_pa_env_igolag, all_yc, by.x = c("year", "country"), by.y = c("year", "country"), all = T)

long_pa_env_igolag <- as.data.frame(long_pa_env_igolag)

# create column for the values lagged by a year
long_pa_env_igolag$ylag <- long_pa_env_igolag$V1

# make year variable numeric
long_pa_env_igolag$year <- as.numeric(long_pa_env_igolag$year)

# lag the values
for (i in 1:length(long_pa_env_igolag$country)) {
  if(long_pa_env_igolag$year[i] == min(long_pa_env_igolag$year)) {
    long_pa_env_igolag$ylag[i] <- NA
  } 
  else  {
    long_pa_env_igolag$ylag[i] <- long_pa_env_igolag$V1[long_pa_env_igolag$country == long_pa_env_igolag$country[i] & long_pa_env_igolag$year == ((long_pa_env_igolag$year[i]) - 1)] }
  
}

# change column names
colnames(long_pa_env_igolag)[3:4] <- c("env_igo_lag", "time_env_igo_lag")

write.csv(long_pa_env_igolag, "long_pa_env_igolag.csv")

# check for missing variables
long_pa_env_igolag_ex <- merge(long_pa_env_igolag, country_ex, by =  c("year", "country"), all.y = T)

table(is.na(long_pa_env_igolag_ex$env_igo_lag))

# save file
write.csv(long_pa_env_igolag_ex, "long_pa_env_igolag_ex.csv")


## Percentage change in land area protected outcome variable ##

all_iyears <- 1961:2014

for(y in all_iyears) {
  matenv_igo <- get(paste0("env_igo_adj", y))
  
  # turn into a numeric matrix
  matenv_igo_m <- as.matrix(matenv_igo)
  matenv_igo_m <- as.numeric(matenv_igo_m)
  matenv_igo_m <- matrix(matenv_igo_m, ncol = length(colnames(matenv_igo)))
  
  # assign column + row names
  colnames(matenv_igo_m) <- rownames(matenv_igo)
  rownames(matenv_igo_m) <- colnames(matenv_igo_m)
  
  # subset pa to keep only the rows corresponding to the countries listed in the distance matrix
  paa_mc <- subset(paapc_m, rownames(paapc_m) %in% rownames(matenv_igo_m))
  
  # subset distance matrix to keep only the rows corresponding to the countries listed in pa
  matenv_igo_m <- as.data.frame(matenv_igo_m)
  matenv_igo_m <- subset(matenv_igo_m, rownames(matenv_igo_m) %in% rownames(paa_mc))
  matenv_igo_m <- matenv_igo_m[, colnames(matenv_igo_m) %in% rownames(paa_mc)]
  
  # subset pa to keep only the year of interest
  paa_mc <- paa_mc[, colnames(paa_mc) == as.character(y)]
  
  # replace missing values by zeros to be able to perform the matrix multiplication
  paa_mc[is.na(paa_mc)] <- 0
  
  # matrix multiplication - building the spatial lag
  paa_mc <- data.matrix(paa_mc)
  matenv_igo_m <- as.matrix(matenv_igo_m)
  env_igolag <- as.data.frame(matenv_igo_m %*% paa_mc)
  
  # add column for country
  env_igolag$country <- rownames(env_igolag)
  
  # add column for year
  if(nrow(env_igolag) != 0) {
    env_igolag$year <- y}
  
  # assign to object
  assign(paste0("papc_env_igolag", y), env_igolag, env = .GlobalEnv)
  
}



ilist <- list()

# create a list of data frames to combine
for (i in 1:length(all_iyears)) {
  
  io <- get(paste0("papc_env_igolag", all_iyears[i]))
  
  ilist[[i]] <- io
  
  assign("ilist", ilist, env = .GlobalEnv)
}

# combine the datasets into a long dataset
long_papc_env_igolag <- do.call(rbind, ilist)

# display each country each year
all_yc <- expand.grid(country = unique(long_papc_env_igolag$country), year = all_iyears)

long_papc_env_igolag <- merge(long_papc_env_igolag, all_yc, by.x = c("year", "country"), by.y = c("year", "country"), all = T)

long_papc_env_igolag <- as.data.frame(long_papc_env_igolag)

# create column for the values lagged by a year
long_papc_env_igolag$ylag <- long_papc_env_igolag$V1

# make year variable numeric
long_papc_env_igolag$year <- as.numeric(long_papc_env_igolag$year)

# lag the values
for (i in 1:length(long_papc_env_igolag$country)) {
  if(long_papc_env_igolag$year[i] == min(long_papc_env_igolag$year)) {
    long_papc_env_igolag$ylag[i] <- NA
  } 
  else  {
    long_papc_env_igolag$ylag[i] <- long_papc_env_igolag$V1[long_papc_env_igolag$country == long_papc_env_igolag$country[i] & long_papc_env_igolag$year == ((long_papc_env_igolag$year[i]) - 1)] }
  
}

# change column names
colnames(long_papc_env_igolag)[3:4] <- c("env_igo_lag_pc", "time_env_igo_lag_pc")

write.csv(long_papc_env_igolag, "long_papc_env_igolag.csv")

# check for missing variables
long_papc_env_igolag_ex <- merge(long_papc_env_igolag, country_ex, by =  c("year", "country"), all.y = T)

table(is.na(long_papc_env_igolag_ex$env_igo_lag_pc))

# save file
write.csv(long_papc_env_igolag_ex, "long_papc_env_igolag_ex.csv")



## Percentage point change in land area protected outcome variable ##

all_iyears <- 1961:2014

for(y in all_iyears) {
  matenv_igo <- get(paste0("env_igo_adj", y))
  
  # turn into a numeric matrix
  matenv_igo_m <- as.matrix(matenv_igo)
  matenv_igo_m <- as.numeric(matenv_igo_m)
  matenv_igo_m <- matrix(matenv_igo_m, ncol = length(colnames(matenv_igo)))
  
  # assign column + row names
  colnames(matenv_igo_m) <- rownames(matenv_igo)
  rownames(matenv_igo_m) <- colnames(matenv_igo_m)
  
  # subset pa to keep only the rows corresponding to the countries listed in the distance matrix
  paa_mc <- subset(paapp_m, rownames(paapp_m) %in% rownames(matenv_igo_m))
  
  # subset distance matrix to keep only the rows corresponding to the countries listed in pa
  matenv_igo_m <- as.data.frame(matenv_igo_m)
  matenv_igo_m <- subset(matenv_igo_m, rownames(matenv_igo_m) %in% rownames(paa_mc))
  matenv_igo_m <- matenv_igo_m[, colnames(matenv_igo_m) %in% rownames(paa_mc)]
  
  # subset pa to keep only the year of interest
  paa_mc <- paa_mc[, colnames(paa_mc) == as.character(y)]
  
  # replace missing values by zeros to be able to perform the matrix multiplication
  paa_mc[is.na(paa_mc)] <- 0
  
  # matrix multiplication - building the spatial lag
  paa_mc <- data.matrix(paa_mc)
  matenv_igo_m <- as.matrix(matenv_igo_m)
  env_igolag <- as.data.frame(matenv_igo_m %*% paa_mc)
  
  # add column for country
  env_igolag$country <- rownames(env_igolag)
  
  # add column for year
  if(nrow(env_igolag) != 0) {
    env_igolag$year <- y}
  
  # assign to object
  assign(paste0("papp_env_igolag", y), env_igolag, env = .GlobalEnv)
  
}



ilist <- list()

# create a list of data frames to combine
for (i in 1:length(all_iyears)) {
  
  io <- get(paste0("papp_env_igolag", all_iyears[i]))
  
  ilist[[i]] <- io
  
  assign("ilist", ilist, env = .GlobalEnv)
}

# combine the datasets into a long dataset
long_papp_env_igolag <- do.call(rbind, ilist)

# display each country each year
all_yc <- expand.grid(country = unique(long_papp_env_igolag$country), year = all_iyears)

long_papp_env_igolag <- merge(long_papp_env_igolag, all_yc, by.x = c("year", "country"), by.y = c("year", "country"), all = T)

long_papp_env_igolag <- as.data.frame(long_papp_env_igolag)

# create column for the values lagged by a year
long_papp_env_igolag$ylag <- long_papp_env_igolag$V1

# make year variable numeric
long_papp_env_igolag$year <- as.numeric(long_papp_env_igolag$year)

# lag the values
for (i in 1:length(long_papc_env_igolag$country)) {
  if(long_papp_env_igolag$year[i] == min(long_papp_env_igolag$year)) {
    long_papp_env_igolag$ylag[i] <- NA
  } 
  else  {
    long_papp_env_igolag$ylag[i] <- long_papp_env_igolag$V1[long_papp_env_igolag$country == long_papp_env_igolag$country[i] & long_papp_env_igolag$year == ((long_papp_env_igolag$year[i]) - 1)] }
  
}

# change column names
colnames(long_papp_env_igolag)[3:4] <- c("env_igo_lag_pp", "time_env_igo_lag_pp")

write.csv(long_papp_env_igolag, "long_papp_env_igolag.csv")

# check for missing variables
long_papp_env_igolag_ex <- merge(long_papp_env_igolag, country_ex, by =  c("year", "country"), all.y = T)

table(is.na(long_papp_env_igolag_ex$env_igo_lag_pp))

# save file
write.csv(long_papp_env_igolag_ex, "long_papp_env_igolag_ex.csv")





#### Environmental IGO network lag - limited influences ####


## Binary outcome variable ##

all_iyears <- 1961:2014

for(y in all_iyears) {
  matenv_igo <- get(paste0("env_igo_adj", y, "_20"))
  
  # turn into a numeric matrix
  matenv_igo_m <- as.matrix(matenv_igo)
  matenv_igo_m <- as.numeric(matenv_igo_m)
  matenv_igo_m <- matrix(matenv_igo_m, ncol = length(colnames(matenv_igo)))
  
  # assign column + row names
  colnames(matenv_igo_m) <- rownames(matenv_igo)
  rownames(matenv_igo_m) <- colnames(matenv_igo_m)
  
  # subset pa to keep only the rows corresponding to the countries listed in the distance matrix
  pab_mc <- as.data.frame(pab_m)
  pab_mc <- subset(pab_m, rownames(pab_m) %in% rownames(matenv_igo_m))
  
  # subset distance matrix to keep only the rows corresponding to the countries listed in pa
  matenv_igo_m <- as.data.frame(matenv_igo_m)
  matenv_igo_m <- subset(matenv_igo_m, rownames(matenv_igo_m) %in% rownames(pab_mc))
  matenv_igo_m <- matenv_igo_m[, colnames(matenv_igo_m) %in% rownames(pab_mc)]
  
  # subset pa to keep only the year of interest
  pab_mc <- pab_mc[, colnames(pab_mc) == as.character(y)]
  
  # replace missing values by zeros to be able to perform the matrix multiplication
  pab_mc[is.na(pab_mc)] <- 0
  
  # matrix multiplication - building the spatial lag
  pab_mc <- data.matrix(pab_mc)
  matenv_igo_m <- as.matrix(matenv_igo_m)
  env_igolagb <- as.data.frame(matenv_igo_m %*% pab_mc)
  
  # add column for country
  env_igolagb$country <- rownames(env_igolagb)
  
  # add column for year
  if(nrow(env_igolagb) != 0) {
    env_igolagb$year <- y}
  
  # assign to object
  assign(paste0("pa_env_igolagb", y, "_20"), env_igolagb, env = .GlobalEnv)
  
}



ilistb <- list()

# create a list of data frames to combine
for (i in 1:length(all_iyears)) {
  
  io <- get(paste0("pa_env_igolagb", all_iyears[i], "_20"))
  
  ilistb[[i]] <- io
  
  assign("ilistb", ilistb, env = .GlobalEnv)
}

# combine the datasets into a long dataset
long_pa_env_igolagb_20 <- do.call(rbind, ilistb)

# display each country each year
all_ycb <- expand.grid(country = unique(long_pa_env_igolagb_20$country), year = all_iyears)

long_pa_env_igolagb_20 <- merge(long_pa_env_igolagb_20, all_ycb, by.x = c("year", "country"), by.y = c("year", "country"), all = T)

long_pa_env_igolagb_20 <- as.data.frame(long_pa_env_igolagb_20)

# create column for the values lagged by a year
long_pa_env_igolagb_20$ylag <- long_pa_env_igolagb_20$V1

# make year variable numeric
long_pa_env_igolagb_20$year <- as.numeric(long_pa_env_igolagb_20$year)

# lag the values
for (i in 1:length(long_pa_env_igolagb_20$country)) {
  if(long_pa_env_igolagb_20$year[i] == min(long_pa_env_igolagb_20$year)) {
    long_pa_env_igolagb_20$ylag[i] <- NA
  } 
  else  {
    long_pa_env_igolagb_20$ylag[i] <- long_pa_env_igolagb_20$V1[long_pa_env_igolagb_20$country == long_pa_env_igolagb_20$country[i] & long_pa_env_igolagb_20$year == ((long_pa_env_igolagb_20$year[i]) - 1)] }
  
}

# change column names
colnames(long_pa_env_igolagb_20)[3:4] <- c("env_igo_lag_20", "time_env_igo_lag_20")

write.csv(long_pa_env_igolagb_20, "long_pa_env_igolagb_20.csv")


# check for missing variables
long_pa_env_igolagb_ex_20 <- merge(long_pa_env_igolagb_20, country_ex, by =  c("year", "country"), all.y = T)

table(is.na(long_pa_env_igolagb_ex_20$env_igo_lag_20))

# save file
write.csv(long_pa_env_igolagb_ex_20, "long_pa_env_igolagb_ex_20.csv")



## Proportion of land area protected outcome variable ##

all_iyears <- 1961:2014

for(y in all_iyears) {
  matenv_igo <- get(paste0("env_igo_adj", y, "_20"))
  
  # turn into a numeric matrix
  matenv_igo_m <- as.matrix(matenv_igo)
  matenv_igo_m <- as.numeric(matenv_igo_m)
  matenv_igo_m <- matrix(matenv_igo_m, ncol = length(colnames(matenv_igo)))
  
  # assign column + row names
  colnames(matenv_igo_m) <- rownames(matenv_igo)
  rownames(matenv_igo_m) <- colnames(matenv_igo_m)
  
  # subset pa to keep only the rows corresponding to the countries listed in the distance matrix
  paa_mc <- as.data.frame(paa_m)
  paa_mc <- subset(paa_m, rownames(paa_m) %in% rownames(matenv_igo_m))
  
  # subset distance matrix to keep only the rows corresponding to the countries listed in pa
  matenv_igo_m <- as.data.frame(matenv_igo_m)
  matenv_igo_m <- subset(matenv_igo_m, rownames(matenv_igo_m) %in% rownames(paa_mc))
  matenv_igo_m <- matenv_igo_m[, colnames(matenv_igo_m) %in% rownames(paa_mc)]
  
  # subset pa to keep only the year of interest
  paa_mc <- paa_mc[, colnames(paa_mc) == as.character(y)]
  
  # replace missing values by zeros to be able to perform the matrix multiplication
  paa_mc[is.na(paa_mc)] <- 0
  
  # matrix multiplication - building the spatial lag
  paa_mc <- data.matrix(paa_mc)
  matenv_igo_m <- as.matrix(matenv_igo_m)
  env_igolag <- as.data.frame(matenv_igo_m %*% paa_mc)
  
  # add column for country
  env_igolag$country <- rownames(env_igolag)
  
  # add column for year
  if(nrow(env_igolag) != 0) {
    env_igolag$year <- y}
  
  # assign to object
  assign(paste0("pa_env_igolag", y, "_20"), env_igolag, env = .GlobalEnv)
  
}



ilist <- list()

# create a list of data frames to combine
for (i in 1:length(all_iyears)) {
  
  io <- get(paste0("pa_env_igolag", all_iyears[i], "_20"))
  
  ilist[[i]] <- io
  
  assign("ilist", ilist, env = .GlobalEnv)
}

# combine the datasets into a long dataset
long_pa_env_igolag_20 <- do.call(rbind, ilist)

# display each country each year
all_yc <- expand.grid(country = unique(long_pa_env_igolag_20$country), year = all_iyears)

long_pa_env_igolag_20 <- merge(long_pa_env_igolag_20, all_yc, by.x = c("year", "country"), by.y = c("year", "country"), all = T)

long_pa_env_igolag_20 <- as.data.frame(long_pa_env_igolag_20)

# create column for the values lagged by a year
long_pa_env_igolag_20$ylag <- long_pa_env_igolag_20$V1

# make year variable numeric
long_pa_env_igolag_20$year <- as.numeric(long_pa_env_igolag_20$year)

# lag the values
for (i in 1:length(long_pa_env_igolag_20$country)) {
  if(long_pa_env_igolag_20$year[i] == min(long_pa_env_igolag_20$year)) {
    long_pa_env_igolag_20$ylag[i] <- NA
  } 
  else  {
    long_pa_env_igolag_20$ylag[i] <- long_pa_env_igolag_20$V1[long_pa_env_igolag_20$country == long_pa_env_igolag_20$country[i] & long_pa_env_igolag_20$year == ((long_pa_env_igolag_20$year[i]) - 1)] }
  
}

# change column names
colnames(long_pa_env_igolag_20)[3:4] <- c("env_igo_lag_20", "time_env_igo_lag_20")

write.csv(long_pa_env_igolag_20, "long_pa_env_igolag_20.csv")

# check for missing variables
long_pa_env_igolag_ex_20 <- merge(long_pa_env_igolag_20, country_ex, by =  c("year", "country"), all.y = T)

table(is.na(long_pa_env_igolag_ex_20$env_igo_lag_20))

# save file
write.csv(long_pa_env_igolag_ex_20, "long_pa_env_igolag_ex_20.csv")


## Percentage change in land area protected outcome variable ##

all_iyears <- 1961:2014

for(y in all_iyears) {
  matenv_igo <- get(paste0("env_igo_adj", y, "_20"))
  
  # turn into a numeric matrix
  matenv_igo_m <- as.matrix(matenv_igo)
  matenv_igo_m <- as.numeric(matenv_igo_m)
  matenv_igo_m <- matrix(matenv_igo_m, ncol = length(colnames(matenv_igo)))
  
  # assign column + row names
  colnames(matenv_igo_m) <- rownames(matenv_igo)
  rownames(matenv_igo_m) <- colnames(matenv_igo_m)
  
  # subset pa to keep only the rows corresponding to the countries listed in the distance matrix
  paa_mc <- subset(paapc_m, rownames(paapc_m) %in% rownames(matenv_igo_m))
  
  # subset distance matrix to keep only the rows corresponding to the countries listed in pa
  matenv_igo_m <- as.data.frame(matenv_igo_m)
  matenv_igo_m <- subset(matenv_igo_m, rownames(matenv_igo_m) %in% rownames(paa_mc))
  matenv_igo_m <- matenv_igo_m[, colnames(matenv_igo_m) %in% rownames(paa_mc)]
  
  # subset pa to keep only the year of interest
  paa_mc <- paa_mc[, colnames(paa_mc) == as.character(y)]
  
  # replace missing values by zeros to be able to perform the matrix multiplication
  paa_mc[is.na(paa_mc)] <- 0
  
  # matrix multiplication - building the spatial lag
  paa_mc <- data.matrix(paa_mc)
  matenv_igo_m <- as.matrix(matenv_igo_m)
  env_igolag <- as.data.frame(matenv_igo_m %*% paa_mc)
  
  # add column for country
  env_igolag$country <- rownames(env_igolag)
  
  # add column for year
  if(nrow(env_igolag) != 0) {
    env_igolag$year <- y}
  
  # assign to object
  assign(paste0("papc_env_igolag", y, "_20"), env_igolag, env = .GlobalEnv)
  
}



ilist <- list()

# create a list of data frames to combine
for (i in 1:length(all_iyears)) {
  
  io <- get(paste0("papc_env_igolag", all_iyears[i], "_20"))
  
  ilist[[i]] <- io
  
  assign("ilist", ilist, env = .GlobalEnv)
}

# combine the datasets into a long dataset
long_papc_env_igolag_20 <- do.call(rbind, ilist)

# display each country each year
all_yc <- expand.grid(country = unique(long_papc_env_igolag_20$country), year = all_iyears)

long_papc_env_igolag_20 <- merge(long_papc_env_igolag_20, all_yc, by.x = c("year", "country"), by.y = c("year", "country"), all = T)

long_papc_env_igolag_20 <- as.data.frame(long_papc_env_igolag_20)

# create column for the values lagged by a year
long_papc_env_igolag_20$ylag <- long_papc_env_igolag_20$V1

# make year variable numeric
long_papc_env_igolag_20$year <- as.numeric(long_papc_env_igolag_20$year)

# lag the values
for (i in 1:length(long_papc_env_igolag_20$country)) {
  if(long_papc_env_igolag_20$year[i] == min(long_papc_env_igolag_20$year)) {
    long_papc_env_igolag_20$ylag[i] <- NA
  } 
  else  {
    long_papc_env_igolag_20$ylag[i] <- long_papc_env_igolag_20$V1[long_papc_env_igolag_20$country == long_papc_env_igolag_20$country[i] & long_papc_env_igolag_20$year == ((long_papc_env_igolag_20$year[i]) - 1)] }
  
}

# change column names
colnames(long_papc_env_igolag_20)[3:4] <- c("env_igo_lag_pc_20", "time_env_igo_lag_pc_20")

write.csv(long_papc_env_igolag_20, "long_papc_env_igolag_20.csv")

# check for missing variables
long_papc_env_igolag_ex_20 <- merge(long_papc_env_igolag_20, country_ex, by =  c("year", "country"), all.y = T)

table(is.na(long_papc_env_igolag_ex_20$env_igo_lag_pc_20))

# save file
write.csv(long_papc_env_igolag_ex_20, "long_papc_env_igolag_ex_20.csv")



## Percentage point change in land area protected outcome variable ##

all_iyears <- 1961:2014

for(y in all_iyears) {
  matenv_igo <- get(paste0("env_igo_adj", y, "_20"))
  
  # turn into a numeric matrix
  matenv_igo_m <- as.matrix(matenv_igo)
  matenv_igo_m <- as.numeric(matenv_igo_m)
  matenv_igo_m <- matrix(matenv_igo_m, ncol = length(colnames(matenv_igo)))
  
  # assign column + row names
  colnames(matenv_igo_m) <- rownames(matenv_igo)
  rownames(matenv_igo_m) <- colnames(matenv_igo_m)
  
  # subset pa to keep only the rows corresponding to the countries listed in the distance matrix
  paa_mc <- subset(paapp_m, rownames(paapp_m) %in% rownames(matenv_igo_m))
  
  # subset distance matrix to keep only the rows corresponding to the countries listed in pa
  matenv_igo_m <- as.data.frame(matenv_igo_m)
  matenv_igo_m <- subset(matenv_igo_m, rownames(matenv_igo_m) %in% rownames(paa_mc))
  matenv_igo_m <- matenv_igo_m[, colnames(matenv_igo_m) %in% rownames(paa_mc)]
  
  # subset pa to keep only the year of interest
  paa_mc <- paa_mc[, colnames(paa_mc) == as.character(y)]
  
  # replace missing values by zeros to be able to perform the matrix multiplication
  paa_mc[is.na(paa_mc)] <- 0
  
  # matrix multiplication - building the spatial lag
  paa_mc <- data.matrix(paa_mc)
  matenv_igo_m <- as.matrix(matenv_igo_m)
  env_igolag <- as.data.frame(matenv_igo_m %*% paa_mc)
  
  # add column for country
  env_igolag$country <- rownames(env_igolag)
  
  # add column for year
  if(nrow(env_igolag) != 0) {
    env_igolag$year <- y}
  
  # assign to object
  assign(paste0("papp_env_igolag", y, "_20"), env_igolag, env = .GlobalEnv)
  
}



ilist <- list()

# create a list of data frames to combine
for (i in 1:length(all_iyears)) {
  
  io <- get(paste0("papp_env_igolag", all_iyears[i], "_20"))
  
  ilist[[i]] <- io
  
  assign("ilist", ilist, env = .GlobalEnv)
}

# combine the datasets into a long dataset
long_papp_env_igolag_20 <- do.call(rbind, ilist)

# display each country each year
all_yc <- expand.grid(country = unique(long_papp_env_igolag_20$country), year = all_iyears)

long_papp_env_igolag_20 <- merge(long_papp_env_igolag_20, all_yc, by.x = c("year", "country"), by.y = c("year", "country"), all = T)

long_papp_env_igolag_20 <- as.data.frame(long_papp_env_igolag_20)

# create column for the values lagged by a year
long_papp_env_igolag_20$ylag <- long_papp_env_igolag_20$V1

# make year variable numeric
long_papp_env_igolag_20$year <- as.numeric(long_papp_env_igolag_20$year)

# lag the values
for (i in 1:length(long_papc_env_igolag_20$country)) {
  if(long_papp_env_igolag_20$year[i] == min(long_papp_env_igolag_20$year)) {
    long_papp_env_igolag_20$ylag[i] <- NA
  } 
  else  {
    long_papp_env_igolag_20$ylag[i] <- long_papp_env_igolag_20$V1[long_papp_env_igolag_20$country == long_papp_env_igolag_20$country[i] & long_papp_env_igolag_20$year == ((long_papp_env_igolag_20$year[i]) - 1)] }
  
}

# change column names
colnames(long_papp_env_igolag_20)[3:4] <- c("env_igo_lag_pp_20", "time_env_igo_lag_pp_20")

write.csv(long_papp_env_igolag_20, "long_papp_env_igolag_20.csv")

# check for missing variables
long_papp_env_igolag_ex_20 <- merge(long_papp_env_igolag_20, country_ex, by =  c("year", "country"), all.y = T)

table(is.na(long_papp_env_igolag_ex_20$env_igo_lag_pp_20))

# save file
write.csv(long_papp_env_igolag_ex_20, "long_papp_env_igolag_ex_20.csv")




#### Geographical lag ####


## Binary outcome variable ##

for(y in 1961:2016) {
  matb <- get(paste0("matb", y))
  
  # turn into a numeric matrix
  matb_m <- as.matrix(matb)
  matb_m <- as.numeric(matb_m)
  matb_m <- matrix(matb_m, ncol = length(colnames(matb)))
  
  # assign column + row names
  colnames(matb_m) <- rownames(matb)
  rownames(matb_m) <- colnames(matb_m)
  
  # subset pa to keep only the rows corresponding to the countries listed in the distance matrix
  pab_mc <- subset(pab_m, rownames(pab_m) %in% rownames(matb_m))
  
  # subset distance matrix to keep only the rows corresponding to the countries listed in pa
  matb_m <- subset(matb_m, rownames(matb_m) %in% rownames(pab_mc))
  matb_m <- matb_m[, colnames(matb_m) %in% rownames(pab_mc)]
  
  # subset pa to keep only the year of interest
  pab_mc <- pab_mc[, paste0(y)]
  
  # matrix multiplication - building the spatial lag
  geoglagb <- as.data.frame(matb_m %*% pab_mc)
  
  # add column for country
  geoglagb$country <- rownames(geoglagb)
  
  # add column for year
  geoglagb$year <- y
  
  # assign to object
  assign(paste0("pa_geoglagb", y), geoglagb, env = .GlobalEnv)
  
}

# create a list of data frames to combine
glistb <- list()
years <- 1961:2016

for (i in 1:length(years)) {
  
  g <- get(paste0("pa_geoglagb", years[i]))
  
  glistb[[i]] <- g
  
  assign("glist", glistb, env = .GlobalEnv)
}

# combine the datasets into a long dataset
long_pa_geoglagb <- do.call(rbind, glistb)

# display each country each year in the dataset
all_yc <- expand.grid(country = unique(long_pa_geoglagb$country), year = years)

long_pa_geoglagb <- merge(long_pa_geoglagb, all_yc, by.x = c("year", "country"), by.y = c("year", "country"), all = T)

long_pa_geoglagb <- as.data.frame(long_pa_geoglagb)


# create column for the values lagged by a year
long_pa_geoglagb$ylag <- long_pa_geoglagb$value

# make year variable numeric
long_pa_geoglagb$year <- as.numeric(long_pa_geoglagb$year)

# lag the values
for (i in 1:length(long_pa_geoglagb$country)) {
  if(long_pa_geoglagb$year[i] == 1961) {
    long_pa_geoglagb$ylag[i] <- NA
  } else {
    long_pa_geoglagb$ylag[i] <- long_pa_geoglagb$V1[long_pa_geoglagb$country == long_pa_geoglagb$country[i] & long_pa_geoglagb$year == ((long_pa_geoglagb$year[i]) - 1)] }
}

# change column names
colnames(long_pa_geoglagb)[3:4] <- c("spatial_lag", "time_spatial_lag")

# save the object
write.csv(long_pa_geoglagb, "long_pa_geoglagb.csv")


long_pa_geoglagb_ex <- merge(long_pa_geoglagb, country_ex, by =  c("year", "country"), all.y = T)

table(is.na(long_pa_geoglagb_ex$spatial_lag))

# save the object
write.csv(long_pa_geoglagb_ex, "long_pa_geoglagb_ex.csv")




## Proportion of total land ##

for(y in 1961:2016) {
  matb <- get(paste0("matb", y))
  
  # turn into a numeric matrix
  matb_m <- as.matrix(matb)
  matb_m <- as.numeric(matb_m)
  matb_m <- matrix(matb_m, ncol = length(colnames(matb)))
  
  # assign column + row names
  colnames(matb_m) <- rownames(matb)
  rownames(matb_m) <- colnames(matb_m)
  
  # subset pa to keep only the rows corresponding to the countries listed in the distance matrix
  paa_mc <- subset(paa_m, rownames(paa_m) %in% rownames(matb_m))
  
  # subset distance matrix to keep only the rows corresponding to the countries listed in pa
  matb_m <- subset(matb_m, rownames(matb_m) %in% rownames(paa_mc))
  matb_m <- matb_m[, colnames(matb_m) %in% rownames(paa_mc)]
  
  # subset pa to keep only the year of interest
  paa_mc <- paa_mc[, paste0(y)]
  
  # matrix multiplication - building the spatial lag
  geoglag <- as.data.frame(matb_m %*% paa_mc)
  
  # replace missing values by zeros to be able to perform the matrix multiplication
  paa_mc[is.na(paa_mc)] <- 0
  
  # add column for country
  geoglag$country <- rownames(geoglag)
  
  # add column for year
  geoglag$year <- y
  
  # assign to object
  assign(paste0("pa_geoglag", y), geoglag, env = .GlobalEnv)
  
}

# create a list of data frames to combine
glist <- list()
years <- 1961:2016

for (i in 1:length(years)) {
  
  g <- get(paste0("pa_geoglag", years[i]))
  
  glist[[i]] <- g
  
  assign("glist", glist, env = .GlobalEnv)
}

# combine the datasets into a long dataset
long_pa_geoglag <- do.call(rbind, glist)

# display each country each year in the dataset
all_yc <- expand.grid(country = unique(long_pa_geoglag$country), year = years)

long_pa_geoglag <- merge(long_pa_geoglag, all_yc, by.x = c("year", "country"), by.y = c("year", "country"), all = T)

long_pa_geoglag <- as.data.frame(long_pa_geoglag)


# create column for the values lagged by a year
long_pa_geoglag$ylag <- long_pa_geoglag$value

# make year variable numeric
long_pa_geoglag$year <- as.numeric(long_pa_geoglag$year)

# lag the values
for (i in 1:length(long_pa_geoglag$country)) {
  if(long_pa_geoglag$year[i] == 1961) {
    long_pa_geoglag$ylag[i] <- NA
  } else {
    long_pa_geoglag$ylag[i] <- long_pa_geoglag$V1[long_pa_geoglag$country == long_pa_geoglag$country[i] & long_pa_geoglag$year == ((long_pa_geoglag$year[i]) - 1)] }
}

# change column names
colnames(long_pa_geoglag)[3:4] <- c("spatial_lag", "time_spatial_lag")

# save the object
write.csv(long_pa_geoglag, "long_pa_geoglag.csv")


long_pa_geoglag_ex <- merge(long_pa_geoglag, country_ex, by =  c("year", "country"), all.y = T)

table(is.na(long_pa_geoglag_ex$spatial_lag))

# save the object
write.csv(long_pa_geoglag_ex, "long_pa_geoglag_ex.csv")


## Percentage change in area protected ##

for(y in 1961:2016) {
  matb <- get(paste0("matb", y))
  
  # turn into a numeric matrix
  matb_m <- as.matrix(matb)
  matb_m <- as.numeric(matb_m)
  matb_m <- matrix(matb_m, ncol = length(colnames(matb)))
  
  # assign column + row names
  colnames(matb_m) <- rownames(matb)
  rownames(matb_m) <- colnames(matb_m)
  
  # subset pa to keep only the rows corresponding to the countries listed in the distance matrix
  paa_mc <- subset(paapc_m, rownames(paapc_m) %in% rownames(matb_m))
  
  # subset distance matrix to keep only the rows corresponding to the countries listed in pa
  matb_m <- subset(matb_m, rownames(matb_m) %in% rownames(paa_mc))
  matb_m <- matb_m[, colnames(matb_m) %in% rownames(paa_mc)]
  
  # subset pa to keep only the year of interest
  paa_mc <- paa_mc[, paste0(y)]
  
  
  # matrix multiplication - building the spatial lag
  geoglag <- as.data.frame(matb_m %*% paa_mc)
  
  # replace missing values by zeros to be able to perform the matrix multiplication
  paa_mc[is.na(paa_mc)] <- 0
  
  # add column for country
  geoglag$country <- rownames(geoglag)
  
  # add column for year
  geoglag$year <- y
  
  # assign to object
  assign(paste0("papc_geoglag", y), geoglag, env = .GlobalEnv)
  
}

# create a list of data frames to combine
glist <- list()
years <- 1961:2016

for (i in 1:length(years)) {
  
  g <- get(paste0("papc_geoglag", years[i]))
  
  glist[[i]] <- g
  
  assign("glist", glist, env = .GlobalEnv)
}

# combine the datasets into a long dataset
long_papc_geoglag <- do.call(rbind, glist)

# display each country each year in the dataset
all_yc <- expand.grid(country = unique(long_papc_geoglag$country), year = years)

long_papc_geoglag <- merge(long_papc_geoglag, all_yc, by.x = c("year", "country"), by.y = c("year", "country"), all = T)

long_papc_geoglag <- as.data.frame(long_papc_geoglag)


# create column for the values lagged by a year
long_papc_geoglag$ylag <- long_papc_geoglag$value

# make year variable numeric
long_papc_geoglag$year <- as.numeric(long_papc_geoglag$year)

# lag the values
for (i in 1:length(long_papc_geoglag$country)) {
  if(long_papc_geoglag$year[i] == 1961) {
    long_papc_geoglag$ylag[i] <- NA
  } else {
    long_papc_geoglag$ylag[i] <- long_papc_geoglag$V1[long_papc_geoglag$country == long_papc_geoglag$country[i] & long_papc_geoglag$year == ((long_papc_geoglag$year[i]) - 1)] }
}

# change column names
colnames(long_papc_geoglag)[3:4] <- c("spatial_lag_pc", "time_spatial_lag_pc")

# save the object
write.csv(long_papc_geoglag, "long_papc_geoglag.csv")


# check for misisng values
long_papc_geoglag_ex <- merge(long_papc_geoglag, country_ex, by =  c("year", "country"), all.y = T)

table(is.na(long_papc_geoglag_ex$spatial_lag_pc))

# save the object
write.csv(long_papc_geoglag_ex, "long_papc_geoglag_ex.csv")




## Percentage point change in area protected ##

for(y in 1961:2016) {
  matb <- get(paste0("matb", y))
  
  # turn into a numeric matrix
  matb_m <- as.matrix(matb)
  matb_m <- as.numeric(matb_m)
  matb_m <- matrix(matb_m, ncol = length(colnames(matb)))
  
  # assign column + row names
  colnames(matb_m) <- rownames(matb)
  rownames(matb_m) <- colnames(matb_m)
  
  # subset pa to keep only the rows corresponding to the countries listed in the distance matrix
  paa_mc <- subset(paapp_m, rownames(paapp_m) %in% rownames(matb_m))
  
  # subset distance matrix to keep only the rows corresponding to the countries listed in pa
  matb_m <- subset(matb_m, rownames(matb_m) %in% rownames(paa_mc))
  matb_m <- matb_m[, colnames(matb_m) %in% rownames(paa_mc)]
  
  # subset pa to keep only the year of interest
  paa_mc <- paa_mc[, paste0(y)]
  
  
  # matrix multiplication - building the spatial lag
  geoglag <- as.data.frame(matb_m %*% paa_mc)
  
  # replace missing values by zeros to be able to perform the matrix multiplication
  paa_mc[is.na(paa_mc)] <- 0
  
  # add column for country
  geoglag$country <- rownames(geoglag)
  
  # add column for year
  geoglag$year <- y
  
  # assign to object
  assign(paste0("papp_geoglag", y), geoglag, env = .GlobalEnv)
  
}

# create a list of data frames to combine
glist <- list()
years <- 1961:2016

for (i in 1:length(years)) {
  
  g <- get(paste0("papp_geoglag", years[i]))
  
  glist[[i]] <- g
  
  assign("glist", glist, env = .GlobalEnv)
}

# combine the datasets into a long dataset
long_papp_geoglag <- do.call(rbind, glist)

# display each country each year in the dataset
all_yc <- expand.grid(country = unique(long_papp_geoglag$country), year = years)

long_papp_geoglag <- merge(long_papp_geoglag, all_yc, by.x = c("year", "country"), by.y = c("year", "country"), all = T)

long_papp_geoglag <- as.data.frame(long_papp_geoglag)


# create column for the values lagged by a year
long_papp_geoglag$ylag <- long_papp_geoglag$value

# make year variable numeric
long_papp_geoglag$year <- as.numeric(long_papp_geoglag$year)

# lag the values
for (i in 1:length(long_papp_geoglag$country)) {
  if(long_papp_geoglag$year[i] == 1961) {
    long_papp_geoglag$ylag[i] <- NA
  } else {
    long_papp_geoglag$ylag[i] <- long_papp_geoglag$V1[long_papp_geoglag$country == long_papp_geoglag$country[i] & long_papp_geoglag$year == ((long_papp_geoglag$year[i]) - 1)] }
}

# change column names
colnames(long_papp_geoglag)[3:4] <- c("spatial_lag_pp", "time_spatial_lag_pp")

# save the object
write.csv(long_papp_geoglag, "long_papp_geoglag.csv")


# check for missing values
long_papp_geoglag_ex <- merge(long_papp_geoglag, country_ex, by =  c("year", "country"), all.y = T)

table(is.na(long_papp_geoglag_ex$spatial_lag_pp))

# save the object
write.csv(long_papp_geoglag_ex, "long_papp_geoglag_ex.csv")


