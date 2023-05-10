#### Compilation of the data sets ####


## Create a data frame with all the variables for the binary outcome variable

# merge datasets
data_bin <- merge(long_pa, long_pa_geoglagb_ex, by.x = c("year", "country"), by.y = c("year", "country"), all = T)

data_bin <- merge(data_bin, long_pa_rellagb_ex, by.x = c("year", "country"), by.y = c("year", "country"), all = T)

data_bin <- merge(data_bin, long_pa_tradelagb_ex, by.x = c("year", "country"), by.y = c("year", "country"), all = T)

data_bin <- merge(data_bin, long_pa_tradelagb_ex_20, by.x = c("year", "country"), by.y = c("year", "country"), all = T)

data_bin <- merge(data_bin, long_pa_lglagb_ex, by.x = c("year", "country"), by.y = c("year", "country"), all = T)

data_bin <- merge(data_bin, long_pa_env_igolagb_ex, by.x = c("year", "country"), by.y = c("year", "country"), all = T)

data_bin <- merge(data_bin, long_pa_env_igolagb_ex_20, by.x = c("year", "country"), by.y = c("year", "country"), all = T)

data_bin <- merge(data_bin, gdppc, by.x = c("year", "country"), by.y = c("year", "country"), all.x = T)

data_bin <- merge(data_bin, tradeo, by.x = c("year", "country"), by.y = c("year", "country"), all.x = T)

data_bin <- merge(data_bin, gdpg, by.x = c("year", "country"), by.y = c("year", "country"), all.x = T)

data_bin <- merge(data_bin, fdiprop, by.x = c("year", "country"), by.y = c("year", "country"), all.x = T)

data_bin <- merge(data_bin, odaprop, by.x = c("year", "country"), by.y = c("year", "country"), all.x = T)

data_bin <- merge(data_bin, nrr, by.x = c("year", "country"), by.y = c("year", "country"), all.x = T)

data_bin <- merge(data_bin, popd, by.x = c("year", "country"), by.y = c("year", "country"), all.x = T)

data_bin <- merge(data_bin, fhipr, by.x = c("year", "country"), by.y = c("year", "country"), all.x = T)

data_bin <- merge(data_bin, env_igo_number, by.x = c("year", "country"), by.y = c("year", "country"), all.x = T)

# remove duplicates
data_bin <- data_bin[!duplicated(data_bin), ]

# create a dummy regional variable
data_bin$region <- countrycode(data_bin$country, "iso3c", "region23")






## Create a data frame with all the variables for the percentage point change in area outcome variable

# merge datasets
data_pp <- merge(long_pa, long_papp_geoglag_ex, by.x = c("year", "country"), by.y = c("year", "country"), all = T)

data_pp <- merge(data_pp, long_papp_rellag_ex, by.x = c("year", "country"), by.y = c("year", "country"), all = T)

data_pp <- merge(data_pp, long_papp_tradelag_ex, by.x = c("year", "country"), by.y = c("year", "country"), all = T)

data_pp <- merge(data_pp, long_papp_tradelag_ex_20, by.x = c("year", "country"), by.y = c("year", "country"), all = T)

data_pp <- merge(data_pp, long_papp_lglag_ex, by.x = c("year", "country"), by.y = c("year", "country"), all = T)

data_pp <- merge(data_pp, long_papp_env_igolag_ex, by.x = c("year", "country"), by.y = c("year", "country"), all = T)

data_pp <- merge(data_pp, long_papp_env_igolag_ex_20, by.x = c("year", "country"), by.y = c("year", "country"), all = T)

data_pp <- merge(data_pp, gdppc, by.x = c("year", "country"), by.y = c("year", "country"), all.x = T)

data_pp <- merge(data_pp, tradeo, by.x = c("year", "country"), by.y = c("year", "country"), all.x = T)

data_pp <- merge(data_pp, gdpg, by.x = c("year", "country"), by.y = c("year", "country"), all.x = T)

data_pp <- merge(data_pp, fdiprop, by.x = c("year", "country"), by.y = c("year", "country"), all.x = T)

data_pp <- merge(data_pp, odaprop, by.x = c("year", "country"), by.y = c("year", "country"), all.x = T)

data_pp <- merge(data_pp, nrr, by.x = c("year", "country"), by.y = c("year", "country"), all.x = T)

data_pp <- merge(data_pp, popd, by.x = c("year", "country"), by.y = c("year", "country"), all.x = T)

data_pp <- merge(data_pp, fhipr, by.x = c("year", "country"), by.y = c("year", "country"), all.x = T)

data_pp <- merge(data_pp, env_igo_number, by.x = c("year", "country"), by.y = c("year", "country"), all.x = T)




#### Creation of other variables ####


## creation of a variable containing proportion of countries which have adopted the policy

# replace NAs by 0s
data_bin$pa_b[is.na(data_bin$pa_b)] <- 0
data_pp$pa_b[is.na(data_pp$pa_b)] <- 0

# compute the proportion of countries which have adopted the policy every year
for (i in 1961:2019) {
  
  data_bin$b_prop[data_bin$year == i+1] <- prop.table(table(data_bin$pa_b[data_bin$year == i]))[2]
}

for (i in 1961:2019) {
  
  data_pc$b_prop[data_pc$year == i+1] <- prop.table(table(data_pc$pa_b[data_pc$year == i]))[2]
}


## creation of variable containing the average proportion of the territory area that is protected


for (i in 1961:2019) {
  
  data_pp$av_pp[data_pp$year == i+1] <- mean(data_pp$pa_pp[data_pp$year == i], na.rm = T)
}





#### Creation of "leader" outcome variables ####

# Binary outcome variable
data_bin$US <- NA

for (y in 1961:2019) {
  
  data_bin$US[data_bin$year == y+1] <- data_bin$pa_b[data_bin$country == "USA" & data_bin$year == y]
}


# Percentage point change in area protected outcome variable
data_pp$US <- NA

for (y in 1961:2020) {
  
  data_pp$US[data_pp$year == y+1] <- data_pp$pa_pp[data_pp$country == "USA" & data_pp$year == y]
}


#### If country did not receive any ODA: impute value = 0 (as the countries which donate ODA have NA values) ####

for(i in unique(data_bin$country)) {
  
  if(all(is.na(data_bin$odaprop[data_bin$country == i]))) {
    
    data_bin$odaprop[data_bin$country == i] <- 0
  }
}

for(i in unique(data_pp$country)) {
  
  if(all(is.na(data_pp$odaprop[data_pp$country == i]))) {
    
    data_pp$odaprop[data_pp$country == i] <- 0
  }
}


#### Creation of risk set data sets ####

## Create the risk set dataset for the binary data set
data_bin_riskset <- data_bin

# create a variable indicating whether a country is part of the risk set (condition: not having adopted the policy yet or having adopted it the year right before)
data_bin_riskset$riskset <- rep(NA, nrow(data_bin_riskset))

# remove duplicates
data_bin_riskset <- data_bin_riskset[!duplicated(data_bin_riskset), ]

# remove rows where country is blank
data_bin_riskset <- data_bin_riskset[!data_bin_riskset$country == "", ]

for(i in 1:nrow(data_bin_riskset)){
  
  if(!is.na(data_bin_riskset$pa_b[i])) {
    
    if(data_bin_riskset$year[i] > min(data_bin_riskset$year)){
      
      if(length(data_bin_riskset$pa_b[data_bin_riskset$country == data_bin_riskset$country[i] & data_bin_riskset$year == (data_bin_riskset$year[i] - 1)]) != 0){
        
        if(!is.na(data_bin_riskset$pa_b[data_bin_riskset$country == data_bin_riskset$country[i] & data_bin_riskset$year == (data_bin_riskset$year[i] - 1)])){
          
          if(data_bin_riskset$pa_b[i] == 1){
            
            if(data_bin_riskset$pa_b[data_bin_riskset$country == data_bin_riskset$country[i] & data_bin_riskset$year == (data_bin_riskset$year[i] - 1)] == 0){
              
              data_bin_riskset$riskset[i] <- 1
            }
          }
        }
      }
    }
  }
}

data_bin_riskset$riskset[data_bin_riskset$pa_b == 0] <- 1

# remove the rows which do not fit this definition
data_bin_riskset <- data_bin_riskset[!is.na(data_bin_riskset$riskset), ]



## Create risk set datasets for the proportion/percentage change/percentage point change datasets too

# risk set (= countries which have already adopted the policy)
data_pp_riskset <- data_pp[data_pp$pa_b != 0,]


#### Save the files ####


# data sets
write.csv(data_bin, "data_bin.csv")
write.csv(data_pp, "data_pp.csv")


# risk sets
write.csv(data_bin_riskset, "data_bin_riskset.csv")
write.csv(data_pp_riskset, "data_pp_riskset.csv")





