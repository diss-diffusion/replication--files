#### Country existence data ####

# create a record of the countries which exist each year
country_ex <- cbind(grav$year, grav$iso3_o, grav$country_exists_o)

# make a data frame
country_ex <- as.data.frame(country_ex)

# change column names
names(country_ex) <- c("year", "country", "exists")

# remove duplicates
country_ex <- country_ex[!duplicated(country_ex), ]

# remove territories which are under the authority of another country
country_ex <- country_ex[country_ex$country != "ANT",]
country_ex <- country_ex[country_ex$country != "ASM",]
country_ex <- country_ex[country_ex$country != "BES",]
country_ex <- country_ex[country_ex$country != "BMU",]
country_ex <- country_ex[country_ex$country != "CCK",]
country_ex <- country_ex[country_ex$country != "COK",]
country_ex <- country_ex[country_ex$country != "CUW",]
country_ex <- country_ex[country_ex$country != "CXR",]
country_ex <- country_ex[country_ex$country != "CYM",]
country_ex <- country_ex[country_ex$country != "ESH",]
country_ex <- country_ex[country_ex$country != "FLK",]
country_ex <- country_ex[country_ex$country != "FRO",]
country_ex <- country_ex[country_ex$country != "GIB",]
country_ex <- country_ex[country_ex$country != "GLP",]
country_ex <- country_ex[country_ex$country != "GRL",]
country_ex <- country_ex[country_ex$country != "GUF",]
country_ex <- country_ex[country_ex$country != "GUM",]
country_ex <- country_ex[country_ex$country != "HKG",]
country_ex <- country_ex[country_ex$country != "IOT",]
country_ex <- country_ex[country_ex$country != "MAC",]
country_ex <- country_ex[country_ex$country != "MNP",]
country_ex <- country_ex[country_ex$country != "MSR",]
country_ex <- country_ex[country_ex$country != "MTQ",]
country_ex <- country_ex[country_ex$country != "MYT",]
country_ex <- country_ex[country_ex$country != "NCL",]
country_ex <- country_ex[country_ex$country != "NFK",]
country_ex <- country_ex[country_ex$country != "PRI",]
country_ex <- country_ex[country_ex$country != "PCN",]
country_ex <- country_ex[country_ex$country != "REU",]
country_ex <- country_ex[country_ex$country != "SHN",]
country_ex <- country_ex[country_ex$country != "SPM",]
country_ex <- country_ex[country_ex$country != "SXM",]
country_ex <- country_ex[country_ex$country != "TCA",]
country_ex <- country_ex[country_ex$country != "TKL",]
country_ex <- country_ex[country_ex$country != "VDR",]
country_ex <- country_ex[country_ex$country != "VGB",]
country_ex <- country_ex[country_ex$country != "WLF",]
country_ex <- country_ex[country_ex$country != "PYF",]
country_ex <- country_ex[country_ex$country != "SCG",]
country_ex <- country_ex[country_ex$country != "GDR",]
country_ex <- country_ex[country_ex$country != "GFR",]
country_ex <- country_ex[country_ex$country != "YUG",]
country_ex <- country_ex[country_ex$country != "CSK",]
country_ex <- country_ex[country_ex$country != "YPR",]
country_ex <- country_ex[country_ex$country != "YAR",]
country_ex <- country_ex[country_ex$country != "ZAN",]
country_ex <- country_ex[country_ex$country != "SUN",]
country_ex <- country_ex[country_ex$country != "DDR",]

# Remove Turkey as it has protected areas but has not reported them to the WDPA database:
# its inclusion in the data analysis would biase the results
country_ex <- country_ex[country_ex$country != "TUR",]

# remove all years before 1960 and after 2020
country_ex <- country_ex[country_ex$year > 1959, ]
country_ex <- country_ex[country_ex$year < 2020, ]

# filter the dataset to keep only the year-country dyads for which the country does exist
country_ex <- country_ex[country_ex$exists == 1, ]

# remove the last column
country_ex <- country_ex[ , -3]

# save the file
write.csv(country_ex, "country_ex.csv")


#### Distance data ####

# load data
dist <- read.csv("contdird.csv")

# keep only years 1960-2016
dist <- dist[dist$year %in% 1960:2016, ]

# remove countries which do not exist anymore
dist <- dist[dist$state1ab != "CZE",]
dist <- dist[dist$state1ab != "GDR",]
dist <- dist[dist$state1ab != "GFR",]
dist <- dist[dist$state1ab != "RVN",]
dist <- dist[dist$state1ab != "RVN",]
dist <- dist[dist$state1ab != "YAR",]
dist <- dist[dist$state1ab != "YPR",]
dist <- dist[dist$state1ab != "YUG",]
dist <- dist[dist$state1ab != "ZAN",]

dist <- dist[dist$state2ab != "CZE",]
dist <- dist[dist$state2ab != "GDR",]
dist <- dist[dist$state2ab != "GFR",]
dist <- dist[dist$state2ab != "RVN",]
dist <- dist[dist$state2ab != "RVN",]
dist <- dist[dist$state2ab != "YAR",]
dist <- dist[dist$state2ab != "YPR",]
dist <- dist[dist$state2ab != "YUG",]
dist <- dist[dist$state2ab != "ZAN",]

# replace CoW codes by ISO3 country codes
dist$state1ab <- countrycode(dist$state1ab, origin = "cowc", destination = "iso3c", origin_regex = T)

# replace CoW codes by ISO3 country codes
dist$state2ab <- countrycode(dist$state2ab, origin = "cowc", destination = "iso3c", origin_regex = T)

# remove dyads where one country is "NA"
dist <- dist[!is.na(dist$state1ab),]
dist <- dist[!is.na(dist$state2ab),]


# subset for each year
for(y in 1960:2016) {
  
  ddist <- dist[dist$year == y,]
  
  assign(paste0("ddist", y), ddist, env = .GlobalEnv)
  
}

# create a network for each year where a tie is direct contiguity or marine contiguity
for(y in 1960:2016) {
  
  d <- get(paste0("ddist", y))[,c(3, 5)]
  
  dnet <- graph_from_data_frame(d, directed = T)
  
  # create adjacency matrix for each network
  distadj <- as.matrix(get.adjacency(dnet))
  
  assign(paste0("dist_adj", y), distadj, env = .GlobalEnv)
  
}


# if in country existence data but not in the connectivity matrices: add a row and column to the matrix
'%notin%' <- Negate('%in%')

for(y in unique(dist$year)) {
  
  d <- get(paste0("dist_adj", y))
  ce <- country_ex[country_ex$year == y,]
  
  for (i in unique(ce$country)) {
    
    if (i %notin% rownames(d)) {
      
      d <- rbind(d, numeric(length = nrow(d)))
      d <- cbind(d, numeric(length = nrow(d)))
      
      rownames(d)[nrow(d)] <- i
      colnames(d)[nrow(d)] <- i
      
    }
  }
  assign(paste0("dist_adj", y), d, env = .GlobalEnv)
  
}


# save the adjacency matrices
for (y in 1960:2016) {
  
  write.csv(get(paste0("dist_adj", y)), paste0("dist_adj", y, ".csv"))
  
}


#### Religion data ####

# load the data
rel <- read.csv("World Religion Data CoW.csv")

# remove countries which do not exist anymore
rel <- rel[rel$name != "CZE",]
rel <- rel[rel$name != "GDR",]
rel <- rel[rel$name != "GFR",]
rel <- rel[rel$name != "RVN",]
rel <- rel[rel$name != "RVN",]
rel <- rel[rel$name != "YAR",]
rel <- rel[rel$name != "YPR",]
rel <- rel[rel$name != "YUG",]
rel <- rel[rel$name != "ZAN",]

# find the main 'big' religion in each country (followed by more than 50% of the population)
for(i in 1:length(rel$name)) {
  
  if (rel$dualrelig[i] == 0 ) {
    
    if (rel$chrstgenpct[i] > 0.5) {
      rel$main[i] <- "Christianity"
    }
    
    if (rel$chrstgenpct[i] > 0.5) {
      rel$main[i] <- "Judaism"
    }
    
    if (rel$budgenpct[i] > 0.5) {
      rel$main[i] <- "Buddhism"
    }
    
    if (rel$islmgenpct[i] > 0.5) {
      rel$main[i] <- "Islam"
    }
    
    if (rel$zorogenpct[i] > 0.5) {
      rel$main[i] <- "Zoroastrianism"
    }
    
    if (rel$hindgenpct[i] > 0.5) {
      rel$main[i] <- "Hinduism"
    }
    
    if (rel$sikhgenpct[i] > 0.5) {
      rel$main[i] <- "Sikhism"
    }
    
    if (rel$shntgenpct[i] > 0.5) {
      rel$main[i] <- "Shintoism"
    }
    
    if (rel$taogenpct[i] > 0.5) {
      rel$main[i] <- "Taoism"
    }
    
    if (rel$jaingenpct[i] > 0.5) {
      rel$main[i] <- "Jainism"
    }
    
    if (rel$confgenpct[i] > 0.5) {
      rel$main[i] <- "Confucianism"
    }
    
    if (rel$syncgenpct[i] > 0.5) {
      rel$main[i] <- "Syncretic Religions"
    }
    
    if (rel$anmgenpct[i] > 0.5) {
      rel$main[i] <- "Animist Religions"
    }
    
    if (rel$nonreligpct[i] > 0.5) {
      rel$main[i] <- "Non Religious"
    }
    
    if (rel$othrgenpct[i] > 0.5) {
      rel$main[i] <- "Other Religions"
    }
  }
}

# create a matrix for each year which stores whether two countries share a main religion
for (y in unique(rel$year)) {
  
  assign(paste0("rel_", y), data.frame(matrix(nrow = length(unique(rel$name[rel$year == y])), ncol = length(unique(rel$name[rel$year == y])))), envir = .GlobalEnv)
  
  rely <- get(paste0("rel_", y))
  
  for(i in 1:length(unique(rel$name[rel$year == y]))){
    
    for (j in 1:length(unique(rel$name[rel$year == y]))){
      
      if (rel$main[rel$year == y & rel$name == unique(rel$name[rel$year == y])[i]] == rel$main[rel$year == y & rel$name == unique(rel$name[rel$year == y])[j]] & i != j) {
        rely[i, j] <- 1
      }
      
      else {
        rely[i, j] <- 0
      }
    }
  }
  
  # create a column containing the country name
  rely$country <- countrycode(unique(rel$name[rel$year == y]), origin = "cowc", destination = "iso3c")
  
  # record the row numbers for which rely$country is an NA
  nas <- c()
  
  for (i in 1:nrow(rely)) {
    if (is.na(rely$country[i])) {
      nas <- c(nas, i)
    }
  }
  
  # remove rows and columns for which the "countrycode" function returned an NA
  if(is.null(nas) == F){
    
    rely <- rely[-nas, ]
    rely <- rely[, -nas]
    
  }
  
  # assign row and column names
  rownames(rely) <- rely$country
  colnames(rely) <- rely$country
  
  # remove the "country" column
  a <- as.numeric(length(rely))
  rely <- rely[, -a]
  
  assign(paste0("rel_", y), rely, envir = .GlobalEnv)
}

# create adjacency matrices for years around the years for which we have data
for(y in 1943:2012) {
  
  a <- substr(paste0(y), 1, 3)
  
  b <- substr(paste0(y), 4, 4)
  
  if(b %in% c("1", "2", "3", "4", "6", "7", "8", "9")) {
    
    if(b %in% c("1", "2")) {
      
      assign(paste0("rel_", y), get(paste0("rel_", a, "0")), env = .GlobalEnv)
      
    }
    
    if(b %in% c("3", "4", "6", "7")) {
      
      assign(paste0("rel_", y), get(paste0("rel_", a, "5")), env = .GlobalEnv)
      
    }
    
    if(b %in% c("8", "9")) {
      
      assign(paste0("rel_", y), get(paste0("rel_", y+(10-as.numeric(b)))), env = .GlobalEnv)
      
    }
  }
  
}


# save the objects created
for (y in 1943:2012) {
  
  rely <- get(paste0("rel_", y))
  
  write.csv(rely, paste0("rel_", y, ".csv"))
}



#### Shared language data ####

# load data
grav <- read.csv("Gravity_V202211.csv")

# subset for each year
for(y in unique(grav$year)) {
  
  dgrav <- grav[grav$year == y,]
  
  assign(paste0("dgrav", y), dgrav, env = .GlobalEnv)
  
}


# create a network for each year where a tie is a shared official language
for(y in unique(grav$year)) {
  
  d <- get(paste0("dgrav", y))[,c(4:5, 23)]
  d <- d[d$comlang_off == "1",]
  
  lnet <- graph_from_data_frame(d, directed = T)
  
  assign(paste0("lnet", y), lnet, env = .GlobalEnv)
  
}

# create adjacency matrices from these networks
for(y in unique(grav$year)) {
  
  lgadj <- as.matrix(get.adjacency(get(paste0("lnet", y))))
  
  lgadj <- lgadj[rownames(lgadj) != "NA", colnames(lgadj) != "NA"]
  
  assign(paste0("lg_adj", y), lgadj, env = .GlobalEnv)
  
}

# add rows and columms for each country which exist and are present in the data
# but are not present in the dataset
'%notin%' <- Negate('%in%')

for(y in 1961:2019) {
  
  ce <- country_ex[country_ex$year == y,]
  lgadj <- get(paste0("lg_adj", y))
  
  for (i in unique(ce$country)) {
    
    if (i %notin% rownames(lgadj)) {
      
      lgadj <- rbind(lgadj, numeric(length = nrow(lgadj)))
      lgadj <- cbind(lgadj, numeric(length = nrow(lgadj)))
      
      rownames(lgadj)[nrow(lgadj)] <- i
      colnames(lgadj)[nrow(lgadj)] <- i
      
    }
  }
  assign(paste0("lg_adj", y), lgadj, env = .GlobalEnv)
  
}


# remove the countries which do not exist then
for (y in 1961:2019){
  
  lgadj <- get(paste0("lg_adj", y))
  ce <- country_ex[country_ex$year == y,]
  
  for (i in unique(rownames(lgadj))) {
    
    if (i %notin% ce$country) {
      
      lgadj <- lgadj[, colnames(lgadj) != i]
      lgadj <- lgadj[rownames(lgadj) != i, ]
      
    }
  }
  assign(paste0("lg_adj", y), lgadj, env = .GlobalEnv)
  
}



# save the adjacency matrices
for (y in 1961:2019) {
  
  write.csv(get(paste0("lg_adj", y)), paste0("lg_adj", y, ".csv"))
  
}


#### Trade - structural equivalence data ####

## Make dataframes with all the countries export profiles for each year

for (y in 1962:2020){
  
  # load the data
  p1 <- read.csv(paste0(y, " codes 01234.csv"))
  p2 <- read.csv(paste0(y, " codes 56789.csv"))
  
  # combine in a dataframe
  c <- rbind(p1, p2)
  
  # keep only the useful columns
  c <- c[, c("RefYear", "ReporterISO", "PartnerISO", "CmdCode", "PrimaryValue")]
  
  # compute the value of the total exports of each type of commodity for each country
  for(i in 1:length(unique(c$ReporterISO))) {
    for(j in 0:9){
      c$totexp[c$ReporterISO == unique(c$ReporterISO)[i] & c$CmdCode == j] <- sum(c$PrimaryValue[c$ReporterISO == unique(c$ReporterISO)[i] & c$CmdCode == j])
    }}
  
  # for each country and commodity, compute the share of exports to each other country (times 100000 to keep degrees of significance)
  c$share <- 100000*c$PrimaryValue/c$totexp
  
  # subset the dataframe into separate dataframes for each type of commodity
  c <- as_tibble(c)
  
  a0 <- c %>% subset(c$CmdCode == 0)
  a1 <- c %>% subset(c$CmdCode == 1)
  a2 <- c %>% subset(c$CmdCode == 2)
  a3 <- c %>% subset(c$CmdCode == 3)
  a4 <- c %>% subset(c$CmdCode == 4)
  a5 <- c %>% subset(c$CmdCode == 5)
  a6 <- c %>% subset(c$CmdCode == 6)
  a7 <- c %>% subset(c$CmdCode == 7)
  a8 <- c %>% subset(c$CmdCode == 8)
  a9 <- c %>% subset(c$CmdCode == 9)
  
  # remove useless columns
  a0 <- a0 %>% select(-c("RefYear", "CmdCode"))
  a1 <- a1 %>% select(-c("RefYear", "CmdCode"))
  a2 <- a2 %>% select(-c("RefYear", "CmdCode"))
  a3 <- a3 %>% select(-c("RefYear", "CmdCode"))
  a4 <- a4 %>% select(-c("RefYear", "CmdCode"))
  a5 <- a5 %>% select(-c("RefYear", "CmdCode"))
  a6 <- a6 %>% select(-c("RefYear", "CmdCode"))
  a7 <- a7 %>% select(-c("RefYear", "CmdCode"))
  a8 <- a8 %>% select(-c("RefYear", "CmdCode"))
  a9 <- a9 %>% select(-c("RefYear", "CmdCode"))
  
  # transform each dataframe into a graph
  g0 <- graph.data.frame(a0, directed = T)
  g1 <- graph.data.frame(a1, directed = T)
  g2 <- graph.data.frame(a2, directed = T)
  g3 <- graph.data.frame(a3, directed = T)
  g4 <- graph.data.frame(a4, directed = T)
  g5 <- graph.data.frame(a5, directed = T)
  g6 <- graph.data.frame(a6, directed = T)
  g7 <- graph.data.frame(a7, directed = T)
  g8 <- graph.data.frame(a8, directed = T)
  g9 <- graph.data.frame(a9, directed = T)
  
  # set the weights on the edges as the value of exports from one country to another
  g0 <- set_edge_attr(g0, "weight", value= E(g0)$share)
  g1 <- set_edge_attr(g1, "weight", value= E(g1)$share)
  g2 <- set_edge_attr(g2, "weight", value= E(g2)$share)
  g3 <- set_edge_attr(g3, "weight", value= E(g3)$share)
  g4 <- set_edge_attr(g4, "weight", value= E(g4)$share)
  g5 <- set_edge_attr(g5, "weight", value= E(g5)$share)
  g6 <- set_edge_attr(g6, "weight", value= E(g6)$share)
  g7 <- set_edge_attr(g7, "weight", value= E(g7)$share)
  g8 <- set_edge_attr(g8, "weight", value= E(g8)$share)
  g9 <- set_edge_attr(g9, "weight", value= E(g9)$share)
  
  # create an adjacency matrix from each graph
  a0 <- as.matrix(get.adjacency(g0, attr = "weight"))
  a1 <- as.matrix(get.adjacency(g1, attr = "weight"))
  a2 <- as.matrix(get.adjacency(g2, attr = "weight"))
  a3 <- as.matrix(get.adjacency(g3, attr = "weight"))
  a4 <- as.matrix(get.adjacency(g4, attr = "weight"))
  a5 <- as.matrix(get.adjacency(g5, attr = "weight"))
  a6 <- as.matrix(get.adjacency(g6, attr = "weight"))
  a7 <- as.matrix(get.adjacency(g7, attr = "weight"))
  a8 <- as.matrix(get.adjacency(g8, attr = "weight"))
  a9 <- as.matrix(get.adjacency(g9, attr = "weight"))
  
  # list all the countries that appear in the adjacency matrices
  allc <- c(unique(rownames(a0)), 
            unique(rownames(a1)),
            unique(rownames(a2)),
            unique(rownames(a3)),
            unique(rownames(a4)),
            unique(rownames(a5)),
            unique(rownames(a6)),
            unique(rownames(a7)),
            unique(rownames(a8)),
            unique(rownames(a9)))
  allc <- unique(allc)
  
  # make every adjacency matrix a dataframe and assign row names
  for(i in 0:9){
    a <- as.data.frame(get(paste0("a", i)))
    a$c <- rownames(a)
    a <- merge(a, data.frame(c = allc), by = "c", all = T)
    rownames(a) <- a$c
    a <- as.data.frame(a)
    
    assign(paste0("a", i), a)
  }
  
  # include the commodities in the column names of the different dataframes
  for(i in 0:9){
    a <- get(paste0("a", i))
    colnames(a)[-1] <- paste0(colnames(a)[-1], i)
    
    assign(paste0("a", i), a)
  }
  
  # create a dataframe which includes each country's export profile
  exp_profiles <- merge(a0, a1, by = "c")
  
  for (i in 2:9) {
    exp_profiles <- merge(exp_profiles, get(paste0("a", i)), by = "c")
  }
  
  # assign row names
  rownames(exp_profiles) <- exp_profiles$c
  exp_profiles <- exp_profiles[, -1]
  
  # transform the dataframe into a long dataframe
  exp_profiles <- t(exp_profiles)
  exp_profiles <- as.data.frame(exp_profiles)
  
  # make the columns numeric
  for(i in 1:length(exp_profiles)){
    exp_profiles[,i] <- as.numeric(exp_profiles[,i])
  }
  
  assign(paste0("exp_profiles", y), exp_profiles, envir = .GlobalEnv)
  
}


# remove the countries which do not exist then
for (y in 1962:2019){
  
  exp_prof <- get(paste0("exp_profiles", y))
  
  exp_prof$cc <- rownames(exp_prof)
  
  for (i in 0:9) {
    
    exp_prof$cc <- gsub(paste0(i), "", exp_prof$cc)
  }
  
  exp_prof <- exp_prof[exp_prof$cc %in% country_ex$country[country_ex$year == y], ]
  
  exp_prof <- exp_prof[, colnames(exp_prof) %in% country_ex$country[country_ex$year == y]]
  
  assign(paste0("exp_profiles", y), exp_prof, envir = .GlobalEnv)

}

## calculate pairwise correlations between export profiles for each year
for (y in 1962:2019){
  
  assign(paste0("cor_", y), data.frame(matrix(nrow = ncol(get(paste0("exp_profiles", y))), ncol = ncol(get(paste0("exp_profiles", y))))), envir = .GlobalEnv)
  
  cor <- data.frame(matrix(nrow = ncol(get(paste0("exp_profiles", y))), ncol = ncol(get(paste0("exp_profiles", y)))))
  
  for (i in 1:(ncol(get(paste0("exp_profiles", y))) - 1)) {
    
    for (j in (i+1):ncol(get(paste0("exp_profiles", y)))) {
      
      corr <- tryCatch(
        cor(get(paste0("exp_profiles", y))[, i], get(paste0("exp_profiles", y))[, j], use = "complete.obs", method = "pearson"),
        error = function(e) NA)
      
      cor[i, j] <- corr
      cor[j, i] <- corr  
      
    }
  }
  
  colnames(cor) <- colnames(get(paste0("exp_profiles", y)))
  rownames(cor) <- colnames(get(paste0("exp_profiles", y)))
  
  assign(paste0("cor_", y), cor, envir = .GlobalEnv)
}

# save the objects
for (y in 1962:2019) {
  
  cory <- get(paste0("cor_", y))
  
  write.csv(cory, paste0("cor_", y, ".csv"))
}


# impute values when values of the year before and after are not missing (average)
for (y in 1963:2018) {
  
  cory <- as.data.frame(get(paste0("cor_", y)))
  
  corybf <- as.data.frame(get(paste0("cor_", y-1)))
  
  coryaf <- as.data.frame(get(paste0("cor_", y+1)))
  
  for (i in 1:nrow(cory)) {
    
    if (anyNA(cory[i,])) {
        
        for (j in 1:ncol(cory)) {
          
          if (is.na(cory[i,j])) {
          
          if (colnames(cory[j]) %in% colnames(corybf) & 
              colnames(cory[j]) %in% colnames(coryaf)) {
            
            bf <- as.numeric(corybf[rownames(corybf) == rownames(cory)[i], 
                                    colnames(corybf) == colnames(cory)[j]])
            af <- as.numeric(coryaf[rownames(coryaf) == rownames(cory)[i], 
                                    colnames(coryaf) == colnames(cory)[j]])
            
            m <- mean(c(bf, af))
            
            if (!is.na(m) & !is.nan(m)) {
              
              cory[i,j] <- m
            }
          }
        }
      }
    }
  }
  assign(paste0("cor_", y, "i"), cory, env = .GlobalEnv)
}




# save the objects
for (y in 1963:2018) {
  
  cory <- get(paste0("cor_", y, "i"))
  
  write.csv(cory, paste0("cor_", y, "i.csv"))
}





#### Trade - limited influences ####

# keep only the 20 largest coefficients
for (y in 1962:2019) {
  
  cory <- get(paste0("cor_", y))
  
  for (i in 1:nrow(cory)) {
    
    v <- cory[i,]
    
    v <- sort(v, decreasing = T)
    
    v20 <- v[20]
    
    if (!is.na(v20)) {
      
      for (j in 1:ncol(cory)) {
        
        if (cory[i,j] < v20 & !is.na(cory[i,j])) {
      
      cory[i,j] <- 0
        }
      }
    }
  }
  
  assign(paste0("cor_", y, "_20"), cory, env = .GlobalEnv)
}


# save the files
for (y in 1962:2019) {
  
  cory <- get(paste0("cor_", y, "_20"))

  write.csv(cory, paste0("cor_", y, "_20.csv"))
}


#### Environmental IGO network data ####

# load the data
env_igo <- read.csv("igo_data_env_coding.csv")

env_igo <- as.data.frame(env_igo)

# keep only environmental organisations
env_igo <- env_igo[env_igo$environment == 1, ]

# remove environment column and the first column
env_igo <- env_igo[, colnames(env_igo) != "environment"]
env_igo <- env_igo[, -1]

# remove years before 1960
env_igo <- env_igo[env_igo$year >1959,]

# remove countries which do not exist anymore
env_igo <- env_igo[, colnames(env_igo) != "austriahungary"]
env_igo <- env_igo[, colnames(env_igo) != "baden"]
env_igo <- env_igo[, colnames(env_igo) != "bavariaelect"]
env_igo <- env_igo[, colnames(env_igo) != "czechoslovakia"]
env_igo <- env_igo[, colnames(env_igo) != "hanover"]
env_igo <- env_igo[, colnames(env_igo) != "hesseelect"]
env_igo <- env_igo[, colnames(env_igo) != "hessegrand"]
env_igo <- env_igo[, colnames(env_igo) != "mecklenburg"]
env_igo <- env_igo[, colnames(env_igo) != "parma"]
env_igo <- env_igo[, colnames(env_igo) != "modena"]
env_igo <- env_igo[, colnames(env_igo) != "saxony"]
env_igo <- env_igo[, colnames(env_igo) != "tuscany"]
env_igo <- env_igo[, colnames(env_igo) != "twosicilies"]
env_igo <- env_igo[, colnames(env_igo) != "wurtenburg"]
env_igo <- env_igo[, colnames(env_igo) != "yugoslaviaserb"]
env_igo <- env_igo[, colnames(env_igo) != "zanzibar"]
env_igo <- env_igo[, colnames(env_igo) != "nyemen"]
env_igo <- env_igo[, colnames(env_igo) != "svietnam"]
env_igo <- env_igo[, colnames(env_igo) != "wgermany"]
env_igo <- env_igo[, colnames(env_igo) != "egermany"]
env_igo <- env_igo[, colnames(env_igo) != "syemen"]


# replace the country codes with ISO3 country codes
colnames(env_igo)[c(4:31, 33:49, 51, 53:93, 95:115, 117:131, 133:157, 161:165, 167:199)] <- countrycode(colnames(env_igo)[c(4:31, 33:49, 51, 53:93, 95:115, 117:131, 133:157, 161:165, 167:199)], 
                                                                                                    origin = "country.name.en.regex", origin_regex = T, destination = "iso3c")
# manually replace unidentified country names by ISO3c codes
colnames(env_igo)[32] <- "CAF"
colnames(env_igo)[50] <- "DOM"
colnames(env_igo)[52] <- "TLS"
colnames(env_igo)[94] <- "KOS"
colnames(env_igo)[116] <- "FSM"
colnames(env_igo)[132] <- "PRK"
colnames(env_igo)[158] <- "ZAF"
colnames(env_igo)[159] <- "KOR"
colnames(env_igo)[160] <- "SSD"
colnames(env_igo)[166] <- "LCA"


# make columns numeric
for (i in 6:197) {
  env_igo[, i] <- as.numeric(env_igo[, i])
}

# replace values: being a member has more weight than being an associate member, which is more important than being an observer
for (i in 6:197) {
  
  for (j in 1:nrow(env_igo)){
    
    if (!is.na(env_igo[j, i])) {
      
      if (env_igo[j, i] == 1) {
        
        env_igo[j, i] <- "a" # full member
      }
      
      else if (env_igo[j, i] == 2) {
        
        env_igo[j, i] <- "b" # associate member
      }
      
      else if (env_igo[j, i] == 3) {
        
        env_igo[j, i] <- "c" # observer
      }
      
      else if (env_igo[j, i] == -9 | env_igo[j, i] == -1) {
        
        env_igo[j, i] <- NA
      }
      
    }
  }
}


for (i in 6:197) {
  
  for (j in 1:nrow(env_igo)){
    
    if (!is.na(env_igo[j, i])) {
      
      if (env_igo[j, i] == "a") {
        
        env_igo[j, i] <- 2 # full member
      }
      
      else if (env_igo[j, i] == "b") {
        
        env_igo[j, i] <- 1 # associate member
      }
      
      else if (env_igo[j, i] == "c") {
        
        env_igo[j, i] <- 0.5 # observer
      }
      
    }
    
    if (is.na(env_igo[j, i])) {
      
      env_igo[j, i] <- 0
      
    }
  }
}

# number of environmentsl IGOs a country is a member of each year
env_igo_number <- expand.grid(country = colnames(env_igo)[4:199], year = unique(env_igo$year))
env_igo_number$env_igo_nb <- NA

for(i in unique(env_igo_number$country)) {
  
  for(y in unique(env_igo_number$year)) {
    
    env_igoy <- env_igo[env_igo$year == y,i]
    env_igoy <- env_igoy[env_igoy != 0]
    
    env_igo_number$env_igo_nb[env_igo_number$country == i & env_igo_number$year == y] <- length(env_igoy)
  }
}

# time lag
env_igo_number$year <- env_igo_number$year + 1

# save object
write.csv(env_igo_number, "env_igo_number.csv")

# split the dataset by year
for (y in unique(env_igo$year)) {
  
  assign(paste0("env_igo_", y), env_igo[env_igo$year == y ,], env = .GlobalEnv)
  
}

# create an adjacency matrix for each year from the incidence matrix

for (y in 1960:2014) {
  
  env_igoy <- get(paste0("env_igo_", y))
  
  env_igoy <- env_igoy[, -c(1:5, 198:213)]
  
  mat_env_igo <- as.matrix(env_igoy)
  mat_env_igo <- sapply(mat_env_igo, FUN=as.numeric)
  mat_env_igo <- matrix(mat_env_igo, ncol = ncol(env_igoy), nrow = nrow(env_igoy))
  mat_env_igo <- t(mat_env_igo)
  
  env_igo_adj <- mat_env_igo %*% t(mat_env_igo)
  
  env_igo_adj <- as.data.frame(env_igo_adj)
  
  for (i in 1:length(colnames(env_igo_adj))) {
    
    env_igo_adj[i,i] <- 0
  }
  
  colnames(env_igo_adj) <- colnames(env_igoy)
  rownames(env_igo_adj) <- colnames(env_igoy)
  
  assign(paste0("env_igo_adj", y), env_igo_adj, env = .GlobalEnv)
  
}


# remove the countries which do not exist then
for (y in 1961:2014){
  
  env_igoadj <- get(paste0("env_igo_adj", y))
  ce <- country_ex[country_ex$year == y,]
  
  for (i in unique(rownames(env_igoadj))) {
    
    if (i %notin% ce$country) {
      
      env_igoadj <- env_igoadj[, colnames(env_igoadj) != i]
      env_igoadj <- env_igoadj[rownames(env_igoadj) != i, ]
      
    }
  }
  assign(paste0("env_igo_adj", y), env_igoadj, env = .GlobalEnv)
  
}



# save the objects created
for (y in 1961:2014) {
  
  env_igo_adj <- get(paste0("env_igo_adj", y))
  
  write.csv(env_igo_adj, paste0("env_igo_adj", y, ".csv"))
}










#### Environmental IGO network - limited influences ####

for (y in 1960:2014) {
  
  env_igoadj <- get(paste0("env_igo_adj", y))
  
  for (i in 1:nrow(env_igoadj)) {
    
    v <- env_igoadj[i,]
    
    v <- sort(v, decreasing = T)
    
    v20 <- v[20]
    
    if (!is.na(v20)) {
      
      for (j in 1:ncol(env_igoadj)) {
        
        if (env_igoadj[i,j] < v20 & !is.na(env_igoadj[i,j])) {
          
          env_igoadj[i,j] <- 0
        }
      }
    }
  }
  
  assign(paste0("env_igo_adj", y, "_20"), env_igoadj, env = .GlobalEnv)
}


# save the files
for (y in 1960:2014) {
  
  env_igoadj <- get(paste0("env_igo_adj", y, "_20"))
  
  write.csv(env_igoadj, paste0("env_igo_adj", y, "_20.csv"))
}
