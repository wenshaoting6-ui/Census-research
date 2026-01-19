# Install all the packages-------
install.packages('tidycensus')
install.packages('tidyverse')
install.packages('ggplot2')
install.packages('Hmisc')
install.packages('tmap')
installed.packages('sf')


library(tidycensus)
library(tidyverse)
library(ggplot2)
library(Hmisc)
library(tmap)
library(sf)

#####---------------Function for downloading census data---------

## Other options are: "all", "all_states", "all_pr", "all_dc", "all_dc_pr", "contig_dc"
get_state_list <- function(states = "contig") {
  #Loads all state fips codes
  all <- c("01","02","04","05","06","08","09","10","11","12","13","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","44","45","46","47","48","49","50","51","53","54","55","56","60","66","69","72","74","78")
  if (states == "contig") {
    #removes non states, and hawaii and alaska
    return(all[-which(all %in% c(alaska = "02", 
                                 hawaii = "15",
                                 dc = "11",
                                 puertorico = "72",
                                 guam = "66",
                                 virginislands = "78",
                                 outlying = "74",
                                 amsamoa = "60",
                                 marianaislands = "69"))])
  } else if (states == "all") {
    #removes nothing
    return(all)  
  } else if (states == "all_states") {
    #removes nonstates
    return(all[-which(all %in% c(dc = "11",
                                 puertorico = "72",
                                 guam = "66",
                                 virginislands = "78",
                                 outlying = "74",
                                 amsamoa = "60",
                                 marianaislands = "69"))])
  } else if (states == "all_pr") {
    #removes non states, leaving Puerto Rico
    return(all[-which(all %in% c(dc = "11",
                                 guam = "66",
                                 virginislands = "78",
                                 outlying = "74",
                                 amsamoa = "60",
                                 marianaislands = "69"))])
  } else if (states == "all_dc") {
    #removes non states, leaving District of Columbia
    return(all[-which(all %in% c(puertorico = "72",
                                 guam = "66",
                                 virginislands = "78",
                                 outlying = "74",
                                 amsamoa = "60",
                                 marianaislands = "69"))])
  } else if (states == "all_dc_pr") {
    #removes non states, leaving Puerto Rico and District of Columbia
    return(all[-which(all %in% c(guam = "66",
                                 virginislands = "78",
                                 outlying = "74",
                                 amsamoa = "60",
                                 marianaislands = "69"))])
  } else if (states == "contig_dc") {
    #removes non states, removes Hawaii and Alaska, leaving District of Columbia
    return(all[-which(all %in% c(alaska = "02", 
                                 hawaii = "15",
                                 puertorico = "72",
                                 guam = "66",
                                 virginislands = "78",
                                 outlying = "74",
                                 amsamoa = "60",
                                 marianaislands = "69"))])
  }
}

### Create the State List
## Arg options: "contig" (default), all", "all_states", "all_pr", "all_dc", "all_dc_pr"
state_list <- get_state_list(state = "contig")









AllUSTracts <- function(yr = 2022, varlist = var_list, states = state_list, level = "tract", surv = "acs5") {
  #   Arguments:
  #     yr - accepts integers 2010-2022 (updated 07.04.2024), defines the end-year of multi-year surveys
  #     varlist - named vector of strings, recalling variables from the ACS
  #     states - accepts list of states or states fips codes. Defaults to list defined above with get_state_list()
  #     level - accepts string, defining level of geography the ACS estimates should evaluate -
  #         Options (small to large): "block group", "tract" (default), "county", "state", "us", "cbsa"
  #     surv - accepts string. Defines which dataset to pull from. 
  #         Options: "acs1", "acs3", "acs5" (default), for 1, 3, and 5 year estimates
  
  #   Loop through ACS query for each state given - allows for single pull of block group and tract estimates across states
  df <- map_dfr(states, ~{
    get_acs(
      geography = level,
      variables = varlist,
      survey = surv,
      year = yr,
      state = .x,
      output = "wide",
      geometry = TRUE
    )
  }, .id = "state"
  )
  
  #   Remove "state" placeholder column
  df$state = NULL
  return(df)
}

#construct the variables needed into a data frame
data=c(ageu5m="B01001_003",age59m="B01001_004",age1014m="B01001_005",age1517m="B01001_006",age1819m="B01001_007",age20m="B01001_008",age21m="B01001_009",age2224m="B01001_010",
    age2529m="B01001_011",age3034m="B01001_012",age3539m="B01001_013",age4044m="B01001_014",age4549m="B01001_015",age5054m="B01001_016",age5559m="B01001_017",age6061="B01001_018",
    age6264m="B01001_019",age6566m="B01001_020",age6769m="B01001_021",age7074m="B01001_022",age7579m="B01001_023",age8084m="B01001_024",ageabv85m="B01001_025",
    ageu5f="B01001_027",age59f="B01001_028",age1014f="B01001_029",age1517f="B01001_030",age1819f="B01001_031",age20f="B01001_032",age21f="B01001_033",age2224f="B01001_034",
    age2529f="B01001_035",age3034f="B01001_036",age3539f="B01001_037",age4044f="B01001_038",age4549f="B01001_039",age5054f="B01001_040",age5559f="B01001_041",age6061f="B01001_042",
    age6264f="B01001_043",age6566f="B01001_044",age6769f="B01001_045",age7074f="B01001_046",age7579f="B01001_047",age8084f="B01001_048",ageabv85f="B01001_049",
    MHI = "B19013_001",racetotal="B02001_001", racewhite="B02001_002", raceblack="B02001_003", raceasian="B02001_005",Indian="B02001_004",Hawaiian="B02001_006",other="B02001_007",twootherraces="B02001_008",
    population="B01003_001")

#input the census api
#census_api_key=("your own census api")


##############------data for 2010-------
# Extract data for 2010
dfall=AllUSTracts(yr=2010,varlist = data,states=state_list,level="tract",surv = "acs5")
#remove all tract with no population/income
dfall_cleaned = dfall %>% filter(populationE !=0, MHIE !=0)
#remove all na value
finaldfall=na.omit(dfall_cleaned)

# Reorganize the data into seven subgroups
cleaned_data_all=data.frame("GEOID"=finaldfall$GEOID,
                            "ageu18E"=finaldfall$ageu5mE+finaldfall$ageu5fE+finaldfall$age59mE+finaldfall$age59fE+finaldfall$age1014mE+finaldfall$age1014fE+finaldfall$age1517mE+finaldfall$age1517fE,
                            "ageu18M"=finaldfall$ageu5mM+finaldfall$ageu5fM+finaldfall$age59mM+finaldfall$age59fM+finaldfall$age1014mM+finaldfall$age1014fM+finaldfall$age1517mM+finaldfall$age1517fM,
                            "age1825E"=finaldfall$age1819mE+finaldfall$age1819fE+finaldfall$age20fE+finaldfall$age20mE+finaldfall$age21fE+finaldfall$age21mE+finaldfall$age2224mE+finaldfall$age2224fE,
                            "age1825M"=finaldfall$age1819mM+finaldfall$age1819fM+finaldfall$age20fM+finaldfall$age20mM+finaldfall$age21fM+finaldfall$age21mM+finaldfall$age2224mM+finaldfall$age2224fM,
                            "age2540E"=finaldfall$age2529mE+finaldfall$age2529fE+finaldfall$age3034mE+finaldfall$age3034fE+finaldfall$age3539mE+finaldfall$age3539fE,
                            "age2540M"=finaldfall$age2529mM+finaldfall$age2529fM+finaldfall$age3034mM+finaldfall$age3034fM+finaldfall$age3539mM+finaldfall$age3539fM,
                            "age4054E"=finaldfall$age4044mE+finaldfall$age4044fE+finaldfall$age4549mE+finaldfall$age4549fE+finaldfall$age5054mE+finaldfall$age5054fE,
                            "age4054M"=finaldfall$age4044mM+finaldfall$age4044fM+finaldfall$age4549mM+finaldfall$age4549fM+finaldfall$age5054mM+finaldfall$age5054fM,
                            "age5565E"=finaldfall$age5559mE+finaldfall$age5559fE+finaldfall$age6061E+finaldfall$age6061fE+finaldfall$age6264mE+finaldfall$age6264fE,
                            "age5565M"=finaldfall$age5559mM+finaldfall$age5559fM+finaldfall$age6061M+finaldfall$age6061fM+finaldfall$age6264mM+finaldfall$age6264fM,
                            "age6575E"=finaldfall$age6566mE+finaldfall$age6566fE+finaldfall$age6769mE+finaldfall$age6769fE+finaldfall$age7074mE+finaldfall$age7074fE,
                            "age6575M"=finaldfall$age6566mM+finaldfall$age6566fM+finaldfall$age6769mM+finaldfall$age6769fM+finaldfall$age7074mM+finaldfall$age7074fM,
                            "ageover75E"=finaldfall$age7579mE+finaldfall$age7579fE+finaldfall$age8084mE+finaldfall$age8084fE+finaldfall$ageabv85mE+finaldfall$ageabv85fE,
                            "ageover75M"=finaldfall$age7579mM+finaldfall$age7579fM+finaldfall$age8084mM+finaldfall$age8084fM+finaldfall$ageabv85mM+finaldfall$ageabv85fM,
                           
                            "populationE"=finaldfall$populationE,"populationM"=finaldfall$populationM,finaldfall[,95:115])



### ----For age 2010----
# Create empty data frame for simulation
index_data_all <- data.frame(GEOID = cleaned_data_all$GEOID)
set.seed(420)

# Calculate the composite index for each iteration
for (j in 1:10000) {
  # Calculating the random number for under 18 from within the MOE parameters
  random_ageu18 <- ifelse((cleaned_data_all$ageu18E-cleaned_data_all$ageu18M)<=0,
                          round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$ageu18E + cleaned_data_all$ageu18M),0),
                          round(runif(nrow(cleaned_data_all), min = cleaned_data_all$ageu18E - cleaned_data_all$ageu18M, max = cleaned_data_all$ageu18E + cleaned_data_all$ageu18M),0))
  
  # Calculating the random number for under 18-25 from within the MOE parameters
  random_age1825 <- ifelse((cleaned_data_all$age1825E-cleaned_data_all$age1825M)<=0,
                          round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$age1825E + cleaned_data_all$age1825M),0),
                          round(runif(nrow(cleaned_data_all), min = cleaned_data_all$age1825E - cleaned_data_all$age1825M, max = cleaned_data_all$age1825E + cleaned_data_all$age1825M),0))
  
  # Calculating the random number for under 25-40 from within the MOE parameters
  random_age2540 <- ifelse((cleaned_data_all$age2540E-cleaned_data_all$age2540M)<=0,
                            round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$age2540E + cleaned_data_all$age2540M),0),
                            round(runif(nrow(cleaned_data_all), min = cleaned_data_all$age2540E - cleaned_data_all$age2540M, max = cleaned_data_all$age2540E + cleaned_data_all$age2540M),0))
  
  # Calculating the random number for under 40-54 from within the MOE parameters
  random_age4054 <- ifelse((cleaned_data_all$age4054E-cleaned_data_all$age4054M)<=0,
                            round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$age4054E + cleaned_data_all$age4054M),0),
                            round(runif(nrow(cleaned_data_all), min = cleaned_data_all$age4054E - cleaned_data_all$age4054M, max = cleaned_data_all$age4054E + cleaned_data_all$age4054M),0))
  
  
  # Calculating the random number for 55-65 from within the MOE parameters
  random_age5565 <- ifelse((cleaned_data_all$age5565E-cleaned_data_all$age5565M)<=0,
                           round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$age5565E + cleaned_data_all$age5565M),0),
                           round(runif(nrow(cleaned_data_all), min = cleaned_data_all$age5565E - cleaned_data_all$age5565M, max = cleaned_data_all$age5565E + cleaned_data_all$age5565M),0))
  
  # Calculating the random number for 65-75 from within the MOE parameters
  random_age6575 <- ifelse((cleaned_data_all$age6575E-cleaned_data_all$age6575M)<=0,
                           round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$age6575E + cleaned_data_all$age6575M),0),
                           round(runif(nrow(cleaned_data_all), min = cleaned_data_all$age6575E - cleaned_data_all$age6575M, max = cleaned_data_all$age6575E + cleaned_data_all$age6575M),0))
  
  # Calculating the random number for over75 from within the MOE parameters
  random_ageover75 <- ifelse((cleaned_data_all$ageover75E-cleaned_data_all$ageover75M)<=0,
                             round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$ageover75E + cleaned_data_all$ageover75M),0),
                             round(runif(nrow(cleaned_data_all), min = cleaned_data_all$ageover75E - cleaned_data_all$ageover75M, max = cleaned_data_all$ageover75E + cleaned_data_all$ageover75M),0))
  
  # Calculating the SDI for each simulation 
  total_random_ages <- random_ageu18 + random_age1825 + random_age2540 + random_age4054 + 
    random_age5565 + random_age6575 + random_ageover75
  
  sim10_numerator <- random_ageu18 * (random_ageu18 - 1) +
    random_age1825 * (random_age1825 - 1) +
    random_age2540 * (random_age2540 - 1) +
    random_age4054 * (random_age4054 - 1) +
    random_age5565 * (random_age5565 - 1) +
    random_age6575 * (random_age6575 - 1) +
    random_ageover75 * (random_ageover75 - 1)
  
  sim10 <- 1 - (sim10_numerator / (total_random_ages * (total_random_ages - 1)))
  
  # Create a vector to store index values
  index_values <- numeric(nrow(cleaned_data_all)) 

  cat("Iteration:", j, "\n")
  
  
  for (i in 1:nrow(cleaned_data_all)){
    index_values[i] <- sim10[i]
    
    
  }
  
  # Add index values to the index_data_all dataframe
  index_data_all[[paste0("Index_", j)]] <- index_values
}
# Reorganize the data and calculate the general statistics  
index_data_all$mean=rowMeans(index_data_all[, 2:ncol(index_data_all)],na.rm = TRUE)
index_data_all$min=apply(index_data_all[, 2:10001], 1, min, na.rm = TRUE)
index_data_all$max=apply(index_data_all[, 2:10001], 1, max, na.rm = TRUE)
index_data_all$median=apply(index_data_all[,2:10001],1,median,na.rm = TRUE)

# Summarize data into a new dataframe
summary_data_all=data.frame("GEOID"=index_data_all[,1],index_data_all[,10002:10005])
summary_data_all$range=summary_data_all$max-summary_data_all$min
# Calculate the estimated SDI
summary_data_all$estimate=1-((cleaned_data_all$ageu18E*(cleaned_data_all$ageu18E-1)+cleaned_data_all$age1825E*(cleaned_data_all$age1825E-1)+cleaned_data_all$age2540E*(cleaned_data_all$age2540E-1)+cleaned_data_all$age4054E*(cleaned_data_all$age4054E-1)+cleaned_data_all$age5565E*(cleaned_data_all$age5565E-1)+cleaned_data_all$age6575E*(cleaned_data_all$age6575E-1)+cleaned_data_all$ageover75E*(cleaned_data_all$ageover75E-1))/
                               ((cleaned_data_all$age1825E+cleaned_data_all$age2540E+cleaned_data_all$age4054E+cleaned_data_all$age5565E+cleaned_data_all$age6575E+cleaned_data_all$ageover75E+cleaned_data_all$ageu18E)*((cleaned_data_all$age1825E+cleaned_data_all$age2540E+cleaned_data_all$age4054E+cleaned_data_all$age5565E+cleaned_data_all$age6575E+cleaned_data_all$ageover75E+cleaned_data_all$ageu18E)-1)))

# Save the data 
save(summary_data_all,file="" )
save(index_data_all,file="")


### Calculate the Perason' s R correlation 
JoinData=cleaned_data_all[,c("GEOID","MHIE","populationE.1")]
Newdata<-merge(JoinData, index_data_all, by="GEOID")

# Create dataframe to store data
index_columns<-Newdata[,c(4:10003)]
var_columns<-Newdata[,c(2:3)]
summary_stats_df_age_2010 <- data.frame(
  Index = character(0),
  MHICorr = numeric(0),
  TPopCorr = numeric(0),
  stringsAsFactors = FALSE
)

# Loop over each index column and calculate the correlation with both variables
for(i in 1:ncol(index_columns)) {
  # Calculate the correlation for both variables (10008 and 10009)
  cor_var1 <- cor(index_columns[, i], var_columns[, 1], use = "complete.obs")  # Correlation with Var1 (10008)
  cor_var2 <- cor(index_columns[, i], var_columns[, 2], use = "complete.obs")  # Correlation with Var2 (10009)
  
  # Collect the summary statistics for the correlations
  summary_stats_df_age_2010[i, ] <- data.frame(
    Index = paste("Index", i),
    MHICorr = cor_var1,
    TPopCorr = cor_var2
  )
}
# Save the data
save(summary_stats_df_age_2010,file="")

######----for race 2010----
# Create empty data frame for simulation
race_data_all <- data.frame(GEOID = cleaned_data_all$GEOID)
race_data_estimatevalue2<- data.frame(GEOID = cleaned_data_all$GEOID)
set.seed(420)

# Calculate the composite index for each iteration
for (j in 1:10000) {
  
  # white
  white <- ifelse((cleaned_data_all$racewhiteE-cleaned_data_all$racewhiteM)<=0,
                  round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$racewhiteE + cleaned_data_all$racewhiteM),0),
                  round(runif(nrow(cleaned_data_all), min = cleaned_data_all$racewhiteE - cleaned_data_all$racewhiteM, max = cleaned_data_all$racewhiteE + cleaned_data_all$racewhiteM),0))
  
  # black
  black <- ifelse((cleaned_data_all$raceblackE-cleaned_data_all$raceblackM)<=0,
                  round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$raceblackE + cleaned_data_all$raceblackM),0),
                  round(runif(nrow(cleaned_data_all), min = cleaned_data_all$raceblackE - cleaned_data_all$raceblackM, max = cleaned_data_all$raceblackE + cleaned_data_all$raceblackM),0))
  
  # asian
  asian <- ifelse((cleaned_data_all$raceasianE-cleaned_data_all$raceasianM)<=0,
                  round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$raceasianE + cleaned_data_all$raceasianM),0),
                  round(runif(nrow(cleaned_data_all), min = cleaned_data_all$raceasianE - cleaned_data_all$raceasianM, max = cleaned_data_all$raceasianE + cleaned_data_all$raceasianM),0))
  # Indian
  Indian <- ifelse((cleaned_data_all$IndianE-cleaned_data_all$IndianM)<=0,
                   round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$IndianE + cleaned_data_all$IndianM),0),
                   round(runif(nrow(cleaned_data_all), min = cleaned_data_all$IndianE - cleaned_data_all$IndianM, max = cleaned_data_all$IndianE + cleaned_data_all$IndianM),0))
  # Hawaiian
  Hawaiian <- ifelse((cleaned_data_all$HawaiianE-cleaned_data_all$HawaiianM)<=0,
                     round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$HawaiianE + cleaned_data_all$HawaiianM),0),
                     round(runif(nrow(cleaned_data_all), min = cleaned_data_all$HawaiianE - cleaned_data_all$HawaiianM, max = cleaned_data_all$HawaiianE + cleaned_data_all$HawaiianM),0))
  
  # other
  other <- ifelse((cleaned_data_all$otherE-cleaned_data_all$otherM)<=0,
                  round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$otherE + cleaned_data_all$otherM),0),
                  round(runif(nrow(cleaned_data_all), min = cleaned_data_all$otherE - cleaned_data_all$otherM, max = cleaned_data_all$otherE + cleaned_data_all$otherM),0))
  
  # twootherraces
  twootherraces <- ifelse((cleaned_data_all$twootherracesE-cleaned_data_all$twootherracesM)<=0,
                          round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$twootherracesE + cleaned_data_all$twootherracesM),0),
                          round(runif(nrow(cleaned_data_all), min = cleaned_data_all$twootherracesE - cleaned_data_all$twootherracesM, max = cleaned_data_all$twootherracesE + cleaned_data_all$twootherracesM),0))
  
  
  
  # Calculate the SDI for each simulation
  for (i in 1:nrow(cleaned_data_all)){
    race_data_all2[i]=1-((white[i]*(white[i]-1)+black[i]*(black[i]-1)+asian[i]*(asian[i]-1)+Indian[i]*(Indian[i]-1)+Hawaiian[i]*(Hawaiian[i]-1)+other[i]*(other[i]-1)+twootherraces[i]*(twootherraces[i]-1))/
                           ((black[i]+asian[i]+Indian[i]+white[i]+Hawaiian[i]+other[i]+twootherraces[i])*((black[i]+asian[i]+Indian[i]+white[i]+Hawaiian[i]+other[i]+twootherraces[i])-1)))
    
    
  }
  

  # Add index values to the index_data_all dataframe
  race_data_all[[paste0("Index_", j)]] <- race_data_all2
  
}  
# Reorganize the data and calculate the general statistics
race_data_all_range=data.frame(mean=rowMeans(race_data_all[, 2:ncol(race_data_all)],na.rm = TRUE))
race_data_all$mean=rowMeans(race_data_all[, 2:10001],na.rm = TRUE)
race_data_all$mean=apply(race_data_all[, 2:10001], 1, mean, na.rm = TRUE)
race_data_all$min=apply(race_data_all[, 2:10001], 1, min, na.rm = TRUE)
race_data_all$max=apply(race_data_all[, 2:10001], 1, max, na.rm = TRUE)
race_data_all$median=apply(race_data_all[, 2:10001], 1, median, na.rm = TRUE)

# Summarize data into a new dataframe
summary_data_race=data.frame("GEOID"=race_data_all[,1],race_data_all[,10002:10005])
summary_data_race$range=summary_data_race$max-summary_data_race$min
# Calculate the estimated SDI
summary_data_race$estimate=1-((cleaned_data_all$racewhiteE*(cleaned_data_all$racewhiteE-1)+cleaned_data_all$raceblackE*(cleaned_data_all$raceblackE-1)+cleaned_data_all$raceasianE*(cleaned_data_all$raceasianE-1)+cleaned_data_all$IndianE*(cleaned_data_all$IndianE-1)+cleaned_data_all$HawaiianE*(cleaned_data_all$HawaiianE-1)+cleaned_data_all$otherE*(cleaned_data_all$otherE-1)+cleaned_data_all$twootherracesE*(cleaned_data_all$twootherracesE-1))/
                             ((cleaned_data_all$raceblackE+cleaned_data_all$raceasianE+cleaned_data_all$IndianE+cleaned_data_all$racewhiteE+cleaned_data_all$HawaiianE+cleaned_data_all$otherE+cleaned_data_all$twootherracesE)*((cleaned_data_all$raceblackE+cleaned_data_all$raceasianE+cleaned_data_all$IndianE+cleaned_data_all$racewhiteE+cleaned_data_all$HawaiianE+cleaned_data_all$otherE+cleaned_data_all$twootherracesE)-1)))

# Save the data 
save(race_data_all,file="")  
save(summary_data_race,file="" )

### Calculate the Pearson's R correlation 
JoinData=cleaned_data_all[,c("GEOID","MHIE","populationE.1")]
New_data_race=merge(JoinData,race_data_all,by="GEOID")

# Create dataframe to store data
index_columns_race<-New_data_race[,c(4:10003)]
var_columns_race<-New_data_race[,c(2:3)]
summary_stats_df_race_2010 <- data.frame(
  Index = character(0),
  MHICorr = numeric(0),
  TPopCorr = numeric(0),
  stringsAsFactors = FALSE
)

# Loop over each index column and calculate the correlation with both variables
for(i in 1:ncol(index_columns_race)) {
  # Calculate the correlation for both variables (10008 and 10009)
  cor_var1 <- cor(index_columns_race[, i], var_columns_race[, 1], use = "complete.obs")  # Correlation with Var1 (10008)
  cor_var2 <- cor(index_columns_race[, i], var_columns_race[, 2], use = "complete.obs")  # Correlation with Var2 (10009)
  
  # Collect the summary statistics for the correlations
  summary_stats_df_race_2010[i, ] <- data.frame(
    Index = paste("Index", i),
    MHICorr = cor_var1,
    TPopCorr = cor_var2
  )
}

# Save the data
save(summary_stats_df_race_2010,file="")

#######-----data for 2014-----
# Extract data for 2014
dfall=AllUSTracts(yr=2014,varlist = data,states=state_list,level="tract",surv = "acs5")
#remove all tract with no population/income
dfall_cleaned = dfall %>% filter(populationE !=0, MHIE !=0)
#remove na data
finaldfall=na.omit(dfall_cleaned)

# Reorganize the data into seven subgroups
cleaned_data_all=data.frame("GEOID"=finaldfall$GEOID,
                            "ageu18E"=finaldfall$ageu5mE+finaldfall$ageu5fE+finaldfall$age59mE+finaldfall$age59fE+finaldfall$age1014mE+finaldfall$age1014fE+finaldfall$age1517mE+finaldfall$age1517fE,
                            "ageu18M"=finaldfall$ageu5mM+finaldfall$ageu5fM+finaldfall$age59mM+finaldfall$age59fM+finaldfall$age1014mM+finaldfall$age1014fM+finaldfall$age1517mM+finaldfall$age1517fM,
                            "age1825E"=finaldfall$age1819mE+finaldfall$age1819fE+finaldfall$age20fE+finaldfall$age20mE+finaldfall$age21fE+finaldfall$age21mE+finaldfall$age2224mE+finaldfall$age2224fE,
                            "age1825M"=finaldfall$age1819mM+finaldfall$age1819fM+finaldfall$age20fM+finaldfall$age20mM+finaldfall$age21fM+finaldfall$age21mM+finaldfall$age2224mM+finaldfall$age2224fM,
                            "age2540E"=finaldfall$age2529mE+finaldfall$age2529fE+finaldfall$age3034mE+finaldfall$age3034fE+finaldfall$age3539mE+finaldfall$age3539fE,
                            "age2540M"=finaldfall$age2529mM+finaldfall$age2529fM+finaldfall$age3034mM+finaldfall$age3034fM+finaldfall$age3539mM+finaldfall$age3539fM,
                            "age4054E"=finaldfall$age4044mE+finaldfall$age4044fE+finaldfall$age4549mE+finaldfall$age4549fE+finaldfall$age5054mE+finaldfall$age5054fE,
                            "age4054M"=finaldfall$age4044mM+finaldfall$age4044fM+finaldfall$age4549mM+finaldfall$age4549fM+finaldfall$age5054mM+finaldfall$age5054fM,
                            "age5565E"=finaldfall$age5559mE+finaldfall$age5559fE+finaldfall$age6061E+finaldfall$age6061fE+finaldfall$age6264mE+finaldfall$age6264fE,
                            "age5565M"=finaldfall$age5559mM+finaldfall$age5559fM+finaldfall$age6061M+finaldfall$age6061fM+finaldfall$age6264mM+finaldfall$age6264fM,
                            "age6575E"=finaldfall$age6566mE+finaldfall$age6566fE+finaldfall$age6769mE+finaldfall$age6769fE+finaldfall$age7074mE+finaldfall$age7074fE,
                            "age6575M"=finaldfall$age6566mM+finaldfall$age6566fM+finaldfall$age6769mM+finaldfall$age6769fM+finaldfall$age7074mM+finaldfall$age7074fM,
                            "ageover75E"=finaldfall$age7579mE+finaldfall$age7579fE+finaldfall$age8084mE+finaldfall$age8084fE+finaldfall$ageabv85mE+finaldfall$ageabv85fE,
                            "ageover75M"=finaldfall$age7579mM+finaldfall$age7579fM+finaldfall$age8084mM+finaldfall$age8084fM+finaldfall$ageabv85mM+finaldfall$ageabv85fM,
                            
                            "populationE"=finaldfall$populationE,"populationM"=finaldfall$populationM,finaldfall[,95:115])



#---- for age 2014----
index_data_all <- data.frame(GEOID = cleaned_data_all$GEOID)
set.seed(420)

# Calculate the composite index for each iteration
for (j in 1:10000) {
  # Calculating the random number for under 18 from within the MOE parameters
  random_ageu18 <- ifelse((cleaned_data_all$ageu18E-cleaned_data_all$ageu18M)<=0,
                          round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$ageu18E + cleaned_data_all$ageu18M),0),
                          round(runif(nrow(cleaned_data_all), min = cleaned_data_all$ageu18E - cleaned_data_all$ageu18M, max = cleaned_data_all$ageu18E + cleaned_data_all$ageu18M),0))
  
  # Calculating the random number for under 18-35 from within the MOE parameters
  random_age1825 <- ifelse((cleaned_data_all$age1825E-cleaned_data_all$age1825M)<=0,
                           round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$age1825E + cleaned_data_all$age1825M),0),
                           round(runif(nrow(cleaned_data_all), min = cleaned_data_all$age1825E - cleaned_data_all$age1825M, max = cleaned_data_all$age1825E + cleaned_data_all$age1825M),0))
  
  # Calculating the random number for under 25-40 from within the MOE parameters
  random_age2540 <- ifelse((cleaned_data_all$age2540E-cleaned_data_all$age2540M)<=0,
                           round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$age2540E + cleaned_data_all$age2540M),0),
                           round(runif(nrow(cleaned_data_all), min = cleaned_data_all$age2540E - cleaned_data_all$age2540M, max = cleaned_data_all$age2540E + cleaned_data_all$age2540M),0))
  
  # Calculating the random number for under 40-54 from within the MOE parameters
  random_age4054 <- ifelse((cleaned_data_all$age4054E-cleaned_data_all$age4054M)<=0,
                           round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$age4054E + cleaned_data_all$age4054M),0),
                           round(runif(nrow(cleaned_data_all), min = cleaned_data_all$age4054E - cleaned_data_all$age4054M, max = cleaned_data_all$age4054E + cleaned_data_all$age4054M),0))
  
  
  # Calculating the random number for 55-65 from within the MOE parameters
  random_age5565 <- ifelse((cleaned_data_all$age5565E-cleaned_data_all$age5565M)<=0,
                           round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$age5565E + cleaned_data_all$age5565M),0),
                           round(runif(nrow(cleaned_data_all), min = cleaned_data_all$age5565E - cleaned_data_all$age5565M, max = cleaned_data_all$age5565E + cleaned_data_all$age5565M),0))
  
  # Calculating the random number for 65-75 from within the MOE parameters
  random_age6575 <- ifelse((cleaned_data_all$age6575E-cleaned_data_all$age6575M)<=0,
                           round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$age6575E + cleaned_data_all$age6575M),0),
                           round(runif(nrow(cleaned_data_all), min = cleaned_data_all$age6575E - cleaned_data_all$age6575M, max = cleaned_data_all$age6575E + cleaned_data_all$age6575M),0))
  
  # Calculating the random number for over75 from within the MOE parameters
  random_ageover75 <- ifelse((cleaned_data_all$ageover75E-cleaned_data_all$ageover75M)<=0,
                             round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$ageover75E + cleaned_data_all$ageover75M),0),
                             round(runif(nrow(cleaned_data_all), min = cleaned_data_all$ageover75E - cleaned_data_all$ageover75M, max = cleaned_data_all$ageover75E + cleaned_data_all$ageover75M),0))
  # Calculating the random number for population from within the MOE parameters
  pop= ifelse((cleaned_data_all$populationE-cleaned_data_all$populationM)<=0,
              round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$populationE + cleaned_data_all$populationM),0),
              round(runif(nrow(cleaned_data_all), min = cleaned_data_all$populationE - cleaned_data_all$populationM, max = cleaned_data_all$populationE + cleaned_data_all$populationM),0))
  
  
  # Create a vector to store index values
  index_values <- numeric(nrow(cleaned_data_all)) 
  cat("Iteration:", j, "\n")
  
  for (i in 1:nrow(cleaned_data_all)){
    index_values[i]=1-((random_ageu18[i]*(random_ageu18[i]-1)+random_age1825[i]*(random_age1825[i]-1)+random_age2540[i]*(random_age2540[i]-1)+random_age4054[i]*(random_age4054[i]-1)+random_age5565[i]*(random_age5565[i]-1)+random_age6575[i]*(random_age6575[i]-1)+random_ageover75[i]*(random_ageover75[i]-1))/
                         ((random_age1825[i]+random_age2540[i]+random_age4054[i]+random_age5565[i]+random_age6575[i]+random_ageover75[i]+random_ageu18[i])*((random_age1825[i]+random_age2540[i]+random_age4054[i]+random_age5565[i]+random_age6575[i]+random_ageover75[i]+random_ageu18[i])-1)))

  }
  
  # Add index values to the index_data_all dataframe
  index_data_all[[paste0("Index_", j)]] <- index_values
}

# Reorganize the data and calculate the general statistics 
index_data_all$mean=rowMeans(index_data_all[, 2:ncol(index_data_all)],na.rm = TRUE)
index_data_all$min=apply(index_data_all[, 2:10001], 1, min, na.rm = TRUE)
index_data_all$max=apply(index_data_all[, 2:10001], 1, max, na.rm = TRUE)
index_data_all$median=apply(index_data_all[,2:10001],1,median,na.rm = TRUE)


# Summarize data into a new dataframe
summary_data_all=data.frame("GEOID"=index_data_all[,1],index_data_all[,10002:10005])
summary_data_all$range=summary_data_all$max-summary_data_all$min
# Calcualte the estimated SDI
summary_data_all$estimate=1-((cleaned_data_all$ageu18E*(cleaned_data_all$ageu18E-1)+cleaned_data_all$age1825E*(cleaned_data_all$age1825E-1)+cleaned_data_all$age2540E*(cleaned_data_all$age2540E-1)+cleaned_data_all$age4054E*(cleaned_data_all$age4054E-1)+cleaned_data_all$age5565E*(cleaned_data_all$age5565E-1)+cleaned_data_all$age6575E*(cleaned_data_all$age6575E-1)+cleaned_data_all$ageover75E*(cleaned_data_all$ageover75E-1))/                                ((cleaned_data_all$age1825E+cleaned_data_all$age2540E+cleaned_data_all$age4054E+cleaned_data_all$age5565E+cleaned_data_all$age6575E+cleaned_data_all$ageover75E+cleaned_data_all$ageu18E)*((cleaned_data_all$age1825E+cleaned_data_all$age2540E+cleaned_data_all$age4054E+cleaned_data_all$age5565E+cleaned_data_all$age6575E+cleaned_data_all$ageover75E+cleaned_data_all$ageu18E)-1)))

# Save the data
save(summary_data_all,file="" )
save(index_data_all,file="")

### Calculate the Pearson's R correlation
JoinData=cleaned_data_all[,c("GEOID","MHIE","populationE.1")]
Newdata<-merge(JoinData, index_data_all, by="GEOID")

# Create dataframe to store data
index_columns<-Newdata[,c(4:10003)]
var_columns<-Newdata[,c(2:3)]
summary_stats_df_age_2014 <- data.frame(
  Index = character(0),
  MHICorr = numeric(0),
  TPopCorr = numeric(0),
  stringsAsFactors = FALSE
)

# Loop over each index column and calculate the correlation with both variables
for(i in 1:ncol(index_columns)) {
  # Calculate the correlation for both variables (10008 and 10009)
  cor_var1 <- cor(index_columns[, i], var_columns[, 1], use = "complete.obs")  # Correlation with Var1 (10008)
  cor_var2 <- cor(index_columns[, i], var_columns[, 2], use = "complete.obs")  # Correlation with Var2 (10009)
  
  # Collect the summary statistics for the correlations
  summary_stats_df_age_2014[i, ] <- data.frame(
    Index = paste("Index", i),
    MHICorr = cor_var1,
    TPopCorr = cor_var2
  )
}

#save the data
save(summary_stats_df_age_2014,file="")


######----for race 2014----
# Create empty data frame for simulation
race_data_all <- data.frame(GEOID = cleaned_data_all$GEOID)

set.seed(420)

# Calculate the composite index for each iteration
for (j in 1:10000) {
  
  # white
  white <- ifelse((cleaned_data_all$racewhiteE-cleaned_data_all$racewhiteM)<=0,
                  round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$racewhiteE + cleaned_data_all$racewhiteM),0),
                  round(runif(nrow(cleaned_data_all), min = cleaned_data_all$racewhiteE - cleaned_data_all$racewhiteM, max = cleaned_data_all$racewhiteE + cleaned_data_all$racewhiteM),0))
  
  # black
  black <- ifelse((cleaned_data_all$raceblackE-cleaned_data_all$raceblackM)<=0,
                  round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$raceblackE + cleaned_data_all$raceblackM),0),
                  round(runif(nrow(cleaned_data_all), min = cleaned_data_all$raceblackE - cleaned_data_all$raceblackM, max = cleaned_data_all$raceblackE + cleaned_data_all$raceblackM),0))
  
  # asian
  asian <- ifelse((cleaned_data_all$raceasianE-cleaned_data_all$raceasianM)<=0,
                  round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$raceasianE + cleaned_data_all$raceasianM),0),
                  round(runif(nrow(cleaned_data_all), min = cleaned_data_all$raceasianE - cleaned_data_all$raceasianM, max = cleaned_data_all$raceasianE + cleaned_data_all$raceasianM),0))
  # Indian
  Indian <- ifelse((cleaned_data_all$IndianE-cleaned_data_all$IndianM)<=0,
                   round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$IndianE + cleaned_data_all$IndianM),0),
                   round(runif(nrow(cleaned_data_all), min = cleaned_data_all$IndianE - cleaned_data_all$IndianM, max = cleaned_data_all$IndianE + cleaned_data_all$IndianM),0))
  # Hawaiian
  Hawaiian <- ifelse((cleaned_data_all$HawaiianE-cleaned_data_all$HawaiianM)<=0,
                     round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$HawaiianE + cleaned_data_all$HawaiianM),0),
                     round(runif(nrow(cleaned_data_all), min = cleaned_data_all$HawaiianE - cleaned_data_all$HawaiianM, max = cleaned_data_all$HawaiianE + cleaned_data_all$HawaiianM),0))
  
  # other
  other <- ifelse((cleaned_data_all$otherE-cleaned_data_all$otherM)<=0,
                  round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$otherE + cleaned_data_all$otherM),0),
                  round(runif(nrow(cleaned_data_all), min = cleaned_data_all$otherE - cleaned_data_all$otherM, max = cleaned_data_all$otherE + cleaned_data_all$otherM),0))
  
  # twootherraces
  twootherraces <- ifelse((cleaned_data_all$twootherracesE-cleaned_data_all$twootherracesM)<=0,
                          round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$twootherracesE + cleaned_data_all$twootherracesM),0),
                          round(runif(nrow(cleaned_data_all), min = cleaned_data_all$twootherracesE - cleaned_data_all$twootherracesM, max = cleaned_data_all$twootherracesE + cleaned_data_all$twootherracesM),0))
  
  
  
  # Create a vector to store index values
  race_data_all2 <- numeric(nrow(cleaned_data_all)) 
  disindex <- numeric(nrow(cleaned_data_all)) 
  race_data_estimate <- numeric(nrow(cleaned_data_all)) 
  cat("Iteration:", j, "\n")
  
  # Calculate the SDI for each simulation
  for (i in 1:nrow(cleaned_data_all)){
    race_data_all2[i]=1-((white[i]*(white[i]-1)+black[i]*(black[i]-1)+asian[i]*(asian[i]-1)+Indian[i]*(Indian[i]-1)+Hawaiian[i]*(Hawaiian[i]-1)+other[i]*(other[i]-1)+twootherraces[i]*(twootherraces[i]-1))/
                           ((black[i]+asian[i]+Indian[i]+white[i]+Hawaiian[i]+other[i]+twootherraces[i])*((black[i]+asian[i]+Indian[i]+white[i]+Hawaiian[i]+other[i]+twootherraces[i])-1)))
    
  }
  
  # Add index values to the index_data_all dataframe
  race_data_all[[paste0("Index_", j)]] <- race_data_all2
}


# Calculate the general statistics 
race_data_all$mean=rowMeans(race_data_all[, 2:ncol(race_data_all)],na.rm = TRUE)
race_data_all$min=apply(race_data_all[, 2:10001], 1, min, na.rm = TRUE)
race_data_all$max=apply(race_data_all[, 2:10001], 1, max, na.rm = TRUE)
race_data_all$median=apply(race_data_all[, 2:10001], 1, median, na.rm = TRUE)

# Summarize data into a new data frame
summary_data_race=data.frame("GEOID"=race_data_all[,1],race_data_all[,10002:10005])
summary_data_race$range=summary_data_race$max-summary_data_race$min
# Calculate the estimated SDI
summary_data_race$estimate=1-((cleaned_data_all$racewhiteE*(cleaned_data_all$racewhiteE-1)+cleaned_data_all$raceblackE*(cleaned_data_all$raceblackE-1)+cleaned_data_all$raceasianE*(cleaned_data_all$raceasianE-1)+cleaned_data_all$IndianE*(cleaned_data_all$IndianE-1)+cleaned_data_all$HawaiianE*(cleaned_data_all$HawaiianE-1)+cleaned_data_all$otherE*(cleaned_data_all$otherE-1)+cleaned_data_all$twootherracesE*(cleaned_data_all$twootherracesE-1))/
                                ((cleaned_data_all$raceblackE+cleaned_data_all$raceasianE+cleaned_data_all$IndianE+cleaned_data_all$racewhiteE+cleaned_data_all$HawaiianE+cleaned_data_all$otherE+cleaned_data_all$twootherracesE)*((cleaned_data_all$raceblackE+cleaned_data_all$raceasianE+cleaned_data_all$IndianE+cleaned_data_all$racewhiteE+cleaned_data_all$HawaiianE+cleaned_data_all$otherE+cleaned_data_all$twootherracesE)-1)))
#Save the data
save(race_data_all,file="")  
save(summary_data_race,file="" )

### Calculate the Pearson's R correlation 
JoinData=cleaned_data_all[,c("GEOID","MHIE","populationE.1")]
New_data_race=merge(JoinData,race_data_all,by="GEOID")

                                        
# Create dataframe to store data
index_columns_race<-New_data_race[,c(4:10003)]
var_columns_race<-New_data_race[,c(2:3)]
summary_stats_df_race_2014 <- data.frame(
  Index = character(0),
  MHICorr = numeric(0),
  TPopCorr = numeric(0),
  stringsAsFactors = FALSE
)
                                                                                
# Loop over each index column and calculate the correlation with both variables
for(i in 1:ncol(index_columns_race)) {
  # Calculate the correlation for both variables (10008 and 10009)
  cor_var1 <- cor(index_columns_race[, i], var_columns_race[, 1], use = "complete.obs")  # Correlation with Var1 (10008)
  cor_var2 <- cor(index_columns_race[, i], var_columns_race[, 2], use = "complete.obs")  # Correlation with Var2 (10009)
  
  # Collect the summary statistics for the correlations
  summary_stats_df_race_2014[i, ] <- data.frame(
    Index = paste("Index", i),
    MHICorr = cor_var1,
    TPopCorr = cor_var2
  )
}

# Save the data
save(summary_stats_df_race_2014,file="")


#######----data for 2018------
#Extract data for 2018
dfall=AllUSTracts(yr=2018,varlist = data,states=state_list,level="tract",surv = "acs5")
#remove all tract with no population/income
dfall_cleaned = dfall %>% filter(populationE !=0, MHIE !=0)
#remove na data
finaldfall=na.omit(dfall_cleaned)

# Reorganize the data into seven subgroups
cleaned_data_all=data.frame("GEOID"=finaldfall$GEOID,
                            "ageu18E"=finaldfall$ageu5mE+finaldfall$ageu5fE+finaldfall$age59mE+finaldfall$age59fE+finaldfall$age1014mE+finaldfall$age1014fE+finaldfall$age1517mE+finaldfall$age1517fE,
                            "ageu18M"=finaldfall$ageu5mM+finaldfall$ageu5fM+finaldfall$age59mM+finaldfall$age59fM+finaldfall$age1014mM+finaldfall$age1014fM+finaldfall$age1517mM+finaldfall$age1517fM,
                            "age1825E"=finaldfall$age1819mE+finaldfall$age1819fE+finaldfall$age20fE+finaldfall$age20mE+finaldfall$age21fE+finaldfall$age21mE+finaldfall$age2224mE+finaldfall$age2224fE,
                            "age1825M"=finaldfall$age1819mM+finaldfall$age1819fM+finaldfall$age20fM+finaldfall$age20mM+finaldfall$age21fM+finaldfall$age21mM+finaldfall$age2224mM+finaldfall$age2224fM,
                            "age2540E"=finaldfall$age2529mE+finaldfall$age2529fE+finaldfall$age3034mE+finaldfall$age3034fE+finaldfall$age3539mE+finaldfall$age3539fE,
                            "age2540M"=finaldfall$age2529mM+finaldfall$age2529fM+finaldfall$age3034mM+finaldfall$age3034fM+finaldfall$age3539mM+finaldfall$age3539fM,
                            "age4054E"=finaldfall$age4044mE+finaldfall$age4044fE+finaldfall$age4549mE+finaldfall$age4549fE+finaldfall$age5054mE+finaldfall$age5054fE,
                            "age4054M"=finaldfall$age4044mM+finaldfall$age4044fM+finaldfall$age4549mM+finaldfall$age4549fM+finaldfall$age5054mM+finaldfall$age5054fM,
                            "age5565E"=finaldfall$age5559mE+finaldfall$age5559fE+finaldfall$age6061E+finaldfall$age6061fE+finaldfall$age6264mE+finaldfall$age6264fE,
                            "age5565M"=finaldfall$age5559mM+finaldfall$age5559fM+finaldfall$age6061M+finaldfall$age6061fM+finaldfall$age6264mM+finaldfall$age6264fM,
                            "age6575E"=finaldfall$age6566mE+finaldfall$age6566fE+finaldfall$age6769mE+finaldfall$age6769fE+finaldfall$age7074mE+finaldfall$age7074fE,
                            "age6575M"=finaldfall$age6566mM+finaldfall$age6566fM+finaldfall$age6769mM+finaldfall$age6769fM+finaldfall$age7074mM+finaldfall$age7074fM,
                            "ageover75E"=finaldfall$age7579mE+finaldfall$age7579fE+finaldfall$age8084mE+finaldfall$age8084fE+finaldfall$ageabv85mE+finaldfall$ageabv85fE,
                            "ageover75M"=finaldfall$age7579mM+finaldfall$age7579fM+finaldfall$age8084mM+finaldfall$age8084fM+finaldfall$ageabv85mM+finaldfall$ageabv85fM,
                            
                            "populationE"=finaldfall$populationE,"populationM"=finaldfall$populationM,finaldfall[,95:115])



#---- for age 2018----
#create empty dataframe
index_data_all <- data.frame(GEOID = cleaned_data_all$GEOID)
set.seed(420)

# Calculate the composite index for each iteration
for (j in 1:10000) {
  # Calculating the random number for under 18 from within the MOE parameters
  random_ageu18 <- ifelse((cleaned_data_all$ageu18E-cleaned_data_all$ageu18M)<=0,
                          round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$ageu18E + cleaned_data_all$ageu18M),0),
                          round(runif(nrow(cleaned_data_all), min = cleaned_data_all$ageu18E - cleaned_data_all$ageu18M, max = cleaned_data_all$ageu18E + cleaned_data_all$ageu18M),0))
  
  # Calculating the random number for under 18-25 from within the MOE parameters
  random_age1825 <- ifelse((cleaned_data_all$age1825E-cleaned_data_all$age1825M)<=0,
                           round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$age1825E + cleaned_data_all$age1825M),0),
                           round(runif(nrow(cleaned_data_all), min = cleaned_data_all$age1825E - cleaned_data_all$age1825M, max = cleaned_data_all$age1825E + cleaned_data_all$age1825M),0))
  
  # Calculating the random number for under 25-40 from within the MOE parameters
  random_age2540 <- ifelse((cleaned_data_all$age2540E-cleaned_data_all$age2540M)<=0,
                           round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$age2540E + cleaned_data_all$age2540M),0),
                           round(runif(nrow(cleaned_data_all), min = cleaned_data_all$age2540E - cleaned_data_all$age2540M, max = cleaned_data_all$age2540E + cleaned_data_all$age2540M),0))
  
  # Calculating the random number for under 40-54 from within the MOE parameters
  random_age4054 <- ifelse((cleaned_data_all$age4054E-cleaned_data_all$age4054M)<=0,
                           round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$age4054E + cleaned_data_all$age4054M),0),
                           round(runif(nrow(cleaned_data_all), min = cleaned_data_all$age4054E - cleaned_data_all$age4054M, max = cleaned_data_all$age4054E + cleaned_data_all$age4054M),0))
  
  
  # Calculating the random number for 55-65 from within the MOE parameters
  random_age5565 <- ifelse((cleaned_data_all$age5565E-cleaned_data_all$age5565M)<=0,
                           round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$age5565E + cleaned_data_all$age5565M),0),
                           round(runif(nrow(cleaned_data_all), min = cleaned_data_all$age5565E - cleaned_data_all$age5565M, max = cleaned_data_all$age5565E + cleaned_data_all$age5565M),0))
  
  # Calculating the random number for 65-75 from within the MOE parameters
  random_age6575 <- ifelse((cleaned_data_all$age6575E-cleaned_data_all$age6575M)<=0,
                           round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$age6575E + cleaned_data_all$age6575M),0),
                           round(runif(nrow(cleaned_data_all), min = cleaned_data_all$age6575E - cleaned_data_all$age6575M, max = cleaned_data_all$age6575E + cleaned_data_all$age6575M),0))
  
  # Calculating the random number for over75 from within the MOE parameters
  random_ageover75 <- ifelse((cleaned_data_all$ageover75E-cleaned_data_all$ageover75M)<=0,
                             round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$ageover75E + cleaned_data_all$ageover75M),0),
                             round(runif(nrow(cleaned_data_all), min = cleaned_data_all$ageover75E - cleaned_data_all$ageover75M, max = cleaned_data_all$ageover75E + cleaned_data_all$ageover75M),0))
  
  # Calculating the random number for population from within the MOE parameters
  pop= ifelse((cleaned_data_all$populationE-cleaned_data_all$populationM)<=0,
              round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$populationE + cleaned_data_all$populationM),0),
              round(runif(nrow(cleaned_data_all), min = cleaned_data_all$populationE - cleaned_data_all$populationM, max = cleaned_data_all$populationE + cleaned_data_all$populationM),0))
  
  
  # Create a vector to store index values
  index_values <- numeric(nrow(cleaned_data_all)) 
  # Calculate SDI for each simulation
  cat("Iteration:", j, "\n")
  
  for (i in 1:nrow(cleaned_data_all)){
    index_values[i]=1-((random_ageu18[i]*(random_ageu18[i]-1)+random_age1825[i]*(random_age1825[i]-1)+random_age2540[i]*(random_age2540[i]-1)+random_age4054[i]*(random_age4054[i]-1)+random_age5565[i]*(random_age5565[i]-1)+random_age6575[i]*(random_age6575[i]-1)+random_ageover75[i]*(random_ageover75[i]-1))/
                         ((random_age1825[i]+random_age2540[i]+random_age4054[i]+random_age5565[i]+random_age6575[i]+random_ageover75[i]+random_ageu18[i])*((random_age1825[i]+random_age2540[i]+random_age4054[i]+random_age5565[i]+random_age6575[i]+random_ageover75[i]+random_ageu18[i])-1)))
  
  }
  
  # Add index values to the index_data_all dataframe
  index_data_all[[paste0("Index_", j)]] <- index_values
}

# Reorganized data and calculate general statistics
index_data_all$mean=rowMeans(index_data_all[, 2:ncol(index_data_all)],na.rm = TRUE)
index_data_all$min=apply(index_data_all[, 2:10001], 1, min, na.rm = TRUE)
index_data_all$max=apply(index_data_all[, 2:10001], 1, max, na.rm = TRUE)
index_data_all$median=apply(index_data_all[,2:10001],1,median,na.rm = TRUE)


# Summarize data into a new dataframe
summary_data_all=data.frame("GEOID"=index_data_all[,1],index_data_all[,10002:10005])
summary_data_all$range=summary_data_all$max-summary_data_all$min
# Calculate the estimated SDI
summary_data_all$estimate=1-((cleaned_data_all$ageu18E*(cleaned_data_all$ageu18E-1)+cleaned_data_all$age1825E*(cleaned_data_all$age1825E-1)+cleaned_data_all$age2540E*(cleaned_data_all$age2540E-1)+cleaned_data_all$age4054E*(cleaned_data_all$age4054E-1)+cleaned_data_all$age5565E*(cleaned_data_all$age5565E-1)+cleaned_data_all$age6575E*(cleaned_data_all$age6575E-1)+cleaned_data_all$ageover75E*(cleaned_data_all$ageover75E-1))/
                               ((cleaned_data_all$age1825E+cleaned_data_all$age2540E+cleaned_data_all$age4054E+cleaned_data_all$age5565E+cleaned_data_all$age6575E+cleaned_data_all$ageover75E+cleaned_data_all$ageu18E)*((cleaned_data_all$age1825E+cleaned_data_all$age2540E+cleaned_data_all$age4054E+cleaned_data_all$age5565E+cleaned_data_all$age6575E+cleaned_data_all$ageover75E+cleaned_data_all$ageu18E)-1)))


# Save the data
save(summary_data_all,file="" )
save(index_data_all,file="")

### Calculate the Pearson's R correlation 
JoinData=cleaned_data_all[,c("GEOID","MHIE","populationE.1")]
Newdata<-merge(JoinData, index_data_all, by="GEOID")

# Create data frame to store data
index_columns<-Newdata[,c(4:10003)]
var_columns<-Newdata[,c(2:3)]
summary_stats_df_age_2018 <- data.frame(
  Index = character(0),
  MHICorr = numeric(0),
  TPopCorr = numeric(0),
  stringsAsFactors = FALSE
)

# Loop over each index column and calculate the correlation with both variables
for(i in 1:ncol(index_columns)) {
  # Calculate the correlation for both variables (10008 and 10009)
  cor_var1 <- cor(index_columns[, i], var_columns[, 1], use = "complete.obs")  # Correlation with Var1 (10008)
  cor_var2 <- cor(index_columns[, i], var_columns[, 2], use = "complete.obs")  # Correlation with Var2 (10009)
  
  # Collect the summary statistics for the correlations
  summary_stats_df_age_2018[i, ] <- data.frame(
    Index = paste("Index", i),
    MHICorr = cor_var1,
    TPopCorr = cor_var2
  )
}

# Save the data
save(summary_stats_df_age_2018,file="")





######---- for race 2018 ----
#Create the data frame to store data
race_data_all <- data.frame(GEOID = cleaned_data_all$GEOID)
set.seed(420)

# Calculate the composite index for each iteration
for (j in 1:10000) {
  
  # white
  white <- ifelse((cleaned_data_all$racewhiteE-cleaned_data_all$racewhiteM)<=0,
                  round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$racewhiteE + cleaned_data_all$racewhiteM),0),
                  round(runif(nrow(cleaned_data_all), min = cleaned_data_all$racewhiteE - cleaned_data_all$racewhiteM, max = cleaned_data_all$racewhiteE + cleaned_data_all$racewhiteM),0))
  
  # black
  black <- ifelse((cleaned_data_all$raceblackE-cleaned_data_all$raceblackM)<=0,
                  round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$raceblackE + cleaned_data_all$raceblackM),0),
                  round(runif(nrow(cleaned_data_all), min = cleaned_data_all$raceblackE - cleaned_data_all$raceblackM, max = cleaned_data_all$raceblackE + cleaned_data_all$raceblackM),0))
  
  # asian
  asian <- ifelse((cleaned_data_all$raceasianE-cleaned_data_all$raceasianM)<=0,
                  round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$raceasianE + cleaned_data_all$raceasianM),0),
                  round(runif(nrow(cleaned_data_all), min = cleaned_data_all$raceasianE - cleaned_data_all$raceasianM, max = cleaned_data_all$raceasianE + cleaned_data_all$raceasianM),0))
  # Indian
  Indian <- ifelse((cleaned_data_all$IndianE-cleaned_data_all$IndianM)<=0,
                   round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$IndianE + cleaned_data_all$IndianM),0),
                   round(runif(nrow(cleaned_data_all), min = cleaned_data_all$IndianE - cleaned_data_all$IndianM, max = cleaned_data_all$IndianE + cleaned_data_all$IndianM),0))
  # Hawaiian
  Hawaiian <- ifelse((cleaned_data_all$HawaiianE-cleaned_data_all$HawaiianM)<=0,
                     round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$HawaiianE + cleaned_data_all$HawaiianM),0),
                     round(runif(nrow(cleaned_data_all), min = cleaned_data_all$HawaiianE - cleaned_data_all$HawaiianM, max = cleaned_data_all$HawaiianE + cleaned_data_all$HawaiianM),0))
  
  # other
  other <- ifelse((cleaned_data_all$otherE-cleaned_data_all$otherM)<=0,
                  round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$otherE + cleaned_data_all$otherM),0),
                  round(runif(nrow(cleaned_data_all), min = cleaned_data_all$otherE - cleaned_data_all$otherM, max = cleaned_data_all$otherE + cleaned_data_all$otherM),0))
  
  # twootherraces
  twootherraces <- ifelse((cleaned_data_all$twootherracesE-cleaned_data_all$twootherracesM)<=0,
                          round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$twootherracesE + cleaned_data_all$twootherracesM),0),
                          round(runif(nrow(cleaned_data_all), min = cleaned_data_all$twootherracesE - cleaned_data_all$twootherracesM, max = cleaned_data_all$twootherracesE + cleaned_data_all$twootherracesM),0))
  
  
  # Create a vector to store index values
  race_data_all2 <- numeric(nrow(cleaned_data_all)) 
  cat("Iteration:", j, "\n")
  
  # Calculate the SDI for each simulation
  for (i in 1:nrow(cleaned_data_all)){
    race_data_all2[i]=1-((white[i]*(white[i]-1)+black[i]*(black[i]-1)+asian[i]*(asian[i]-1)+Indian[i]*(Indian[i]-1)+Hawaiian[i]*(Hawaiian[i]-1)+other[i]*(other[i]-1)+twootherraces[i]*(twootherraces[i]-1))/
                           ((black[i]+asian[i]+Indian[i]+white[i]+Hawaiian[i]+other[i]+twootherraces[i])*((black[i]+asian[i]+Indian[i]+white[i]+Hawaiian[i]+other[i]+twootherraces[i])-1)))
    
  }
  
  # Add index values to the index_data_all dataframe
  race_data_all[[paste0("Index_", j)]] <- race_data_all2
  
}

# Calculate the gengeral statistic
race_data_all$mean=rowMeans(race_data_all[, 2:ncol(race_data_all)],na.rm = TRUE)
race_data_all$min=apply(race_data_all[, 2:10001], 1, min, na.rm = TRUE)
race_data_all$max=apply(race_data_all[, 2:10001], 1, max, na.rm = TRUE)
race_data_all$median=apply(race_data_all[, 2:10001], 1, median, na.rm = TRUE)

#Summarize into a new data frame
summary_data_race=data.frame("GEOID"=race_data_all[,1],race_data_all[,10002:10005])
summary_data_race$range=summary_data_race$max-summary_data_race$min
# Calculate the estimated SDI
summary_data_race$estimate=1-((cleaned_data_all$racewhiteE*(cleaned_data_all$racewhiteE-1)+cleaned_data_all$raceblackE*(cleaned_data_all$raceblackE-1)+cleaned_data_all$raceasianE*(cleaned_data_all$raceasianE-1)+cleaned_data_all$IndianE*(cleaned_data_all$IndianE-1)+cleaned_data_all$HawaiianE*(cleaned_data_all$HawaiianE-1)+cleaned_data_all$otherE*(cleaned_data_all$otherE-1)+cleaned_data_all$twootherracesE*(cleaned_data_all$twootherracesE-1))/
                                ((cleaned_data_all$raceblackE+cleaned_data_all$raceasianE+cleaned_data_all$IndianE+cleaned_data_all$racewhiteE+cleaned_data_all$HawaiianE+cleaned_data_all$otherE+cleaned_data_all$twootherracesE)*((cleaned_data_all$raceblackE+cleaned_data_all$raceasianE+cleaned_data_all$IndianE+cleaned_data_all$racewhiteE+cleaned_data_all$HawaiianE+cleaned_data_all$otherE+cleaned_data_all$twootherracesE)-1)))
# Save the data
save(race_data_all,file="")  
save(summary_data_race,file="" )

### Calculate the Pearson's R correlation 
JoinData=cleaned_data_all[,c("GEOID","MHIE","populationE.1")]
New_data_race=merge(JoinData,race_data_all,by="GEOID")

# Create data frame to store data
index_columns_race<-New_data_race[,c(4:10003)]
var_columns_race<-New_data_race[,c(2:3)]
summary_stats_df_race_2018 <- data.frame(
  Index = character(0),
  MHICorr = numeric(0),
  TPopCorr = numeric(0),
  stringsAsFactors = FALSE
)

# Loop over each index column and calculate the correlation with both variables
for(i in 1:ncol(index_columns_race)) {
  # Calculate the correlation for both variables (10008 and 10009)
  cor_var1 <- cor(index_columns_race[, i], var_columns_race[, 1], use = "complete.obs")  # Correlation with Var1 (10008)
  cor_var2 <- cor(index_columns_race[, i], var_columns_race[, 2], use = "complete.obs")  # Correlation with Var2 (10009)
  
  # Collect the summary statistics for the correlations
  summary_stats_df_race_2018[i, ] <- data.frame(
    Index = paste("Index", i),
    MHICorr = cor_var1,
    TPopCorr = cor_var2
  )
}

# Save the data
save(summary_stats_df_race_2018,file="")


#######----2022------
# Extract data for 2022
dfall=AllUSTracts(yr=2022,varlist = data,states=state_list,level="tract",surv = "acs5")
#remove all tract with no population/income
dfall_cleaned = dfall %>% filter(populationE !=0, MHIE !=0)
#remove na data
finaldfall=na.omit(dfall_cleaned)

# Reorganize the data into seven subgroups
cleaned_data_all=data.frame("GEOID"=finaldfall$GEOID,
                            "ageu18E"=finaldfall$ageu5mE+finaldfall$ageu5fE+finaldfall$age59mE+finaldfall$age59fE+finaldfall$age1014mE+finaldfall$age1014fE+finaldfall$age1517mE+finaldfall$age1517fE,
                            "ageu18M"=finaldfall$ageu5mM+finaldfall$ageu5fM+finaldfall$age59mM+finaldfall$age59fM+finaldfall$age1014mM+finaldfall$age1014fM+finaldfall$age1517mM+finaldfall$age1517fM,
                            "age1825E"=finaldfall$age1819mE+finaldfall$age1819fE+finaldfall$age20fE+finaldfall$age20mE+finaldfall$age21fE+finaldfall$age21mE+finaldfall$age2224mE+finaldfall$age2224fE,
                            "age1825M"=finaldfall$age1819mM+finaldfall$age1819fM+finaldfall$age20fM+finaldfall$age20mM+finaldfall$age21fM+finaldfall$age21mM+finaldfall$age2224mM+finaldfall$age2224fM,
                            "age2540E"=finaldfall$age2529mE+finaldfall$age2529fE+finaldfall$age3034mE+finaldfall$age3034fE+finaldfall$age3539mE+finaldfall$age3539fE,
                            "age2540M"=finaldfall$age2529mM+finaldfall$age2529fM+finaldfall$age3034mM+finaldfall$age3034fM+finaldfall$age3539mM+finaldfall$age3539fM,
                            "age4054E"=finaldfall$age4044mE+finaldfall$age4044fE+finaldfall$age4549mE+finaldfall$age4549fE+finaldfall$age5054mE+finaldfall$age5054fE,
                            "age4054M"=finaldfall$age4044mM+finaldfall$age4044fM+finaldfall$age4549mM+finaldfall$age4549fM+finaldfall$age5054mM+finaldfall$age5054fM,
                            "age5565E"=finaldfall$age5559mE+finaldfall$age5559fE+finaldfall$age6061E+finaldfall$age6061fE+finaldfall$age6264mE+finaldfall$age6264fE,
                            "age5565M"=finaldfall$age5559mM+finaldfall$age5559fM+finaldfall$age6061M+finaldfall$age6061fM+finaldfall$age6264mM+finaldfall$age6264fM,
                            "age6575E"=finaldfall$age6566mE+finaldfall$age6566fE+finaldfall$age6769mE+finaldfall$age6769fE+finaldfall$age7074mE+finaldfall$age7074fE,
                            "age6575M"=finaldfall$age6566mM+finaldfall$age6566fM+finaldfall$age6769mM+finaldfall$age6769fM+finaldfall$age7074mM+finaldfall$age7074fM,
                            "ageover75E"=finaldfall$age7579mE+finaldfall$age7579fE+finaldfall$age8084mE+finaldfall$age8084fE+finaldfall$ageabv85mE+finaldfall$ageabv85fE,
                            "ageover75M"=finaldfall$age7579mM+finaldfall$age7579fM+finaldfall$age8084mM+finaldfall$age8084fM+finaldfall$ageabv85mM+finaldfall$ageabv85fM,
                            "populationE"=finaldfall$populationE,"populationM"=finaldfall$populationM,finaldfall[,95:115])


#---- for age 2022----
# Create empty data frame for simulation
index_data_all <- data.frame(GEOID = cleaned_data_all$GEOID)
set.seed(420)

# Calculate the composite index for each iteration
for (j in 1:10000) {
  # Calculating the random number for under 18 from within the MOE parameters
  random_ageu18 <- ifelse((cleaned_data_all$ageu18E-cleaned_data_all$ageu18M)<=0,
                          round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$ageu18E + cleaned_data_all$ageu18M),0),
                          round(runif(nrow(cleaned_data_all), min = cleaned_data_all$ageu18E - cleaned_data_all$ageu18M, max = cleaned_data_all$ageu18E + cleaned_data_all$ageu18M),0))
  
  # Calculating the random number for under 18-25 from within the MOE parameters
  random_age1825 <- ifelse((cleaned_data_all$age1825E-cleaned_data_all$age1825M)<=0,
                           round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$age1825E + cleaned_data_all$age1825M),0),
                           round(runif(nrow(cleaned_data_all), min = cleaned_data_all$age1825E - cleaned_data_all$age1825M, max = cleaned_data_all$age1825E + cleaned_data_all$age1825M),0))
  
  # Calculating the random number for under 25-40 from within the MOE parameters
  random_age2540 <- ifelse((cleaned_data_all$age2540E-cleaned_data_all$age2540M)<=0,
                           round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$age2540E + cleaned_data_all$age2540M),0),
                           round(runif(nrow(cleaned_data_all), min = cleaned_data_all$age2540E - cleaned_data_all$age2540M, max = cleaned_data_all$age2540E + cleaned_data_all$age2540M),0))
  
  # Calculating the random number for under 40-54 from within the MOE parameters
  random_age4054 <- ifelse((cleaned_data_all$age4054E-cleaned_data_all$age4054M)<=0,
                           round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$age4054E + cleaned_data_all$age4054M),0),
                           round(runif(nrow(cleaned_data_all), min = cleaned_data_all$age4054E - cleaned_data_all$age4054M, max = cleaned_data_all$age4054E + cleaned_data_all$age4054M),0))
  
  
  # Calculating the random number for 55-65 from within the MOE parameters
  random_age5565 <- ifelse((cleaned_data_all$age5565E-cleaned_data_all$age5565M)<=0,
                           round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$age5565E + cleaned_data_all$age5565M),0),
                           round(runif(nrow(cleaned_data_all), min = cleaned_data_all$age5565E - cleaned_data_all$age5565M, max = cleaned_data_all$age5565E + cleaned_data_all$age5565M),0))
  
  # Calculating the random number for 65-75 from within the MOE parameters
  random_age6575 <- ifelse((cleaned_data_all$age6575E-cleaned_data_all$age6575M)<=0,
                           round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$age6575E + cleaned_data_all$age6575M),0),
                           round(runif(nrow(cleaned_data_all), min = cleaned_data_all$age6575E - cleaned_data_all$age6575M, max = cleaned_data_all$age6575E + cleaned_data_all$age6575M),0))
  
  # Calculating the random number for over75 from within the MOE parameters
  random_ageover75 <- ifelse((cleaned_data_all$ageover75E-cleaned_data_all$ageover75M)<=0,
                             round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$ageover75E + cleaned_data_all$ageover75M),0),
                             round(runif(nrow(cleaned_data_all), min = cleaned_data_all$ageover75E - cleaned_data_all$ageover75M, max = cleaned_data_all$ageover75E + cleaned_data_all$ageover75M),0))
  
  # Calculating the random number for population from within the MOE parameters
  pop= ifelse((cleaned_data_all$populationE-cleaned_data_all$populationM)<=0,
              round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$populationE + cleaned_data_all$populationM),0),
              round(runif(nrow(cleaned_data_all), min = cleaned_data_all$populationE - cleaned_data_all$populationM, max = cleaned_data_all$populationE + cleaned_data_all$populationM),0))
  
  
  # Create a vector to store index values
  index_values <- numeric(nrow(cleaned_data_all)) 
  
  cat("Iteration:", j, "\n")
  
  #Calculate SDI for each simulation
  for (i in 1:nrow(cleaned_data_all)){
    index_values[i]=1-((random_ageu18[i]*(random_ageu18[i]-1)+random_age1825[i]*(random_age1825[i]-1)+random_age2540[i]*(random_age2540[i]-1)+random_age4054[i]*(random_age4054[i]-1)+random_age5565[i]*(random_age5565[i]-1)+random_age6575[i]*(random_age6575[i]-1)+random_ageover75[i]*(random_ageover75[i]-1))/
                         ((random_age1825[i]+random_age2540[i]+random_age4054[i]+random_age5565[i]+random_age6575[i]+random_ageover75[i]+random_ageu18[i])*((random_age1825[i]+random_age2540[i]+random_age4054[i]+random_age5565[i]+random_age6575[i]+random_ageover75[i]+random_ageu18[i])-1)))

  }
  
  # Add index values to the index_data_all dataframe
  index_data_all[[paste0("Index_", j)]] <- index_values
}

# Reorganize the data and calculate the general statistics  
index_data_all$mean=rowMeans(index_data_all[, 2:ncol(index_data_all)],na.rm = TRUE)
index_data_all$min=apply(index_data_all[, 2:10001], 1, min, na.rm = TRUE)
index_data_all$max=apply(index_data_all[, 2:10001], 1, max, na.rm = TRUE)
index_data_all$median=apply(index_data_all[,2:10001],1,median,na.rm = TRUE)


# Summarize data into a new dataframe
summary_data_all=data.frame("GEOID"=index_data_all[,1],index_data_all[,10002:10005])
summary_data_all$range=summary_data_all$max-summary_data_all$min
# Calculate the estimated SDI
summary_data_all$estimate=1-((cleaned_data_all$ageu18E*(cleaned_data_all$ageu18E-1)+cleaned_data_all$age1825E*(cleaned_data_all$age1825E-1)+cleaned_data_all$age2540E*(cleaned_data_all$age2540E-1)+cleaned_data_all$age4054E*(cleaned_data_all$age4054E-1)+cleaned_data_all$age5565E*(cleaned_data_all$age5565E-1)+cleaned_data_all$age6575E*(cleaned_data_all$age6575E-1)+cleaned_data_all$ageover75E*(cleaned_data_all$ageover75E-1))/
                               ((cleaned_data_all$age1825E+cleaned_data_all$age2540E+cleaned_data_all$age4054E+cleaned_data_all$age5565E+cleaned_data_all$age6575E+cleaned_data_all$ageover75E+cleaned_data_all$ageu18E)*((cleaned_data_all$age1825E+cleaned_data_all$age2540E+cleaned_data_all$age4054E+cleaned_data_all$age5565E+cleaned_data_all$age6575E+cleaned_data_all$ageover75E+cleaned_data_all$ageu18E)-1)))


# Save the data
save(summary_data_all,file="" )
save(index_data_all,file="")

### Calculate the Pearson's R correlation 
JoinData=cleaned_data_all[,c("GEOID","MHIE","populationE.1")]
Newdata<-merge(JoinData, index_data_all, by="GEOID")

# Create data frame to store data
index_columns<-Newdata[,c(4:10003)]
var_columns<-Newdata[,c(2:3)]
summary_stats_df_age_2022 <- data.frame(
  Index = character(0),
  MHICorr = numeric(0),
  TPopCorr = numeric(0),
  stringsAsFactors = FALSE
)

# Loop over each index column and calculate the correlation with both variables
for(i in 1:ncol(index_columns)) {
  # Calculate the correlation for both variables (10008 and 10009)
  cor_var1 <- cor(index_columns[, i], var_columns[, 1], use = "complete.obs")  # Correlation with Var1 (10008)
  cor_var2 <- cor(index_columns[, i], var_columns[, 2], use = "complete.obs")  # Correlation with Var2 (10009)
  
  # Collect the summary statistics for the correlations
  summary_stats_df_age_2022[i, ] <- data.frame(
    Index = paste("Index", i),
    MHICorr = cor_var1,
    TPopCorr = cor_var2
  )
}

#Save the data
save(summary_stats_df_age_2022,file="")



######---- for race 2022 ----
# Create empty data frame for simulation
race_data_all <- data.frame(GEOID = cleaned_data_all$GEOID)
set.seed(420)

# Calculate the composite index for each iteration
for (j in 1:10000) {
  
  # white
  white <- ifelse((cleaned_data_all$racewhiteE-cleaned_data_all$racewhiteM)<=0,
                  round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$racewhiteE + cleaned_data_all$racewhiteM),0),
                  round(runif(nrow(cleaned_data_all), min = cleaned_data_all$racewhiteE - cleaned_data_all$racewhiteM, max = cleaned_data_all$racewhiteE + cleaned_data_all$racewhiteM),0))
  
  # black
  black <- ifelse((cleaned_data_all$raceblackE-cleaned_data_all$raceblackM)<=0,
                  round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$raceblackE + cleaned_data_all$raceblackM),0),
                  round(runif(nrow(cleaned_data_all), min = cleaned_data_all$raceblackE - cleaned_data_all$raceblackM, max = cleaned_data_all$raceblackE + cleaned_data_all$raceblackM),0))
  
  # asian
  asian <- ifelse((cleaned_data_all$raceasianE-cleaned_data_all$raceasianM)<=0,
                  round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$raceasianE + cleaned_data_all$raceasianM),0),
                  round(runif(nrow(cleaned_data_all), min = cleaned_data_all$raceasianE - cleaned_data_all$raceasianM, max = cleaned_data_all$raceasianE + cleaned_data_all$raceasianM),0))
  # Indian
  Indian <- ifelse((cleaned_data_all$IndianE-cleaned_data_all$IndianM)<=0,
                   round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$IndianE + cleaned_data_all$IndianM),0),
                   round(runif(nrow(cleaned_data_all), min = cleaned_data_all$IndianE - cleaned_data_all$IndianM, max = cleaned_data_all$IndianE + cleaned_data_all$IndianM),0))
  # Hawaiian
  Hawaiian <- ifelse((cleaned_data_all$HawaiianE-cleaned_data_all$HawaiianM)<=0,
                     round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$HawaiianE + cleaned_data_all$HawaiianM),0),
                     round(runif(nrow(cleaned_data_all), min = cleaned_data_all$HawaiianE - cleaned_data_all$HawaiianM, max = cleaned_data_all$HawaiianE + cleaned_data_all$HawaiianM),0))
  
  # other
  other <- ifelse((cleaned_data_all$otherE-cleaned_data_all$otherM)<=0,
                  round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$otherE + cleaned_data_all$otherM),0),
                  round(runif(nrow(cleaned_data_all), min = cleaned_data_all$otherE - cleaned_data_all$otherM, max = cleaned_data_all$otherE + cleaned_data_all$otherM),0))
  
  # twootherraces
  twootherraces <- ifelse((cleaned_data_all$twootherracesE-cleaned_data_all$twootherracesM)<=0,
                          round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$twootherracesE + cleaned_data_all$twootherracesM),0),
                          round(runif(nrow(cleaned_data_all), min = cleaned_data_all$twootherracesE - cleaned_data_all$twootherracesM, max = cleaned_data_all$twootherracesE + cleaned_data_all$twootherracesM),0))
  
  
  
  # Create a vector to store index values
  race_data_all2 <- numeric(nrow(cleaned_data_all)) 
  cat("Iteration:", j, "\n")
  
  # Calculate the SDI for each simulation
  for (i in 1:nrow(cleaned_data_all)){
    race_data_all2[i]=1-((white[i]*(white[i]-1)+black[i]*(black[i]-1)+asian[i]*(asian[i]-1)+Indian[i]*(Indian[i]-1)+Hawaiian[i]*(Hawaiian[i]-1)+other[i]*(other[i]-1)+twootherraces[i]*(twootherraces[i]-1))/
                           ((black[i]+asian[i]+Indian[i]+white[i]+Hawaiian[i]+other[i]+twootherraces[i])*((black[i]+asian[i]+Indian[i]+white[i]+Hawaiian[i]+other[i]+twootherraces[i])-1)))
   
  }
  
  # Add index values to the index_data_all dataframe
  race_data_all[[paste0("Index_", j)]] <- race_data_all2
  
}


# Reorganize the data and calculate the general statistics 
race_data_all$mean=rowMeans(race_data_all[, 2:ncol(race_data_all)],na.rm = TRUE)
race_data_all$min=apply(race_data_all[, 2:10001], 1, min, na.rm = TRUE)
race_data_all$max=apply(race_data_all[, 2:10001], 1, max, na.rm = TRUE)
race_data_all$median=apply(race_data_all[, 2:10001], 1, median, na.rm = TRUE)

# Summarize data into a new dataframe
summary_data_race=data.frame("GEOID"=race_data_all[,1],race_data_all[,10002:10005])
summary_data_race$range=summary_data_race$max-summary_data_race$min

# Calculate the estimated SDI
summary_data_race$estimate=1-((cleaned_data_all$racewhiteE*(cleaned_data_all$racewhiteE-1)+cleaned_data_all$raceblackE*(cleaned_data_all$raceblackE-1)+cleaned_data_all$raceasianE*(cleaned_data_all$raceasianE-1)+cleaned_data_all$IndianE*(cleaned_data_all$IndianE-1)+cleaned_data_all$HawaiianE*(cleaned_data_all$HawaiianE-1)+cleaned_data_all$otherE*(cleaned_data_all$otherE-1)+cleaned_data_all$twootherracesE*(cleaned_data_all$twootherracesE-1))/
                                ((cleaned_data_all$raceblackE+cleaned_data_all$raceasianE+cleaned_data_all$IndianE+cleaned_data_all$racewhiteE+cleaned_data_all$HawaiianE+cleaned_data_all$otherE+cleaned_data_all$twootherracesE)*((cleaned_data_all$raceblackE+cleaned_data_all$raceasianE+cleaned_data_all$IndianE+cleaned_data_all$racewhiteE+cleaned_data_all$HawaiianE+cleaned_data_all$otherE+cleaned_data_all$twootherracesE)-1)))
#Save the data
save(race_data_all,file="")  
save(summary_data_race,file="" )

### Calculate the Pearson's R correlation
JoinData=cleaned_data_all[,c("GEOID","MHIE","populationE.1")]
New_data_race=merge(JoinData,race_data_all,by="GEOID")

# Create new data frame 
index_columns_race<-New_data_race[,c(4:10003)]
var_columns_race<-New_data_race[,c(2:3)]
summary_stats_df_race_2022 <- data.frame(
  Index = character(0),
  MHICorr = numeric(0),
  TPopCorr = numeric(0),
  stringsAsFactors = FALSE
)

# Loop over each index column and calculate the correlation with both variables
for(i in 1:ncol(index_columns_race)) {
  # Calculate the correlation for both variables (10008 and 10009)
  cor_var1 <- cor(index_columns_race[, i], var_columns_race[, 1], use = "complete.obs")  # Correlation with Var1 (10008)
  cor_var2 <- cor(index_columns_race[, i], var_columns_race[, 2], use = "complete.obs")  # Correlation with Var2 (10009)
  
  # Collect the summary statistics for the correlations
  summary_stats_df_race_2022[i, ] <- data.frame(
    Index = paste("Index", i),
    MHICorr = cor_var1,
    TPopCorr = cor_var2
  )
}

# Save the data
save(summary_stats_df_race_2022,file="")





