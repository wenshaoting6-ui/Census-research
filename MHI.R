####----------install the packages------------
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



#####---------------Function for downloading census data-----

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

###iuput your own census api key

#census_api_key(api)


####------Code for Running the simulation

###create the list with groups of population in incomes

MHI=c(inless10="B19001_002",
      in1015="B19001_003",
      in1520="B19001_004",
      in2025="B19001_005",
      in2530="B19001_006",
      in3035="B19001_007",
      in3540="B19001_008",
      in4045="B19001_009",
      in4550="B19001_010",
      in5060="B19001_011",
      in6075="B19001_012",
      in75100="B19001_013",
      in100125="B19001_014",
      in125150="B19001_015",
      in150200="B19001_016",
      inover200="B19001_017",
      population="B01003_001",
      MHI="B19013_001"
      )

##########2010------
####Extract data for 2010
dfall=AllUSTracts(yr=2010,varlist = MHI,states=state_list,level="tract",surv = "acs5")
dfall_cleaned = dfall %>% filter(populationE !=0)
finaldfall=na.omit(dfall_cleaned)
# Reorganize the data into seven subgroups
cleaned_data_all=data.frame("GEOID"=finaldfall$GEOID,
                            "inless25E"=finaldfall$inless10E+finaldfall$in1015E+finaldfall$in1520E+finaldfall$in2025E,
                            "inless25M"=finaldfall$inless10M+finaldfall$in1015M+finaldfall$in1520M+finaldfall$in2025M,
                            "in2550E"=finaldfall$in2530E+finaldfall$in3035E+finaldfall$in3540E+finaldfall$in4045E+finaldfall$in4550E,
                            "in2550M"=finaldfall$in2530M+finaldfall$in3035M+finaldfall$in3540M+finaldfall$in4045M+finaldfall$in4550M,
                            "in5075E"=finaldfall$in5060E+finaldfall$in6075E,
                            "in5075M"=finaldfall$in5060M+finaldfall$in6075M,
                            "in75100E"=finaldfall$in75100E,
                            "in75100M"=finaldfall$in75100M,
                            "in100150E"=finaldfall$in100125E+finaldfall$in125150E,
                            "in100150M"=finaldfall$in100125M+finaldfall$in125150M,
                            "in150200E"=finaldfall$in150200E,
                            "in150200M"=finaldfall$in150200M,
                            "inover200E"=finaldfall$inover200E,
                            "inover200M"=finaldfall$inover200M,
                            finaldfall[,35:39])


# Create empty data frame for simulation
index_data_all <- data.frame(GEOID = cleaned_data_all$GEOID)


set.seed(420)

# Calculate the composite index for each iteration
for (j in 1:10000) {
  # Calculating the random number for under $25,000 from within the MOE parameters
  random_inless25 <- ifelse((cleaned_data_all$inless25E-cleaned_data_all$inless25M)<=0,
                            round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$inless25E + cleaned_data_all$inless25M),0),
                            round(runif(nrow(cleaned_data_all), min = cleaned_data_all$inless25E - cleaned_data_all$inless25M, max = cleaned_data_all$inless25E + cleaned_data_all$inless25M),0))
  
  # Calculating the random number for $25,000-50,000 from within the MOE parameters
  random_in2550 <- ifelse((cleaned_data_all$in2550E-cleaned_data_all$in2550M)<=0,
                          round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$in2550E + cleaned_data_all$in2550M),0),
                          round(runif(nrow(cleaned_data_all), min = cleaned_data_all$in2550E - cleaned_data_all$in2550M, max = cleaned_data_all$in2550E + cleaned_data_all$in2550M),0))
  
  # Calculating the random number for $50,000-75,000 from within the MOE parameters
  random_in5075 <- ifelse((cleaned_data_all$in5075E-cleaned_data_all$in5075M)<=0,
                          round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$in5075E + cleaned_data_all$in5075M),0),
                          round(runif(nrow(cleaned_data_all), min = cleaned_data_all$in5075E - cleaned_data_all$in5075M, max = cleaned_data_all$in5075E + cleaned_data_all$in5075M),0))
  
  # Calculating the random number for $75,000-100,000 from within the MOE parameters
  random_in75100 <- ifelse((cleaned_data_all$in75100E-cleaned_data_all$in75100M)<=0,
                           round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$in75100E + cleaned_data_all$in75100M),0),
                           round(runif(nrow(cleaned_data_all), min = cleaned_data_all$in75100E - cleaned_data_all$in75100M, max = cleaned_data_all$in75100E + cleaned_data_all$in75100M),0))
  
  
  # Calculating the random number for $100,000-150,000 from within the MOE parameters
  random_in100150 <- ifelse((cleaned_data_all$in100150E-cleaned_data_all$in100150M)<=0,
                            round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$in100150E + cleaned_data_all$in100150M),0),
                            round(runif(nrow(cleaned_data_all), min = cleaned_data_all$in100150E - cleaned_data_all$in100150M, max = cleaned_data_all$in100150E + cleaned_data_all$in100150M),0))
  
  # Calculating the random number for $150,000-200,000 from within the MOE parameters
  random_in150200 <- ifelse((cleaned_data_all$in150200E-cleaned_data_all$in150200M)<=0,
                            round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$in150200E + cleaned_data_all$in150200M),0),
                            round(runif(nrow(cleaned_data_all), min = cleaned_data_all$in150200E - cleaned_data_all$in150200M, max = cleaned_data_all$in150200E + cleaned_data_all$in150200M),0))
  
  # Calculating the random number for over $200,000 from within the MOE parameters
  random_inover200 <- ifelse((cleaned_data_all$inover200E-cleaned_data_all$inover200M)<=0,
                             round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$inover200E + cleaned_data_all$inover200M),0),
                             round(runif(nrow(cleaned_data_all), min = cleaned_data_all$inover200E - cleaned_data_all$inover200M, max = cleaned_data_all$inover200E + cleaned_data_all$inover200M),0))
  
  
  # Create a vector to store index values
  index_values <- numeric(nrow(cleaned_data_all)) 
 
 
  cat("Iteration:", j, "\n")
  
  # Calculate the SDI for each simulation
  for (i in 1:nrow(cleaned_data_all)){
    index_values[i]=1-((random_inless25[i]*(random_inless25[i]-1)+random_in2550[i]*(random_in2550[i]-1)+random_in5075[i]*(random_in5075[i]-1)+random_in75100[i]*(random_in75100[i]-1)+random_in100150[i]*(random_in100150[i]-1)+random_in150200[i]*(random_in150200[i]-1)+random_inover200[i]*(random_inover200[i]-1))/
                         ((random_in2550[i]+random_in5075[i]+random_in75100[i]+random_in100150[i]+random_in150200[i]+random_inover200[i]+random_inless25[i])*((random_in2550[i]+random_in5075[i]+random_in75100[i]+random_in100150[i]+random_in150200[i]+random_inover200[i]+random_inless25[i])-1)))
    
  }
  # Add index values to the index_data_all dataframe
  index_data_all[[paste0("Index_", j)]] <- index_values

}


# Reorganize the data and calculate the general statistics  

index_data_all$mean=rowMeans(index_data_all[, 2:ncol(index_data_all)],na.rm = TRUE)
index_data_all$min=apply(index_data_all[, 2:10001], 1, min, na.rm = TRUE)
index_data_all$max=apply(index_data_all[, 2:10001], 1, max, na.rm = TRUE)
index_data_all$median=apply(index_data_all[, 2:10001], 1,median, na.rm = TRUE)

# Summarize data into a new dataframe
summary_data_all=data.frame("GEOID"=index_data_all[,1],index_data_all[,10002:10005])
summary_data_all$range=summary_data_all$max-summary_data_all$min
# Calculate the estimated SDI
summary_data_all$estimate=estimate_value=1-((cleaned_data_all$inless25E*(cleaned_data_all$inless25E-1)+cleaned_data_all$in2550E*(cleaned_data_all$in2550E-1)+cleaned_data_all$in5075E*(cleaned_data_all$in5075E-1)+cleaned_data_all$in75100E*(cleaned_data_all$in75100E-1)+cleaned_data_all$in100150E*(cleaned_data_all$in100150E-1)+cleaned_data_all$in150200E*(cleaned_data_all$in150200E-1)+cleaned_data_all$inover200E*(cleaned_data_all$inover200E-1))/
                                              ((cleaned_data_all$in2550E+cleaned_data_all$in5075E+cleaned_data_all$in75100E+cleaned_data_all$in100150E+cleaned_data_all$in150200E+cleaned_data_all$inover200E+cleaned_data_all$inless25E)*((cleaned_data_all$in2550E+cleaned_data_all$in5075E+cleaned_data_all$in75100E+cleaned_data_all$in100150E+cleaned_data_all$in150200E+cleaned_data_all$inover200E+cleaned_data_all$inless25E)-1)))



# Save the data 
save(index_data_all, file="")
save(summary_data_all,file="")


### Calculate the Pearson's R correlation 
JoinData=cleaned_data_all[,c("GEOID","MHIE","populationE")]
Newdata<-merge(JoinData, index_data_all, by="GEOID")

# Create new data frame
index_columns<-Newdata[,c(4:10003)]
var_columns<-Newdata[,c(2:3)]

summary_stats_df_mhi_2010 <- data.frame(
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
  summary_stats_df_mhi_2010[i, ] <- data.frame(
    Index = paste("Index", i),
    MHICorr = cor_var1,
    TPopCorr = cor_var2
  )
}

# Save the data
save(summary_stats_df_mhi_2010,file="")



##########2014------
# Extract data for 2014
dfall=AllUSTracts(yr=2014,varlist = MHI,states=state_list,level="tract",surv = "acs5")
dfall_cleaned = dfall %>% filter(populationE !=0)
finaldfall=na.omit(dfall_cleaned)

# Reorganize the data into seven subgroups
cleaned_data_all=data.frame("GEOID"=finaldfall$GEOID,
                            "inless25E"=finaldfall$inless10E+finaldfall$in1015E+finaldfall$in1520E+finaldfall$in2025E,
                            "inless25M"=finaldfall$inless10M+finaldfall$in1015M+finaldfall$in1520M+finaldfall$in2025M,
                            "in2550E"=finaldfall$in2530E+finaldfall$in3035E+finaldfall$in3540E+finaldfall$in4045E+finaldfall$in4550E,
                            "in2550M"=finaldfall$in2530M+finaldfall$in3035M+finaldfall$in3540M+finaldfall$in4045M+finaldfall$in4550M,
                            "in5075E"=finaldfall$in5060E+finaldfall$in6075E,
                            "in5075M"=finaldfall$in5060M+finaldfall$in6075M,
                            "in75100E"=finaldfall$in75100E,
                            "in75100M"=finaldfall$in75100M,
                            "in100150E"=finaldfall$in100125E+finaldfall$in125150E,
                            "in100150M"=finaldfall$in100125M+finaldfall$in125150M,
                            "in150200E"=finaldfall$in150200E,
                            "in150200M"=finaldfall$in150200M,
                            "inover200E"=finaldfall$inover200E,
                            "inover200M"=finaldfall$inover200M,
                            finaldfall[,35:39])


# Create empty data frame for simulation
index_data_all <- data.frame(GEOID = cleaned_data_all$GEOID)

set.seed(420)

# OPTIMIZED VERSION - Vectorized calculations
for (j in 1:10000) {
  
  # Generate all random values (vectorized)
  random_inless25 <- ifelse(
    (cleaned_data_all$inless25E - cleaned_data_all$inless25M) <= 0,
    round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$inless25E + cleaned_data_all$inless25M), 0),
    round(runif(nrow(cleaned_data_all), min = cleaned_data_all$inless25E - cleaned_data_all$inless25M, 
                max = cleaned_data_all$inless25E + cleaned_data_all$inless25M), 0)
  )
  
  random_in2550 <- ifelse(
    (cleaned_data_all$in2550E - cleaned_data_all$in2550M) <= 0,
    round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$in2550E + cleaned_data_all$in2550M), 0),
    round(runif(nrow(cleaned_data_all), min = cleaned_data_all$in2550E - cleaned_data_all$in2550M, 
                max = cleaned_data_all$in2550E + cleaned_data_all$in2550M), 0)
  )
  
  random_in5075 <- ifelse(
    (cleaned_data_all$in5075E - cleaned_data_all$in5075M) <= 0,
    round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$in5075E + cleaned_data_all$in5075M), 0),
    round(runif(nrow(cleaned_data_all), min = cleaned_data_all$in5075E - cleaned_data_all$in5075M, 
                max = cleaned_data_all$in5075E + cleaned_data_all$in5075M), 0)
  )
  
  random_in75100 <- ifelse(
    (cleaned_data_all$in75100E - cleaned_data_all$in75100M) <= 0,
    round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$in75100E + cleaned_data_all$in75100M), 0),
    round(runif(nrow(cleaned_data_all), min = cleaned_data_all$in75100E - cleaned_data_all$in75100M, 
                max = cleaned_data_all$in75100E + cleaned_data_all$in75100M), 0)
  )
  
  random_in100150 <- ifelse(
    (cleaned_data_all$in100150E - cleaned_data_all$in100150M) <= 0,
    round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$in100150E + cleaned_data_all$in100150M), 0),
    round(runif(nrow(cleaned_data_all), min = cleaned_data_all$in100150E - cleaned_data_all$in100150M, 
                max = cleaned_data_all$in100150E + cleaned_data_all$in100150M), 0)
  )
  
  random_in150200 <- ifelse(
    (cleaned_data_all$in150200E - cleaned_data_all$in150200M) <= 0,
    round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$in150200E + cleaned_data_all$in150200M), 0),
    round(runif(nrow(cleaned_data_all), min = cleaned_data_all$in150200E - cleaned_data_all$in150200M, 
                max = cleaned_data_all$in150200E + cleaned_data_all$in150200M), 0)
  )
  
  random_inover200 <- ifelse(
    (cleaned_data_all$inover200E - cleaned_data_all$inover200M) <= 0,
    round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$inover200E + cleaned_data_all$inover200M), 0),
    round(runif(nrow(cleaned_data_all), min = cleaned_data_all$inover200E - cleaned_data_all$inover200M, 
                max = cleaned_data_all$inover200E + cleaned_data_all$inover200M), 0)
  )
  
  # VECTORIZED SDI CALCULATION - All rows at once!
  numerator <- (random_inless25 * (random_inless25 - 1) +
                  random_in2550 * (random_in2550 - 1) +
                  random_in5075 * (random_in5075 - 1) +
                  random_in75100 * (random_in75100 - 1) +
                  random_in100150 * (random_in100150 - 1) +
                  random_in150200 * (random_in150200 - 1) +
                  random_inover200 * (random_inover200 - 1))
  
  total_pop <- (random_inless25 + random_in2550 + random_in5075 + 
                  random_in75100 + random_in100150 + random_in150200 + random_inover200)
  
  denominator <- total_pop * (total_pop - 1)
  
  index_values <- 1 - (numerator / denominator)
  
  # Add to dataframe
  index_data_all[[paste0("Index_", j)]] <- index_values
  
  if (j %% 100 == 0) cat("Iteration:", j, "\n")  # Progress every 100 iterations
}


# Reorganize the data and calculate the general statistics 
index_data_all$mean=rowMeans(index_data_all[, 2:ncol(index_data_all)],na.rm = TRUE)
index_data_all$min=apply(index_data_all[, 2:10001], 1, min, na.rm = TRUE)
index_data_all$max=apply(index_data_all[, 2:10001], 1, max, na.rm = TRUE)
index_data_all$median=apply(index_data_all[, 2:10001], 1,median, na.rm = TRUE)

# Summarize data into a new dataframe
summary_data_all=data.frame("GEOID"=index_data_all[,1],index_data_all[,10002:10005])
summary_data_all$range=summary_data_all$max-summary_data_all$min
# Calculate the estimated SDI
summary_data_all$estimate=estimate_value=1-((cleaned_data_all$inless25E*(cleaned_data_all$inless25E-1)+cleaned_data_all$in2550E*(cleaned_data_all$in2550E-1)+cleaned_data_all$in5075E*(cleaned_data_all$in5075E-1)+cleaned_data_all$in75100E*(cleaned_data_all$in75100E-1)+cleaned_data_all$in100150E*(cleaned_data_all$in100150E-1)+cleaned_data_all$in150200E*(cleaned_data_all$in150200E-1)+cleaned_data_all$inover200E*(cleaned_data_all$inover200E-1))/
                                              ((cleaned_data_all$in2550E+cleaned_data_all$in5075E+cleaned_data_all$in75100E+cleaned_data_all$in100150E+cleaned_data_all$in150200E+cleaned_data_all$inover200E+cleaned_data_all$inless25E)*((cleaned_data_all$in2550E+cleaned_data_all$in5075E+cleaned_data_all$in75100E+cleaned_data_all$in100150E+cleaned_data_all$in150200E+cleaned_data_all$inover200E+cleaned_data_all$inless25E)-1)))

# Save the data
save(summary_data_all,file="")
save(index_data_all, file="")

### Calculate the Pearson's R correlation
JoinData=cleaned_data_all[,c("GEOID","MHIE","populationE")]
Newdata<-merge(JoinData, index_data_all, by="GEOID")

# Create new data frame 
index_columns<-Newdata[,c(4:10003)]
var_columns<-Newdata[,c(2:3)]
summary_stats_df_mhi_2014 <- data.frame(
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
  summary_stats_df_mhi_2014[i, ] <- data.frame(
    Index = paste("Index", i),
    MHICorr = cor_var1,
    TPopCorr = cor_var2
  )
}

# Save the data
save(summary_stats_df_mhi_2014,file="")



##########2018------
# Extract data for 2018
dfall=AllUSTracts(yr=2018,varlist = MHI,states=state_list,level="tract",surv = "acs5")
dfall_cleaned = dfall %>% filter(populationE !=0)
finaldfall=na.omit(dfall_cleaned)

# Reorganize the data into seven subgroups
cleaned_data_all=data.frame("GEOID"=finaldfall$GEOID,
                            "inless25E"=finaldfall$inless10E+finaldfall$in1015E+finaldfall$in1520E+finaldfall$in2025E,
                            "inless25M"=finaldfall$inless10M+finaldfall$in1015M+finaldfall$in1520M+finaldfall$in2025M,
                            "in2550E"=finaldfall$in2530E+finaldfall$in3035E+finaldfall$in3540E+finaldfall$in4045E+finaldfall$in4550E,
                            "in2550M"=finaldfall$in2530M+finaldfall$in3035M+finaldfall$in3540M+finaldfall$in4045M+finaldfall$in4550M,
                            "in5075E"=finaldfall$in5060E+finaldfall$in6075E,
                            "in5075M"=finaldfall$in5060M+finaldfall$in6075M,
                            "in75100E"=finaldfall$in75100E,
                            "in75100M"=finaldfall$in75100M,
                            "in100150E"=finaldfall$in100125E+finaldfall$in125150E,
                            "in100150M"=finaldfall$in100125M+finaldfall$in125150M,
                            "in150200E"=finaldfall$in150200E,
                            "in150200M"=finaldfall$in150200M,
                            "inover200E"=finaldfall$inover200E,
                            "inover200M"=finaldfall$inover200M,
                            finaldfall[,35:39])

# Create empty data frame for simulation
index_data_all <- data.frame(GEOID = cleaned_data_all$GEOID)
set.seed(420)

# Calculate the composite index for each iteration
for (j in 1:10000) {
  # Calculating the random number for under $25,000 from within the MOE parameters
  random_inless25 <- ifelse((cleaned_data_all$inless25E-cleaned_data_all$inless25M)<=0,
                            round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$inless25E + cleaned_data_all$inless25M),0),
                            round(runif(nrow(cleaned_data_all), min = cleaned_data_all$inless25E - cleaned_data_all$inless25M, max = cleaned_data_all$inless25E + cleaned_data_all$inless25M),0))
  
  # Calculating the random number for $25,000-50,000 from within the MOE parameters
  random_in2550 <- ifelse((cleaned_data_all$in2550E-cleaned_data_all$in2550M)<=0,
                          round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$in2550E + cleaned_data_all$in2550M),0),
                          round(runif(nrow(cleaned_data_all), min = cleaned_data_all$in2550E - cleaned_data_all$in2550M, max = cleaned_data_all$in2550E + cleaned_data_all$in2550M),0))
  
  # Calculating the random number for $50,000-75,000 from within the MOE parameters
  random_in5075 <- ifelse((cleaned_data_all$in5075E-cleaned_data_all$in5075M)<=0,
                          round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$in5075E + cleaned_data_all$in5075M),0),
                          round(runif(nrow(cleaned_data_all), min = cleaned_data_all$in5075E - cleaned_data_all$in5075M, max = cleaned_data_all$in5075E + cleaned_data_all$in5075M),0))
  
  # Calculating the random number for $75,000-100,000 from within the MOE parameters
  random_in75100 <- ifelse((cleaned_data_all$in75100E-cleaned_data_all$in75100M)<=0,
                           round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$in75100E + cleaned_data_all$in75100M),0),
                           round(runif(nrow(cleaned_data_all), min = cleaned_data_all$in75100E - cleaned_data_all$in75100M, max = cleaned_data_all$in75100E + cleaned_data_all$in75100M),0))
  
  
  # Calculating the random number for $100,000-150,000 from within the MOE parameters
  random_in100150 <- ifelse((cleaned_data_all$in100150E-cleaned_data_all$in100150M)<=0,
                            round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$in100150E + cleaned_data_all$in100150M),0),
                            round(runif(nrow(cleaned_data_all), min = cleaned_data_all$in100150E - cleaned_data_all$in100150M, max = cleaned_data_all$in100150E + cleaned_data_all$in100150M),0))
  
  # Calculating the random number for $150,000-200,000 from within the MOE parameters
  random_in150200 <- ifelse((cleaned_data_all$in150200E-cleaned_data_all$in150200M)<=0,
                            round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$in150200E + cleaned_data_all$in150200M),0),
                            round(runif(nrow(cleaned_data_all), min = cleaned_data_all$in150200E - cleaned_data_all$in150200M, max = cleaned_data_all$in150200E + cleaned_data_all$in150200M),0))
  
  # Calculating the random number for over $200,000 from within the MOE parameters
  random_inover200 <- ifelse((cleaned_data_all$inover200E-cleaned_data_all$inover200M)<=0,
                             round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$inover200E + cleaned_data_all$inover200M),0),
                             round(runif(nrow(cleaned_data_all), min = cleaned_data_all$inover200E - cleaned_data_all$inover200M, max = cleaned_data_all$inover200E + cleaned_data_all$inover200M),0))
  

  # Create a vector to store index values
  index_values <- numeric(nrow(cleaned_data_all)) 
 
 
  cat("Iteration:", j, "\n")
  
  # Calculate SDI for each simulation
  for (i in 1:nrow(cleaned_data_all)){
    index_values[i]=1-((random_inless25[i]*(random_inless25[i]-1)+random_in2550[i]*(random_in2550[i]-1)+random_in5075[i]*(random_in5075[i]-1)+random_in75100[i]*(random_in75100[i]-1)+random_in100150[i]*(random_in100150[i]-1)+random_in150200[i]*(random_in150200[i]-1)+random_inover200[i]*(random_inover200[i]-1))/
                         ((random_in2550[i]+random_in5075[i]+random_in75100[i]+random_in100150[i]+random_in150200[i]+random_inover200[i]+random_inless25[i])*((random_in2550[i]+random_in5075[i]+random_in75100[i]+random_in100150[i]+random_in150200[i]+random_inover200[i]+random_inless25[i])-1)))
    
  }
  # Add index values to the index_data_all dataframe
  index_data_all[[paste0("Index_", j)]] <- index_values

 
}

# Reorganize the data and calcualte the general statistics

index_data_all$mean=rowMeans(index_data_all[, 2:ncol(index_data_all)],na.rm = TRUE)
index_data_all$min=apply(index_data_all[, 2:10001], 1, min, na.rm = TRUE)
index_data_all$max=apply(index_data_all[, 2:10001], 1, max, na.rm = TRUE)
index_data_all$median=apply(index_data_all[, 2:10001], 1,median, na.rm = TRUE)

# Summarize data into a new dataframe
summary_data_all=data.frame("GEOID"=index_data_all[,1],index_data_all[,10002:10005])
summary_data_all$range=summary_data_all$max-summary_data_all$min
# Calcualte the estimated SDI
summary_data_all$estimate=estimate_value=1-((cleaned_data_all$inless25E*(cleaned_data_all$inless25E-1)+cleaned_data_all$in2550E*(cleaned_data_all$in2550E-1)+cleaned_data_all$in5075E*(cleaned_data_all$in5075E-1)+cleaned_data_all$in75100E*(cleaned_data_all$in75100E-1)+cleaned_data_all$in100150E*(cleaned_data_all$in100150E-1)+cleaned_data_all$in150200E*(cleaned_data_all$in150200E-1)+cleaned_data_all$inover200E*(cleaned_data_all$inover200E-1))/
                                              ((cleaned_data_all$in2550E+cleaned_data_all$in5075E+cleaned_data_all$in75100E+cleaned_data_all$in100150E+cleaned_data_all$in150200E+cleaned_data_all$inover200E+cleaned_data_all$inless25E)*((cleaned_data_all$in2550E+cleaned_data_all$in5075E+cleaned_data_all$in75100E+cleaned_data_all$in100150E+cleaned_data_all$in150200E+cleaned_data_all$inover200E+cleaned_data_all$inless25E)-1)))
# Save the data
save(summary_data_all,file="")
save(index_data_all, file="")

### Calculate the Pearson's R correlation
JoinData=cleaned_data_all[,c("GEOID","MHIE","populationE")]
Newdata<-merge(JoinData, index_data_all, by="GEOID")

# Create new data frame
index_columns<-Newdata[,c(4:10003)]
var_columns<-Newdata[,c(2:3)]
summary_stats_df_mhi_2018 <- data.frame(
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
  summary_stats_df_mhi_2018[i, ] <- data.frame(
    Index = paste("Index", i),
    MHICorr = cor_var1,
    TPopCorr = cor_var2
  )
}

# Save the data
save(summary_stats_df_mhi_2018,file="")

##########2022--------
# Extract data for 2022
dfall=AllUSTracts(yr=2022,varlist = MHI,states=state_list,level="tract",surv = "acs5")
dfall_cleaned = dfall %>% filter(populationE !=0)
finaldfall=na.omit(dfall_cleaned)

# Reorganize the data into seven subgroups
cleaned_data_all=data.frame("GEOID"=finaldfall$GEOID,
                            "inless25E"=finaldfall$inless10E+finaldfall$in1015E+finaldfall$in1520E+finaldfall$in2025E,
                            "inless25M"=finaldfall$inless10M+finaldfall$in1015M+finaldfall$in1520M+finaldfall$in2025M,
                            "in2550E"=finaldfall$in2530E+finaldfall$in3035E+finaldfall$in3540E+finaldfall$in4045E+finaldfall$in4550E,
                            "in2550M"=finaldfall$in2530M+finaldfall$in3035M+finaldfall$in3540M+finaldfall$in4045M+finaldfall$in4550M,
                            "in5075E"=finaldfall$in5060E+finaldfall$in6075E,
                            "in5075M"=finaldfall$in5060M+finaldfall$in6075M,
                            "in75100E"=finaldfall$in75100E,
                            "in75100M"=finaldfall$in75100M,
                            "in100150E"=finaldfall$in100125E+finaldfall$in125150E,
                            "in100150M"=finaldfall$in100125M+finaldfall$in125150M,
                            "in150200E"=finaldfall$in150200E,
                            "in150200M"=finaldfall$in150200M,
                            "inover200E"=finaldfall$inover200E,
                            "inover200M"=finaldfall$inover200M,
                            finaldfall[,35:39])



# Create empty data frame for simulation
index_data_all <- data.frame(GEOID = cleaned_data_all$GEOID)





set.seed(420)

# Calculate the composite index for each iteration
for (j in 1:10000) {
  # Calculating the random number for under $25,000 from within the MOE parameters
  random_inless25 <- ifelse((cleaned_data_all$inless25E-cleaned_data_all$inless25M)<=0,
                            round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$inless25E + cleaned_data_all$inless25M),0),
                            round(runif(nrow(cleaned_data_all), min = cleaned_data_all$inless25E - cleaned_data_all$inless25M, max = cleaned_data_all$inless25E + cleaned_data_all$inless25M),0))
  
  # Calculating the random number for $25,000-50,000 from within the MOE parameters
  random_in2550 <- ifelse((cleaned_data_all$in2550E-cleaned_data_all$in2550M)<=0,
                          round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$in2550E + cleaned_data_all$in2550M),0),
                          round(runif(nrow(cleaned_data_all), min = cleaned_data_all$in2550E - cleaned_data_all$in2550M, max = cleaned_data_all$in2550E + cleaned_data_all$in2550M),0))
  
  # Calculating the random number for $50,000-75,000 from within the MOE parameters
  random_in5075 <- ifelse((cleaned_data_all$in5075E-cleaned_data_all$in5075M)<=0,
                          round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$in5075E + cleaned_data_all$in5075M),0),
                          round(runif(nrow(cleaned_data_all), min = cleaned_data_all$in5075E - cleaned_data_all$in5075M, max = cleaned_data_all$in5075E + cleaned_data_all$in5075M),0))
  
  # Calculating the random number for $75,000-100,000 from within the MOE parameters
  random_in75100 <- ifelse((cleaned_data_all$in75100E-cleaned_data_all$in75100M)<=0,
                           round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$in75100E + cleaned_data_all$in75100M),0),
                           round(runif(nrow(cleaned_data_all), min = cleaned_data_all$in75100E - cleaned_data_all$in75100M, max = cleaned_data_all$in75100E + cleaned_data_all$in75100M),0))
  
  
  # Calculating the random number for $100,000-150,000 from within the MOE parameters
  random_in100150 <- ifelse((cleaned_data_all$in100150E-cleaned_data_all$in100150M)<=0,
                            round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$in100150E + cleaned_data_all$in100150M),0),
                            round(runif(nrow(cleaned_data_all), min = cleaned_data_all$in100150E - cleaned_data_all$in100150M, max = cleaned_data_all$in100150E + cleaned_data_all$in100150M),0))
  
  # Calculating the random number for $150,000-200,000 from within the MOE parameters
  random_in150200 <- ifelse((cleaned_data_all$in150200E-cleaned_data_all$in150200M)<=0,
                            round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$in150200E + cleaned_data_all$in150200M),0),
                            round(runif(nrow(cleaned_data_all), min = cleaned_data_all$in150200E - cleaned_data_all$in150200M, max = cleaned_data_all$in150200E + cleaned_data_all$in150200M),0))
  
  # Calculating the random number for over $200,000 from within the MOE parameters
  random_inover200 <- ifelse((cleaned_data_all$inover200E-cleaned_data_all$inover200M)<=0,
                             round(runif(nrow(cleaned_data_all), min = 0, max = cleaned_data_all$inover200E + cleaned_data_all$inover200M),0),
                             round(runif(nrow(cleaned_data_all), min = cleaned_data_all$inover200E - cleaned_data_all$inover200M, max = cleaned_data_all$inover200E + cleaned_data_all$inover200M),0))
  
  
  # Create a vector to store index values
  index_values <- numeric(nrow(cleaned_data_all)) 
 
 
  cat("Iteration:", j, "\n")
  
  # Calculate SDI for each simulation
  for (i in 1:nrow(cleaned_data_all)){
    index_values[i]=1-((random_inless25[i]*(random_inless25[i]-1)+random_in2550[i]*(random_in2550[i]-1)+random_in5075[i]*(random_in5075[i]-1)+random_in75100[i]*(random_in75100[i]-1)+random_in100150[i]*(random_in100150[i]-1)+random_in150200[i]*(random_in150200[i]-1)+random_inover200[i]*(random_inover200[i]-1))/
                         ((random_in2550[i]+random_in5075[i]+random_in75100[i]+random_in100150[i]+random_in150200[i]+random_inover200[i]+random_inless25[i])*((random_in2550[i]+random_in5075[i]+random_in75100[i]+random_in100150[i]+random_in150200[i]+random_inover200[i]+random_inless25[i])-1)))
    
  }
  # Add index values to the index_data_all dataframe
  index_data_all[[paste0("Index_", j)]] <- index_values

 
}

# Reorganize the data and calcualte the general statistics

index_data_all$mean=rowMeans(index_data_all[, 2:ncol(index_data_all)],na.rm = TRUE)
index_data_all$min=apply(index_data_all[, 2:10001], 1, min, na.rm = TRUE)
index_data_all$max=apply(index_data_all[, 2:10001], 1, max, na.rm = TRUE)
index_data_all$median=apply(index_data_all[, 2:10001], 1,median, na.rm = TRUE)

# Summarize data into a new data frame
summary_data_all=data.frame("GEOID"=index_data_all[,1],index_data_all[,10002:10005])
summary_data_all$range=summary_data_all$max-summary_data_all$min
# Calculate the estimated SDI 
summary_data_all$estimate=estimate_value=1-((cleaned_data_all$inless25E*(cleaned_data_all$inless25E-1)+cleaned_data_all$in2550E*(cleaned_data_all$in2550E-1)+cleaned_data_all$in5075E*(cleaned_data_all$in5075E-1)+cleaned_data_all$in75100E*(cleaned_data_all$in75100E-1)+cleaned_data_all$in100150E*(cleaned_data_all$in100150E-1)+cleaned_data_all$in150200E*(cleaned_data_all$in150200E-1)+cleaned_data_all$inover200E*(cleaned_data_all$inover200E-1))/
                                              ((cleaned_data_all$in2550E+cleaned_data_all$in5075E+cleaned_data_all$in75100E+cleaned_data_all$in100150E+cleaned_data_all$in150200E+cleaned_data_all$inover200E+cleaned_data_all$inless25E)*((cleaned_data_all$in2550E+cleaned_data_all$in5075E+cleaned_data_all$in75100E+cleaned_data_all$in100150E+cleaned_data_all$in150200E+cleaned_data_all$inover200E+cleaned_data_all$inless25E)-1)))

# Save the data
save(summary_data_all,file="")
save(index_data_all, file="")

### Calculate the Pearson's R correlation
Newdata<-merge(JoinData, index_data_all, by="GEOID")
JoinData=cleaned_data_all[,c("GEOID","MHIE","populationE")]

# Create new data frame
index_columns<-Newdata[,c(4:10003)]
var_columns<-Newdata[,c(2:3)]
summary_stats_df_mhi_2022 <- data.frame(
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
  summary_stats_df_mhi_2022[i, ] <- data.frame(
    Index = paste("Index", i),
    MHICorr = cor_var1,
    TPopCorr = cor_var2
  )
}

# Save the data
save(summary_stats_df_mhi_2022,file="")








