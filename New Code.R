# 1. SETUP AND LIBRARIES
# -----------------------------------------------------------------------------
library(tidycensus)
library(tidyverse)
library(future.apply)
library(sf)

# census_api_key("YOUR_API_KEY_HERE", install = TRUE)

# Use all available CPU cores for parallel processing
plan(multisession)

# 2. HELPER FUNCTIONS
# -----------------------------------------------------------------------------
get_state_list <- function(states = "contig") {
  all <- c("01","02","04","05","06","08","09","10","11","12","13","15","16",
           "17","18","19","20","21","22","23","24","25","26","27","28","29",
           "30","31","32","33","34","35","36","37","38","39","40","41","42",
           "44","45","46","47","48","49","50","51","53","54","55","56","60",
           "66","69","72","74","78")
  
  exclude <- switch(states,
                    "contig" = c("02","15","11","72","66","78","74","60","69"),
                    "all_states" = c("11","72","66","78","74","60","69"),
                    "all" = NULL)
  
  if (is.null(exclude)) return(all)
  return(all[!all %in% exclude])
}

AllUSTracts <- function(yr, varlist, states, level = "tract", surv = "acs5") {
  map_dfr(states, ~{
    get_acs(geography = level, variables = varlist, survey = surv, 
            year = yr, state = .x, output = "wide", geometry = FALSE)
  })
}

# 3. OPTIMIZED SIMULATION ENGINE
# -----------------------------------------------------------------------------
run_sdi_simulation <- function(E_mat, M_mat, n_iterations = 10000, seed = 420) {
  n_rows <- nrow(E_mat)
  set.seed(seed)
  
  results_list <- future_lapply(1:n_iterations, function(j) {
    mins <- pmax(0, E_mat - M_mat)
    maxs <- E_mat + M_mat
    random_counts <- round(mins + matrix(runif(length(E_mat)), nrow = n_rows) * (maxs - mins))
    
    numerator <- rowSums(random_counts * (random_counts - 1))
    total_pop <- rowSums(random_counts)
    denominator <- total_pop * (total_pop - 1)
    
    1 - (numerator / ifelse(denominator <= 0, NA, denominator))
  }, future.seed = TRUE)
  
  do.call(cbind, results_list)
}

# 4. VARIABLE DEFINITIONS
# -----------------------------------------------------------------------------
vars_mhi <- c(
  inless10="B19001_002", in1015="B19001_003", in1520="B19001_004", in2025="B19001_005",
  in2530="B19001_006", in3035="B19001_007", in3540="B19001_008", in4045="B19001_009",
  in4550="B19001_010", in5060="B19001_011", in6075="B19001_012", in75100="B19001_013",
  in100125="B19001_014", in125150="B19001_015", in150200="B19001_016", inover200="B19001_017",
  population="B01003_001", MHIE_var="B19013_001"
)

vars_age_race <- c(
  # Male & Female Age variables as defined in your research scripts
  ageu5m="B01001_003", age59m="B01001_004", age1014m="B01001_005", age1517m="B01001_006", 
  age1819m="B01001_007", age20m="B01001_008", age21m="B01001_009", age2224m="B01001_010",
  age2529m="B01001_011", age3034m="B01001_012", age3539m="B01001_013", age4044m="B01001_014", 
  age4549m="B01001_015", age5054m="B01001_016", age5559m="B01001_017", age6061m="B01001_018",
  age6264m="B01001_019", age6566m="B01001_020", age6769m="B01001_021", age7074m="B01001_022", 
  age7579m="B01001_023", age8084m="B01001_024", ageabv85m="B01001_025",
  ageu5f="B01001_027", age59f="B01001_028", age1014f="B01001_029", age1517f="B01001_030", 
  age1819f="B01001_031", age20f="B01001_032", age21f="B01001_033", age2224f="B01001_034",
  age2529f="B01001_035", age3034f="B01001_036", age3539f="B01001_037", age4044f="B01001_038", 
  age4549f="B01001_039", age5054f="B01001_040", age5559f="B01001_041", age6061f="B01001_042",
  age6264f="B01001_043", age6566f="B01001_044", age6769f="B01001_045", age7074f="B01001_046", 
  age7579f="B01001_047", age8084f="B01001_048", ageabv85f="B01001_049",
  # Race and Income reference variables
  MHI_var="B19013_001", racetotal="B02001_001", racewhite="B02001_002", raceblack="B02001_003", 
  raceasian="B02001_005", Indian="B02001_004", Hawaiian="B02001_006", other="B02001_007", 
  twootherraces="B02001_008", population="B01003_001"
)

# 5. MULTI-YEAR EXECUTION LOOP
# -----------------------------------------------------------------------------
years <- 2023
states_contig <- get_state_list("contig")

for (yr in years) {
  message(paste("\n--- Processing Year:", yr, "---"))
  
  # A. Processing Income (MHI)
  df_mhi <- AllUSTracts(yr, vars_mhi, states_contig) %>% filter(populationE != 0) %>% na.omit()
  clean_mhi <- df_mhi %>% transmute(
    inless25E = inless10E + in1015E + in1520E + in2025E,
    inless25M = inless10M + in1015M + in1520M + in2025M,
    in2550E   = in2530E + in3035E + in3540E + in4045E + in4550E,
    in2550M   = in2530M + in3035M + in3540M + in4045M + in4550M,
    in5075E   = in5060E + in6075E, in5075M = in5060M + in6075M,
    in75100E, in75100M, 
    in100150E = in100125E + in125150E, in100150M = in100125M + in125150M,
    in150200E, in150200M, inover200E, inover200M
  )
  mhi_sim_results <- run_sdi_simulation(as.matrix(clean_mhi[, grep("E$", names(clean_mhi))]),
                                        as.matrix(clean_mhi[, grep("M$", names(clean_mhi))]))
  
  # B. Processing Age and Race
  df_age_race <- AllUSTracts(yr, vars_age_race, states_contig) %>% filter(populationE != 0, MHI_varE != 0) %>% na.omit()
  clean_age <- df_age_race %>% transmute(
    # Aggregated Age groups
    ageu18E = ageu5mE+ageu5fE+age59mE+age59fE+age1014mE+age1014fE+age1517mE+age1517fE,
    ageu18M = ageu5mM+ageu5fM+age59mM+age59fM+age1014mM+age1014fM+age1517mM+age1517fM,
    age1825E = age1819mE+age1819fE+age20fE+age20mE+age21fE+age21mE+age2224mE+age2224fE,
    age1825M = age1819mM+age1819fM+age20fM+age20mM+age21fM+age21mM+age2224mM+age2224fM,
    age2540E = age2529mE+age2529fE+age3034mE+age3034fE+age3539mE+age3539fE,
    age2540M = age2529mM+age2529fM+age3034mM+age3034fM+age3539mM+age3539fM,
    age4054E = age4044mE+age4044fE+age4549mE+age4549fE+age5054mE+age5054fE,
    age4054M = age4044mM+age4044fM+age4549mM+age4549fM+age5054mM+age5054fM,
    age5565E = age5559mE+age5559fE+age6061mE+age6061fE+age6264mE+age6264fE,
    age5565M = age5559mM+age5559fM+age6061mM+age6061fM+age6264mM+age6264fM,
    age6575E = age6566mE+age6566fE+age6769mE+age6769fE+age7074mE+age7074fE,
    age6575M = age6566mM+age6566fM+age6769mM+age6769fM+age7074mM+age7074fM,
    ageover75E = age7579mE+age7579fE+age8084mE+age8084fE+ageabv85mE+ageabv85fE,
    ageover75M = age7579mM+age7579fM+age8084mM+age8084fM+ageabv85mM+ageabv85fM
  )
  age_sim_results <- run_sdi_simulation(as.matrix(clean_age[, grep("E$", names(clean_age))]),
                                        as.matrix(clean_age[, grep("M$", names(clean_age))]))
  
  clean_race <- df_age_race %>% select(
    racewhiteE, racewhiteM, raceblackE, raceblackM, raceasianE, raceasianM,
    IndianE, IndianM, HawaiianE, HawaiianM, otherE, otherM, twootherracesE, twootherracesM
  )
  race_sim_results <- run_sdi_simulation(as.matrix(clean_race[, grep("E$", names(clean_race))]),
                                         as.matrix(clean_race[, grep("M$", names(clean_race))]))
  
  # C. CORRELATION CALCULATIONS (Pearson's R)
  # Efficiently calculate all 10,000 correlations at once
  mhi_cor_mhi <- as.vector(cor(mhi_sim_results, df_mhi$MHIE_varE, use = "complete.obs"))
  mhi_cor_pop <- as.vector(cor(mhi_sim_results, df_mhi$populationE, use = "complete.obs"))
  
  age_cor_mhi <- as.vector(cor(age_sim_results, df_age_race$MHI_varE, use = "complete.obs"))
  age_cor_pop <- as.vector(cor(age_sim_results, df_age_race$populationE, use = "complete.obs"))
  
  race_cor_mhi <- as.vector(cor(race_sim_results, df_age_race$MHI_varE, use = "complete.obs"))
  race_cor_pop <- as.vector(cor(race_sim_results, df_age_race$populationE, use = "complete.obs"))
  
  # D. CONSOLIDATE AND SAVE
  final_correlations <- list(
    mhi = data.frame(Index = 1:10000, MHICorr = mhi_cor_mhi, TPopCorr = mhi_cor_pop),
    age = data.frame(Index = 1:10000, MHICorr = age_cor_mhi, TPopCorr = age_cor_pop),
    race = data.frame(Index = 1:10000, MHICorr = race_cor_mhi, TPopCorr = race_cor_pop)
  )
  final_stats <- list(
    income = data.frame(GEOID = df_mhi$GEOID, mean_sdi = rowMeans(mhi_sim_results)),
    age    = data.frame(GEOID = df_age_race$GEOID, mean_sdi = rowMeans(age_sim_results)),
    race   = data.frame(GEOID = df_age_race$GEOID, mean_sdi = rowMeans(race_sim_results))
  )
  save(final_stats, file = paste0("Census_SDI_Summary_", yr, ".RData"))
  save(final_correlations, file = paste0("SDI_Pearson_Correlations_", yr, ".RData"))
}