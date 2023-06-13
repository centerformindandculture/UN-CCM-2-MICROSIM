library(dplyr)
library(readxl)
library(purrr)
library(tidyr)
library(readr)

## From a given file name and the observed UN values (DF_Obs), output the metrics for 50 yr date ranges
get_metrics_from_file <- function(DF_Obs, DF, ModelTitle, fifty=TRUE) { 
  
  if (fifty) {
    DF<-DF[,c("SimID", "Period","Current_Period_Pop","Num_Births","Num_Deaths")]
    DF.1950 <- DF %>% filter(grepl("^195|^196|^197|^198|^199", Period))
    DF.2000 <- DF %>% filter(grepl("^200|^201|^202|^203|^204", Period))
    DF.2050 <- DF %>% filter(grepl("^205|^206|^207|^208|^209", Period))
    
    ## data frame with observed UN values
    DF_Obs<-DF_Obs[,c("Period","Population","Births","Deaths" )]
    colnames(DF.1950) <- c("SimID", "Period", "sim_Population","sim_Births","sim_Deaths")
    colnames(DF.2000) <- c("SimID", "Period", "sim_Population","sim_Births","sim_Deaths")
    colnames(DF.2050) <- c("SimID", "Period", "sim_Population","sim_Births","sim_Deaths")
    
    return(rbind(
      get_metrics(DF=DF.1950, DF_Obs=DF_Obs, YearRange="1950-1999", ModelTitle=ModelTitle),
      get_metrics(DF=DF.2000, DF_Obs=DF_Obs, YearRange="2000-2049", ModelTitle=ModelTitle),
      get_metrics(DF=DF.2050, DF_Obs=DF_Obs, YearRange="2050-2099", ModelTitle=ModelTitle)
    ))
  } else {
    DF<-DF[,c("SimID", "Period","Current_Period_Pop","Num_Births","Num_Deaths")]
    
    ## data frame with observed UN values
    DF_Obs<-DF_Obs[,c("Period","Population","Births","Deaths" )]
    
    colnames(DF) <- c("SimID", "Period", "sim_Population","sim_Births","sim_Deaths")
    return(
      get_metrics(DF=DF, DF_Obs=DF_Obs, YearRange="1950-2099", ModelTitle=ModelTitle)
    )
  }
}

library(moments)
## From a given output (DF) and the UN Values (DF_Obs), calculate comparison metrics
get_metrics <- function (DF, DF_Obs, YearRange, ModelTitle) {
  
  joined <- DF %>% inner_join(DF_Obs)
  joined$PopDiff <- joined$sim_Population - joined$Population
  joined$BirthDiff <- joined$sim_Births - joined$Births
  joined$DeathDiff <- joined$sim_Deaths - joined$Deaths
  joined$PopDiffSq <- joined$PopDiff^2
  joined$BirthDiffSq <- joined$BirthDiff^2
  joined$DeathDiffSq <- joined$DeathDiff^2
  
  metricsOnly <- joined[,c("SimID", "Population", "PopDiff", "PopDiffSq", "Births", "BirthDiff", "BirthDiffSq", "Deaths", "DeathDiff", "DeathDiffSq")]
  metric <- metricsOnly %>% 
    group_by(SimID) %>%
    summarise(
      avgPop = mean(Population),
      avgAbs_PopDiff = mean(abs(PopDiff)),
      errPosPercPeriod_PopDiff = sum(PopDiff >= 0) / sum(ifelse(PopDiff == 0, 1, 1)),
      
      avgBirths = mean(Births),
      avgAbs_BirthDiff = mean(abs(BirthDiff)),
      errPosPercPeriod_BirthDiff = sum(BirthDiff >= 0) / sum(ifelse(BirthDiff == 0, 1, 1)),
      
      avgDeath = mean(Deaths),
      avgAbs_DeathDiff = mean(abs(DeathDiff)),
      errPosPercPeriod_DeathDiff = sum(DeathDiff >= 0) / sum(ifelse(DeathDiff == 0, 1, 1)),
    ) %>%
    select(-SimID) %>%
    summarise(across(everything(), mean),) 
  
  metric <- metric %>% 
    transmute(Average_Period_Population = avgPop, 
              Average_Period_Population_Diff = avgAbs_PopDiff, 
              Average_Perc_Pop_Error = avgAbs_PopDiff / avgPop,
              Perc_Pos_Pop_Error = errPosPercPeriod_PopDiff,
              Average_Period_Births = avgBirths,
              Average_Period_Births_Diff = avgAbs_BirthDiff,
              Average_Perc_Bir_Error = avgAbs_BirthDiff / avgBirths,
              Perc_Pos_Birth_Error = errPosPercPeriod_BirthDiff,
              Average_Period_Deaths = avgDeath,
              Average_Period_Deaths_Diff = avgAbs_DeathDiff,
              Average_Perc_Dea_Error = avgAbs_DeathDiff / avgDeath,
              Perc_Pos_Death_Error = errPosPercPeriod_DeathDiff)
  metric$YearRange <- YearRange
  metric$ModelTitle <- ModelTitle
  
  return(metric)
}

read_excel_and_replace_spaces_in_headers <- function(filename) {
  data<-as.data.frame(read_excel(filename, col_names=TRUE))
  names(data)<-sub(" ", ".", names(data))
  data
}



NorwayStartingPop <- 3265300
USAStartingPop <- 158804397
IndiaStartingPop <- 376325200

country_id = c(1, 2, 3)

## Important note: Metrics will only be calculated for parameter variations
## that exist in this parameter_variations data frame. If you ran the model
## under different settings, you may need to edit these values. For instance,
## if you ran with 50k agents instead of 100k, you will need to change the
## NumberOfInitialAgents here.
parameter_variations <- 
  data.frame(NumberOfInitialAgents = c(100000)) %>%
  crossing(data.frame(FiveYearStep = c(TRUE, FALSE))) %>%
  crossing(data.frame(TopDown = c(TRUE, FALSE))) %>%
  crossing(data.frame(AgentInitializationPerAgeYear = c(TRUE, FALSE))) %>%
  crossing(data.frame(ForceUseSplitAge = c(TRUE, FALSE))) %>%
  crossing(data.frame(splitFertility = c(TRUE, FALSE))) %>%
  crossing(data.frame(country_id))

DF.all <-read_csv("PaperFigureAssetsAndData/CRED_Annual_Stats.csv")
for(c_id in country_id) {
  parameter_variations.country <- parameter_variations %>% filter(country_id == c_id)
  if (c_id == 1) {
    DF_Obs <- read_excel_and_replace_spaces_in_headers("data_assets/Observed_UN_values.xlsx")
    saved_names <- names(DF_Obs)
    DF_Obs <- cbind(DF_Obs[, 1], DF_Obs[, 2:20] * 100)
    names(DF_Obs) <- saved_names
  } else if (c_id == 2) {
    DF_Obs <- read_excel_and_replace_spaces_in_headers("data_assets/Observed_UN_values_US.xlsx")
    saved_names <- names(DF_Obs)
    DF_Obs <- cbind(DF_Obs[, 1], DF_Obs[, 2:20] * (1 / 0.0003))
    names(DF_Obs) <- saved_names
  } else if (c_id == 3) {
    DF_Obs <- read_excel_and_replace_spaces_in_headers("data_assets/Observed_UN_values_India.xlsx")
    saved_names <- names(DF_Obs)
    DF_Obs <- cbind(DF_Obs[, 1], DF_Obs[, 2:20] * (1 / 0.00015))
    names(DF_Obs) <- saved_names
  }
  
  
  metrics50 <- data.frame(matrix(ncol = 15 + ncol(parameter_variations), nrow = 0))
  metrics <- data.frame(matrix(ncol = 15 + ncol(parameter_variations), nrow = 0))
  
  for (i in 1:nrow(parameter_variations.country)) {
    DF <- DF.all %>% 
      inner_join(parameter_variations.country[i,]) %>%
      select(SimID, Period, Current_Period_Pop, Num_Births, Num_Deaths) %>%
      filter(is.na(SimID) == 0)
    if (nrow(DF) == 0) {
      next
    }
    starting_pop <- parameter_variations.country[i, "NumberOfInitialAgents"][[1]]
    multiplier <- ifelse(c_id == 1, 
                         NorwayStartingPop / starting_pop,
                         ifelse(c_id == 2,
                                USAStartingPop / starting_pop,
                                IndiaStartingPop / starting_pop))
    DF$Current_Period_Pop <- multiplier * DF$Current_Period_Pop
    DF$Num_Births <- multiplier * DF$Num_Births
    DF$Num_Deaths <- multiplier * DF$Num_Deaths
    
    metrics <- rbind(metrics, crossing(get_metrics_from_file(DF_Obs, DF, "", FALSE), parameter_variations.country[i,]))
    metrics50 <- rbind(metrics50, crossing(get_metrics_from_file(DF_Obs, DF, "", TRUE), parameter_variations.country[i,]))
  }
  
  write_csv(rbind(metrics50), paste("Country", c_id, "_Metrics_50YrChunks.csv", sep=""))
  write_csv(rbind(metrics), paste("Country", c_id, "_Metrics.csv", sep=""))
}
