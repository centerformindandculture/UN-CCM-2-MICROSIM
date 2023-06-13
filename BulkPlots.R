#####################################################################################
######################### CHECK MODEL OUTPUT MAIN FIGURES
####################
### libraries needed
library(dplyr)
library(readxl)
library(readr)

# SET TO FALSE TO DO BOTTOM UP FIGURES INSTEAD (SUPPLEMENTARY MATERIAL)
generate_top_down_figures = TRUE

read_excel_and_replace_spaces_in_headers <- function(filename) {
  data<-as.data.frame(read_excel(filename, col_names=TRUE))
  names(data)<-sub(" ", ".", names(data))
  data
}

DF.all <- read_csv("PaperFigureAssetsAndData/CRED_Annual_Stats.csv") 

NorwayStartingPop <- 3265300
USAStartingPop <- 158804397
IndiaStartingPop <- 376325200
DF.all$country <- as.factor(ifelse(DF.all$country_id == 1, "Norway", ifelse(DF.all$country_id == 2, "USA", "India")))
DF.all$country_starting_pop <- ifelse(DF.all$country == "Norway", NorwayStartingPop, ifelse(DF.all$country == "USA", USAStartingPop, IndiaStartingPop))
DF.all$Current_Period_Pop <- (DF.all$country_starting_pop / DF.all$NumberOfInitialAgents) * DF.all$Current_Period_Pop
DF.all$Num_Births <- (DF.all$country_starting_pop / DF.all$NumberOfInitialAgents) * DF.all$Num_Births
DF.all$Num_Deaths <- (DF.all$country_starting_pop / DF.all$NumberOfInitialAgents) * DF.all$Num_Deaths
DF.all$SimulationType <- as.factor(
  paste(
  ifelse(DF.all$TopDown, "Top Down", "Bottom Up"),
  ifelse(DF.all$FiveYearStep, " Five Year", " One Year"),
  ifelse(DF.all$FiveYearStep & DF.all$splitFertility, " Split Fertility", ""),
  ifelse(DF.all$FiveYearStep & !DF.all$splitFertility, " Intuitive", ""),
  ifelse(!DF.all$FiveYearStep & DF.all$splitFertility, " Split Fertility", ""),
  ifelse(!DF.all$FiveYearStep & !DF.all$splitFertility & DF.all$ForceUseSplitAge, " Split Age", ""),
  ifelse(!DF.all$FiveYearStep & !DF.all$splitFertility & !DF.all$ForceUseSplitAge, " Intuitive", ""),
  sep = ""
))

DF.all <- DF.all %>% filter(TopDown == generate_top_down_figures)

# Country 1 == Norway, 2 == USA, 3 == India
for (c_id in c(1 ,2 ,3)) {
  DF <- DF.all %>% filter(country_id == c_id)
  
  if (c_id == 1) {
    DF_Obs <- read_excel_and_replace_spaces_in_headers("data_assets/Observed_UN_values.xlsx")
    saved_names <- names(DF_Obs)
    DF_Obs <- cbind(DF_Obs[2:31, 1], DF_Obs[2:31, 2:20] * 100 / 100000)
    names(DF_Obs) <- saved_names
    
    DF$Current_Period_Pop <- DF$Current_Period_Pop / 100000
    DF$Num_Births <- DF$Num_Births / 100000
    DF$Num_Deaths <- DF$Num_Deaths / 100000
  } else if (c_id == 2) {
    DF_Obs <- read_excel_and_replace_spaces_in_headers("data_assets/Observed_UN_values_US.xlsx")
    saved_names <- names(DF_Obs)
    DF_Obs <- cbind(DF_Obs[2:31, 1], DF_Obs[2:31, 2:20] * (1 / 0.0003) / 1000000)
    names(DF_Obs) <- saved_names
    
    DF$Current_Period_Pop <- DF$Current_Period_Pop / 1000000
    DF$Num_Births <- DF$Num_Births / 1000000
    DF$Num_Deaths <- DF$Num_Deaths / 1000000
  } else if (c_id == 3) {
    DF_Obs <- read_excel_and_replace_spaces_in_headers("data_assets/Observed_UN_values_India.xlsx")
    saved_names <- names(DF_Obs)
    DF_Obs <- cbind(DF_Obs[2:31, 1], DF_Obs[2:31, 2:20] * (1 / 0.00015) / 1000000)
    names(DF_Obs) <- saved_names
    
    DF$Current_Period_Pop <- DF$Current_Period_Pop / 1000000
    DF$Num_Births <- DF$Num_Births / 1000000
    DF$Num_Deaths <- DF$Num_Deaths / 1000000
  }
  DF_Obs$SimulationType <- "UN Estimates"
  
  ## data frame with model outputs
  DF_Plot3<-DF[,c("Period","Current_Period_Pop","Num_Births","Num_Deaths", "SimulationType")]
  ## data frame with observed UN values
  DF_Obs_Plot3<-DF_Obs[,c("Period","Population","Births","Deaths", "SimulationType")]
  ## names in both dataframes should be equal
  names(DF_Plot3)<-names(DF_Obs_Plot3)
  
  
  ### In order to plot the data in ggplot, the dataframes have to be reshaped
  ### we used reshape library
  library(reshape)
  library(ggplot2)
  DF_Plot3 <- as.data.frame(DF_Plot3)
  DF_Obs_Plot3 <- as.data.frame(DF_Obs_Plot3)

  DF_Plot3<-melt(DF_Plot3,id.vars = c("Period", "SimulationType"),variable_name = "Measurement")
  DF_Obs_Plot3<-melt(DF_Obs_Plot3,id.vars = c("Period", "SimulationType"),variable_name = "Measurement")

  
  ### Once reshaped, we plot them, we create one plot per dataframe
  ### note that the geom_boxplot will take the data with the model outputs and geom_point the UN data
  
  p3<-ggplot(DF_Plot3, aes(x=Period, value)) + 
    geom_boxplot(aes(color=SimulationType))+
    facet_grid(rows=vars(Measurement),scales = "free") +  
    geom_point(data = DF_Obs_Plot3, colour = "black", size = 2) +
    theme(strip.text.y = element_text(size = 20, color = "#002B49")) + 
    theme(axis.text.x=element_text(size=rel(1), angle=60, vjust = 0.5, color = "#002B49")) +
    theme(axis.title = element_text(size=rel(1.45), color = "#002B49")) +
    theme(axis.text.y=element_text(size=rel(1), color = "#002B49")) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(strip.background =element_rect(fill="#99A698"), strip.text = element_text(color = "#002B49")) +
    theme(legend.text=element_text(color= "#002B49"), 
          legend.title = element_text(color = "#002B49"),
          legend.position = c(0.86, 0.085)) + 
    labs(color = "Model Design") +
    theme(legend.key.size = unit(0.1, 'cm'), #change legend key size
          legend.key.height = unit(0.1, 'cm'), #change legend key height
          legend.key.width = unit(0.5, 'cm'), #change legend key width
          legend.title = element_text(size=12), #change legend title font size
          legend.text = element_text(size=10)) #change legend text font size))
  
    
  if (c_id == 1) {
    p3 <- p3 + ylab("Count (/100K)")
  } else {
    p3 <- p3 + ylab("Count (/1MIL)")
  }
  p3
  ### save the plots (uncomment for other formats)
  ### if not specify, the files will be saved in the working directory
  #ggsave(paste("Country", c_id, ifelse(generate_top_down_figures, "TopDown", "BottomUp"), "_Pop_Births_Deaths.pdf", sep="") ,p3,width = 11, height = 8.5, units = "in",dpi = 300)
  ggsave(paste("Country", c_id, ifelse(generate_top_down_figures, "TopDown", "BottomUp"), "_Pop_Births_Deaths.png", sep="") ,p3,width = 11, height = 8.5, units = "in",dpi = 300)
  #ggsave(paste("Country", c_id, ifelse(generate_top_down_figures, "TopDown", "BottomUp"), "_Pop_Births_Deaths.eps", sep="") ,p3,width = 11, height = 8.5, units = "in",dpi = 300)
  
}
