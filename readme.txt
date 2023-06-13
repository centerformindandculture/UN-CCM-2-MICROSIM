AnyLogic code is written using AnyLogic version 8.7.10.202202091718
R code was run with R version 4.1.2 (2021-11-01)
R packages:
 - dplyr 1.0.7
 - readxl 1.3.1
 - purrr 0.3.4
 - tidyr 1.2.0
 - readr 2.1.1
 - moments 0.14
 - reshape 0.8.8
 - ggplot2 3.3.5

AnyLogic can be downloaded and installed at: https://www.anylogic.com/downloads/
There is a free personal learning edition that can load the model and run the
model at low agent counts. An AnyLogic Professional or Researcher license is
required to run the model at the scale ran in the paper (100K initial agents).

To run the model after installing AnyLogic, open Cohort_ComponentMethod.alp
in AnyLogic. Under Projects, right click ParametersVariation and run it.
This will run under the settings that recreates the data used in the paper:
100 replications starting with 100K initial agents for Norway, USA, and India,
with both TopDown, BottomUp variations, with and without split fertility.
Model output goes to the root directory as a file called CRED_Annual_Stats.csv.
The model can be ran for fewer replications by decreasing the range of the 
LHS_Experiment_Number parameter. The model can be ran with fewer inital agents
by decreasing the parameter input_NumberOfInitialAgents. Other input_* parameters
can be changed as wanted. Note that integers are often mapped to booleans
(0 == false, 0 != true).

Note the the model relies on the ./database directory, so do not copy the .alp
file to a new directory without also copying ./database.

=============================================================================
=  Directories explained                                                    =
=============================================================================

./ Root directory
The root directory contains the model and the R scripts used to generate model
figures from model output. The model will generate output to the root directory.
The R Scripts are currently set up to run using data in ./PaperFigureAssetsAndData/,
so this code can be ran immediately, and will need to be updated to model on new
model output. The R Scripts generate output to the root directory. MetricsCalculator.R
generates aggregate metric outputs of how close the model output is to the UN
data. BulkPlots.R plot multiple model variations on one plot.

./PaperFigureAssetsAndData
This directory contains the assets (metrics and figures) used in the paper, as well
as the raw data that was used to generate them 
(./PaperFigureAssetsAndData/CRED_Annual_Stats.csv).

./data_assets
This contains the original data files used by the model and/or model output
processing. ./data_assets/UN_Tables contains the original UN tables, while
./data_assets/*.xlsx are processed version of the data.
./data_assets/Initialization_Tables.xlsx is the initialization data which is
directly loaded in to the model database.
./data_assets/Observed_UN_values_<countr>.xlsx is the population data we 
measure the model data up against, used in our output processing.

./database
This contains the database files for the AnyLogic model. Do not edit this 
directory.
