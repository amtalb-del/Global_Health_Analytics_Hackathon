# install.pacakges('tidyverse') # if you don't have it installed yet
library(tidyverse, quietly=T)
library(readxl, quietly=T)

setwd("~/Global Health Analytics Hackathon")

# dynamically read in multiple files (from GUI) <- make another file 
path <- "data/FINAL Guinea TIPAC Hackathon Data Set.xlsx"

disease_burden_df <- read_excel(path, sheet="Disease Burden Data", guess_max=Inf)
disease_burden_df <- df[c(22,length(df)),]

target_population_df <- read_excel(path, sheet="Target Population", guess_max=Inf)
target_population_df <- df[c(14,length(df)),]

population_data_df <- read_excel(path, sheet="Population Data", guess_max=Inf)
population_data_1_df <- df[c(6,12),]
population_data_2_df <- df[c(17,21),]

personnel_costs_df <- read_excel(path, sheet="Personnel Costs", guess_max=Inf)
personnel_costs_df <- df[c(10,length(df)),]

per_diems_df <- read_excel(path, sheet="Per Diems", guess_max=Inf)
per_diems_df <- df[c(8,length(df)),]

transport_costs_df <- read_excel(path, sheet="Transport Costs", guess_max=Inf)
transport_costs_df <- df[c(13,length(df)),]

other_df <- read_excel(path, sheet="Other", guess_max=Inf)
other_df <- df[c(4,length(df)),]

activity_df <- read_excel(path, sheet="Activity List", guess_max=Inf)

subactivity_df <- read_excel(path, sheet="Subactivity List", guess_max=Inf)

financing_activities_df <- read_excel(path, sheet="Financing of Activities", guess_max=Inf)

pc_drug_df <- read_excel(path, sheet="PC-Drug Data", guess_max=Inf)
pc_drug_df <- df[c(1,length(df)),]

drug_needs_df <- read_excel(path, sheet="Drug Needs by District", guess_max=Inf)
drug_needs_df <- df[c(2,length(df)),]