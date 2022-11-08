# install.pacakges('tidyverse') # if you don't have it installed yet
library(tidyverse, quietly=T)
library(readxl, quietly=T)

setwd("~/Global Health Analytics Hackathon")

# dynamically read in multiple files (from GUI) <- make another file 
path <- "data/FINAL Guinea TIPAC Hackathon Data Set.xlsx"

# get first header row (years)
(cnames <- read_excel(path, sheet = "Disease Burden Data", skip=18, n_max = 0) %>% 
    names())
cnames <- cnames[!startsWith(cnames, '...')]
# read in sheet again using actual column names
disease_burden_df <- read_excel(path, sheet="Disease Burden Data", guess_max=Inf, skip=19)
# remove empty columns
disease_burden_df <- disease_burden_df[, -which(startsWith(names(disease_burden_df), '...'))]
# prefix the colnames with their respective year
new_cnames <- c()
first_cols <- colnames(disease_burden_df)[1:4]
col_set <- colnames(disease_burden_df)[6:11]
for (year in cnames) {
  new_cnames <- append(new_cnames, paste0(year, col_set))
}

names(disease_burden_df) <- append(first_cols, new_cnames)
disease_burden_df <- disease_burden_df[-c(1:2),]


target_population_df <- read_excel(path, sheet="Target Population", skip=11, guess_max=20000)
target_population_df <- target_population_df[-c(1:2),]
target_population_df <- target_population_df[target_population_df$Region != 0,]


population_data_1_df <- read_excel(path, sheet="Population Data", skip=5, guess_max=20000)
population_data_1_df <- population_data_1_df[c(1:6),]
population_data_1_df = subset(population_data_1_df, select = -c(3))
population_data_2_df <- read_excel(path, sheet="Population Data", skip=16, guess_max=20000)


personnel_costs_df <- read_excel(path, sheet="Personnel Costs", skip=9, guess_max=20000)


per_diems_df <- read_excel(path, sheet="Per Diems", skip=7, guess_max=20000)


transport_costs_df <- read_excel(path, sheet="Transport Costs", skip=12, guess_max=20000)
gov_owned_vehicles_df <- transport_costs_df[,1:2]
hired_vehicles_df <- transport_costs_df[,4:7]


other_df <- read_excel(path, sheet="Other", skip=2, col_names = F, guess_max=20000)
other_df <- other_df[,c(1,5)]
other_df <- other_df %>% pivot_wider(names_from = 1, values_from = 2)


activity_df <- read_excel(path, sheet="Activity List", guess_max=20000)


subactivity_df <- read_excel(path, sheet="Subactivity List", guess_max=20000)


financing_activities_df <- read_excel(path, sheet="Financing of Activities", guess_max=20000)


cnames1 <- read_excel(path, sheet="PC-Drug Data", guess_max=20000) %>% names()
cnames2 <- read_excel(path, sheet="PC-Drug Data", skip=1, guess_max=20000) %>% names()
cnames <- append(cnames2[1:2], cnames1[3:6])
pc_drug_df <- read_excel(path, sheet="PC-Drug Data", skip=2, col_names=cnames, guess_max=20000)


drug_needs_df <- read_excel(path, sheet="Drug Needs by District", guess_max=20000)
drug_needs_df <- drug_needs_df[-c(1:2),]
