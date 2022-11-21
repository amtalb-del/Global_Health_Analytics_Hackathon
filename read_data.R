# install.pacakges('tidyverse') # if you don't have it installed yet
library(tidyverse, quietly=T)
library(readxl, quietly=T)

setwd("~/Global Health Analytics Hackathon")

# dynamically read in multiple files (from GUI) <- make another file 
path <- "data/FINAL Guinea TIPAC Hackathon Data Set.xlsx"

# get first header row (years)
(years <- read_excel(path, sheet = "Disease Burden Data", skip=18, n_max = 0, .name_repair = "universal") %>% 
    names())
years <- years[sapply(years, function(n) nchar(n) > 6)]
years <- str_replace(years, "\\.\\.\\.", "")
# read in sheet again using actual column names
disease_burden_df <- read_excel(path, sheet="Disease Burden Data", guess_max=Inf, skip=19, .name_repair = "universal")
# remove empty columns
disease_burden_df <- disease_burden_df[, -which(startsWith(names(disease_burden_df), '...'))]
# prefix the colnames with their respective year
new_cnames <- c()
first_cols <- colnames(disease_burden_df)[1:4]
col_set <- colnames(disease_burden_df)[5:10]
for (year in years) {
  new_cnames <- append(new_cnames, paste0(year, '_', col_set))
}
new_cnames <- str_replace(new_cnames, "\\.\\.\\.[0-9]+$", "")
names(disease_burden_df) <- append(first_cols, new_cnames)
disease_burden_df <- disease_burden_df[-c(1:2),]
disease_burden_df <- disease_burden_df %>% pivot_longer(cols = -c(1:4),
                                                        names_to = c("year", "field"),
                                                        names_sep = "_",
                                                        values_to = "total")
disease_burden_df <- disease_burden_df %>% mutate(type = if_else(field == "Total.population", "population", "disease"))
disease_burden_df$Regions <- disease_burden_df$Regions %>% as.factor()
disease_burden_df$Districts <- disease_burden_df$Districts %>% as.factor()
disease_burden_df$year <- disease_burden_df$year %>% as.numeric()
disease_burden_df$field <- disease_burden_df$field %>% as.factor()
disease_burden_df$type <- disease_burden_df$type %>% as.factor()


year_pop_df <- disease_burden_df %>% subset(field == "Total.population") %>% subset(year == 2022) %>% select(Regions, Districts, year, total)


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
names(gov_owned_vehicles_df) <- str_replace(names(gov_owned_vehicles_df), "\\.\\.\\.[0-9]+$", "")
names(hired_vehicles_df) <- str_replace(names(hired_vehicles_df), "\\.\\.\\.[0-9]+$", "")
gov_owned_vehicles_df$type <- "gov"
hired_vehicles_df$type <- "hired"


other_df <- read_excel(path, sheet="Other", skip=2, col_names = F, guess_max=20000)
other_df <- other_df[,c(1,5)]
other_df <- other_df %>% pivot_wider(names_from = 1, values_from = 2)


activity_df <- read_excel(path, sheet="Activity List", guess_max=20000)


subactivity_df <- read_excel(path, sheet="Subactivity List", guess_max=20000)


financing_activities_df <- read_excel(path, sheet="Financing of Activities", guess_max=20000, .name_repair = "universal")


pc_cnames1 <- read_excel(path, sheet="PC-Drug Data", guess_max=20000) %>% names()
pc_cnames2 <- read_excel(path, sheet="PC-Drug Data", skip=1, guess_max=20000) %>% names()
pc_cnames <- append(pc_cnames2[1:2], pc_cnames1[3:6])
pc_drug_df <- read_excel(path, sheet="PC-Drug Data", skip=2, col_names=pc_cnames, guess_max=20000)


drug_needs_df <- read_excel(path, sheet="Drug Needs by District", guess_max=20000)
drug_needs_df <- drug_needs_df[-c(1:2),]
drug_needs_df <- drug_needs_df %>% separate(District, c("region", "district"), ": ")
