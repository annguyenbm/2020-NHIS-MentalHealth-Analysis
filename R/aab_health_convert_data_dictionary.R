library(tidyverse)

# Load in the data dictionary CSV: 

data_dictionary <- read_csv("../data-raw/gp-001-aab-data-dictionary.csv")

# Examine the data dictionary CSV: 

head(data_dictionary)

# Write RDS file and save to data folder: 

write_rds(data_dictionary, "../data/tab7_dictionary.rds", compress = "gz")
