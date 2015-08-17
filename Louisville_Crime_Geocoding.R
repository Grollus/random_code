# Source in the csv file
library(data.table)
library(dplyr)

addresses <- fread("unique_addresses.csv", data.table = FALSE)

# 