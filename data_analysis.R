library(tidyr)
library(janitor)
library(dplyr)

shipments_raw <- read.csv('shipments.csv')
carriers_raw <- read.csv('carriers.csv')

shipments <- shipments_raw %>% clean_names()
carriers <- carriers_raw %>% clean_names()

