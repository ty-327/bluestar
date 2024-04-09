library(tidyr)
library(janitor)
library(dplyr)
library(tidyverse)
library(tidymodels)
library(lubridate)
library(ggplot2)

# setwd('bluestar')

shipments_raw <- read.csv('shipments.csv') %>% as_tibble()
carriers_raw <- read.csv('carriers.csv') %>% as_tibble()

# remove characters from freight_paid and convert to numeric 
shipments <- shipments_raw %>% 
  clean_names() %>% 
  mutate(
    freight_paid = sub("\\$", "", freight_paid),
    freight_paid = sub(",", "", freight_paid),
    freight_paid = as.numeric(freight_paid),
    origin_zip = as.character(origin_zip),
    origin_zip = if_else(nchar(origin_zip) == 4, str_pad(origin_zip, width = 5, side = "left", pad = "0"), origin_zip),
    ship_date = mdy(ship_date)
  )

carriers <- carriers_raw %>% 
  clean_names()
  

# check for NA values
shipments %>%
  summarise(across(everything(), ~any(is.na(.))))

shipments %>% glimpse

# check milage
shipments %>% arrange(desc(miles)) %>% view()
shipments %>% filter(origin_city == 'GREENFIELD' & dest_city == 'RIVERSIDE') %>% arrange(desc(miles))
shipments %>% filter(miles < 7000) %>% 
  group_by(origin_city, dest_city) %>% 
  summarize(avg_miles = mean(miles)) %>% 
  filter(origin_city == 'GREENFIELD' & dest_city == 'RIVERSIDE')

# fix issues with four rows of high milage
shipments <- shipments %>% 
  mutate(miles = if_else(origin_city == 'GREENFIELD' & 
                           dest_city == 'RIVERSIDE' & 
                           miles > 7000, 608, miles))


# --------------=QUESTION 3=---------------
# top origin/destination pairs by row count
top_pairs_by_count <- shipments %>% 
  group_by(origin_city, dest_city) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))

# top origin/destination pairs by freight_paid
top_pairs_by_freight_paid <- shipments %>%
  group_by(origin_city, dest_city) %>%
  summarize(total_freight_paid = sum(freight_paid, na.rm = TRUE)) %>% 
  arrange(desc(total_freight_paid))

# top origin/destination pairs by volume
top_pairs_by_volume <- shipments %>%
  group_by(origin_city, dest_city) %>%
  summarize(volume = sum(volume, na.rm = TRUE)) %>% 
  arrange(desc(volume))

# OBSERVATION:: TOP 3 ORIGIN-DESTINATION PAIRS ARE: (ALL TOP 3 IN ROW COUNT, TOTAL VOLUME, AND TOP 4 IN TOTAL FREIGHT PAID)
# PAINESVILLE-HAMILTON
# FT WAYNE-FLOWERY BRANCH
# ATLANTA-CHARLOTTE

# THESE ARE IN APPROXIMATE ORDER^



# --------------=QUESTION 4=---------------
# number of carriers serving each of the top 10 pairs by row count
shipments %>%
  right_join(top_pairs_by_count, by = c("origin_city", "dest_city")) %>%
  group_by(origin_city, dest_city) %>%
  summarize(unique_carriers = n_distinct(scac)) %>% 
  filter(origin_city == 'ATLANTA' & dest_city == 'CHARLOTTE' |
           origin_city == 'PAINESVILLE' & dest_city == 'HAMILTON' |
           origin_city == 'FT WAYNE' & dest_city == 'FLOWERY BRANCH' |
           origin_city == 'PAINESVILLE' & dest_city == 'MILTON' |
           origin_city == 'CHICOPEE' & dest_city == 'MILFORD' |
           origin_city == 'RIVERSIDE' & dest_city == 'OMAHA' |
           origin_city == 'ATLANTA' & dest_city == 'CRANBURY' |
           origin_city == 'GREENSBORO' & dest_city == 'CHARLOTTE' |
           origin_city == 'CRANBURY' & dest_city == 'HAGERSTOWN' |
           origin_city == 'ATLANTA' & dest_city == 'FORT WAYNE')

# num total carriers serving top 3 pairs by row count
shipments %>%
  right_join(top_pairs_by_count, by = c("origin_city", "dest_city")) %>%
  filter(origin_city == 'ATLANTA' & dest_city == 'CHARLOTTE' |
           origin_city == 'PAINESVILLE' & dest_city == 'HAMILTON' |
           origin_city == 'FT WAYNE' & dest_city == 'FLOWERY BRANCH') %>% 
           # origin_city == 'PAINESVILLE' & dest_city == 'MILTON' |
           # origin_city == 'CHICOPEE' & dest_city == 'MILFORD' |
           # origin_city == 'RIVERSIDE' & dest_city == 'OMAHA' |
           # origin_city == 'ATLANTA' & dest_city == 'CRANBURY' |
           # origin_city == 'GREENSBORO' & dest_city == 'CHARLOTTE' |
           # origin_city == 'CRANBURY' & dest_city == 'HAGERSTOWN' |
           # origin_city == 'ATLANTA' & dest_city == 'FORT WAYNE') %>% 
  summarize(unique_carriers = n_distinct(scac))

# num total carriers serving top 3 pairs by volume
shipments %>%
  right_join(top_pairs_by_volume, by = c("origin_city", "dest_city")) %>%
  filter(origin_city == 'PAINESVILLE' & dest_city == 'HAMILTON' |
           origin_city == 'FT WAYNE' & dest_city == 'FLOWERY BRANCH' |
           origin_city == 'ATLANTA' & dest_city == 'CHARLOTTE') %>% 
  summarize(unique_carriers = n_distinct(scac))

# num total carriers serving top 10 pairs by volume
shipments %>%
right_join(top_pairs_by_volume, by = c("origin_city", "dest_city")) %>%
  filter(origin_city == 'PAINESVILLE' & dest_city == 'HAMILTON' |
           origin_city == 'FT WAYNE' & dest_city == 'FLOWERY BRANCH' |
           origin_city == 'ATLANTA' & dest_city == 'CHARLOTTE') %>% 
  summarize(unique_carriers = n_distinct(scac))

# OBSERVATION:: ATLANTA-CRANBURY IS 5TH IN TOTAL VOLUME, 7TH IN ROW COUNT, AND 1ST**** IN TOTAL FREIGHT PAID ---> look into this/fix this?
# OBERVATION:: 48 UNIQUE CARRIERS ACCOUNT FOR THE TOP 3 PAIRS


# --------------=QUESTION 5=---------------
# gonna add the carrier data to this table
combined <- shipments %>% left_join(carriers, by = "scac")
combined %>% 
  group_by(carrier_type) %>% 
  summarize(
    avg_shipment_volume = mean(volume),
    avg_shipment_weight = mean(weight),
    avg_length = mean(miles)
    )


# --------------=QUESTION 6=---------------
top_carriers <- shipments %>%
  count(scac) %>%
  arrange(desc(n)) %>% 
  slice(1:10) %>% 
  pull(scac)

# --------------=QUESTION 7=---------------
combined %>% 
  ggplot(aes(x = miles, y = freight_paid / miles)) + 
  geom_line()



combined <- combined %>% 
  mutate(top_carrier_name = if_else(scac %in% top_carriers, scac, 'Other'))


combined %>% distinct(top_carrier_name)
combined %>% 
  ggplot(aes(x = miles, y = freight_paid / weight)) + 
  geom_point() +
  facet_wrap(~carrier_type, ncol = 1) +
  #facet_grid(top_carrier_name ~ carrier_type, scales= 'free') +
  theme_bw() + 
  labs(
    title = 'Price per lb per mile by carrier type',
    y = 'Price per pound',
    x = 'Miles'
  )

# combined %>% 
#   mutate(stat = ) %>% 
#   group_by(scac) %>% 
#   summarize(
#     most_expensive = max(mean(price_per_lb_per_mile))
#   )




# --------------=OTHER ANALYSIS=---------------
# freight_paid by carrier
freight_paid_by_carrier <- shipments %>%
  group_by(scac) %>%
  summarize(freight_paid = sum(freight_paid, na.rm = TRUE)) %>% 
  arrange(desc(freight_paid))

# miles by carrier
miles_by_carrier <- shipments %>%
  group_by(scac) %>%
  summarize(miles = sum(miles, na.rm = TRUE)) %>% 
  arrange(desc(miles))

# volume by carrier
volume_by_carrier <- shipments %>%
  group_by(scac) %>%
  summarize(volume = sum(volume, na.rm = TRUE)) %>% 
  arrange(desc(volume))

# weight by carrier
weight_by_carrier <- shipments %>%
  group_by(scac) %>%
  summarize(weight = sum(weight, na.rm = TRUE)) %>% 
  arrange(desc(weight))

# average price/volume unit by carrier
avg_price_per_volume_by_carrier <- shipments %>%
  mutate(price_per_volume_unit = freight_paid / volume) %>% 
  group_by(scac) %>%
  summarize(avg_price_per_volume = mean(price_per_volume_unit, na.rm = TRUE)) %>% 
  arrange(desc(avg_price_per_volume))

#
combined %>% 
  filter(scac %in% top_carriers) %>% 
  ggplot(aes(x = miles, y = freight_paid / volume)) + 
  geom_point() +
  facet_wrap(~scac, ncol = 1) +
  #facet_grid(top_carrier_name ~ carrier_type, scales= 'free') +
  theme_bw() + 
  labs(
    title = 'Price per volume unit per mile by carrier type',
    y = 'Price per volume unit',
    x = 'Miles'
  )

# OBSERVATION:: CRSE DOES A LOT OF MILES AND HOLDS A LOT OF VOLUME FOR RELATIVELY CHEAP

# Could also look at freight_paid per mile/volume unit/weight unit by carrier
# Could also ask chatGPT what it thinks on where we can look to know how we can improve (give it context and columns)


# --------------=ML ANALYSIS=---------------
shipments_ml <- shipments %>% 
  mutate(across(c(
    origin_zip, 
    dest_zip,
    scac,
    origin_city,
    origin_state,
    dest_city,
    dest_state,
    dest_zip
    ), ~as.factor(.))) %>% 
  select(weight, volume, miles, on_time, delivered_complete, damage_free, billed_accurately, freight_paid, scac)

shipments_split <- shipments_ml %>% initial_split(strata = freight_paid)
shipments_training <- shipments_split %>% training()
shipments_testing <- shipments_split %>% testing()

# Adding a step to normalize independent variables
shipments_rec <- recipe(freight_paid ~ ., data = shipments_training) %>%
  step_normalize(all_numeric(), -all_outcomes())

# Preparing the model specification for linear regression
lin_reg_spec <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

# Bundling the recipe and model spec
shipments_workflow <- workflow() %>%
  add_recipe(shipments_rec) %>%
  add_model(lin_reg_spec)

# Fitting the model
shipments_fit <- fit(shipments_workflow, data = shipments_training)

# Summarizing the model fit
summary(shipments_fit)
  






