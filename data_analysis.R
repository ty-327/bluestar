library(tidyverse)
library(lubridate)

setwd('C:/Users/mason/Home/Personal/Coding Projects/bluestar')

shipments <- read.csv('shipments.csv')
carriers <- read.csv('carriers.csv')

# How much does BlueStar annually spend on transportation?
shipments_clean <- shipments %>% 
  janitor::clean_names() %>% 
  mutate(
    freight_paid = parse_number(freight_paid),
    year = year(mdy(ship_date))
    )

annual_spend <- shipments_clean %>% 
  group_by(year) %>% 
  summarise(total_freight_paid = sum(freight_paid))

# What are some of the key metrics for LTL (Less Than Truckload) shipments?
# Weight: The total weight of the shipment is a fundamental metric. It helps carriers determine pricing and capacity utilization.
# Volume: Volume refers to the amount of space a shipment occupies. It's important for carriers to optimize space utilization in their trailers.
# Freight Class: Freight class categorizes shipments based on density, value, stow-ability, and handling requirements. It affects pricing and determines the appropriate handling procedures.
# Transit Time: Transit time is the duration it takes for a shipment to reach its destination. It's a critical metric for both shippers and receivers to plan their operations effectively.
# On-Time Delivery: This metric measures the percentage of shipments that are delivered on time as promised. It reflects carrier reliability and performance.
# Claims Ratio: The claims ratio compares the total value of claims for lost or damaged shipments to the total revenue generated. It indicates the carrier's effectiveness in handling shipments without incidents.
# Linehaul Cost: Linehaul cost is the expense associated with transporting freight between terminals or hubs. It includes fuel, labor, equipment maintenance, and other operational costs.
# Pickup and Delivery Time: This metric measures the time it takes for carriers to pick up and deliver shipments. Efficient pickup and delivery processes contribute to customer satisfaction.
# Empty Miles: Empty miles represent the distance traveled by carriers without carrying any freight. Minimizing empty miles improves efficiency and reduces operational costs.
# Utilization Rate: Utilization rate measures the percentage of capacity utilized on trucks. Higher utilization rates indicate better resource optimization and profitability.

# Let me make this simple. Tell me what I need to know, tell me what I need to do, and tell me what we
# can improve. I need to reduce costs and improve service. Give me tangible, data driven
# recommendations that are actionable.


