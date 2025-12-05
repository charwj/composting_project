library(tidyverse)
library(janitor)

DSNY_tonnage <- read_csv("DSNY_Monthly_Tonnage_Data_20251130.csv")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Looking at total refuse tonnage overtime~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Create year and month columns that are separate and numeric
DSNY_tonnage <- DSNY_tonnage %>% 
  separate(MONTH, into = c("year", "month"), sep = "/", remove = FALSE) %>%
  mutate(
    year  = as.numeric(str_trim(year)),
    month = as.numeric(str_trim(month))
  ) %>% # making a date formatted month/date column
  mutate(
    date = ym(gsub(" ", "", MONTH))   # remove spaces → "2025/10"
  )

# Total tonnage between 2005 and 2015
graph_tonnage_goal <- DSNY_tonnage %>%
  filter(between(year, 2005, 2015)) %>%
  group_by(year) %>%
  summarise(
    total_refuse = sum(REFUSETONSCOLLECTED, na.rm = TRUE)
  )

# Total tonnage from 2005 to now
graph_tonnage_reality <- DSNY_tonnage %>%
  filter(between(year, 2005, 2024)) %>%
  group_by(year) %>%
  summarise(
    total_refuse = sum(REFUSETONSCOLLECTED, na.rm = TRUE)
  )

# Pull total tonnage in 2005
baseline_2005 <- graph_tonnage_goal %>% 
  filter(year == 2005) %>% 
  pull(total_refuse)

# Create a 2030 value that is 10% of the total tonnage in 2005
graph_tonnage_goal <- graph_tonnage_goal %>%
  bind_rows(
    tibble(
      year = 2030,
      total_refuse = baseline_2005 * 0.10
    )
  )


# Create line graph with total refuse from 2005 to 2015, then the goal by 2030
# I want to do this in datawrapper so I can have the hover features
graph_tonnage_goal %>% write_csv("graph_tonnage_goal.csv")

# Create another line graph with actual total refuse from 2005 to now
# I want to do this in datawrapper so I can have the hover features
graph_tonnage_reality %>% write_csv("graph_tonnage_reality.csv")

# Comparing total refuse amount in 2024 to 2015
with(graph_tonnage_reality,
     total_refuse[year == 2024] / total_refuse[year == 2015]
)

# Comparing total refuse amount in 2024 to 2005
with(graph_tonnage_reality,
     total_refuse[year == 2024] / total_refuse[year == 2005]
)







#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Composting by CD and by Boro (normalized by population) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# creating the community district # in the same format as the other files
DSNY_tonnage <- DSNY_tonnage %>%
  mutate(
    boro_ID = paste0(BOROUGH_ID, COMMUNITYDISTRICT)
  )

# Calculating the last 12 months of composting tonnage data, by CD, to see the amount over one year 
Nov2024_Oct2025 <- DSNY_tonnage %>%
  filter(
    (year == 2024 & month >= 11) |
      (year == 2025 & month <= 10)
    ) %>% 
  group_by(boro_ID) %>%
  summarise(
    total_res_compost = sum(RESORGANICSTONS, na.rm = TRUE),
    total_res_refuse = sum(REFUSETONSCOLLECTED, na.rm = TRUE)
    )

# add in the data on CD population so that I can normalize it
pop_2023 <- read_csv("Dem_19-23_CDTA.csv")

clean_pop_2023 <- pop_2023 %>% 
  filter(CDTAType == "CD") %>% 
  select(GeogName, GeoID, Borough, Pop_1E)

# putting CD number in the same format as the other dataset
borough_lookup <- c(
  "MN" = "1",
  "BX" = "2",
  "BK" = "3",
  "QN" = "4",
  "SI" = "5"
)

clean_pop_2023 <- clean_pop_2023 %>% 
  mutate(
    borough_prefix = substr(GeoID, 1, 2),
    cd_number = substr(GeoID, 3, 4),
    boro_ID = paste0(borough_lookup[borough_prefix], cd_number)
  )

# joining the two datasets
Nov2024_Oct2025_pop <- Nov2024_Oct2025 %>%
  left_join(
    clean_pop_2023 %>% select(boro_ID, Pop_1E),
    by = "boro_ID"
  )

# creating a normalized variable compost tonnage per 100,000 residents
Nov2024_Oct2025_pop <- Nov2024_Oct2025_pop %>%
  mutate(
    rate_per_100k = (total_res_compost / Pop_1E) * 100000,
    refuse_per_100k = (total_res_refuse / Pop_1E) * 100000
  )

Nov2024_Oct2025_pop %>% write_csv("Nov2024_Oct2025_pop.csv")

# putting this all at the borough level

boro_population <- clean_pop_2023 %>%
  group_by(Borough) %>% 
  summarise(
    boro_pop = sum(Pop_1E, na.rm = TRUE)
  )

# calculating the last 12 months of composting tonnage data, by borough, to see the amount over one year 
boro_Nov2024_Oct2025 <- DSNY_tonnage %>%
  filter(
    (year == 2024 & month >= 11) |
      (year == 2025 & month <= 10)
  ) %>% 
  group_by(BOROUGH) %>%
  summarise(
    total_res_compost = sum(RESORGANICSTONS, na.rm = TRUE)
  )

# joining the two datasets
boro_Nov2024_Oct2025_pop <- boro_Nov2024_Oct2025 %>%
  left_join(
    boro_population %>% select(Borough, boro_pop),
    by = c("BOROUGH" = "Borough")
  )

# creating a normalized variable compost tonnage per 100,000 residents
boro_Nov2024_Oct2025_pop <- boro_Nov2024_Oct2025_pop %>%
  mutate(
    rate_per_100k = (total_res_compost / boro_pop) * 100000
  )

boro_Nov2024_Oct2025_pop %>% write_csv("boro_Nov2024_Oct2025_pop.csv")





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Creating Data for Line Graph 3 (Borough Comparison)~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Calculating the last 12 months of composting tonnage data, by CD, to see the monthly amount over one year 
monthly_Nov2024_Oct2025 <- DSNY_tonnage %>%
  filter(
    (date >= as.Date("2022-10-01"))
  ) %>% 
  group_by(boro_ID, date) %>%
  summarise(
    total_res_compost = sum(RESORGANICSTONS, na.rm = TRUE)
  ) %>% 
  mutate( # added back the borough names
    borough = case_when(
      startsWith(boro_ID, "1") ~ "Manhattan",
      startsWith(boro_ID, "2") ~ "Bronx",
      startsWith(boro_ID, "3") ~ "Brooklyn",
      startsWith(boro_ID, "4") ~ "Queens",
      startsWith(boro_ID, "5") ~ "Staten Island"
    ),
  )

# adding population
monthly_Nov2024_Oct2025 <- monthly_Nov2024_Oct2025 %>%
  left_join(
    clean_pop_2023 %>% select(boro_ID, Pop_1E),
    by = "boro_ID"
  )

# creating a normalized variable compost tonnage per 100,000 residents
monthly_Nov2024_Oct2025 <- monthly_Nov2024_Oct2025 %>%
  mutate(
    rate_per_100k = (total_res_compost / Pop_1E) * 100000
  )

monthly_boro <- monthly_Nov2024_Oct2025 %>%
  group_by(borough, date) %>%
  summarise(
    total_res_compost = sum(total_res_compost, na.rm = TRUE)
  ) %>%
  left_join(
    boro_population %>% 
      select(Borough, boro_pop),
    by = c("borough" = "Borough")
  ) %>%
  mutate(
    rate_per_100k = (total_res_compost / boro_pop) * 100000
  )

# Creating wide format table to put into datawrapper
monthly_boro_wider <- monthly_boro %>%
  select(date, borough, rate_per_100k) %>% 
  pivot_wider(
    names_from = borough,
    values_from = rate_per_100k
    )

monthly_boro_wider %>% write_csv("monthly_boro_wider.csv")





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Bringing in data from Stephanie MacBride's FY24 published analyses~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

FY24_capture_rates <- read_csv("FY24_CD_capture.csv")

FY24_capture_rates %>% 
  summarize(avg_value = mean(perc_capture_rate, na.rm = TRUE))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Bringing in data from Stephanie MacBride's FY25 published analyses~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


FY25_spring_capture_rates <- read_csv("FY25_CD_capture.csv")

# Calculating the change in capture rates from March to April
FY25_spring_capture_rates <- FY25_spring_capture_rates %>% 
  mutate(april_perc_change = ((`2025-04` - `2025-03`) / `2025-03`) * 100) %>% 
  mutate(april_change = (`2025-04` - `2025-03`))

FY25_spring_capture_rates %>% 
  summarize(avg_value = mean(april_change, na.rm = TRUE))

FY25_spring_capture_rates %>% write_csv("FY25_CD_capture_with_change.csv")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Calculating CD Populations~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Average population of the 8 best performing districts in the Queens composting pilot
avg_pop <- clean_pop_2023 %>% 
  filter(boro_ID == 412 | 411 | 407 | 413 | 409 | 408 | 410 | 405) %>% 
  summarize(avg_pop = mean(Pop_1E, na.rm = TRUE))

# Population of the best performing opt-in district
BK06_pop <- clean_pop_2023 %>% 
  filter(boro_ID == 306) %>% 
  summarize(avg_pop = mean(Pop_1E, na.rm = TRUE))

# How much % higher is is the avg of the Queens pop than the opt-in district?
(avg_pop - BK06_pop) / BK06_pop








