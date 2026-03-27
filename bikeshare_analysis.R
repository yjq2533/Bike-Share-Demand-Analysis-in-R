# Bike Share Data Analysis
# Author: Jiaqi Yan
# Description: Analyze seasonal and winter bike usage patterns using R

# Load libraries
library(tidyverse)
library(knitr)
library(lubridate)

# 1. Number of hours required to reach 50,000 bikers by season using a loop
# Get all seasons
seasons <- unique(bikeshare$season)

# Create a named vector to store the required hours
hours <- rep(0, 4)
names(hours) <- seasons

# Loop through each season
for (s in seasons) {
  filtered_bikeshare <- bikeshare %>% filter(season == s)

  hour <- 0    # accumulated number of hours
  count <- 0   # accumulated number of bikers
  i <- 1       # row index

  while (i <= nrow(filtered_bikeshare)) {
    count <- count + filtered_bikeshare$bikers[i]
    hour <- hour + 1
    if (count >= 50000) {
      break
    }
    i <- i + 1
  }
  hours[s] <- hour
}

hours

# Same task using vectorization
bikeshare %>%
  group_by(season) %>%
  summarise(hours = min(which(cumsum(bikers) >= 50000)))

# Focus on winter data only

bike_winter <- bikeshare %>% filter(season == "Winter")
head(bike_winter)

# Aggregate hourly winter data into daily total bikers
bike_winter_daily <- bike_winter %>%
  mutate(date = as.Date(date_time)) %>%
  group_by(date) %>%
  summarise(total_bikers = sum(bikers)) %>%
  arrange(date)

bike_winter_daily

# Identify days with fewer than 1000 total bikers
day_less_than_1000 <- which(bike_winter_daily$total_bikers < 1000)
day_less_than_1000

# Compare average weather conditions between:
#    - days with < 1000 bikers
#    - days with >= 1000 bikers

# Dates corresponding to days with fewer than 1000 bikers
dates_less_than_1000 <- bike_winter_daily$date[day_less_than_1000]

# Dates corresponding to days with 1000 or more bikers
dates_more_than_1000 <- bike_winter_daily$date[-day_less_than_1000]

# Mean temp, humidity, and windspeed for days with < 1000 bikers
mean_less_than_1000 <- c(
  mean(bike_winter$temp[as.Date(bike_winter$date_time) %in% dates_less_than_1000]),
  mean(bike_winter$hum[as.Date(bike_winter$date_time) %in% dates_less_than_1000]),
  mean(bike_winter$windspeed[as.Date(bike_winter$date_time) %in% dates_less_than_1000])
)

# Mean temp, humidity, and windspeed for days with >= 1000 bikers
mean_more_than_1000 <- c(
  mean(bike_winter$temp[as.Date(bike_winter$date_time) %in% dates_more_than_1000]),
  mean(bike_winter$hum[as.Date(bike_winter$date_time) %in% dates_more_than_1000]),
  mean(bike_winter$windspeed[as.Date(bike_winter$date_time) %in% dates_more_than_1000])
)

# Combine into a comparison table
mean_comparison <- data.frame(
  "days<1000" = mean_less_than_1000,
  "days>=1000" = mean_more_than_1000,
  check.names = FALSE,
  row.names = c("temp", "hum", "windspeed")
)

mean_comparison
