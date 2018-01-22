
# Prerequisites -----------------------------------------------------------

library(tidyverse)
library(lubridate)


# Acquire & Clean Data ----------------------------------------------------

url <- "http://academic.udayton.edu/kissock/http/Weather/gsod95-current/OHDAYTON.txt"

df_raw <- read.table(
  url, 
  na.strings = -99,
  col.names = c("month", "day", "year", "temp")
  ) %>%
  as_tibble()


# Create Required Data Sets -----------------------------------------------

# create dataframe that represents 1995-2013 historical data

past <- df_raw %>%
  group_by(year) %>%
  mutate(new_day = seq(1, length(day))) %>%
  filter(year < 2017) %>%
  ungroup() %>%
  group_by(new_day) %>%
  mutate(
    upper     = max(temp, na.rm = TRUE), # identify max value for each day
    lower     = min(temp, na.rm = TRUE), # identify min value for each day
    avg       = mean(temp, na.rm = TRUE),  # calculate mean value for each day
    se        = sd(temp, na.rm = TRUE) / sqrt(length(temp)), # calculate standard error of mean
    avg_upper = avg + (2.101 * se),  # calculate 95% CI for mean
    avg_lower = avg - (2.101 * se)   # calculate 95% CI for mean
    ) %>% 
  ungroup()

# create dataframe that represents current year data
present <- df_raw %>%
  filter(year >= 2017) %>%
  group_by(year) %>%
  mutate(new_day = seq(1, length(day))) %>%  # create matching x-axis as historical data
  ungroup()
