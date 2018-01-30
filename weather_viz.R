
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
  filter(!(month == 2 & day == 29)) %>%
  mutate(
    date = ymd(paste(year, month, day)),
    new_day = yday(date)
    ) %>%
  as_tibble()


# Create Required Data Sets -----------------------------------------------

# create dataframe that represents 1995-2013 historical data

past <- df_raw %>%
  group_by(year) %>%
  filter(year < 2017) %>%
  ungroup() %>%
  group_by(new_day) %>%
  mutate(
    upper     = max(temp, na.rm = TRUE), # identify max value for each day
    lower     = min(temp, na.rm = TRUE), # identify min value for each day
    avg       = mean(temp, na.rm = TRUE),  # calculate mean value for each day
    se        = sd(temp, na.rm = TRUE) / sqrt(length(temp)), # calculate standard error of mean
    avg_upper = avg + (qt(.975, df = n()) * se),  # calculate 95% CI for mean
    avg_lower = avg - (qt(.975, df = n()) * se)   # calculate 95% CI for mean
    ) %>% 
  ungroup()

# create dataframe that represents current winter data
present <- df_raw %>%
  filter(year == 2017) %>%
  group_by(year) %>%  # create matching x-axis as historical data
  ungroup()

# create dataframe that represents the lowest temp for each day for the historical data
past_lows <- past %>%
  group_by(new_day) %>%
  summarise(past_low = min(temp, na.rm = TRUE)) # identify lowest temp for each day from 1995-2013

# create dataframe that identifies the days in 2014 in which the temps were lower than all previous 19 years
present_lows <- present %>%
  left_join(past_lows) %>%  # merge historical lows to current year low data
  mutate(record = ifelse(temp < past_low, "Y", "N")) %>% # identifies if current year was record low
  filter(record == "Y")  # filter for days that represent current year record lows

# create dataframe that represents the highest temp for each day for the historical data
past_highs <- past %>%
  group_by(new_day) %>%
  summarise(past_high = max(temp, na.rm = TRUE))  # identify highest temp for each day from 1995-2013

# create dataframe that identifies the days in 2014 in which the temps were higher than all previous 19 years
present_highs <- present %>%
  left_join(past_highs) %>%  # merge historical highs to current year low data
  mutate(record = ifelse(temp > past_high, "Y", "N")) %>% # identifies if current year was record high
  filter(record == "Y")  # filter for days that represent current year record highs


# Required Formatting -----------------------------------------------------


# function to turn y-axis labels into degree formatted values
dgr_fmt <- function(x, ...) {
  parse(text = paste(x, "*degree", sep = ""))
}

# create y-axis variable
a <- dgr_fmt(seq(-20, 100, by = 10))

# create a small dataframe to represent legend symbol for 2017 Temperature
legend_data <- data.frame(x = seq(175, 182), y = rnorm(8, 15, 2))

# create dates for last day of each month
last_days <- present %>%
  group_by(month) %>%
  slice(last(day)) %>%
  .$new_day

# Create graphic ----------------------------------------------------------

ggplot(past, aes(new_day, temp)) +
  theme(plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        #axis.text = element_blank(),  
        axis.title = element_blank()) +
  geom_linerange(past, mapping = aes(x = new_day, ymin = lower, ymax = upper), colour = "wheat2", alpha=.1) +
  geom_linerange(past, mapping = aes(x = new_day, ymin = avg_lower, ymax = avg_upper), colour = "wheat4") +
  geom_line(present, mapping = aes(x = new_day, y = temp, group = 1)) +
  geom_vline(xintercept = 0, colour = "wheat4", linetype=1, size=1) +
  geom_hline(yintercept = seq(-20, 100, by = 10), colour = "white", linetype = 1) +
  geom_vline(xintercept = last_days, colour = "wheat4", linetype = 3, size = .5) +
  coord_cartesian(ylim = c(-20, 100)) +
  scale_y_continuous(breaks = seq(-20, 100, by = 10), labels = a) +
  scale_x_continuous(expand = c(0, 0), 
                     breaks = c(15, 45, 75, 105, 135, 165, 195, 228, 258, 288, 320, 350),
                     labels = month.name) +
  geom_point(data = present_lows, aes(x = new_day, y = temp), colour = "blue3") +
  geom_point(data = present_highs, aes(x = new_day, y = temp), colour = "firebrick3") +
  annotate("segment", x = 78, xend = 84, y = 20, yend = 17, colour = "blue3") +
  annotate("text", x = 86, y = 15, label = "We had 23 days that were \nthe coldest since 1995", size = 3, colour = "blue3", hjust = 0) +
  annotate("segment", x = 268, xend = 274, y = 80, yend = 84, colour = "firebrick3") +
  annotate("text", x = 276, y = 84, label = "We had 18 days that were \nthe warmest since 1995", size = 3, colour = "firebrick3", hjust = 0) +
  ggtitle("Dayton's Weather in 2017",
          subtitle = "Data represents average daily temperatures for the time period of January 1, 1995 through December 31, 2017. \nAverage temperature for the year was 59° making it the 4th warmest year since 1995.") +
  labs(caption = "\nCode: https://github.com/bradleyboehmke/Dayton-Weather-2018 \nData: http://academic.udayton.edu/kissock/http/Weather/default.htm") +
  theme(
    plot.title = element_text(face = "bold", colour = "#3C3C3C", size = 20),
    plot.subtitle = element_text(size = 9, color = "#3F3F3F"),
    plot.caption = element_text(size = 6, color = "#3F3F3F", hjust = 0)
    ) +
  annotate("segment", x = 181, xend = 181, y = 5, yend = 25, colour = "wheat2", size=3) +
  annotate("segment", x = 181, xend = 181, y = 12, yend = 18, colour = "wheat4", size=3) +
  geom_line(data=legend_data, aes(x=x,y=y)) +
  annotate("segment", x = 183, xend = 185, y = 17.7, yend = 17.7, colour = "wheat4", size=.5) +
  annotate("segment", x = 183, xend = 185, y = 12.2, yend = 12.2, colour = "wheat4", size=.5) +
  annotate("segment", x = 185, xend = 185, y = 12.2, yend = 17.7, colour = "wheat4", size=.5) +
  annotate("text", x = 196, y = 14.75, label = "NORMAL RANGE", size=2, colour="gray30") +
  annotate("text", x = 162, y = 14.75, label = "2017 TEMPERATURE", size=2, colour="gray30") +
  annotate("text", x = 193, y = 25, label = "RECORD HIGH", size=2, colour="gray30") +
  annotate("text", x = 193, y = 5, label = "RECORD LOW", size=2, colour="gray30")
