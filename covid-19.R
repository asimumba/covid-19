# -------------------------------------
# Script: Trend Analysis for COVID-19 Confirmed Cases in Zambia and (Selected Countries)
# Author: Aaron Simumba
# Purpose:
# Notes:
#
# Copyright(c) Aaron Simumba
# -------------------------------------

library(covid19.analytics) # import the data
library(tidyverse) # for data wrangling and manipulation
library(lubridate) # date manipulation
library(tidyr)
library(gganimate)
theme_set(theme_minimal())

covid <- covid19.data(case = 'ts-confirmed')

covid_tidy <- covid %>%
  pivot_longer(cols = starts_with("2020"),
               names_to = "date_reported",
               values_to = "confirmed_cases") %>%
  mutate(date_reported = ymd(date_reported), month = ymd(date_reported), label = TRUE) %>%
  janitor::clean_names()

zm_covid <- covid_tidy %>%
  filter(country_region == 'Zambia')

zm_covid %>%
  filter(confirmed_cases > 0) %>%
  ggplot(aes(x = date_reported , y = confirmed_cases)) +
  geom_line(size = 1, color = "#276419") +
  geom_text(aes(x = as.Date('2020-05-21'),
                label = date_reported), hjust = 0) +
  transition_reveal(date_reported) +
  labs(title = 'Date: {frame_along}',
       subtitle = "Trend Analysis for confirmed COVID-19 Cases in Zambia",
       x = "Period",
       y = "Confirmed Cases" )

anim_save("zambia_covid_19")

country_colours <- c(Zambia = "#7F3B08",
                     Zimbabwe = "#A50026",
                     `South Africa` = "#40004B",
                     Botswana = "#276419")

covid_tidy %>%
  filter(confirmed_cases > 0,
         country_region %in% c("Zambia",
                               "Zimbabwe",
                               "South Africa",
                               "Botswana")) %>%
ggplot(aes(x = date_reported, y = confirmed_cases, colour = country_region)) +
  geom_line(show.legend = TRUE, size = 1) +
  scale_colour_manual(values = country_colours) +
  scale_size(range = c(2, 12)) +
  facet_wrap(~ country_region, scales = "free_y") +
  labs(title = 'Date: {frame_along}',
       x = 'Period',
       y = 'Confirmed Cases',
       subtitle = "Trend Analysis for confirmed COVID-19 Cases in Selected Countries") +
  transition_reveal(date_reported)
