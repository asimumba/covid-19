# -------------------------------------
# Script: Trend Analysis for COVID-19 Confirmed Cases in Zambia and (Selected Countries)
# Author: Aaron Simumba
# Purpose:
# Notes:
#
# Copyright(c) Aaron Simumba
# -------------------------------------

library(covid19.analytics)
library(tidyverse)
library(lubridate)
library(gganimate)
theme_set(theme_minimal())

covid <- covid19.data(case = 'ts-confirmed')

covid_tidy <- covid %>%
  pivot_longer(cols = starts_with("2020"),
               names_to = "date_reported",
               values_to = "confirmed_cases") %>%
  mutate(date_reported = ymd(date_reported),
         month = month(date_reported,
                       label = TRUE)) %>%
  janitor::clean_names()

zm_covid <- covid_tidy %>%
  filter(country_region == 'Zambia')

zed_cases <- zm_covid %>%
  filter(confirmed_cases != 0) %>%
  ggplot(aes(x = date_reported , y = confirmed_cases, group = month)) +
  geom_line(size = 1, color = "#40004B") +
  geom_text(aes(x = as.Date('2020-05-19'),
                label = as.factor(confirmed_cases)),
            hjust = 0) +
  transition_reveal(date_reported) +
  labs(title = 'Date: {frame_along}',
       subtitle = "Trend Analysis for confirmed COVID-19 Cases in Zambia",
       x = "Period",
       y = "Confirmed Cases" )

zed_cases %>%
  animate() %>%
anim_save(filename = "zambia_covid_19.gif")

animate(zed_cases,
        renderer = av_renderer("zed_cases.mp4"),
        width = 1920, height = 1080,
        res = 250, fps = 25)

# All other countries
country_colours <- c(Zambia = "#7F3B08",
                     Zimbabwe = "#A50026",
                     `South Africa` = "#40004B",
                     Botswana = "#276419")

final_all <- covid_tidy %>%
  filter(confirmed_cases > 0,
         country_region %in% c("Zambia",
                               "Zimbabwe",
                               "South Africa",
                               "Botswana")) %>%
ggplot(aes(x = date_reported,
           y = confirmed_cases,
           colour = country_region,
           group = month)) +
  geom_line(show.legend = TRUE, size = 1) +
  scale_colour_manual(values = country_colours) +
  scale_size(range = c(2, 12)) +
  facet_wrap(~ country_region, scales = "free_y") +
  geom_text(aes(x = as.Date('2020-05-19'),
                label = as.factor(confirmed_cases)),
            hjust = 0, show.legend = FALSE) +
  labs(title = 'Date: {frame_along}',
       x = 'Period',
       y = 'Confirmed Cases',
       subtitle = "Trend Analysis for confirmed COVID-19 Cases in Selected Countries",
       color = "Country") +
  transition_reveal(date_reported)

final_all %>%
  animate() %>%
  anim_save(filename = "final_all.gif")

animate(zed_cases,
        renderer = av_renderer("final_all.mp4"),
        width = 1920, height = 1080,
        res = 250, fps = 25)
