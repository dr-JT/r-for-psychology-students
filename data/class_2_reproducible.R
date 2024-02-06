# Class 2 Reproducible Script

# load packages
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)

# import data
cog_import <- read_csv(here("data/class_2_cog_data.csv"))
mood_import <- read_delim(here("data/class_2_mood_data.txt"), 
                          delim = "\t", escape_double = FALSE, trim_ws = TRUE)

# restructure data
cog_data <- cog_import |>
  pivot_longer(cols = starts_with("cognitive_week"), 
               names_to = "week", 
               values_to = "cognitive_score") |>
  mutate(week = parse_number(week))

mood_data <- mood_import |>
  pivot_longer(cols = starts_with("mood_week"), 
               names_to = "week", 
               values_to = "mood_score") |>
  mutate(week = parse_number(week))

# merge data
data <- full_join(cog_data, mood_data, 
                  by = c("participant_id", "intervention_type", "week"))

# plot data
ggplot(data, aes(x = week, y = cognitive_score, 
                 color = intervention_type, group = intervention_type)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = .25) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun = mean, geom = "point") +
  coord_cartesian(ylim = c(65, 85)) +
  theme_classic()

ggsave(here("lectures/images/class_2_cognitive_plot.png"), 
       width = 6, height = 4, dpi = 300)

ggplot(data, aes(x = week, y = mood_score, 
                 color = intervention_type, group = intervention_type)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = .25) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun = mean, geom = "point") +
  coord_cartesian(ylim = c(60, 80)) +
  theme_classic()

ggsave(here("lectures/images/class_2_mood_plot.png"), 
       width = 6, height = 4, dpi = 300)
