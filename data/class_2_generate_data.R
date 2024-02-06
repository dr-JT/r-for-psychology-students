# Code created by ChatGPT 4.0
# Code modified by Jason Tsukahara

library(tidyverse)
library(ggplot2)
library(here)

# Set seed for reproducibility
set.seed(1988)

# Function to generate mood scores with interaction effect for individual participants
generate_mood_scores <- function(group, week) {
  base_mean <- if (group == "Sleep Hygiene") 65 else 66
  week_2_effect <- if (week == 2) if (group == "Sleep Hygiene") 0 else 1 else 0
  week_3_effect <- if (week == 3) if (group == "Sleep Hygiene") 0 else 5 else 0
  week_4_effect <- if (week == 4) if (group == "Sleep Hygiene") 0 else 7 else 0
  rnorm(1, mean = base_mean + week_2_effect + week_3_effect + week_4_effect, sd = 10)
}

# Generate participant responses
participant_responses <- tibble(
  participant_id = 1:100,
  intervention_type = rep(c("Sleep Hygiene", "Relaxation Techniques"), each = 50),
  cognitive_week_1 = rnorm(100, mean = 75, sd = 8),
  cognitive_week_2 = rnorm(100, mean = 76, sd = 8),
  cognitive_week_3 = rnorm(100, mean = 77, sd = 8),
  cognitive_week_4 = rnorm(100, mean = 75, sd = 8)
)

# Apply generate_mood_scores individually for each row and week
participant_responses <- participant_responses |>
  rowwise() |>
  mutate(
    mood_week_1 = generate_mood_scores(intervention_type, 1),
    mood_week_2 = generate_mood_scores(intervention_type, 2),
    mood_week_3 = generate_mood_scores(intervention_type, 3),
    mood_week_4 = generate_mood_scores(intervention_type, 4)
  ) |>
  ungroup()

data_cognitive <- participant_responses |>
  select(participant_id, intervention_type,
         cognitive_week_1, cognitive_week_2, 
         cognitive_week_3, cognitive_week_4)

data_mood <- participant_responses |>
  select(participant_id, intervention_type,
         mood_week_1, mood_week_2, mood_week_3, mood_week_4)

# Save to CSV files
write_csv(data_cognitive, here("data/class_2_cog_data.csv"))
write_delim(data_mood, here("data/class_2_mood_data.txt"), delim = "\t")

rm(list = ls())
