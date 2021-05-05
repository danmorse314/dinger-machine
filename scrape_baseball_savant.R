# DINGER MACHINE

# gathering hit data

#devtools::install_github("BillPetti/baseballr")

library(baseballr)
library(tidyverse)

last_update <- readRDS(url("https://github.com/danmorse314/dinger-machine/raw/main/data/hit_data.rds")) %>%
  arrange(desc(game_date)) %>%
  slice(1) %>%
  dplyr::pull(game_date)

hit_data <- scrape_statcast_savant(start_date = last_update + 1) %>%
  janitor::clean_names() %>%
  mutate(player_team = ifelse(inning_topbot == "Top", away_team, home_team)) %>%
  # discard unneccessary columns
  select(game_date, game_type, player_name, player_team, events, des, home_team,
         away_team, bb_type, outs_when_up, inning, inning_topbot,
         #  these here are the ones used in calculations
         plate_z, hc_x, hc_y, hit_distance_sc,
         launch_angle, launch_speed) %>%
  filter(hit_distance_sc >= 300)

hit_data %>% write_csv("daily_hit_data.csv")
