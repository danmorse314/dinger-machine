# DINGER MACHINE

# transforming hit data to find out the ultimate question:

#       WOULD IT DONG?

# first, scrape data from scrape_baseball_savant.R

library(GeomMLBStadiums)
library(units)

setwd("~/R/Dinger Machine")

# get highest index from old data
last_index <- read_csv("hit_data.csv", col_types = cols()) %>%
  pull(index) %>%
  max()

# transform batted balls data in same fashion so we're working in feet all around
hit_data <- hit_data %>%
  separate(player_name, into = c("last","first"), sep = ", ", remove = TRUE) %>%
  unite("player_name", c(first,last), sep = " ", remove = TRUE) %>%
  select(player_name, everything()) %>%
  filter(!is.na(hc_x) & !is.na(hc_y)) %>%
  mlbam_xy_transformation() %>%
  mutate(
    # add launch angle in radians column
    launch_angle = set_units(launch_angle, "degrees"),
    launch_angle_rads = as.double(set_units(launch_angle, "radians")),
    launch_angle = as.double(launch_angle),
    # launch speed in ft/s
    launch_speed_fts = launch_speed * 5280 / 3600,
    # add exit velo x & exit velo y
    launch_speed_x = launch_speed_fts * cos(launch_angle_rads),
    launch_speed_y = launch_speed_fts * sin(launch_angle_rads),
    spray_angle = round(
      (atan(
        (hc_x_)/(hc_y_)
      )*180/pi*.75)
      ,1)
  ) %>%
  mutate(index = row_number()) %>%
  # update the index starting with the last hit in db +1
  mutate(index = index + last_index) %>%
  mutate(
    hit_direction = case_when(
      spray_angle <= -21 ~ "Left",
      spray_angle > -21 & spray_angle <= -7 ~ "Left-Center",
      spray_angle > -7 & spray_angle < 7 ~ "Center",
      spray_angle >= 7 & spray_angle < 21 ~ "Right-Center",
      spray_angle >= 21 ~ "Right"
    )
  ) %>%
  mutate(
    events = case_when(
      events == "field_out" ~ "In play, out(s)",
      events == "home_run" ~ "Home Run",
      events == "double" ~ "Double",
      events == "double_play" ~ "In play, out(s)",
      events == "triple" ~ "Triple",
      events == "sac_fly" ~ "Sac fly",
      events == "sing;e" ~ "Single",
      events == "field_error" ~ "Error"
    )
  )

# get outfield dimensions
fences <- read_csv("fence_heights_complete.csv",
                   col_types = cols())

# acceleration due to gravity in ft/s^2
g <- -32.174

# adding fence height at batted ball location for each stadium, each hit
hits_new <- NULL

# calculate dingerness in each ballpark
for(j in c(unique(fences$team))){
  print(paste("CALCULATING: would it dong at the",j,"ballpark?"))
  tictoc::tic()
  fences_i <- filter(fences, team == j)
  for(i in 1:nrow(hit_data)){
    nearest_fence <- tibble(angle_diff = abs(hit_data$spray_angle[i] - fences_i$spray_angle_stadia),
                            index = hit_data$index[i]) %>%
      bind_cols(fences_i) %>%
      arrange(angle_diff) %>%
      slice(1) %>%
      select(stadium, x, y, d_wall, index, fence_height, team, team_abbr)
    hits_new <- bind_rows(hits_new, nearest_fence)
    rm(nearest_fence)
  }
  rm(fences_i)
  tictoc::toc()
}

# calculate whether or not it would've been a dinger
hits_new <- hit_data %>%
  left_join(hits_new, by = "index") %>%
  mutate(
    #launch_speed_x = launch_speed_fts * cos(launch_angle_rads),
    #launch_speed_y = launch_speed_fts * sin(launch_angle_rads),
    total_time = -(launch_speed_y + sqrt(launch_speed_y^2 + (2*g * plate_z))) / g,
    acceleration_x = (-2*launch_speed_x / total_time) + (2*hit_distance_sc/total_time^2),
    time_wall = (-launch_speed_x + sqrt(launch_speed_x^2 + 2*acceleration_x*d_wall))/acceleration_x,
    height_at_wall = (launch_speed_y * time_wall) + (.5*g*(time_wall^2)),
    height_at_wall = ifelse(is.na(height_at_wall), 0, height_at_wall),
    would_dong = ifelse(height_at_wall > fence_height, 1, 0),
    would_dong = ifelse(team_abbr == home_team & events == "Home Run", 1, would_dong),
    would_dong = ifelse(team_abbr == home_team & events != "Home Run", 0, would_dong)
  )

# get total stadiums each hit would've donged in
total_dongs <- hits_new  %>%
  group_by(index, player_name, player_team, game_date, events, launch_speed, launch_angle,
           hit_distance_sc, hit_direction) %>%
  summarize(
    total_dongs = sum(would_dong),
    .groups = "drop"
  ) %>%
  arrange(-total_dongs)

# add stadium in which hit was observed in
stadium_details <- hits_new %>% select(stadium, team_abbr) %>% distinct() %>%
  mutate(stadium_observed = stadium) %>%
  select(stadium_observed, team_abbr)

hit_data <- hit_data %>%
  left_join(stadium_details, by = c("home_team" = "team_abbr"))

# saving initial data
#hit_data %>% write_csv("hit_data.csv")
#hits_new %>% write_csv("dinger_detail.csv")
#total_dongs %>% write_csv("dinger_total.csv")

# combine with hits already in database and resave
hit_data %>%
  bind_rows(read_csv("hit_data.csv", col_types = cols())) %>%
  write_csv("hit_data.csv")
hits_new %>%
  bind_rows(read_csv("dinger_detail.csv", col_types = cols())) %>%
  write_csv("dinger_detail.csv")
total_dongs %>%
  bind_rows(read_csv("dinger_total.csv", col_types = cols())) %>%
  write_csv("dinger_total.csv")
