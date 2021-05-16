# DINGER MACHINE

# transforming hit data to find out the ultimate question:

#       WOULD IT DONG?

# first, scrape data from scrape_baseball_savant.R

library(GeomMLBStadiums)
library(tidyverse)
suppressWarnings(library(units))
library(baseballr)
library(glue)

last_update <- readRDS(url("https://github.com/danmorse314/dinger-machine/raw/main/data/hit_data.rds")) %>%
  arrange(desc(game_date)) %>%
  slice(1) %>%
  dplyr::pull(game_date)

print(paste("Last updated on",last_update))

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

if(nrow(hit_data) > 0) {
  
  # get highest index from old data
  last_index <- readRDS(url("https://github.com/danmorse314/dinger-machine/raw/main/data/hit_data.rds")) %>%
    dplyr::pull(index) %>%
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
        events == "single" ~ "Single",
        events == "field_error" ~ "Error"
      )
    )
  
  # get outfield dimensions
  fences <- read_csv("https://raw.githubusercontent.com/danmorse314/dinger-machine/main/data/fence_heights_complete.csv",
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
  
  # add headshots
  # get team logos for failed headshots
  mlb_logos <- readRDS(url("https://github.com/danmorse314/dinger-machine/raw/main/data/mlb_logos.rds"))
  
  team_logos <- mlb_logos %>%
    select(team_abbr, logo_html)
  
  # skipping player ids now, will add in with switch to direct
  # pull from mlb api using baseballr soon instead
  do_player_ids <- FALSE
  
  if (do_player_ids == TRUE) {
    # get player ids from baseballr
    chadwick_player_lu_table <- baseballr::get_chadwick_lu()
    
    # fix a few names
    ids <- chadwick_player_lu_table %>%
      mutate(
        name_first = chartr("áéèàôîíúñóÁ", "aeeaoiiunoA", name_first),
        name_last = chartr("áéèàôîíúñóÁ", "aeeaoiiunoA", name_last),
        name_first = case_when(
          name_first == "J. P." ~ "J.P.",
          name_first == "J. D." ~ "J.D.",
          name_first == "J. T." ~ "J.T.",
          name_first == "C. J." ~ "C.J.",
          name_first == "Philip" & name_last == "Gosselin" ~ "Phil",
          name_last == "Vogelbach" ~ "Daniel",
          name_first == "Matthew" & name_last == "Joyce" ~ "Matt",
          TRUE ~ name_first
        )) %>%
      select(name_first, name_last, key_mlbam) %>%
      filter(!is.na(key_mlbam))
    
    # append player headshots to hit data
    hit_data <- hit_data %>%
      # remove accents temporarily
      mutate(gringo = chartr("áéèàôîíúñó", "aeeaoiiuno", player_name)) %>%
      separate(gringo, into = c("name_first","name_last"), sep = " ",
               remove = TRUE, extra = "drop") %>%
      # fix a few discovered problem names
      mutate(name_last = ifelse(name_first == "Tommy" & name_last == "La", "La Stella", name_last)) %>%
      mutate(name_last = ifelse(name_first == "Michael" & name_last == "A.", "Taylor", name_last)) %>%
      left_join(ids, by = c("name_first","name_last")) %>%
      mutate(key_mlbam = ifelse(player_name == "Miguel Rojas", 500743, key_mlbam)) %>%
      select(-name_first, -name_last) %>%
      left_join(team_logos, by = c("player_team" = "team_abbr")) %>%
      mutate(
        # if the player_id is missing, use team logo
        headshot = ifelse(!is.na(key_mlbam),
                          glue("<img src = 'https://img.mlbstatic.com/mlb-photos/image/upload/q_100/v1/people/{key_mlbam}/headshot/67/current' height = '75'></img>"),
                          logo_html)
      ) %>%
      # remove duplicate player_id guys
      group_by(index) %>%
      arrange(-key_mlbam) %>%
      # using the latest player_id, not very scientific but hopefully works for the most part
      slice(1) %>%
      ungroup() %>%
      select(-key_mlbam, -logo_html)
  } else {
    
    hit_data <- hit_data %>%
      left_join(team_logos, by = c("player_team" = "team_abbr"))
    
  }
  
  # saving initial data
  #hit_data %>% saveRDS("data/hit_data.rds")
  #hits_new %>% saveRDS("data/dinger_detail.rds")
  #total_dongs %>% saveRDS("data/dinger_total.rds")
  
  # combine with hits already in database and save
  hit_data %>%
    bind_rows(readRDS(url("https://github.com/danmorse314/dinger-machine/raw/main/data/hit_data.rds"))) %>%
    saveRDS("data/hit_data.rds")
  hits_new %>%
    bind_rows(readRDS(url("https://github.com/danmorse314/dinger-machine/raw/main/data/dinger_detail.rds"))) %>%
    saveRDS("data/dinger_detail.rds")
  total_dongs %>%
    bind_rows(readRDS(url("https://github.com/danmorse314/dinger-machine/raw/main/data/dinger_total.rds"))) %>%
    saveRDS("data/dinger_total.rds")
  
  # commit & push to github
  suppressWarnings(library(git2r, exclude = "pull"))
  repo <- repository(getwd())
  
  add(repo, "data/hit_data.rds")
  add(repo, "data/dinger_detail.rds")
  add(repo, "data/dinger_total.rds")
  
  git2r::pull(repo)
  
  commit(repo, message = paste0("Data updated: ", Sys.time()))
  
  push(repo, credentials = cred_token())
  
}
