# twitter bot

library(tidyverse)
library(baseballr)

# reverse %in%
`%not_in%` <- purrr::negate(`%in%`)

# get outfield dimensions
fences <- readr::read_csv("https://raw.githubusercontent.com/danmorse314/dinger-machine/main/data/fence_heights_complete.csv",
                          col_types = cols())

# acceleration due to gravity in ft/s^2
g <- -32.174

# team abbreviations, hashtags, and stadium details
team_hashtags <- readr::read_csv("dong-bot/data/team_logos_hashtags.csv",
                                 col_types = cols())

# get plays that have already been tweeted
done_plays <- readr::read_csv("dong-bot/data/done_plays.csv",
                              col_types = cols())

# today
dates_to_pull <- Sys.Date()

# get helper functions
source("dong-bot/dong_bot_functions.R")

# get games
games <- fetch_live_games(dates_to_pull)

# get game ids for live games
game_ids <- games %>%
  pull(game_pk)

# pull all live games
pbp <- purrr::map_df(.x = game_ids, ~get_pbp_mlb(game_pk = .x))

# clean it up
hit_data <- clean_hits(pbp, games)

# getting dong details for all 30 stadiums
hit_detail <- calculate_dingers(hit_data)

# counting up the dinger parks
total_dongs <- get_dong_total(hit_detail)

# 
hit_data <- hit_data %>%
  dplyr::left_join(
    total_dongs %>%
      dplyr::select(play_id, total_dongs),
    by = "play_id"
  ) %>%
  # only tweet if it would've donged in at least one park
  dplyr::filter(total_dongs > 0) %>%
  # make sure we haven't tweeted the play already
  dplyr::filter(play_id %not_in% done_plays$play_id)

# this part is a test
# trying out just 3 tweets at once
#hit_data <- slice(hit_data, 1:3)

if(nrow(hit_data > 0)) {
  for(i in 1:nrow(hit_data)){
    hit <- hit_data %>% slice(i)
    tweet <- write_tweet(hit)
    draw_hit_plot(hit)
    rtweet::post_tweet(tweet, media = "dong-bot/hit_chart.png")
    tweeted_play <- select(hit, play_id)
    done_plays <- bind_rows(done_plays, tweeted_play)
    rm(tweet, hit, tweeted_play)
    # wait 30 seconds before tweeting again
    Sys.sleep(30)
  }
}

# update the finished plays
done_plays %>% write_csv("dong-bot/data/done_plays.csv")
