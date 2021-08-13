# twitter bot

library(tidyverse)
library(baseballr)

# get helper functions
source("dong_bot_functions.R")

# reverse %in%
`%not_in%` <- purrr::negate(`%in%`)

# get outfield dimensions
fences <- readRDS("data/fences.rds")

# acceleration due to gravity in ft/s^2
g <- -32.174

# team abbreviations, hashtags, and stadium details
team_hashtags <- readr::read_csv("data/team_logos_hashtags.csv",
                                 col_types = cols())

# get plays that have already been tweeted
done_plays <- readr::read_csv("data/done_plays.csv",
                              col_types = cols())
# today
dates_to_pull <- Sys.Date()

# get games
games <- fetch_live_games(dates_to_pull)

# get game ids for live games
game_ids <- games %>%
  pull(game_pk)

# get twitter authorization token saved in environment
twitter_token <- rtweet::get_token()

if(length(game_ids) > 0) {
  
  # pull all live games
  pbp <- purrr::map_df(.x = game_ids, ~get_pbp_mlb(game_pk = .x))
  
  # clean it up
  hit_data <- clean_hits(pbp, games)
  
  # getting dong details for all 30 stadiums
  hit_detail <- calculate_dingers(hit_data)
  
  # counting up the dinger parks
  total_dongs <- get_dong_total(hit_detail)
  
  hit_data <- hit_data %>%
    dplyr::left_join(
      total_dongs %>%
        dplyr::select(play_id, total_dongs),
      by = "play_id"
    ) %>%
    # only tweet if it would've donged in at least one park
    dplyr::filter(total_dongs > 0) %>%
    # make sure we haven't tweeted the play already
    dplyr::filter(play_id %not_in% done_plays$play_id) %>%
    # post oldest hits first
    # shouldn't be necessary if it automates often enough
    dplyr::mutate(post_order = row_number()) %>%
    dplyr::arrange(-post_order)

  if(nrow(hit_data) > 0) {
    for(i in 1:nrow(hit_data)){
      hit <- hit_data %>% dplyr::slice(i)
      tweet <- write_tweet(hit)
      draw_hit_plot(hit)
      rtweet::post_tweet(tweet, media = "hit_chart.png",
                         token = twitter_token)
      Sys.sleep(10)
      tweet_url <- rtweet::get_my_timeline(n=1, token = twitter_token) %>% dplyr::pull(status_url)
      tweeted_play <- dplyr::select(hit, play_id) %>%
        dplyr::mutate(url = tweet_url)
      done_plays <- dplyr::bind_rows(done_plays, tweeted_play)
      rm(tweet, hit, tweeted_play, tweet_url)
      # wait 30 seconds before tweeting again if there's multiple tweets
      if(nrow(hit_data) - i > 0){
        Sys.sleep(30)
      }
    }
    # update the finished plays
    done_plays %>% write_csv("data/done_plays.csv")
    }
  
}

