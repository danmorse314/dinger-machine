# bluesky bot
app <- "bsky"

library(bskyr)

`%>%` <- magrittr::`%>%`

# get helper functions
source("dong-bot/dong_bot_functions.R")

# reverse %in%
`%not_in%` <- purrr::negate(`%in%`)

# get outfield dimensions
fences <- readRDS("data/fences.rds") %>%
  dplyr::filter(stadium != "Sahlen Field")

# acceleration due to gravity in ft/s^2
g <- -32.174

# team abbreviations, hashtags, and stadium details
team_hashtags <- readr::read_csv("dong-bot/data/team_logos_hashtags.csv",
                                 col_types = readr::cols()) #%>%
  # use Sahlen Field for Blue Jays at the moment
  #mutate(stadium = ifelse(stadium == "Rogers Centre", "Sahlen Field", stadium))

# get plays that have already been tweeted
done_plays <- readRDS("data/done_plays.rds") %>%
#done_plays <- readRDS(url("https://github.com/danmorse314/dinger-machine/raw/main/dong-bot/data/done_plays.rds")) %>%
  dplyr::mutate(game_date = as.character(game_date))

# load manually updated stadium paths
#   -fixed astros/marlins center fields
stadium_paths <- readRDS("data/stadium_paths.rds") %>%
  dplyr::filter(stadium != "Sahlen Field")

# today
dates_to_pull <- Sys.Date()
#dates_to_pull <- "2025-03-18"

# get games
games <- fetch_live_games(dates_to_pull)

# get game ids for live games
game_ids <- games %>%
  dplyr::filter(game_type != "S") %>%
  dplyr::pull(game_pk)

if(length(game_ids) > 0) {

  # pull all live games
  pbp <- purrr::map_df(.x = game_ids, ~baseballr::get_pbp_mlb(game_pk = .x))
  
  # clean it up
  hit_data_cleaned <- clean_hits(pbp, games) %>%
    # make sure we haven't tweeted the play already
    dplyr::filter(play_id %not_in% done_plays$play_id)
  
  if(nrow(hit_data_cleaned) > 0){
    
    # getting dong details for all 30 stadiums
    hit_detail <- calculate_dingers(hit_data_cleaned)
    
    # counting up the dinger parks
    total_dongs <- get_dong_total(hit_detail)
    
    hit_data <- hit_data_cleaned %>%
      dplyr::left_join(
        total_dongs %>%
          dplyr::select(play_id, total_dongs, road_dong, road_stadium),
        by = "play_id"
      ) %>%
      # only tweet if it would've donged in at least one park
      dplyr::filter(total_dongs > 0) %>%
      #dplyr::arrange(inning) %>%
      # make sure we can do the stadium plot or it'll error out
      dplyr::filter(stadium_observed %in% unique(stadium_paths$stadium))
    
  }
  
  if(nrow(hit_data) < 1) {
    message("no new hits found")
  } else {
    
    if(app == "bsky"){
      
      set_bluesky_user("woulditdong.bsky.social")
      set_bluesky_pass(Sys.getenv("bskypw"))
      
      bs_auth(
        user = get_bluesky_user(),
        pass = get_bluesky_pass()
      )
    }
    
    for(i in 1:nrow(hit_data)){
      #i <- 2
      
      message(paste("posting", i, "of", nrow(hit_data), "hits..."))
      
      hit <- hit_data %>% dplyr::slice(i)
      tweet <- write_tweet(hit)
      alt_text <- draw_hit_plot(hit)
      
      if(app == "twitter"){
        rtweet::post_tweet(
          tweet,
          media = "hit_chart.png",
          media_alt_text = alt_text,
          token = rtweet::rtweet_bot(
            api_key       = Sys.getenv("DONG_TWITTER_API_KEY"),
            api_secret    = Sys.getenv("DONG_TWITTER_API_SECRET"),
            access_token  = Sys.getenv("DONG_TWITTER_ACCESS_TOKEN"),
            access_secret = Sys.getenv("DONG_TWITTER_ACCESS_SECRET")
          )
        )
        Sys.sleep(3)
        tweet_url <- rtweet::get_timeline(
          user = "would_it_dong",
          n=1,
          token = rtweet::rtweet_bot(
            api_key       = Sys.getenv("DONG_TWITTER_API_KEY"),
            api_secret    = Sys.getenv("DONG_TWITTER_API_SECRET"),
            access_token  = Sys.getenv("DONG_TWITTER_ACCESS_TOKEN"),
            access_secret = Sys.getenv("DONG_TWITTER_ACCESS_SECRET")
          )
        )
        tweet_url <- stringr::str_remove(tweet_url$entities[[1]]$media$expanded_url, "photo/1")
      } else if(app == "bsky"){

        bs_post_custom(
          text = tweet, 
          images = "hit_chart.png", 
          images_alt = alt_text
        )
        
        Sys.sleep(3)
        
        post_df <- bs_get_author_feed(
          "woulditdong.bsky.social",
          cursor = NULL,
          limit = 1
        )
        
        tweet_url <- paste0(
          "https://bsky.app/profile/woulditdong.bsky.social/post/",
          gsub(".*post\\/","",post_df$uri)
        )
        
      } else {stop("argument app must be either 'twitter' or 'bsky'")}
      
      tweeted_play <- dplyr::select(
        hit, player_name, player_team, game_date, inning, home_team, away_team, events,
        launch_speed, launch_angle, hit_distance_sc, plate_z, hc_x_, hc_y_,
        spray_angle, hit_direction, stadium_observed, total_dongs,
        launch_speed_x, launch_speed_y, play_id
        ) %>%
        dplyr::mutate(url = tweet_url)
      done_plays <- dplyr::bind_rows(done_plays, tweeted_play)
      # update the finished plays
      done_plays %>% saveRDS("data/done_plays.rds")
      done_plays %>% readr::write_csv("data/done_plays.csv")
      rm(tweet, hit, tweeted_play)
      # wait 30 seconds before tweeting again if there's multiple tweets
      if(nrow(hit_data) - i > 0){
        Sys.sleep(30)
      } else {message("Done!")}
    }
    
    # push data to github for app every hour
    if(lubridate::minute(Sys.time()) < 5){
      repo <- git2r::repository(getwd())
      #git2r::add(repo = repo, path = "data/done_plays.rds")
      #git2r::add(repo = repo, path = "data/done_plays.csv")
      git2r::commit(repo = repo, all = TRUE, message = glue::glue("data updated - {Sys.time()}"))
      git2r::push(repo, credentials = git2r::cred_token())
    }
  }
}
