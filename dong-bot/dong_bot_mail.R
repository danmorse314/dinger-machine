# twitter bot

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

# get games
games <- fetch_live_games(dates_to_pull) |>
  dplyr::filter(status_detailed_state == "Final") |>
  dplyr::filter(game_pk %not_in% done_games$game_pk)

# get game ids for live games
game_ids <- games %>%
  dplyr::pull(game_pk)

if(length(game_ids) > 0) {
  
  # pull all live games
  pbp <- purrr::map_df(.x = game_ids, ~baseballr::get_pbp_mlb(game_pk = .x))
  
  # clean it up
  hit_data <- clean_hits(pbp, games)
  
  if(nrow(hit_data) > 0){
    
    # getting dong details for all 30 stadiums
    hit_detail <- calculate_dingers(hit_data)
    
    # counting up the dinger parks
    total_dongs <- get_dong_total(hit_detail)
    
    hit_data <- hit_data %>%
      dplyr::left_join(
        total_dongs %>%
          dplyr::select(play_id, total_dongs, road_dong, road_stadium),
        by = "play_id"
      ) %>%
      # only tweet if it would've donged in at least one park
      dplyr::filter(total_dongs > 0) %>%
      dplyr::arrange(inning) %>%
      # make sure we can do the stadium plot or it'll error out
      dplyr::filter(stadium_observed %in% unique(stadium_paths$stadium))
    
  }
  
  if(nrow(hit_data) > 0) {
    
    for(id in unique(games$game_pk)){
      
      game_hits <- dplyr::filter(hit_data, game_pk == id) |>
        dplyr::left_join(
          dplyr::select(games, game_pk, home_final = home_score, away_final = away_score),
          by = "game_pk"
        ) |>
        dplyr::left_join(
          dplyr::select(team_hashtags, full_team_name, team_abbr, home_hashtag = team_hashtag) %>%
            dplyr::distinct(), #  because TOR is on there twice
          by = c("home_team" = "full_team_name")
        ) %>%
        dplyr::rename(home_abbr = team_abbr) %>%
        dplyr::left_join(
          dplyr::select(team_hashtags, full_team_name, team_abbr, away_hashtag = team_hashtag) %>%
            dplyr::distinct(), #  because TOR is on there twice
          by = c("away_team" = "full_team_name")
        ) %>%
        dplyr::rename(away_abbr = team_abbr)
      
      home <- game_hits[1,]$home_abbr
      away <- game_hits[1,]$away_abbr
      
      text <- write_text(game_hits)
      
      for(i in 1:nrow(game_hits)){
        hit <- game_hits[i,]
        draw_hit_plot(
          hit, email = TRUE,
          filepath = glue::glue("dong-bot/game-reports/{away}_{home}/hitchart{i}.png")
        )
      }
      
      attachments <- "dong-bot/game-reports/TEX_KC/hitchart1.png"
      
      # send email report
      mailR::send.mail(
        from = "danmorse8642@gmail.com",
        to = "danmorse8642@gmail.com",
        subject = paste("Dong report",home,"vs",away),
        body = text,
        attach.files = attachments,
        smtp = list(
          host.name = "smtp.gmail.com",
          port = 465,
          user.name = "danmorse8642@gmail.com",
          passwd = Sys.getenv("GIT_PW"),
          ssl = T
          ),
        authenticate = TRUE,
        send = TRUE
      )
      
      sendmailR::sendmail("danmorse8642@gmail.com", "danmorse8642@gmail.com", "test", "body", 
               control=list(smtpServer="ASPMX.L.GOOGLE.COM")) 
      
      email <- emayili::envelope() %>%
        emayili::from("danmorse8642@gmail.com") %>%
        emayili::to("danmorse8642@gmail.com") %>%
        emayili::subject("This is a plain text message!") %>%
        emayili::body("Hello!") %>%
        emayili::attachment("dong-bot/game-reports/TEX_KC/hitchart1.png")
      
      smtp <- emayili::server(host = "smtp.gmail.com",
                     port = 465,
                     username = "danmorse8642@gmail.com",
                     password = Sys.get.env("PASSWORD"))
      
      emayili::smtp(email, verbose = TRUE)
      
    }
    
    # push data to github for app every hour
    if(lubridate::minute(Sys.time()) < 5){
      repo <- git2r::repository(getwd())
      git2r::add(repo = repo, path = "data/done_plays.rds")
      git2r::add(repo = repo, path = "data/done_plays.csv")
      git2r::commit(repo = repo, message = glue::glue("data updated - {Sys.time()}"))
      git2r::push(repo, credentials = git2r::cred_token())
    }
  }
}
