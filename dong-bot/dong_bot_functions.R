# twitter bot functions

fetch_live_games <- function(day){
  # day = desired date to pull games for, as.Date
  
  games <- baseballr::get_game_pks_mlb(day, level_ids = c(1)) %>%
    dplyr::select(game_pk, gameType, season, officialDate, status.abstractGameState,
           status.detailedState, teams.away.team.name, teams.home.team.name, venue.name) %>%
    janitor::clean_names() %>%
    dplyr::filter(status_detailed_state == "In Progress" | status_detailed_state == "Final") %>%
    dplyr::rename(away_team = teams_away_team_name) %>%
    dplyr::rename(home_team = teams_home_team_name)
  
  return(games)
}

clean_hits <- function(mlb_pbp, mlb_games){
  # pbp pulled from baseballr::get_pbp_mlb
  # games from fetch_live_games

  hit_data <- mlb_pbp %>%
    dplyr::select(-game_date) %>%
    dplyr::left_join(
      dplyr::select(
        mlb_games,
        game_type, game_pk, venue_name, official_date
        ),
      by = "game_pk") %>%
    janitor::clean_names() %>%
    # change column names to more familiar form
    dplyr::mutate(
      player_name = matchup_batter_full_name,
      player_team = batting_team,
      player_id = matchup_batter_id,
      game_date = official_date,
      headshot = glue::glue("https://img.mlbstatic.com/mlb-photos/image/upload/q_100/v1/people/{player_id}/headshot/67/current"),
      events = result_event,
      des = result_description,
      bb_type = hit_data_trajectory,
      outs_when_up = count_outs_start,
      inning = about_inning,
      inning_topbot = about_half_inning,
      inning_topbot = dplyr::case_when(
        about_half_inning == "bottom" ~ "Bottom",
        about_half_inning == "top" ~ "Top",
        TRUE ~ about_half_inning
      ),
      plate_z = pitch_data_coordinates_p_z,
      hc_x = hit_data_coordinates_coord_x,
      hc_y = hit_data_coordinates_coord_y,
      hit_distance_sc = hit_data_total_distance,
      launch_angle = hit_data_launch_angle,
      launch_speed = hit_data_launch_speed,
      stadium_observed = venue_name,
      pitcher_name = matchup_pitcher_full_name,
      home_score = result_home_score,
      away_score = result_away_score
    ) %>%
    # discard unneccessary columns
    dplyr::select(play_id, game_date, game_type, player_name, player_team,
           events, des, home_team, away_team, bb_type, outs_when_up,
           inning, inning_topbot, stadium_observed, pitcher_name,
           home_score, away_score,
           #  these here are the ones used in calculations
           plate_z, hc_x, hc_y, hit_distance_sc,
           launch_angle, launch_speed, headshot) %>%
    # only want deep fly balls
    dplyr::filter(hit_distance_sc >= 300) %>%
    dplyr::select(player_name, tidyselect::everything()) %>%
    # need to have x-y coordinates or we won't have hit direction
    dplyr::filter(!is.na(hc_x) & !is.na(hc_y)) %>%
    # fix hit coordinates
    GeomMLBStadiums::mlbam_xy_transformation() %>%
    dplyr::mutate(
      # add launch angle in radians column
      launch_angle = units::set_units(launch_angle, "degrees"),
      launch_angle_rads = as.double(units::set_units(launch_angle, "radians")),
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
    dplyr::mutate(
      hit_direction = dplyr::case_when(
        spray_angle <= -21 ~ "Left",
        spray_angle > -21 & spray_angle <= -7 ~ "Left-Center",
        spray_angle > -7 & spray_angle < 7 ~ "Center",
        spray_angle >= 7 & spray_angle < 21 ~ "Right-Center",
        spray_angle >= 21 ~ "Right"
      )
    )
  
  return(hit_data)
}

calculate_dingers <- function(hits){
  # hits: cleaned pbp data from clean_hits function
  # require: fences <- data/fences.rds
  
  # adding fence height at batted ball location for each stadium, each hit
  hits_new <- NULL
  
  # calculate dingerness in each ballpark
  for(j in c(unique(fences$stadium))){
    fences_i <- dplyr::filter(fences, stadium == j)
    for(i in 1:nrow(hit_data)){
      nearest_fence <- dplyr::tibble(angle_diff = abs(hit_data$spray_angle[i] - fences_i$spray_angle_stadia),
                              play_id = hit_data$play_id[i]) %>%
        dplyr::bind_cols(fences_i) %>%
        dplyr::arrange(angle_diff) %>%
        dplyr::slice(1) %>%
        dplyr::select(stadium, x, y, d_wall, play_id, fence_height, team, team_abbr)
      hits_new <- dplyr::bind_rows(hits_new, nearest_fence)
      rm(nearest_fence)
    }
    rm(fences_i, i, j)
  }

  # calculate whether or not it would've been a dinger
  hits_new <- hit_data %>%
    dplyr::left_join(hits_new, by = "play_id") %>%
    dplyr::left_join(
      dplyr::select(team_hashtags, full_team_name, team_abbr) %>%
        dplyr::rename(home_abbr = team_abbr),
      by = c("home_team" = "full_team_name")
    ) %>%
    dplyr::left_join(
      dplyr::select(team_hashtags, full_team_name, team_abbr) %>%
        dplyr::rename(away_abbr = team_abbr),
      by = c("away_team" = "full_team_name")
    ) %>%
    dplyr::mutate(
      total_time = -(launch_speed_y + sqrt(launch_speed_y^2 + (2*g * plate_z))) / g,
      acceleration_x = (-2*launch_speed_x / total_time) + (2*hit_distance_sc/total_time^2),
      time_wall = (-launch_speed_x + sqrt(launch_speed_x^2 + 2*acceleration_x*d_wall))/acceleration_x,
      height_at_wall = (launch_speed_y * time_wall) + (.5*g*(time_wall^2)),
      height_at_wall = ifelse(is.na(height_at_wall), 0, height_at_wall),
      would_dong = ifelse(height_at_wall > fence_height, 1, 0),
      would_dong = ifelse(team_abbr == home_abbr & events == "Home Run", 1, would_dong),
      would_dong = ifelse(team_abbr == home_abbr & events != "Home Run", 0, would_dong)
    )
  
  return(hits_new)
}

get_dong_total <- function(hits_detailed){
  # hits_detailed from calculate dingers function
  
  total_dongs <- hits_detailed  %>%
    dplyr::filter(stadium != "Sahlen Field") %>%
    dplyr::group_by(player_name, player_team, game_date, events, launch_speed, launch_angle,
             hit_distance_sc, hit_direction, stadium_observed, play_id) %>%
    dplyr::summarize(
      total_dongs = sum(would_dong),
      .groups = "drop"
    )
  
  return(total_dongs)
}

write_tweet <- function(hit){
  # hit: single hit sliced from calculate dingers function
  # require:
  #   -hit from calculate dingers function

  # get team abbreviations for matchup data
  hit <- hit %>%
    dplyr::left_join(
      dplyr::select(team_hashtags, full_team_name, team_abbr) %>%
        dplyr::distinct(), #  because TOR is on there twice
      by = c("home_team" = "full_team_name")
    ) %>%
    dplyr::rename(home_abbr = team_abbr) %>%
    dplyr::left_join(
      dplyr::select(team_hashtags, full_team_name, team_abbr) %>%
        dplyr::distinct(), #  because TOR is on there twice
      by = c("away_team" = "full_team_name")
    ) %>%
    dplyr::rename(away_abbr = team_abbr)
  
  if(stringr::str_detect(hit$des, "reviewed") | stringr::str_detect(hit$des, "challenged")){
    hit <- hit %>%
      tidyr::separate(
        des, into = c("review","des"), sep = ": ", remove = TRUE
      ) %>%
      dplyr::select(-review)
  }
  
  home_team <- dplyr::pull(hit, home_abbr)
  
  away_team <- dplyr::pull(hit, away_abbr)
  
  home_score <- dplyr::pull(hit, home_score)
  
  away_score <- dplyr::pull(hit, away_score)
  
  player_name <- dplyr::pull(hit, player_name)
  
  play_result <- dplyr::pull(hit, events)
  
  exit_velo <- dplyr::pull(hit, launch_speed)
  
  launch_angle <- dplyr::pull(hit, launch_angle)
  
  hit_distance <- dplyr::pull(hit, hit_distance_sc)
  
  pitcher_name <- dplyr::pull(hit, pitcher_name)
  
  inning <- hit %>%
    dplyr::mutate(inning = dplyr::case_when(
      inning == 1 ~ "1st",
      inning == 2 ~ "2nd",
      inning == 3 ~ "3rd",
      TRUE ~ as.character(glue::glue("{inning}th"))
    )) %>%
    dplyr::pull(inning)
  
  inning_half <- dplyr::pull(hit, inning_topbot)
  
  dongs <- dplyr::pull(hit, total_dongs)
  
  hashtag <- team_hashtags %>%
    dplyr::filter(full_team_name == dplyr::pull(hit, player_team)) %>%
    dplyr::slice(1) %>% #  because toronto is on there twice for 2 stadiums
    dplyr::pull(team_hashtag)
  
  result_emoji <- dplyr::case_when(
    play_result == "Single" ~ "\U00026be",
    play_result == "Double" ~ "\U0001f3c3\U0001f4a8",
    play_result == "Triple" ~ "\U0001f3c7\U0001f4a8",
    play_result == "Home Run" ~ "\U0001f4a3",
    play_result == "Lineout" ~ "\U0001f3af",
    play_result == "Flyout" ~ "\U0001f4a2",
    play_result == "Pop Out" ~ "\U0001f6ab",
    play_result == "Double Play" ~ "\U001f3af\U001f3af",
    play_result == "Sac Fly" ~ "\U0001f64c",
    play_result == "Fielders Choice" ~ "\U0001f937",
    play_result == "Field Error" ~ "\U00026a0",
    play_result == "Sac Fly Double Play" ~ "\U0001f64c\U001f3af\U001f3af",
    play_result == "Fielders Choice Out" ~ "\U0001f937\U001f3af",
    TRUE ~ "\U00026be"
  )
  
  ytd_hrs <- gsub(
    "\\(([^()]+)\\)",    # Extract characters within parentheses
    "\\1",
    stringr::str_extract_all(
      hit$des, "\\(([^()]+)\\)"
      )[[1]]
    )
  
  inning_emoji <- ifelse(
    inning_half == "Top", "\U0001f53a", "\U0001f53b"
  )
  
  out_events <- c("Flyout","Lineout","Pop Out","Sac Fly","Double Play",
                  "Sac Fly Double Play","Fielders Choice Out")
  
  if(player_name == "Nick Castellanos"){
    
    direction <- dplyr::pull(hit, hit_direction) %>%
      tolower()
    
    player_score <- ifelse(
      dplyr::pull(hit, home_team) == dplyr::pull(hit, player_team),
      home_score,
      away_score
      )
    
    opponent_score <- ifelse(
      dplyr::pull(hit, home_team) == dplyr::pull(hit, player_team),
      away_score,
      home_score
    )
    
    if(length(ytd_hrs) != 0){
      tweet <- glue::glue(
        "{player_name} vs {pitcher_name}
    #{hashtag}
    
    {play_result} {(ytd_hrs)} {result_emoji}
    
    Exit velo: {exit_velo} mph
    Launch angle: {launch_angle} deg
    Proj. distance: {hit_distance} ft
    
    And there's a drive into deep {direction} field by Castellanos, and that'll be a home run in {dongs}/30 MLB ballparks
    
    {away_team} ({away_score}) @ {home_team} ({home_score})
    {inning_emoji} {inning}
    
    I don't know if I'm going to be putting on this headset again. I don't know if it's going to be for the Reds. I don't know if it's going to be for my bosses at Fox"
      ) %>% substr(1, 278)
    } else {
      tweet <- glue::glue(
        "{player_name} vs {pitcher_name}
    #{hashtag}
    
    {play_result} {result_emoji}
    
    Exit velo: {exit_velo} mph
    Launch angle: {launch_angle} deg
    Proj. distance: {hit_distance} ft
    
    And there's a drive into deep {direction} field by Castellanos, and that'll be a home run in {dongs}/30 MLB ballparks
    
    {away_team} ({away_score}) @ {home_team} ({home_score})
    {inning_emoji} {inning}
    
    I don't know if I'm going to be putting on this headset again. I don't know if it's going to be for the Reds. I don't know if it's going to be for my bosses at Fox"
      ) %>% substr(1, 278)
    }
    
  } else if(stringr::str_detect(hit$des,"inside-the-park") & dongs > 1){
    
    play_result <- "Inside-the-park HR!"
    
    result_emoji <- "\U0001f3AA"
    
    tweet <- glue::glue(
      "{player_name} vs {pitcher_name}
    #{hashtag}
    
    {result_emoji} {play_result} {result_emoji}
    
    Exit velo: {exit_velo} mph
    Launch angle: {launch_angle} deg
    Proj. distance: {hit_distance} ft
    
    This would have been a home run in {dongs}/30 MLB ballparks
      
    {away_team} ({away_score}) @ {home_team} ({home_score})
    {inning_emoji} {inning}"
    )
    
  } else if(stringr::str_detect(hit$des,"inside-the-park") & dongs == 1){
    
    play_result <- "Inside-the-park HR!"
    
    result_emoji <- "\U0001f3AA"
    
    tweet <- glue::glue(
      "{player_name} vs {pitcher_name}
    #{hashtag}
    
    {result_emoji} {play_result} {result_emoji}
    
    Exit velo: {exit_velo} mph
    Launch angle: {launch_angle} deg
    Proj. distance: {hit_distance} ft
    
    This would  not have left the yard anywhere.
      
    {away_team} ({away_score}) @ {home_team} ({home_score})
    {inning_emoji} {inning}"
    )
    
  } else if(dongs == 30){
    
    lock_emoji <- "\U0001f512"
    
    tweet <- glue::glue(
      "{player_name} vs {pitcher_name}
    #{hashtag}
    
    {play_result} ({ytd_hrs}) {result_emoji}
    
    Exit velo: {exit_velo} mph
    Launch angle: {launch_angle} deg
    Proj. distance: {hit_distance} ft
    
    No doubt about that one {lock_emoji}
    That's a dinger in all 30 MLB ballparks
      
    {away_team} ({away_score}) @ {home_team} ({home_score})
    {inning_emoji} {inning}"
    )
    
  } else if(dongs == 1 & play_result == "Home Run" & stringr::str_detect(hit$des,"inside-the-park", negate = TRUE)) {
    
    unicorn_emoji <- "\U0001f984"
    
    dong_stadium <- hit_detail %>%
      dplyr::filter(stadium != "Sahlen Field") %>%
      dplyr::filter(play_id == dplyr::pull(hit, play_id)) %>%
      dplyr::filter(would_dong == 1) %>%
      dplyr::pull(stadium)
    
    tweet <- glue::glue(
      "{player_name} vs {pitcher_name}
    #{hashtag}
    
    {unicorn_emoji} IT'S A UNICORN {unicorn_emoji}
    
    {play_result} ({ytd_hrs}) {result_emoji}
    
    Exit velo: {exit_velo} mph
    Launch angle: {launch_angle} deg
    Proj. distance: {hit_distance} ft
    
    This would have been a home run at {dong_stadium} and nowhere else.
    
    {away_team} ({away_score}) @ {home_team} ({home_score})
    {inning_emoji} {inning}"
    )
    
  } else if(dongs == 1 & play_result != "Home Run") {
    
    dong_stadium <- hit_detail %>%
      dplyr::filter(stadium != "Sahlen Field") %>%
      dplyr::filter(play_id == dplyr::pull(hit, play_id)) %>%
      dplyr::filter(would_dong == 1) %>%
      dplyr::pull(stadium)
    
    if(length(ytd_hrs) != 0){
      
      tweet <- glue::glue(
        "{player_name} vs {pitcher_name}
    #{hashtag}
    
    {play_result} ({ytd_hrs}) {result_emoji}
    
    Exit velo: {exit_velo} mph
    Launch angle: {launch_angle} deg
    Proj. distance: {hit_distance} ft
    
    This would have been a home run at {dong_stadium} and nowhere else
    
    {away_team} ({away_score}) @ {home_team} ({home_score})
    {inning_emoji} {inning}"
      )
    } else {
      
      tweet <- glue::glue(
        "{player_name} vs {pitcher_name}
    #{hashtag}
    
    {play_result} {result_emoji}
    
    Exit velo: {exit_velo} mph
    Launch angle: {launch_angle} deg
    Proj. distance: {hit_distance} ft
    
    This would have been a home run at {dong_stadium} and nowhere else
    
    {away_team} ({away_score}) @ {home_team} ({home_score})
    {inning_emoji} {inning}"
      )
    }
    
  } else if(dongs == 29 & play_result != "Home Run") {
    
    unicorn_emoji <- "\U0001f984"
    
    dong_stadium <- hit_detail %>%
      dplyr::filter(stadium != "Sahlen Field") %>%
      dplyr::filter(play_id == dplyr::pull(hit, play_id)) %>%
      dplyr::filter(would_dong == 0) %>%
      dplyr::pull(stadium)
    
    if(length(ytd_hrs) != 0){
      
      tweet <- glue::glue(
        "{player_name} vs {pitcher_name}
    #{hashtag}
    
    {unicorn_emoji} IT'S A UNICORN {unicorn_emoji}
    
    {play_result} ({ytd_hrs}) {result_emoji}
    
    Exit velo: {exit_velo} mph
    Launch angle: {launch_angle} deg
    Proj. distance: {hit_distance} ft
    
    This {tolower(play_result)} would have been a home run in every park except {dong_stadium}. That's gotta sting.
    
    {away_team} ({away_score}) @ {home_team} ({home_score})
    {inning_emoji} {inning}"
      )
    } else {
      
      tweet <- glue::glue(
        "{player_name} vs {pitcher_name}
    #{hashtag}
    
    {unicorn_emoji} IT'S A UNICORN {unicorn_emoji}
    
    {play_result} {result_emoji}
    
    Exit velo: {exit_velo} mph
    Launch angle: {launch_angle} deg
    Proj. distance: {hit_distance} ft
    
    This {tolower(play_result)} would have been a home run in every park except {dong_stadium}. That's gotta sting.
    
    {away_team} ({away_score}) @ {home_team} ({home_score})
    {inning_emoji} {inning}"
      ) 
    }
    
  } else if(dongs == 29 & play_result == "Home Run") {
    
    dong_stadium <- hit_detail %>%
      dplyr::filter(stadium != "Sahlen Field") %>%
      dplyr::filter(play_id == dplyr::pull(hit, play_id)) %>%
      dplyr::filter(would_dong == 0) %>%
      dplyr::pull(stadium)
    
    tweet <- glue::glue(
      "{player_name} vs {pitcher_name}
    #{hashtag}
    
    {play_result} ({ytd_hrs}) {result_emoji}
    
    Exit velo: {exit_velo} mph
    Launch angle: {launch_angle} deg
    Proj. distance: {hit_distance} ft
    
    This would have been a home run in {dongs}/30 MLB ballparks.
    Only {dong_stadium} would've held this one in.
    
    {away_team} ({away_score}) @ {home_team} ({home_score})
    {inning_emoji} {inning}"
    )
    
  } else {
    
    if(length(ytd_hrs) != 0){
      
      tweet <- glue::glue(
        "{player_name} vs {pitcher_name}
    #{hashtag}
    
    {play_result} ({ytd_hrs}) {result_emoji}
    
    Exit velo: {exit_velo} mph
    Launch angle: {launch_angle} deg
    Proj. distance: {hit_distance} ft
    
    This would have been a home run in {dongs}/30 MLB ballparks
    
    {away_team} ({away_score}) @ {home_team} ({home_score})
    {inning_emoji} {inning}"
      )
    } else {
     
      tweet <- glue::glue(
        "{player_name} vs {pitcher_name}
    #{hashtag}
    
    {play_result} {result_emoji}
    
    Exit velo: {exit_velo} mph
    Launch angle: {launch_angle} deg
    Proj. distance: {hit_distance} ft
    
    This would have been a home run in {dongs}/30 MLB ballparks
    
    {away_team} ({away_score}) @ {home_team} ({home_score})
    {inning_emoji} {inning}"
      ) 
    }
  }
  
  return(tweet)
}

draw_hit_plot <- function(hit){
  # hit: single hit sliced from calculate dingers function
  # require:
  #   -hit from calculate dingers function
  
  stadium_name <- dplyr::pull(hit, stadium_observed)
  
  stadium_logo <- team_hashtags %>%
    dplyr::filter(stadium == stadium_name) %>%
    dplyr::pull(team_logo)
  
  stadium_id <- team_hashtags %>%
    dplyr::filter(stadium == stadium_name) %>%
    dplyr::pull(team)
  
  # add fence heights
  park_fences <- fences %>%
    dplyr::filter(stadium == stadium_name) %>%
    dplyr::select(stadium, x, y, fence_height)
  
  play_result <- dplyr::pull(hit, events)
  
  # for the emoji choice
  hit_events <- c("Single","Double","Triple","Field Error","Fielders Choice")
  out_events <- c("Flyout","Lineout","Pop Out","Sac Fly","Double Play",
                  "Sac Fly Double Play","Fielders Choice Out")
  
  hit_color <- ifelse(play_result %in% hit_events | play_result == "Home Run",
                      "blue", "red")
  
  # make balls hit the wall if they got to the wall but didn't get over the fence
  hit_path_wall <- hit_detail %>%
    dplyr::filter(play_id == dplyr::pull(hit, play_id)) %>%
    dplyr::filter(stadium == stadium_name) %>%
    dplyr::mutate(
      wall_y = ifelse(hc_y_ > y & would_dong == 0, y, hc_y_),
      wall_x = ifelse(hc_y_ > y & would_dong == 0, x, hc_x_)
    )
  
  # for geom_image on field diagram
  transparent <- function(img) {
    magick::image_fx(img, expression = "0.5*a", channel = "alpha")
  }
  
  # add a lil curve to the hit path for aesthetic purposes
  curv <- dplyr::pull(hit, spray_angle)/(-90)
  
  # get player headshot
  headshot <- dplyr::pull(hit, headshot)
  
  # get dongs total
  dongs <- dplyr::pull(hit, total_dongs)
  
  # get min x and y values to find plot corner
  stadium_path <- dplyr::filter(stadium_paths, stadium == stadium_name)
  min_x <- min(stadium_path$x)
  min_y <- min(stadium_path$y)
  max_x <- max(stadium_path$x)
  
  # get player info
  player_name <- dplyr::pull(hit, player_name)
  
  ggplot2::ggplot() +
    ggimage::geom_image(ggplot2::aes(x = 0, y = 250, image = stadium_logo),
                        size = 0.25, image_fun = transparent) +
    ggplot2::geom_text(
      ggplot2::aes(x = 0, y = 160, label = glue::glue("{stadium_name}")),
      alpha = .3, vjust = 0
    ) +
    ggplot2::geom_path(
      data = stadium_path,
      ggplot2::aes(x, y, group = segment)
    ) +
    # add colored fence heights
    ggplot2::geom_path(data = park_fences,
                       ggplot2::aes(x, y, color = fence_height),
                       size = 2) +
    # curve for original hit path
    ggplot2::geom_curve(
      data = hit, linetype = "dashed",
      ggplot2::aes(x = 0, y = 0, xend = hc_x_, yend = hc_y_),
      curvature = curv, angle = 135, size = .5, color = hit_color
    ) +
    # curve for hitting the wall (or full path for dongers)
    ggplot2::geom_curve(
      data = hit_path_wall,
      ggplot2::aes(x = 0, y = 0, xend = wall_x, yend = wall_y),
      curvature = curv, angle = 135, size = 2, color = hit_color
    ) +
    # ball landing spot
    ggimage::geom_emoji(data = hit,
                        ggplot2::aes(hc_x_, hc_y_,
                            image = dplyr::case_when(
                              play_result == "Home Run" ~ '1f4a5',
                              play_result %in% out_events ~ '274c',
                              play_result %in% hit_events ~ '26be'
                            )
                            #image = ifelse(did_dong == 1, '1f4a5', '274c')
                        ), size = .07) +
    # headshot
    ggimage::geom_image(
      data = hit,
      ggplot2::aes(x = min_x, y = min_y+140, image = headshot),
      size = .12, hjust = 0
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = min_x, y = min_y,
        label = glue::glue("{player_name}\n{play_result}\nHR in {dongs}/30 parks")
      ), hjust = 0, vjust = 0
    ) +
    ggplot2::geom_text(
      ggplot2::aes(x = max_x, y = min_y, label = "@would_it_dong"),
      hjust = 1, vjust = 0, alpha = .7
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.margin = grid::unit(c(0.5,0.5,0.5,0.5), "mm")
    ) +
    ggplot2::coord_fixed() +
    ggplot2::labs(color = "Wall Height (ft)")
  
  ggplot2::ggsave("hit_chart.png", width = 6, height = 4, dpi = 500)
  
  description <- stringr::str_remove(hit$des, hit$player_name)
  
  description <- substring(description,1,nchar(description)-1)
  
  player_team <- hit$player_team
  
  player_name <- hit$player_name
  
  alt_text <- glue::glue(
    "{player_name} of the {player_team}{description} at {stadium_name}. This would have been a home run in {dongs} MLB parks."
  )
  
  return(alt_text)
}
