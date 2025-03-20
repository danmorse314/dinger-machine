library(gt)
library(dplyr)

daily <- hit_data |>
  left_join(
    select(team_hashtags, player_team = full_team_name, team_logo),
    by = "player_team"
  ) |>
  mutate(filler = " ") |>
  select(
  player_name,
  team_logo,
  events,
  inning,
  stadium_observed,
  launch_angle,
  exit_velo = launch_speed,
  total_dongs,
  filler
)

splits <- ceiling(nrow(daily)/17)

for(i in 1:splits){
  sub <- daily |>
    slice((17*(i-1)+1):(i*17))
  
  sub |>
    gt() |>
    gtExtras::gt_img_rows(
      columns = team_logo,
      height = 40
    ) |>
    cols_label(
      player_name = "Batter",
      team_logo = "Team",
      events = "Result",
      inning = "Inning",
      stadium_observed = "Stadium",
      launch_angle = "Launch Angle",
      exit_velo = "Exit Velo",
      total_dongs = "Dong in:",
      filler = " "
    ) |>
    cols_align(
      columns = c(team_logo, events, inning),
      align = "center"
    ) |>
    gtExtras::gt_hulk_col_numeric(
      columns = total_dongs,
      domain = c(0,30)
    ) |>
    tab_header(glue::glue("{min(hit_data$game_date)} Dong Report")) |>
    gtsave(glue::glue("images/daily_table_{i}.png"))
  
}

