###################################################################
#                                                                 #
#               WOULD   IT   DONG   ?                             #
#                                                                 #
#   an app to investigate whether any deep fly ball would've      #
#   been a home run if it were hit in a different stadium         #
#                                                                 #
###################################################################

library(shiny)
library(shinycssloaders)
library(tidyverse)
library(GeomMLBStadiums)
library(DT)
library(scroller)
library(glue)
library(ggimage)
library(shinyWidgets)
library(units)
#library(gt) change hit detail later to gt table(?)

#setwd("~/R/Dinger Machine")

# TO DO:
#   -add degree symbol(?)
#   -add font to r file

# initial hit data
hit_data <- readRDS(url("https://github.com/danmorse314/dinger-machine/raw/main/data/hit_data.rds"))

# full hit data with a row for each stadium, each hit
#hits_new <- readRDS(url("https://github.com/danmorse314/dinger-machine/raw/main/data/dinger_detail.rds"))

# initial hit data with number of stadiums it woud've donged in
total_dongs <- readRDS(url("https://github.com/danmorse314/dinger-machine/raw/main/data/dinger_total.rds"))

# get initial player list for selectinput
player_list <- hit_data %>% select(player_name, player_team) %>% distinct() %>%
  arrange(player_name)

# get latest date in data
last_update <- arrange(hit_data, desc(game_date)) %>% slice(1) %>% pull(game_date)

# get team logos
mlb_logos <- readRDS(url("https://github.com/danmorse314/dinger-machine/raw/main/data/mlb_logos.rds"))

team_logos <- mlb_logos %>%
  select(team_abbr, logo_html)

stadium_logos <- mlb_logos %>%
  select(stadium, logo_html)

# adding HTML code for venue select options list
# stadium_options is what the client will see
stadium_options <- list(content =  
                          mapply(pull(mlb_logos, stadium), pull(mlb_logos, team_logo), FUN = function(stadium, team_logo) {
                            HTML(paste(
                              tags$img(src=team_logo, width=20, height=15),
                              stadium
                            ))
                          }, SIMPLIFY = FALSE, USE.NAMES = FALSE)
)


# acceleration due to gravity in ft/s^2
g <- -32.174

# adding full team names for picker input for filtering by team
# get team list
team_list <- tibble(team_abbr = "All") %>%
  bind_rows(
    arrange(mlb_logos, full_team_name) %>%
      select(team_abbr)
  ) %>%
  pull(team_abbr)
# getting full name for display
team_options <- list(content = c("All",sort(pull(mlb_logos, full_team_name))), SIMPLIFY = FALSE, USE.NAMES = FALSE)

# get fence height data
fences <- read_csv("https://github.com/danmorse314/dinger-machine/raw/main/data/fence_heights_complete.csv",
                   col_types = cols())

# for geom_image on field diagram
transparent <- function(img) {
  magick::image_fx(img, expression = "0.5*a", channel = "alpha")
}

# add custom font
# this doesn't work yet
#dir.create('~/.fonts')
#file.copy("~/www/Starjedi.ttf", "-/.fonts")
#system('fc_cache -f ~/.fonts')

ui <- fluidPage(
  scroller::use_scroller(),
  titlePanel(
    div(
      tags$em(style = "font-size: 65px; text-align:left; font-family:Star Jedi;
              color: white; background-color: darkgreen; display: block; margin-bottom:10px",
              "Would it dong?",
              br()
      ),
      fluidRow(
        column(
          width = 6,
          div(style = "font-size:14px; text-align:left;",
              "an app by  ",
              tags$a(href = "https://www.twitter.com/danmorse_",
                     "Dan Morse", target = "_blank")
          ),
        ),
        column(
          width = 6,
          div(style = "font-size:14px; text-align:left;",
              glue("Updated: {last_update}"))
        )
      )
    ),
    windowTitle = "the dinger machine"
  ),
  mainPanel(
    fluidRow(
      column(
        width = 4,
        pickerInput(inputId = "team",  label = "Select team(s)",
                    multiple = FALSE, options = list(`none-selected-text` = "All"),
                    choices = team_list, choicesOpt = team_options)
      ),
      column(
        width = 4,
        pickerInput(inputId = "player", label = "Select player(s)",
                    choices = player_list$player_name, multiple = TRUE,
                    options = list(`none-selected-text` = "All"))
      ),
      column(
        width = 4,
        pickerInput(inputId = "stadium", label = "Select ballpark(s)",
                    multiple = TRUE, options = list(`none-selected-text` = "All"),
                    choices = pull(mlb_logos, stadium),
                    choicesOpt = stadium_options)
      )
    ),
    actionButton(inputId = "submit", label = "View hits") %>%
      shiny::a() %>%
      shiny::tagAppendAttributes(href = "##hitlist"),
    br(), br(),
    DT::dataTableOutput(outputId = "hitlist"),
    br(), br(),
    uiOutput(outputId = "hr_qty"),
    br(), br(),
    fluidRow(
      column(
        width = 6,
        withSpinner(DT::dataTableOutput(outputId = "parklist"), type = 5)
      ),
      column(
        width = 6,
        withSpinner(plotOutput(outputId = "parkplot"), type = 5),
        uiOutput(outputId = "hitdetail")
      )
    ),
    tags$hr(),
    HTML("<span>
            <img src = 'https://image.flaticon.com/icons/png/512/25/25231.png' height = '24px'
            </img>
            <a href='https://github.com/danmorse314/dinger-machine' target='_blank'>View code
            </a>
         </span>"),
    br(),
    HTML("<span>
            <img src = 'https://user-images.githubusercontent.com/16066404/77041853-a2044100-69e0-11ea-8da6-d64822a2c72a.jpg' height = '24px'
            </img>
            <a href='https://www.buymeacoffee.com/danmorse' target='_blank'>Buy me a coffee
            </a>
         </span>"),
    tags$hr()
  )
)

server <- function(input, output, session){
  
  observeEvent(input$team,{
    if(input$team == "All"){
      team_filter <- team_list
    } else {
      team_filter <- input$team
    }
    updatePickerInput(session, inputId = "player",
                      choices = filter(player_list, player_team %in% team_filter) %>%
                        pull(player_name))
  })
  
  hits <- eventReactive(input$submit, {
    
    # show all if any selections are blank
    if(input$team == "All"){
      team_filter <- team_list
    } else {
      team_filter <- input$team
    }
    
    if(is.null(input$player)){
      player_filter <- player_list %>% pull(player_name)
    } else {
      player_filter <- input$player
    }
    
    if(is.null(input$stadium)){
      park_filter <- pull(mlb_logos, stadium)
    } else {
      park_filter <- input$stadium
    }
    
    hits <- hit_data %>%
      filter(player_team %in% team_filter &
               player_name %in% player_filter &
               stadium_observed %in% park_filter) %>%
      left_join(team_logos, by = c("player_team" = "team_abbr")) %>%
      # this was an attempt to add the degree symbol, it failed miserably
      #mutate(launch_angle = glue("{launch_angle}\\degree")) %>%
      select(index, player_name, logo_html, game_date, stadium_observed, inning,
             events, launch_speed, launch_angle, hit_distance_sc, hit_direction)
    
    hits
  })
  
  hit_new <- reactive({
    
    # calculates if selected hit would've donged
    
    s <- input$hitlist_rows_selected
    
    hit_select <- hits()[s, , drop = FALSE] %>%
      pull(index)
    
    hit_path <- hit_data %>%
      filter(index == hit_select)
    
    # adding fence height at batted ball location for each stadium, each hit
    hit_new <- NULL
    
    for(j in c(unique(fences$team))){
      # who is screaming 'stop using for loops' outside my house
      # show yourself coward, I will never stop using for loops
      fences_i <- filter(fences, team == j)
      nearest_fence <- tibble(
        angle_diff = abs(dplyr::pull(hit_path, spray_angle) -
                           dplyr::pull(fences_i, spray_angle_stadia)),
        index = dplyr::pull(hit_path, index)) %>%
        bind_cols(fences_i) %>%
        arrange(angle_diff) %>%
        slice(1) %>%
        select(stadium, x, y, d_wall, index, fence_height, team, team_abbr)
      hit_new <- bind_rows(hit_new, nearest_fence)
      rm(nearest_fence)
      rm(fences_i)
      
    }
    
    hit_new <- hit_path %>%
      left_join(hit_new, by = "index") %>%
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
    
    hit_new
    
  })
  
  observeEvent(input$submit,{
    
    output$hitlist <- DT::renderDataTable({
      
      DT::datatable(
        select(hits(), -index),
        colnames = c(
          "Player" = "player_name",
          "Team" = "logo_html",
          "Date" = "game_date",
          "Venue" = "stadium_observed",
          "Inning" = "inning",
          "Result" = "events",
          "Exit Velocity (mph)" = "launch_speed",
          "Launch Angle (deg)" = "launch_angle",
          "Projected Distance (ft)" = "hit_distance_sc",
          "Hit Direction" = "hit_direction"
        ),
        escape = FALSE,
        callback = JS("
    table.on( 'order.dt search.dt', function () {
        table.column(0, {search:'applied', order:'applied'}).nodes().each( 
            function (cell, i) {
                cell.innerHTML = i+1;
            } );
    } )"),
        selection = "single",
        options = list(
          columnDefs = list(list(
            className = "dt-center", targets = 1:9
          )),
          lengthMenu = list(c(10, 25, 50), c('10', '25', '50')),
          pageLength = 10)
      )
    })
    
    output$hr_qty <- renderUI({
      
      s <- input$hitlist_rows_selected
      
      if(length(s) == FALSE){
        tags$i("Click on a hit to see in how many parks it would've left the yard")
      } else {
        hit_select <- hits()[s, , drop = FALSE] %>%
          pull(index)
        
        dong_qty <- filter(total_dongs, index == hit_select) %>% pull(total_dongs)
        
        HTML(glue("<div><span style='font-size: 24px;'>This hit would have gone yard in  </span><span style='font-size:50px'><b>{dong_qty}/30</b></span><span style='font-size: 24px;'>  MLB ballparks</span></div>"))
        
      }
      
    })
    
  })
  
  output$parklist <- DT::renderDataTable({
    
    s <- input$hitlist_rows_selected
    
    if(length(s) == FALSE){
      NULL
    } else {
      hit_select <- hits()[s, , drop = FALSE] %>%
        pull(index)
      
      hit_path <- hit_data %>%
        filter(index == hit_select)
      
      # a lot of this notation is super ugly because I made a major edit and was lazy
      hits_new <- hit_new()
      
      parks <- filter(hits_new, index == hit_select) %>%
        mutate(would_dong = ifelse(would_dong == 1, "Yes", "No")) %>%
        left_join(stadium_logos, by = "stadium") %>%
        select(stadium, logo_html, would_dong) %>% arrange(stadium) %>% arrange(would_dong)
      
      DT::datatable(
        parks,
        colnames = c(
          "Venue" = "stadium",
          "Team" = "logo_html",
          "Would it dong?" = "would_dong"
        ),
        escape = FALSE,
        callback = JS("
    table.on( 'order.dt search.dt', function () {
        table.column(0, {search:'applied', order:'applied'}).nodes().each( 
            function (cell, i) {
                cell.innerHTML = i+1;
            } );
    } )"),
        selection = "single",
        options = list(
          columnDefs = list(list(
            className = "dt-center", targets = c(2,3)
          )),
          lengthMenu = list(c(10, 20, 30), c('10', '20', 'All')),
          pageLength = 10,
          searching = FALSE)
      )
      
    }
  })
  
  output$parkplot <- renderPlot({
    
    s <- input$hitlist_rows_selected
    
    s2 <- input$parklist_rows_selected
    
    if(length(s) == FALSE){
      NULL
    } else if(length(s) == TRUE & length(s2) == FALSE) {
      
      hit_select <- hits()[s, , drop = FALSE] %>%
        pull(index)
      
      hit_path <- hit_data %>%
        filter(index == hit_select)
      
      hits_new <- hit_new()
      
      park_name <- pull(hit_path, stadium_observed)
      
      did_dong <- filter(hits_new, stadium == park_name) %>%
        #mutate(would_dong = ifelse(would_dong == 1, "Yes", "No")) %>%
        pull(would_dong)
      
      hit_color <- ifelse(did_dong == 1, "blue", "red")
      
      stadium_id <- hit_path %>%
        left_join(
          select(mlb_logos, stadium, team),
          by = c("stadium_observed" = "stadium")
        ) %>%
        pull(team)
      
      stadium_logo <- hit_path %>%
        left_join(
          select(mlb_logos, stadium, team_logo),
          by = c("stadium_observed" = "stadium")
        ) %>%
        pull(team_logo)
      
      # make balls hit the wall if they got to the wall but didn't get over the fence
      hit_path_wall <- hits_new %>%
        filter(stadium == park_name) %>%
        mutate(
          wall_y = ifelse(hc_y_ > y & would_dong == 0, y, hc_y_),
          wall_x = ifelse(hc_y_ > y & would_dong == 0, x, hc_x_)
        )
      
      # add a lil curve to the hit path for aesthetic purposes
      curv <- pull(hit_path, spray_angle)/(-90)
      
      # add fence heights
      park_fences <- fences %>%
        filter(stadium == park_name) %>%
        select(stadium, x, y, fence_height)
      
      ggplot() +
        ggimage::geom_image(aes(x = 0, y = 250, image = stadium_logo),
                            size = 0.25, image_fun = transparent) +
        geom_mlb_stadium(stadium_ids = stadium_id,
                         stadium_segments = "all",
                         stadium_transform_coords = TRUE) +
        # add colored fence heights
        geom_path(data = park_fences,
                  aes(x, y, color = fence_height),
                  size = 2) +
        # ball landing spot
        geom_point(data = hit_path, aes(hc_x_, hc_y_), size = 6,
                   color = hit_color,
                   shape = ifelse(did_dong == 1, 19, 10)) +
        # curve for original hit path
        geom_curve(
          data = hit_path, linetype = "dashed",
          aes(x = 0, y = 0, xend = hc_x_, yend = hc_y_),
          curvature = curv, angle = 135, size = .5, color = hit_color
        ) +
        # curve for hitting the wall (or full path for dongers)
        geom_curve(
          data = hit_path_wall,
          aes(x = 0, y = 0, xend = wall_x, yend = wall_y),
          curvature = curv, angle = 135, size = 2, color = hit_color
        ) +
        theme_void() +
        theme(
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 14)
        ) +
        coord_fixed() +
        labs(color = "Wall Height (ft)")
      
    } else {
      
      hit_select <- hits()[s, , drop = FALSE] %>%
        pull(index)
      
      hit_path <- hit_data %>%
        filter(index == hit_select)
      
      hits_new <- hit_new()
      
      parks <- hits_new %>%
        mutate(would_dong = ifelse(would_dong == 1, "Yes", "No")) %>%
        left_join(stadium_logos, by = "stadium") %>%
        select(stadium, logo_html, would_dong) %>% arrange(stadium) %>% arrange(would_dong)
      
      stadium_id <- parks[s2, , drop = FALSE] %>%
        left_join(
          select(mlb_logos, stadium, team),
          by = "stadium"
        ) %>%
        pull(team)
      
      stadium_logo <- parks[s2, , drop = FALSE] %>%
        left_join(
          select(mlb_logos, stadium, team_logo),
          by = "stadium"
        ) %>%
        pull(team_logo)
      
      park_name <- pull(parks[s2, , drop = FALSE],stadium)
      
      did_dong <- filter(hits_new, stadium == park_name) %>%
        #mutate(would_dong = ifelse(would_dong == 1, "Yes", "No")) %>%
        pull(would_dong)
      
      hit_color <- ifelse(did_dong == 1, "blue", "red")
      
      # make balls hit the wall if they got to the wall but didn't get over the fence
      hit_path_wall <- hits_new %>%
        filter(stadium == park_name) %>%
        mutate(
          wall_y = ifelse(hc_y_ > y & would_dong == 0, y, hc_y_),
          wall_x = ifelse(hc_y_ > y & would_dong == 0, x, hc_x_)
        )
      
      # add a lil curve to the hit path for aesthetic purposes
      curv <- pull(hit_path, spray_angle)/(-90)
      
      # add fence heights
      park_fences <- fences %>%
        filter(stadium == park_name) %>%
        select(stadium, x, y, fence_height)
      
      ggplot() +
        ggimage::geom_image(aes(x = 0, y = 250, image = stadium_logo),
                            size = 0.25, image_fun = transparent) +
        geom_mlb_stadium(stadium_ids = stadium_id,
                         stadium_segments = "all",
                         stadium_transform_coords = TRUE) +
        # add colored fence heights
        geom_path(data = park_fences,
                  aes(x, y, color = fence_height),
                  size = 2) +
        # ball landing spot
        geom_point(data = hit_path, aes(hc_x_, hc_y_), size = 6,
                   color = hit_color,
                   shape = ifelse(did_dong == 1, 19, 10)) +
        # curve for original hit path
        geom_curve(
          data = hit_path, linetype = "dashed",
          aes(x = 0, y = 0, xend = hc_x_, yend = hc_y_),
          curvature = curv, angle = 135, size = .5, color = hit_color
        ) +
        # curve for hitting the wall (or full path for dongers)
        geom_curve(
          data = hit_path_wall,
          aes(x = 0, y = 0, xend = wall_x, yend = wall_y),
          curvature = curv, angle = 135, size = 2, color = hit_color
        ) +
        theme_void() +
        theme(
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 14)
        ) +
        coord_fixed() +
        labs(color = "Wall Height (ft)")
    }
    
  })
  
  output$hitdetail <- renderUI({
    
    s <- input$hitlist_rows_selected
    
    s2 <- input$parklist_rows_selected
    
    if(length(s) == FALSE){
      NULL
    } else if(length(s) == TRUE & length(s2) == FALSE) {
      
      hit_select <- hits()[s, , drop = FALSE] %>%
        pull(index)
      
      hit_path <- hit_data %>%
        filter(index == hit_select)
      
      hits_new <- hit_new()
      
      hit_detail <- hit_path %>%
        select(player_name, player_team, launch_speed, launch_angle, events, stadium_observed)
      
      park_name <- pull(hit_detail, stadium_observed)
      
      park_view <- filter(hits_new, stadium == park_name) %>%
        #mutate(would_dong = ifelse(would_dong == 1, "Yes", "No")) %>%
        select(stadium, would_dong)
      
      team_logo <- hit_detail %>%
        left_join(team_logos, by = c("player_team" = "team_abbr")) %>%
        pull(logo_html)
      
      div(
        HTML(paste(team_logo)),
        tags$b(style = "font-size: 32px;",
               glue("{pull(hit_detail,player_name)}")),
        br(),
        HTML(glue("<span style = 'font-size:20px;'><b>Exit velo:</b> {pull(hit_detail, launch_speed)} mph</span>")),
        br(),
        HTML(glue("<span style = 'font-size:20px;'><b>Launch angle:</b> {pull(hit_detail, launch_angle)} deg</span>")),
        br(),
        HTML(glue("<span style = 'font-size:20px;'><b>Actual result:</b> {pull(hit_detail, events)}</span>")),
        br(),
        if(pull(park_view, would_dong) == 1){
          tags$em(style = "font-size: 24px;",
                  glue("This would have been a dinger at {park_name}"))
        } else {
          tags$em(style = "font-size: 24px;",
                  glue("This would have stayed in the yard at {park_name}"))
        }
      )
      
    } else {
      
      hit_select <- hits()[s, , drop = FALSE] %>%
        pull(index)
      
      hit_path <- hit_data %>%
        filter(index == hit_select)
      
      hits_new <- hit_new()
      
      parks <- hits_new %>%
        mutate(would_dong = ifelse(would_dong == 1, "Yes", "No")) %>%
        left_join(stadium_logos, by = "stadium") %>%
        select(stadium, logo_html, would_dong) %>% arrange(stadium) %>% arrange(would_dong)
      
      park_name <- parks[s2, , drop = FALSE] %>%
        pull(stadium)
      
      hit_detail <- hit_path %>%
        select(player_name, player_team, launch_speed, launch_angle, events)
      
      park_view <- filter(hits_new, stadium == park_name) %>%
        #mutate(would_dong = ifelse(would_dong == 1, "Yes", "No")) %>%
        select(stadium, would_dong)
      
      team_logo <- hit_detail %>%
        left_join(team_logos, by = c("player_team" = "team_abbr")) %>%
        pull(logo_html)
      
      div(
        HTML(paste(team_logo)),
        tags$b(style = "font-size: 32px;",
               glue("{pull(hit_detail,player_name)}")),
        br(),
        HTML(glue("<span style = 'font-size:20px;'><b>Exit velo:</b> {pull(hit_detail, launch_speed)} mph</span>")),
        br(),
        HTML(glue("<span style = 'font-size:20px;'><b>Launch angle:</b> {pull(hit_detail, launch_angle)} deg</span>")),
        br(),
        HTML(glue("<span style = 'font-size:20px;'><b>Actual result:</b> {pull(hit_detail, events)}</span>")),
        br(),
        if(pull(park_view, would_dong) == 1){
          tags$em(style = "font-size: 24px;",
                  glue("This would have been a dinger at {park_name}"))
        } else {
          tags$em(style = "font-size: 24px;",
                  glue("This would have stayed in the yard at {park_name}"))
        }
      )
      
    }
    
  })
  
}

shinyApp(ui = ui, server = server)