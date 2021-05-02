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
#library(gt) change hit detail later to gt table(?)

#setwd("~/R/Dinger Machine")

# TO DO:
#   -add degree symbol(?)
#   -upload to github

# initial hit data
hit_data <- read_csv("hit_data.csv",
                     col_types = cols())

# full hit data with a row for each stadium, each hit
hits_new <- read_csv("dinger_detail.csv",
                     col_types = cols())

# initial hit data with number of stadiums it woud've donged in
total_dongs <- read_csv("dinger_total.csv",
                        col_types = cols())

# stadium list
stadium_list_detail <- hits_new %>% select(stadium, team) %>% distinct() %>%
  mutate(ballpark = glue("{stadium} ({team})"))
stadium_list <- stadium_list_detail %>% pull(ballpark)

# get team list for selectinput
team_list <- unique(hit_data$player_team) %>% sort()

# get initial player list for selectinput
player_list <- hit_data %>% select(player_name, player_team) %>% distinct() %>%
  arrange(player_name)

# get latest date in data
last_update <- arrange(hit_data, desc(game_date)) %>% slice(1) %>% pull(game_date)

# get team logos
mlb_logos <- read_csv("mlb_logos.csv", col_types = cols()) %>%
  mutate(logo_html = glue("<img src = '{team_logo}' height = '45'></img>"))

team_logos <- mlb_logos %>%
  select(team_abbr, logo_html)

stadium_logos <- mlb_logos %>%
  select(stadium, logo_html)

# for geom_image on field
transparent <- function(img) {
  magick::image_fx(img, expression = "0.5*a", channel = "alpha")
}

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
        selectInput(inputId = "team", label = "Select team(s)",
                    choices = team_list, multiple = TRUE)
      ),
      column(
        width = 4,
        selectInput(inputId = "player", label = "Select player(s)",
                    choices = player_list$player_name, multiple = TRUE)
      ),
      column(
        width = 4,
        selectInput(inputId = "stadium", label = "Select ballpark(s)",
                    choices = stadium_list, multiple = TRUE)
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
    updateSelectInput(inputId = "player",
                      choices = filter(player_list, player_team %in% input$team) %>%
                        pull(player_name))
  })
  
  hits <- reactive({
    
    # show all if any selections are blank
    if(is.null(input$team)){
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
      park_filter <- stadium_list
    } else {
      park_filter <- input$stadium
    }
    
    stadiums <- filter(stadium_list_detail, ballpark %in% park_filter) %>%
      pull(stadium)
    
    hits <- hit_data %>%
      filter(player_team %in% team_filter &
               player_name %in% player_filter &
               stadium_observed %in% stadiums) %>%
      left_join(team_logos, by = c("player_team" = "team_abbr")) %>%
      #mutate(launch_angle = glue("{launch_angle}\\degree")) %>%
      select(index, player_name, logo_html, game_date, stadium_observed, inning,
             events, launch_speed, launch_angle, hit_distance_sc, hit_direction)
    
    hits
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
        
        #div(glue("This hit would have gone yard in {dong_qty}/30 MLB ballparks"))
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
      
      parks <- filter(hits_new, index == hit_select) %>%
        mutate(would_dong = ifelse(would_dong == 1, "Yes", "No")) %>%
        left_join(stadium_logos, by = "stadium") %>%
        select(stadium, logo_html, would_dong) %>% arrange(stadium) %>% arrange(desc(would_dong))
      
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
      
      stadium_id <- hit_path %>%
        left_join(
          select(mlb_logos, team_abbr, team),
          by = c("player_team" = "team_abbr")
        ) %>%
        pull(team)
      
      stadium_logo <- hit_path %>%
        left_join(
          select(mlb_logos, team_abbr, team_logo),
          by = c("player_team" = "team_abbr")
        ) %>%
        pull(team_logo)
      
      curv <- pull(hit_path, spray_angle)/(-90)
      
      ggplot() +
        ggimage::geom_image(aes(x = 0, y = 250, image = stadium_logo),
                            size = 0.25, image_fun = transparent) +
        geom_mlb_stadium(stadium_ids = stadium_id,
                         stadium_segments = "all",
                         stadium_transform_coords = TRUE) +
        geom_point(data = hit_path, aes(hc_x_, hc_y_), size = 4)+
        geom_curve(
          data = hit_path,
          aes(x = 0, y = 0, xend = hc_x_, yend = hc_y_),
          curvature = curv, angle = 135, size = 2
        ) +
        theme_void() +
        coord_fixed()
      
    } else {
      
      hit_select <- hits()[s, , drop = FALSE] %>%
        pull(index)
      
      parks <- filter(hits_new, index == hit_select) %>%
        mutate(would_dong = ifelse(would_dong == 1, "Yes", "No")) %>%
        left_join(stadium_logos, by = "stadium") %>%
        select(stadium, logo_html, would_dong) %>% arrange(stadium) %>% arrange(desc(would_dong))
      
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
      
      hit_path <- hit_data %>%
        filter(index == hit_select)
      
      curv <- pull(hit_path, spray_angle)/(-90)
      
      ggplot() +
        ggimage::geom_image(aes(x = 0, y = 250, image = stadium_logo),
                            size = 0.25, image_fun = transparent) +
        geom_mlb_stadium(stadium_ids = stadium_id,
                         stadium_segments = "all",
                         stadium_transform_coords = TRUE) +
        geom_point(data = hit_path, aes(hc_x_, hc_y_), size = 4)+
        geom_curve(
          data = hit_path,
          aes(x = 0, y = 0, xend = hc_x_, yend = hc_y_),
          curvature = curv, angle = 135, size = 2
          ) +
        theme_void() +
        coord_fixed()
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
      
      hit_detail <- hit_data %>%
        filter(index == hit_select) %>%
        select(player_name, player_team, launch_speed, launch_angle, events, stadium_observed)
      
      park_name <- pull(hit_detail, stadium_observed)
      
      park_view <- filter(hits_new, index == hit_select & stadium == park_name) %>%
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
      
      parks <- filter(hits_new, index == hit_select) %>%
        mutate(would_dong = ifelse(would_dong == 1, "Yes", "No")) %>%
        left_join(stadium_logos, by = "stadium") %>%
        select(stadium, logo_html, would_dong) %>% arrange(stadium) %>% arrange(desc(would_dong))
      
      park_name <- parks[s2, , drop = FALSE] %>%
        pull(stadium)
      
      hit_detail <- hit_data %>%
        filter(index == hit_select) %>%
        select(player_name, player_team, launch_speed, launch_angle, events)
      
      park_view <- filter(hits_new, index == hit_select & stadium == park_name) %>%
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