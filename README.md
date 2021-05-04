# The Dinger Machine

An app to test whether any batted ball would've been a homer in another park.

Updated with all batted balls that traveled at least 300 feet, the day after the game.

Ever see a deep fly ball that doesn't quite make it over the 25ft wall in dead center of Chase Field and think, _that would have been a homer if we weren't playing in this stadium_? Or maybe it's a line drive that taps the upper quarter of the green monster at Fenway Park, or this 425 ft double from Kyle Seager:

https://user-images.githubusercontent.com/44938476/116891297-61b35280-abe3-11eb-89e7-0ad2f915aef4.mp4

As it turns out, there is enough tracking data in baseball to actually test these theories! Right now you can look at how many home runs would've been "no-doubters" (out of every ballpark) at [Baseball Savant](https://baseballsavant.mlb.com/leaderboard/home-runs), but it doesn't give very granular data in regards to specific hits, or exactly how many parks each home run would've been out at. There's also [The No Doubter Report](https://thedataface.com/2019/09/sports/no-doubter-report) which gives data on specific home runs (along with some spectacular visualizations) but doesn't appear to account for deep fly balls that didn't leave the yard, but may have in another stadium, and also appears to no longer be up to date.

### Enter: The Dinger Machine.

The Dinger Machine looks at every batted ball that travels at least 300 feet in the air and tests the home-run-ability of the hit across all 30 Major League Baseball parks. Statcast provides the exit velocity, launch angle, and height of the ball when it crosses the plate for every batted ball in the league. It also tracks the x-y coordinates of the landing location of each ball, though it should be noted that these are not always perfectly accurate. In general though, they do point to the direction of the hit. All of these statistics are readily available for every game on the following day at [Baseball Savant](https://baseballsavant.mlb.com/statcast_search).

```{r echo = FALSE}
library(tidyverse)
hit_data <- read_csv("https://raw.githubusercontent.com/danmorse314/dinger-machine/main/data/hit_data.csv", col_types = cols()) %>%
janitor::clean_names() %>%
select(player_name, launch_speed, launch_angle, plate_z, hc_x, hc_y) %>%
head()
```
