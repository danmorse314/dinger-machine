# The Dinger Machine

_Try it out here!_

https://danmorse.shinyapps.io/the-dinger-machine/

Ever see a deep fly ball that doesn't quite make it over the 25ft wall in dead center of Chase Field and think, _that would have been a homer if we weren't playing in this stadium_? Or maybe it's a line drive that taps the upper quarter of the green monster at Fenway Park, or this 425 ft double from Kyle Seager:

https://user-images.githubusercontent.com/44938476/116891297-61b35280-abe3-11eb-89e7-0ad2f915aef4.mp4

As it turns out, there is enough tracking data in baseball to actually test these theories! Right now you can look at how many home runs would've been "no-doubters" (out of every ballpark) at [Baseball Savant](https://baseballsavant.mlb.com/leaderboard/home-runs), but it doesn't give very granular data in regards to specific hits, or exactly how many parks each home run would've been out at [EDIT (2021/05/06): Baseball Savant does, in fact, give granular data re:specific parks each hit would be gone at, but NEVERTHELESS]. There's also [The No Doubter Report](https://thedataface.com/2019/09/sports/no-doubter-report) which gives data on specific home runs (along with some spectacular visualizations) but doesn't appear to account for deep fly balls that didn't leave the yard but may have in another stadium, and also appears to no longer be up to date.

## Enter: The Dinger Machine.

The Dinger Machine looks at every batted ball that travels at least 300 feet in the air and tests the home-run-ability of the hit across all 30 Major League Baseball parks. Statcast provides the exit velocity, launch angle, and height of the ball when it crosses the plate for every batted ball in the league. It also tracks the x-y coordinates of the landing location of each ball, though it should be noted that these are not always perfectly accurate. In general though, they do point to the direction of the hit. All of these statistics are readily available for every game on the following day at [Baseball Savant](https://baseballsavant.mlb.com/statcast_search).

The other necessary data for these calculations is the distance to the wall and the height of the wall at each point in the outfield. The R package [GeomMLBStadiums](https://github.com/bdilday/GeomMLBStadiums), a tool that makes it easy to plot statcast data on a given baseball field, provides 500 points of data to outline each field. The outfield wall isn't explicitly given in this data, but it is possible to infer with some manipulation.

![outifeld_wall_plots](https://github.com/danmorse314/dinger-machine/blob/main/images/outfield_walls.png)

Each outfield wall has between 40 and 46 data points to use. The angle from home plate to each point on the wall was calculated in the same manner described [here](https://tht.fangraphs.com/research-notebook-new-format-for-statcast-data-export-at-baseball-savant/) to calculate the spray angle of a given hit using Statcast data. In the calculations for each hit at each stadium, the spray angle of the hit was rounded to the nearest outfield wall spray angle. Using that point along the wall, the distance from home plate to the wall could then be determined.

The only thing that remains is to find the height of the ball when it reaches the fence (using some fancy [projectile motion equations](https://www.omnicalculator.com/physics/projectile-motion#projectile-motion-equations)) and compare it to the height of the fence at that particular spot. Data was gathered from [Clem's Baseball](http://www.andrewclem.com/Baseball/Stadium_statistics.html) as well as official MLB team sites where available. Using plots from the GeomMLBStadiums package and comparing it to publicly available images from each outfield, estimates were made for the height of each wall at each available point. The points at which the height of the wall changes may be slightly off, but for the majority of batted balls they should be correct.

![fence_heights](https://github.com/danmorse314/dinger-machine/blob/main/images/fence_heights.png)

With all of those calculations done, each batted ball now has an estimate for whether or not it would've sailed over the wall at every ballpark. This is all entered in to an app made with [R Shiny](https://github.com/rstudio/shiny), the code for which can be found in [this repository](https://github.com/danmorse314/dinger-machine/).

Enjoy, and as always, [goms](https://www.youtube.com/watch?v=TIgK56cAjfY)

![seager_double](https://github.com/danmorse314/dinger-machine/blob/main/images/seager_double.png)
