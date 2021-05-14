### STILL UNDER CONSTRUCTION
### TWEETS ARE CURRENTLY MANUALLY CURATED, AUTOMATION COMING SOON

This folder contains the scripts that power the [Would it dong?](https://twitter.com/would_it_dong) twitter account.

The [dong_bot.R](https://github.com/danmorse314/dinger-machine/blob/main/dong-bot/dong_bot.R) file pulls play-by-play data for every live MLB game using the [baseballr](https://github.com/BillPetti/baseballr) package. Each hit is analyzed with the functions in [dong_bot_functions.R](https://github.com/danmorse314/dinger-machine/blob/main/dong-bot/dong_bot_functions.R). Any hit found to have been deep enough and high enough to be a home run at at least one MLB stadium gets automatically tweeted out by [@would_it_dong](https://twitter.com/would_it_dong), and play ids are saved after tweeting to prevent multiple posts of the same hit.

![example_tweet](https://github.com/danmorse314/dinger-machine/blob/main/dong-bot/example_tweet.png)
