library(dplyr)
library(rvest)


# scrape upcoming games
games.2020 <- read_html('https://sportsdatabase.com/nba/query?output=default&sdql=date%2C+team%2C+site%2C+o%3Ateam%2C+line%2C+streak%2C+margin%2C+wins%2C+losses+%40season%3D2020+&submit=++S+D+Q+L+%21++') %>% html_table(fill=TRUE)

games.2020 <- data.frame(games.2020[4])

# change column to date type
games.2020$date <- as.Date(as.character(games.2020$date), format='%Y%m%d')
ind <- games.2020$date == Sys.Date()

# drop margin column
games.2020$margin <- NULL

# change data types to match the training set
games.2020$line <- as.numeric(games.2020$line)
games.2020$streak <- as.integer(games.2020$streak)

# create win.p predictor
games.2020$win.p <- games.2020$wins/(games.2020$wins + games.2020$losses)

# all the games today
today.games <- games.2020[ind,-c(1)]

# today's games predictions

dt.today.pred <- predict(models$random.forest, today.games, type='prob')
cbind(today.games,dt.today.pred)
        
        