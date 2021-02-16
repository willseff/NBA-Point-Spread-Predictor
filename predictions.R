library(dplyr)
library(rvest)


# scrape upcoming games
games.2020 <- read_html('https://sportsdatabase.com/nba/query?output=default&sdql=date%2C+team%2C+site%2C+o%3Ateam%2C+line%2C+streak%2C+margin%2C+wins%2C+losses+%40season%3D2020+&submit=++S+D+Q+L+%21++') %>% html_table(fill=TRUE)

dat <- data.frame(games.2020[4])

# change column to date type
dat$date <- as.Date(as.character(dat$date), format='%Y%m%d')
ind <- dat$date == Sys.Date()

# all the games today
today.games <- dat[ind,-c(1)]

# drop margin column
today.games$margin <- NULL

today.games$win.p = today.games$wins/(today.games$wins + today.games$losses)

# change data types to match the training set
lapply(dat.b.training, class)
lapply(today.games, class)

# today's games predictions
rv.pred <- predict(models$decision.tree, today.games)

today.games$line <- as.numeric(today.games$line)
today.games$streak <- as.integer(today.games$streak)

dt.today.pred <- predict(models$decision.tree, today.games)


        
        