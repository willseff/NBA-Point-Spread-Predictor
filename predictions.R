library(dplyr)
library(rvest)


# scrape upcoming games
dat <- read_html('https://sportsdatabase.com/nba/query?output=default&sdql=date%2C+team%2C+site%2C+o%3Ateam%2C+line%2C+streak%2C+margin%2C+wins%2C+losses%2C+o%3Astreak%2C+o%3Awins%2C+o%3Alosses+%40season%3D2020+and+site%3Daway+&submit=++S+D+Q+L+%21++') %>% html_table(fill=TRUE)

dat <- data.frame(dat[4])

summary(dat)
dat

games.2020 <- mutate(dat,
                win.p = wins/(wins+losses),
                o.win.p = o.wins/(o.wins+o.losses),
                margin = NULL,
                site = NULL)

games.2020$win.p[is.nan(games.2020$win.p)] <- 0.5
games.2020$o.win.p[is.nan(games.2020$o.win.p)] <- 0.5

games.2020

head(games.2020)

# change column to date type
games.2020$date <- as.Date(as.character(games.2020$date), format='%Y%m%d')
ind <- games.2020$date == Sys.Date()

# all the games today
today.games <- games.2020[ind,-c(1)]
today.games

# today's games predictions

dt.today.pred <- predict(models$random.forest, today.games, type='prob')
cbind(today.games,dt.today.pred)
        
        