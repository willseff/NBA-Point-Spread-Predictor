library(dplyr)
library(rvest)


# scrape upcoming games
games.2020 <- read_html('https://sportsdatabase.com/nba/query?output=default&sdql=date%2C+team%2C+site%2C+o%3Ateam%2C+line%2C+streak%2C+margin%2C+wins%2C+losses+%40season%3D2020+&submit=++S+D+Q+L+%21++') %>% html_table(fill=TRUE)

dat <- data.frame(games.2020[4])

# change column to date type
dat$date <- as.Date(as.character(dat$date), format='%Y%m%d')
ind <- dat$date == Sys.Date()

# all the games today
dat[ind,-c(1)]
