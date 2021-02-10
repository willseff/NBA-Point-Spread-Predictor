library(dplyr)
library(rvest)
library(caret)
library(rpart.plot)
library(caret)
library(e1071)
library(purrr)
library(pROC)


# scrape data
games.2019 <- read_html('https://sportsdatabase.com/nba/query?output=default&sdql=team%2C+site%2C+o%3Ateam%2C+line%2C+streak%2C+margin%2C+wins%2C+losses+%40season%3D2019&submit=++S+D+Q+L+%21++') %>% html_table(fill=TRUE)

dat <- data.frame(games.2019[4])

# feature engineering

dat.b <- mutate(dat,
                win.p = wins/(wins+losses), 
                beat.line=ifelse(margin > line,'yes','no'), 
                margin=NULL)

# replace nan win percentages with 0.500
dat.b$win.p[is.nan(dat.b$win.p)] <- 0.5


# create training and testing sets

test.train.ratio <- 0.75

dat.b.training <- head(dat.b,nrow(dat.b)*test.train.ratio)
dat.b.testing <- tail(dat.b,nrow(dat.b)*(1-test.train.ratio))



# decision tree model

set.seed(123)
dt.model <- train(x = dat.b.training[,c(1:8)],
                  y = factor(dat.b.training$beat.line),
                  method = "rpart",
                  tuneLength = 10,
                  metric = "ROC",
                  trControl = trainControl(method = "cv",
                                           number = 10,
                                           classProbs = TRUE,
                                           summaryFunction = twoClassSummary))

rpart.plot <- rpart.plot(dt.model$finalModel, 
           main = "Original CART Model", 
           box.palette = "Reds",
           type=5)

dt.pred <- predict(dt.model, dat.b.testing)
dt.cm <- confusionMatrix(table(dat.b.testing$beat.line,dt.pred))

dt.importance <- varImp(dt.model, scale = FALSE)
plot(dt.importance, main = "Variable Importance")


# Naive Bayes Model

nb.model <- train(x = dat.b.training[,c(1:8)],
                  y = factor(dat.b.training$beat.line),
                  method = "nb",
                  tuneLength = 10,
                  metric = "ROC",
                  trControl = trainControl(method = "cv",
                                           number = 10,
                                           classProbs = TRUE,
                                           summaryFunction = twoClassSummary)) %>% invisible()

nb.pred <- predict(nb.model, dat.b.testing)
nb.cm <- confusionMatrix(table(nb.pred, dat.b.testing$beat.line))


# Random Forest Model

rv.model <- train(x = dat.b.training[,c(1:8)],
                  y = factor(dat.b.training$beat.line),
                  method = "ranger",
                  importance = "impurity",
                  tuneLength = 10,
                  metric = "ROC",
                  trControl = trainControl(method = "cv",
                                           number = 10,
                                           classProbs = TRUE,
                                           summaryFunction = twoClassSummary))


rv.pred <- predict(rv.model, dat.b.testing)
rv.cm <- confusionMatrix(table(rv.pred, dat.b.testing$beat.line))

rv.importance <- varImp(rv.model, scale = FALSE)
plot(importance, main = "Variable Importance")

# model comparison

models <- list(decision.tree = dt.model,
               naive.bayes = nb.model,
               random.forest = rv.model)

confusion.matrix <- list(dt.cm, nb.cm, rv.cm)

models.resampling <- resamples(models)

summary(models.resampling)

bwplot(models.resampling)




    
    