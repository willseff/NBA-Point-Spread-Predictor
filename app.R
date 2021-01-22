library(caret)
library(dplyr)
library(rvest)
library(caret)
library(rpart.plot)
library(caret)
library(e1071)
library(dplyr)
library(purrr)
library(pROC)
library(klaR)
library(ranger)
load('models.RData')

ui <- fluidPage(
  titlePanel('Over / Under Moneyline Prediction'),
  sidebarPanel(" Select model to use on the right then input data on the teams and prediction will be made on whether the team beats the spread "),
  sidebarPanel(
  radioButtons("model", "Model:",
                     names(models), inline=TRUE),
  selectInput('team','Team',unique(dat$team)),
  selectInput('site','Site',unique(dat$site)),
  selectInput('o.team','Opposing Team',unique(dat$team)),
  numericInput("line", "Line:", 0, step=0.5),
  numericInput("streak", "Streak:", 0, step=1),
  numericInput("wins", "Wins:", 0, min = 0, step=1),
  numericInput("losses", "Losses:", 0, min = 0, step=1),
  ),
  h4('Probablity of Beating Moneyline'),
  tableOutput("data")
)

server <- function(input, output, session) {
  output$data <- renderTable({
    pred.i <- data.frame (team = input$team,
                          site = input$site,
                          o.team = input$o.team,
                          line = input$line,
                          streak = input$streak,
                          wins = input$wins,
                          losses = input$losses,
                          win.p = ifelse(is.nan(input$wins/(input$wins+input$losses)),
                                         0.5,
                                         input$wins/(input$wins+input$losses)))
    data.frame(predict(models[input$model],pred.i,type='prob'))
    
  })

}

shinyApp(ui, server, options = list(height = 500))
