load('models.RData')

ui <- fluidPage(
  titlePanel('Over / Under Moneyline Prediction'),
  sidebarPanel(" Select model to use on the right then input data on the teams and prediction will be made on whether the team beats the spread "),
  sidebarPanel(
    radioButtons("model", "Model:",
                 names(models), inline=TRUE),
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