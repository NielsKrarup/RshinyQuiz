#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(ggplot2)
#library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  h1(paste0("JulefrokostQuiz ", 
            format(
              as.Date(Sys.Date(), format = "%d/%m/%Y"), "%Y"
            ),
            "  -   UCT+1:   ",
            format(
              Sys.time(), format = "%H:%M:%S"
            )
  )
  ),
  #Number of teams: NoTeams
  numericInput(inputId = 'NoTeams', label = 'Number of teams',value = 1,min = 1, max = 6),
  ##UI for setting team names.
  
  uiOutput('TeamNamesUI'),
  
  
  ##### Question inputs Sidebar with a slider input for number of bins
  fluidRow(
    column(12,
           h3("Submissons:")
    ),
    column(
      6,
      ##UI for selecting the team answering.
      uiOutput("TeamsRadioButtons"),
      
      ## Selecting the current question being answered
      selectInput(inputId = "CurQuestion", label = "Question",choices = 1:13))
  ),
fluidRow(
  ### Left Interval: al
  column(
    3,
    passwordInput("al", 'Left Interval Quess', placeholder = 'Crypto_PassWord')
  ),
  ### Right Interval: ar
  column(
    3,
    passwordInput("ar", 'Question 1, Left Interval', placeholder = 'CryptoPassWord')
  ),
  column(6,
         # Show a plot of the generated distribution
         textOutput(outputId = "text1"),
         tableOutput("distPlot3"),
         plotOutput(outputId = 'distPlot1',height = 600))
),
fluidRow(
  column(12,
         verbatimTextOutput(outputId = 'out1'),
         verbatimTextOutput(outputId = 'out2'),
         verbatimTextOutput(outputId = 'out3')
  )
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$distPlot1 <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- rnorm(100)
    y  <- x^2 + rnorm(100)/10
    df <- data.frame(x = x, y = y)
    
    # draw the histogram with the specified number of bins
    ggplot(data = df, aes(x = x, y = y) ) + geom_point()
  })
  
  output$distPlot2 <- renderPlot({
    plot(cars, pch = input$ans2)
  })
  
  output$out3 <- renderPrint({
    team_names <- c()
    for(i in 1:input$NoTeams){
      team_names <- c(team_names, input[[ paste0("team_",as.character(i))]])
    }
    team_names2 <- reactive(team_names)
    team_names2()
  })
  
  #### Interactive UI
  ## For setting team names; 
  output$TeamNamesUI <- renderUI({
    fluidRow(
      h3('Enter names of teams'),
      lapply(1:input$NoTeams,function(iter){
        column(11.8/input$NoTeams,
               column(12, textInput(inputId = paste0("team_", iter), label = paste0("Name of team", iter)))
        )
      })
    )
  })
  ## Radiobuttons - For selecting the team answering, based on given names
  output$TeamsRadioButtons <- renderUI({
    team_names <- c()
    for(i in 1:input$NoTeams){
      team_names <- c(team_names, input[[ paste0("team_",as.character(i))]])
    }
    radioButtons(inputId = "CurTeam", label = "Team", choices = team_names, inline = TRUE, width = "500px")
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
