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
  fluidRow(
    column(12,
           verbatimTextOutput(outputId = 'out1'),
           verbatimTextOutput(outputId = 'Score'),
           verbatimTextOutput(outputId = 'out3')
    )
  ),
  # Application title
  h1(paste0("JulefrokostQuiz ", 
            format(
              as.Date(Sys.Date(), format = "%d/%m/%Y"), "%Y"
            ),
            "  -   UCT+1:   ",
            format(
              Sys.time(), format = "%H:%M:%S"
            ))
  ),
  #Number of teams: NoTeams
  numericInput(inputId = 'NoTeams', label = 'Number of teams',value = 1,min = 1, max = 6),
  
  ## For printing
  
  ##UI for setting team names.
  
  fluidRow(
    br(),
    uiOutput('TeamNamesUI')
  ),

  
  
  ##### Question inputs Sidebar with a slider input for number of bins
  fluidRow(
           h3("Submissons:")
    ),
  fluidRow(
    column(
      6,
                    ##UI for selecting the team answering.
      uiOutput("TeamsRadioButtons")
      )
    ),
  fluidRow(
    column(3,
                    ## Selecting the current question being answered
      selectInput(inputId = "Cur_Question", label = "Question",choices = 1:13)
      ),
    column(3,
           ## Selecting the current TRY  (for correcting errors)
           ## Should look up from df_info to see number of tries made.
           selectInput(inputId = "Cur_Try", label = "TRY",choices = 1:3)
    )
    
  ),
fluidRow(
  ### Left Interval: al
  column(
    3,
    passwordInput("Cur_L", 'Left Interval Quess', placeholder = 'Crypto_PassWord')
  ),
  ### Right Interval: ar
  column(
    3,
    passwordInput("Cur_R", 'Right Interval Quess', placeholder = 'Crypto_PassWord')
  )
  ),
fluidRow(
  column(6,
         actionButton(inputId = "Cur_submit", label = "Submit")
         ),
  column(6,
         # Show a plot of the generated distribution
         tableOutput("distPlot3"),
         plotOutput(outputId = 'distPlot1',height = 600))
)

)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ########### Get Reactive team names
  team_names <- reactive({
    sapply(1:(input$NoTeams), function(i) {
      req(input[[ paste0("team_", i)]]);
      input[[  paste0("team_", i)]]})
  }) 



  #Questions and corresponding answers
  df_QA <- data.frame(question = 1:11,
                      answer = c(
                        193, #Snurre snup
                        10918, #Mugabe
                        NA, #3
                        NA, #4
                        NA, #5
                        NA, #6
                        NA, #7
                        NA, #8
                        NA, #9
                        NA, #10
                        NA #11
                                ) 
                        )
  #Score Info Data frame, Reactive
  df_Info <- reactive({
    data.frame(Team = character(0), 
                        Question = character(0), 
                        Try = character(0), 
                        Score = character(0))
  })
  
  #Plot data frame, containing the score after 0 <= n <= 16 tries.
  df_Plot <- reactive({
    #input$Teams_set_go
    
    isolate({
      data.frame(Team = "team_names2()",
                 Answes_spent = rep(0,input$NoTeams),
                 Score = rep(20480, input$NoTeams))
    })

  }) 

  Cur_Info <- data.frame(Team = 'PIK', Question = 2, Try = 1, Answers_Left = 15, Score = 'X')  
  

  
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
  

  
  #### Interactive UI
  
  ################################### For setting team names; 
  output$TeamNamesUI <- renderUI({

      lapply(1:input$NoTeams,function(iter){
        column( floor(12 / input$NoTeams ),
          textInput(inputId = paste0("team_", iter), label = paste0("Name of team", iter))
        )
      })
  })
  
  
  ######################## Radiobuttons - For selecting the current team answering, based on given names
  output$TeamsRadioButtons <- renderUI({

    radioButtons(inputId = "Cur_Team", label = "Team", choices = team_names(), inline = TRUE, width = "500px", selected = NA)
    
  })
  
  output$Score <- renderText({
    #Go button 
    if (input$Cur_submit == 0)
      return()
    input$Cur_submit
    
    #Calculate Score, X if not correct
    Score <- reactive({
      Cur_Question <- input$Cur_Question
      Cur_Answer <-  df_QA[df_QA$question == Cur_Question, ]$answer
      
      
      
      Cur_L <- as.numeric(input$Cur_L)
      Cur_R <- as.numeric(input$Cur_R)
      
      if( Cur_L > Cur_R) return(warning("ERROR: LEFT BIGGER THAN RIGHT"))
      
      Score <- 
        if( Cur_L <= Cur_Answer && Cur_Answer <= Cur_R){
          Score <- floor(Cur_R / Cur_L) 
        } else 'X'
      Score
    })
    isolate( Score() )
   


  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
