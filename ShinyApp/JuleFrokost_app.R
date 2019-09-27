#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/



library(ggplot2)
#library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(
  fluidRow(
    column(12,
           tableOutput(outputId = 'out1'),
           tableOutput(outputId = 'out2'),
           tableOutput(outputId = 'Score')

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
           uiOutput("TRY")
    )
    
  ),
fluidRow(
  ### Left Interval: al
  column(
    3,
    uiOutput("UI_Cur_L")  
    ),
  ### Right Interval: ar
  column(
    3,
    uiOutput("UI_Cur_R")  
  )
  ),
fluidRow(
  column(6,
         actionButton(inputId = "Cur_Submit", label = "Submit")
         ),
  column(6,
         # Show a plot of the generated distribution
         tableOutput("distPlot3"),
         plotOutput(outputId = 'distPlot1',height = 600))
)

)



# Define server logic
server <- function(input, output) {
  
  ########### Reactive team names taken from TextInput ####
  team_names <- reactive({
    sapply(1:(input$NoTeams), function(i) {
      req(input[[ paste0("team_", i)]]); #require, so that nothing us done untill all names are set
      input[[  paste0("team_", i)]]})
  }) 



  #Questions and corresponding answers, fixed! not reactive.
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
  df_Table_Scores <-    data.frame(
                            Team = character(0L), 
                            Question = character(0L), 
                            Try = character(0L), 
                            Points = character(0L)
                                  )

  
  output$out2 <- renderTable({
    input$Cur_Submit
    
    df_Table_Scores <- isolate(
                          rbind(df_Table_Scores, Cur_df_tpm()) 
                          )
    df_Table_Scores
  })
  
  #Plot data frame, containing the score after 0 <= n <= 16 tries.
  df_Plot <- reactive({
    #input$Teams_set_go
    req(input$NoTeams)
    req(team_names())
        
      data.frame(Team = team_names(),
                 Answes_spent = rep(0L,input$NoTeams),
                 Total_Score = rep(20480, input$NoTeams))
    })
  
  output$out1 <- renderTable({
    df_Plot()
  })


  

  
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
  
  ################################### input UI: team names, for entering team names #####
  output$TeamNamesUI <- renderUI({

      lapply(1:input$NoTeams,function(iter){
        column( floor(12 / input$NoTeams ),
          textInput(inputId = paste0("team_", iter), label = paste0("Name of team", iter))
        )
      })
  })
  
  ###################### UI: Reset Left and Right entries after submission
  output$UI_Cur_L <- renderUI({
    times <- input$Cur_Submit
    div(id=letters[(times %% length(letters)) + 1],
        passwordInput("Cur_L", 'Left Interval Quess', placeholder = 'Crypto_PassWord')
    )
  })
  
  #right
  output$UI_Cur_R <- renderUI({
    times <- input$Cur_Submit
    div(id=letters[(times %% length(letters)) + 1],
        passwordInput("Cur_R", 'Right Interval Quess', placeholder = 'Crypto_PassWord')
    )
  })
  
  
  ######################## Radiobuttons - For selecting the current team answering, based on given names
  output$TeamsRadioButtons <- renderUI({

    radioButtons(inputId = "Cur_Team", label = "Team", choices = team_names(), inline = TRUE, width = "500px", selected = NA)
    
  })
  
  ######################## renderUI - TRY - number of tries used
  output$TRY <- renderUI({
    
    input$Cur_Submit
 
    df_Table_Scores <- rbind(df_Table_Scores, Cur_df_tpm()) 
 
    
    print(paste0('nrowTable: ', nrow(df_Table_Scores)))
    
   if(nrow(df_Table_Scores) == 0){
    Max_Try <- 1
   } else Max_try <- 9
    
    selectInput(inputId = "Cur_Try", label = "TRY", choices = 1:Max_Try)
    

  })
  
  ############################## SCORE
  
  Cur_df_tpm <- reactive({
    
    #Go button 
    if(input$Cur_Submit == 0) return()
    
    input$Cur_Submit
    
    #Calculate Score, X if not correct
    isolate({
      
      validate(
        need(input$Cur_Team, 'Select a Team!!'),
        need(input$Cur_L, 'Select Left Interval!'),
        need(input$Cur_R, 'Select Right Interval!')
      )
      
      #Set the current question being answered and the correct value for ref
      Cur_Question <- input$Cur_Question
      Cur_Answer <-  df_QA[df_QA$question == Cur_Question, ]$answer

      Cur_L <- as.numeric(input$Cur_L)
      Cur_R <- as.numeric(input$Cur_R)
      
      if( Cur_L > Cur_R) return(warning("ERROR: LEFT BIGGER THAN RIGHT"))
      
      Score <- if( Cur_L <= Cur_Answer && Cur_Answer <= Cur_R){
          floor(Cur_R / Cur_L) 
        } else 'X'

      
      Cur_df_tpm <- data.frame(Team = input$Cur_Team, 
                               Question = input$Cur_Question, 
                               Try = input$Cur_Try, 
                               Score = as.character(Score))  
      
      
      Cur_df_tpm
    }) 
    
  })
  
  #Render the new df_
  
}

# Run the application
shinyApp(ui = ui, server = server)
