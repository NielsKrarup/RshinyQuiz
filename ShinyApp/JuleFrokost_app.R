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
           tableOutput(outputId = 'table1'),
           tableOutput(outputId = 'table2'),
           tableOutput(outputId = 'table3'),
           textOutput(outputId = 'text1')
           #verbatimTextOutput(outputId = 'showInputs'
    )
  ),
  # Application title
  uiOutput("appTitleUI"),
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
         actionButton(inputId = "Cur_Submit", label = "Submit"),
         verbatimTextOutput("Validate_Submit")
  ),
  column(6,
         # Show a plot of the generated distribution
         tableOutput("distPlot3"),
         plotOutput(outputId = 'distPlot1',height = 600))
)

)



# Define server logic
server <- function(input, output, session) {
  
  ########### Reactive team names taken from TextInput ####
  team_names <- reactive({
    sapply(1:(input$NoTeams), function(i) {
      req(input[[ paste0("team_", i)]]); #require, so that nothing us done untill all names are set
      input[[  paste0("team_", i)]]})
  }) 
  

  #Validate Submit button
  #NOTE THIS ONLY VALIDATES LOCALLY FOR THE OUTPUT, REMEMBER TO VALIDATE IN CRITIAL CODE PARTS
  output$Validate_Submit <- renderText({
    
    test <-  eventReactive(input$Cur_Submit,{
      validate(
        need(input$Cur_Team,'HEJ Team navn mangler!'),
        need(input$Cur_L, 'LEFT QUESS MISSING'),
        need(input$Cur_R, 'RIGHT QUESS MISSING'),
        need(as.numeric(input$Cur_L) <= as.numeric(input$Cur_R), 'LEFT <= RIGHT' )
      )
    })
    
    test()
    
  })

  
  #Questions and corresponding answers, fixed! not reactive.
  df_QA <- data.frame(Question = 1:11,
                      Answer = c(
                        193, #1: Snurre snup
                        10918, #2: Mugabe
                        NA, #3
                        1015135770, #4: triple Integral
                        547.2246, #5: US state size prop
                        NA, #6: Sum of OL records
                        1554, #7: Dices first occurence of 4 six'es
                        514, # Lowest Highest Point in South America
                        NA, #9
                        NA, #10
                        NA #11
                                ) 
                        )

  #Score Info Data frame, Reactive
  values <- reactiveValues()
  
  values$df_Table_Scores <- data.frame(stringsAsFactors = FALSE,
                      ID = numeric(0L),                 
                      Team = character(0L), 
                      Question = integer(0L), 
                      Try = integer(0L), 
                      Points = character(0L)
                    )
  
  observe({
    #input$Teams_set_go
    req(input$NoTeams)
    req(team_names())
    
    values$df_plot <- data.frame(Team = team_names(),
                                 Answers_Spent = rep(0L,input$NoTeams),
                                 Total_Score = rep(20480, input$NoTeams))
  })


  #Score table
  output$table1 <- renderTable({

    values$df_Table_Scores
  })
  
  #Plot data frame, containing the score after 0 <= n <= 16 tries.

  
  #Output_Plot table
  output$table2 <- renderTable({
    req(input$NoTeams)
    req(team_names())
    values$df_plot
  })
  

  
  output$distPlot1 <- renderPlot({
    #make it dependent on submissions
    req(team_names())
    # draw the histogram with the specified number of bins
    ggplot(data = values$df_plot, aes(x = Answers_Spent, y = Total_Score, group = Team) ) + 
      geom_line(aes(colour = Team)) + 
      geom_point(aes(col = Team), size = 2) +
      scale_x_continuous(breaks = 0:16, limits = c(0,16))
  })
  

  

  
  #### Interactive UI
  # get all inputs for reference 
  
  output$showInputs <- renderPrint({
    n <- length(input)
    lapply(X = 1:n, FUN = function(i) input[[paste(i)]])
    input
  })
  
  # Title UI ----
  # Application title, with clock 
  output$appTitleUI <- renderUI({
    invalidateLater(1000, session)
    h1(paste0("JulefrokostQuiz ", 
              format(
                as.Date(Sys.Date(), format = "%d/%m/%Y"), "%Y"
              ),
              "  -   UCT+1:   ",
              format(
                Sys.time(), format = "%H:%M:%S"
              ))
    )
  })
  
  ################################### input UI: team names, for entering team names #####
  output$TeamNamesUI <- renderUI({

      lapply(1:input$NoTeams, function(iter){
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

    req(input$Cur_Question, input$Cur_Team)

    print(paste0('nrow df_Table_Scores: ', nrow(values$df_Table_Scores)))
    
    tmp_df_subset <- values$df_Table_Scores[values$df_Table_Scores$Question == as.numeric(input$Cur_Question) &
                                            values$df_Table_Scores$Team == input$Cur_Team, ]
    
    print(paste0('nrowTable_subset: ', nrow(tmp_df_subset)))
    
   if(nrow(tmp_df_subset) == 0){
    Max_Try <- 1
   } else Max_Try <- nrow(tmp_df_subset) + 1
    
    selectInput(inputId = "Cur_Try", label = "TRY", choices = 1:Max_Try, selected = Max_Try)
    
  })
  
  ############################## SCORE ----

  
    observeEvent( input$Cur_Submit,{
    
    #Go button 
    if(input$Cur_Submit == 0) return()

    validate(
      need(input$Cur_Team, label = "Provide Team Name "),
      need(input$Cur_L, label = "Provide Left Quess"),
      need(input$Cur_R, label = "Provide Right Quess"),
      need(as.numeric(input$Cur_L) <= as.numeric(input$Cur_R), 'must have: LEFT <= RIGHT' )
    )

    
    #Calculate Score, X if not correct, character

      #Set the current question being answered and the correct value for ref
      Cur_Question <- input$Cur_Question
      Cur_Answer <-  df_QA[df_QA$Question == Cur_Question, ]$Answer

      Cur_L <- as.numeric(input$Cur_L)
      Cur_R <- as.numeric(input$Cur_R)
      
      if( Cur_L > Cur_R) return(warning("ERROR: LEFT BIGGER THAN RIGHT"))
      
      Points <- if( Cur_L <= Cur_Answer && Cur_Answer <= Cur_R){
          floor(Cur_R / Cur_L) 
        } else 'X'

    #Update the score table
      
      #if the try is not max:
      tmp_df_subset_Try <- values$df_Table_Scores[values$df_Table_Scores$Question == as.numeric(input$Cur_Question) &
                                                values$df_Table_Scores$Team == input$Cur_Team, "Try"]

      #Correcting 
      if(max(c(tmp_df_subset_Try,0)) == input$Cur_Try){ 
        print("SameTRY")
        #override point
        values$df_Table_Scores[values$df_Table_Scores$Question == as.integer(input$Cur_Question) &
                                 values$df_Table_Scores$Team == input$Cur_Team &
                                 values$df_Table_Scores$Try == as.integer(input$Cur_Try)
                                 , "Points"] <- as.character(Points) 
      }else{
              values$df_Table_Scores <- rbind(
                data.frame(stringsAsFactors = FALSE,
                ID = as.integer(input$Cur_Submit),
                Team = input$Cur_Team,
                Question = as.integer(input$Cur_Question),
                Try = as.integer(input$Cur_Try),
                Points = as.character(Points)
              ), 
              values$df_Table_Scores)
        }
      
      #Render the new df_plot /Total score


        #Update the total score after submittion, for plotting
         name <- input$Cur_Team
         
           df_tmp_teamsubset <- values$df_Table_Scores[values$df_Table_Scores$Team == name, ]
           #Answers spent
           tmp_team_Answers_Spent <- nrow(df_tmp_teamsubset)
           
           #The questions answered
           tmp_Q <- df_tmp_teamsubset$Question
           print(tmp_Q)
           
           tmp_team_numcorrect_counter <- 0
           tmp_team_sum_points <- 0

           for(k in unique(tmp_Q)){

             print(head(df_tmp_teamsubset[df_tmp_teamsubset$Question == k, "Points"],1))
             #The latest points received
             point <- head(df_tmp_teamsubset[df_tmp_teamsubset$Question == k, "Points"],1)
             
             if(point != "X"){
               tmp_team_numcorrect_counter <- tmp_team_numcorrect_counter + 1
               tmp_team_sum_points <- tmp_team_sum_points + as.numeric(point)
             }
             
           }
        #Update the total score
        new_Total_Score <-    (10 + tmp_team_sum_points)*2^(11 - tmp_team_numcorrect_counter )
        #insert in data_table
        values$df_plot <- rbind(
          data.frame(stringsAsFactors = FALSE,
                     Team = name,
                     Answers_Spent = tmp_team_Answers_Spent,
                     Total_Score = new_Total_Score
                      ),
          values$df_plot)
        
  })#Observe event
  

  
}#server fun end

# Run the application
shinyApp(ui = ui, server = server)
