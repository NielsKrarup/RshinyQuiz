#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# Git: v1.0


library(ggplot2)
library(DT)
getwd()
source(file = "answers.R")

#Hardcoded values
Max_No_Guesses <- 16
pic_name <- "JMB"
#library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(
  tabsetPanel(
  tabPanel("Intro-Setup",

  # Application title
  uiOutput("appTitleUI"),
  
  #Number of teams: NoTeams
  numericInput(inputId = 'NoTeams', label = 'Number of teams',value = 1, min = 1, max = 6),
  #Questions used
  radioButtons(inputId = "year_questions", label = "Questions", choices = c("2018", "2019", "2020", "2021"), selected = "2021"),
  
  ##UI for setting team names.
  
  fluidRow(
    br(),
    uiOutput('TeamNamesUI')
  ),
  fluidRow(
    br(),
    column(10,
           imageOutput("image2")
           ),
    column(2,
           p("Jean-Michel Basquiat"),
           p('Boy and Dog in a Johnnypump (1982)'),
           p('Art Institute of Chicago')
           )
  )
  ),
  

# TapPanel for Scores & Submission  -----------------------------------------------------

  
  tabPanel("Submissions - Score",

           fluidRow(
             h3("Submissons:")
           ),
  
  ##### Question inputs Sidebar with a slider input for number of bins
column(width = 4, #overall colum
  fluidRow(
    column(
      10,
      ##UI for selecting the team answering.
      uiOutput("TeamsRadioButtons")
      )
    ),
  fluidRow(
    column(6,
      ## Selecting the current question being answered
      selectInput(inputId = "Cur_Question", label = "Question", choices = 1:11)
      ),
    column(4,
           ## Selecting the current TRY  (for correcting errors)
           ## Should look up from df_info to see number of tries made.
           uiOutput("TRY")
    )
    
  ),
fluidRow(
  ### Left Interval: al
  column(
    5,
    uiOutput("UI_Cur_L")  
    ),
  ### Right Interval: ar
  column(
    5,
    uiOutput("UI_Cur_R")  
  )
  ),
fluidRow(
  column(1,
         actionButton(inputId = "Cur_Submit", label = "Submit"),
         verbatimTextOutput("Validate_Submit")
  ),
  column(offset = 1, 1,
         actionButton(inputId = "Cur_Delete", label = "Delete Selected")
         )
  ),
  fluidRow(
    column(8,
           #leader board
           h3('Leader Board'),
           DT::dataTableOutput(outputId = "table_leaderboard", width = "125%"),
           br(),
           #table1 df_table_scores
           h3('10 latest submissions'),
           tableOutput(outputId = 'table1')
           # df_plot table
           #tableOutput(outputId = 'table2'),

    ))),
    column(width = 8, align = 'left',
         # Show a plot of the generated distribution
         plotOutput(outputId = 'distPlot1',height = 600), class = 'leftAlign')
  )

)
)




# Define server logic
server <- function(input, output, session){
  
  # pics
  output$image2 <- renderImage({

    invalidateLater(1000, session)

    sec <- as.numeric(format(
      Sys.time(), format = "%S"
    )) %% 10
    sample <- as.logical(runif(1) < 0.5)
    
    if (sec == 9 && sample == 0) {
      return(list(
        src = "Pics/Thyge.jpg",
        contentType = "image/jpeg",
        alt = "Face",
        style="display: block; margin-left: auto; margin-right: auto;"
      ))
    } else {
      return(list(
        src = paste0("Pics/", pic_name, ".jpg"),
        width = "1100",
        filetype = "image/jpeg",
        alt = "This is a chainring",
        style="display: block; margin-left: auto; margin-right: auto;"
      ))
    }
    
  }, deleteFile = FALSE)
  
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
      
      tmp <-  max(values$df_plot[values$df_plot$Team == input$Cur_Team, "Answers_Spent"],0)
      
      validate(
        need(tmp <= Max_No_Guesses-1, "ALL Answers Spent"),
        need(input$Cur_Team,'HEJ Team navn mangler!'),
        need(input$Cur_L, 'LEFT QUESS MISSING'),
        need(input$Cur_R, 'RIGHT QUESS MISSING'),
        need(as.numeric(input$Cur_L) <= as.numeric(input$Cur_R), 'LEFT <= RIGHT' )
      )
    })
    
    test()
    
  })

  

  #Score Info Data frame, Reactive
  values <- reactiveValues()
  
  values$Max_Try <- numeric(0L)
  
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
    #Latest scores
    df <- values$df_Table_Scores
    head(df, 10)
  })
  
  #Plot data frame, containing the score after 0 <= n <= 16 tries.

  
  #Output_Plot table, not shown
  output$table2 <- renderTable({
    req(input$NoTeams)
    req(team_names())
    values$df_plot
  })
  
  #Leader board
  output$table_leaderboard <- DT::renderDataTable({
    req(input$NoTeams)
    req(team_names())
    
    leader_table <- values$df_plot[match(unique(values$df_plot$Team),values$df_plot$Team), ]
    leader_table <- leader_table[order(leader_table$Total_Score, decreasing = F),]
    
    DT::datatable(leader_table, options = list(dom = 't'), filter = "none")
    

  })
  

#### plot of scores ----  
  output$distPlot1 <- renderPlot({
    #make it dependent on submissions
    req(team_names())
    # draw the histogram with the specified number of bins
    g <- ggplot(data = values$df_plot, aes(x = Answers_Spent, y = Total_Score, group = Team) ) + 
      geom_line(aes(colour = Team), size = 1) + 
      geom_point(aes(col = Team, shape = Team), size = 3) +
      scale_x_continuous(breaks = 0:16, limits = c(0,16)) + scale_y_continuous(trans = "log2") +
      
      theme(axis.title = element_text(size = 12),
            axis.text = element_text(size = 10),
            legend.title = element_text(size = 16, face = "bold"),
            legend.text=element_text(size = 14),
            legend.key.size = unit(3, "line"),
            #legend position
            legend.position = 'top', 
            legend.direction = "horizontal", 
            legend.box = "horizontal"
            ) + 
      guides(shape = guide_legend(override.aes = list(size = 3)),
             color = guide_legend(title.position = "top", title.hjust = 0.5)
             )
    #supress only one group warning,(when only one team has been made)
    suppressWarnings(g)
  })
  

  

  
  #### Interactive UI

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

    tmp_df_subset <- values$df_Table_Scores[values$df_Table_Scores$Question == as.numeric(input$Cur_Question) &
                                            values$df_Table_Scores$Team == input$Cur_Team, ]
    
   if(nrow(tmp_df_subset) == 0){
    values$Max_Try <- 1
   } else values$Max_Try <- nrow(tmp_df_subset) + 1
    
    selectInput(inputId = "Cur_Try", label = "TRY", choices = 1:values$Max_Try, selected = values$Max_Try)
    
  })

  
  #Delete 
  
  observeEvent(input$Cur_Delete,{
    
    if(input$Cur_Delete == 0) return()
    
    #Get the Row in score data frame 
    #remove >= cur_try, recall iterates from above i df
    index_rm <- (values$df_Table_Scores$Question == as.integer(input$Cur_Question) &
                   values$df_Table_Scores$Team == input$Cur_Team &
                   values$df_Table_Scores$Try == as.integer(input$Cur_Try))
    #if index_rm is only false i.e. only most recent
    if(any(index_rm)){
      values$df_Table_Scores <- values$df_Table_Scores[!index_rm,]
    }else warning('Some delete error , could not finde row to remove')
    
    #Update plot
    
    #Render the new df_plot / Total score
    
    #Update the total score after submittion, for plotting
    name <- input$Cur_Team
    
    #Subset data by current team
    df_tmp_teamsubset <- values$df_Table_Scores[values$df_Table_Scores$Team == name, ]
    #Answers spent, this will be ONE lower after deletion
    tmp_team_Answers_Spent <- nrow(df_tmp_teamsubset)
    
    #The questions answered
    tmp_Q <- df_tmp_teamsubset$Question
    
    tmp_team_numcorrect_counter <- 0
    tmp_team_sum_points <- 0
    
    for(k in unique(tmp_Q)){
      
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
    
    #override point of try and remove "above"
      values$df_plot[values$df_plot$Team == name &
                       values$df_plot$Answers_Spent == tmp_team_Answers_Spent
                     , "Total_Score"] <- new_Total_Score
      
      #remove >= tmp_team_Answers_Spent, recall iterates from above i df
      index_rm <- (values$df_plot$Team == name &
                     values$df_plot$Answers_Spent == tmp_team_Answers_Spent + 1)
      #if index_rm is only false i.e. only most recent
      if(any(index_rm)){
        values$df_plot <- values$df_plot[!index_rm,]
      } else warning(' Delete error 2')
  
  })
    
  ############################## SCORE ----
  
    observeEvent( input$Cur_Submit,{
    
    #Go button 
    if(input$Cur_Submit == 0) return()
      
      tmp_max_ans <-  max(values$df_plot[values$df_plot$Team == input$Cur_Team, "Answers_Spent"],0)

    validate(
      need(tmp_max_ans <= Max_No_Guesses - 1 , 'All ans spent No2'),
      need(input$Cur_Team, label = "Provide Team Name "),
      need(input$Cur_L, label = "Provide Left Quess"),
      need(input$Cur_R, label = "Provide Right Quess"),
      need(as.numeric(input$Cur_L) <= as.numeric(input$Cur_R), 'must have: LEFT <= RIGHT' )
    )
    
    #Questions and corresponding answers, fixed! not reactive.
    df_QA <- answers_list[[input$year_questions]]

    #Calculate Score, X if not correct, character

      #Set the current question being answered and the correct value for ref
      Cur_Question <- input$Cur_Question
      Cur_Answer <-  df_QA[df_QA$Question == Cur_Question, ]$Answer

      Cur_L <- as.numeric(input$Cur_L)
      Cur_R <- as.numeric(input$Cur_R)
      
      Points <- if( Cur_L <= Cur_Answer && Cur_Answer <= Cur_R){
          floor(Cur_R / Cur_L) 
        } else 'X'

    #Update the score table
      
      #Get try. for if the try is not max:
      tmp_df_subset_Try <- values$df_Table_Scores[values$df_Table_Scores$Question == as.numeric(input$Cur_Question) &
                                                values$df_Table_Scores$Team == input$Cur_Team, "Try"]

      #Correcting 
      #input.cur_try will always we ++1 of the tmp_df_subset_try
      if(max(c(tmp_df_subset_Try,0)) >= input$Cur_Try){ 
        #override point of try and remove "above"
        values$df_Table_Scores[values$df_Table_Scores$Question == as.integer(input$Cur_Question) &
                                 values$df_Table_Scores$Team == input$Cur_Team &
                                 values$df_Table_Scores$Try == as.integer(input$Cur_Try)
                                 , "Points"] <- as.character(Points) 
        
        #remove >= cur_try, recall iterates from above i df
        index_rm <- (values$df_Table_Scores$Question == as.integer(input$Cur_Question) &
                    values$df_Table_Scores$Team == input$Cur_Team &
                    values$df_Table_Scores$Try > as.integer(input$Cur_Try))
        #if index_rm is only false i.e. only most recent
        if(any(index_rm)){
          values$df_Table_Scores <- values$df_Table_Scores[!index_rm,]
        }
          
      }else{
        #no correction
        #ad new value to df
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
         
         #Subset data by current team
           df_tmp_teamsubset <- values$df_Table_Scores[values$df_Table_Scores$Team == name, ]
           #Answers spent, this will be lower after correction
           tmp_team_Answers_Spent <- nrow(df_tmp_teamsubset)
           
           #The questions answered
           tmp_Q <- df_tmp_teamsubset$Question

           tmp_team_numcorrect_counter <- 0
           tmp_team_sum_points <- 0

           for(k in unique(tmp_Q)){

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
        
        #If correction, i.e. MaxTru != Cur - answers spent is not <= prior number of answeres spent
        if(input$Cur_Try != values$Max_Try ) {
          
          #override point of try and remove "above"
          values$df_plot[values$df_plot$Team == name &
                                   values$df_plot$Answers_Spent == tmp_team_Answers_Spent
                                 , "Total_Score"] <- new_Total_Score
          
          #remove >= tmp_team_Answers_Spent, recall iterates from above i df
          index_rm <- (values$df_plot$Team == name &
                         values$df_plot$Answers_Spent > tmp_team_Answers_Spent)
          #if index_rm is only false i.e. only most recent
          if(any(index_rm)){
            values$df_plot <- values$df_plot[!index_rm,]
          }
          
          #--------- normal case
          
        } else{
          values$df_plot <- rbind(
            data.frame(stringsAsFactors = FALSE,
                       Team = name,
                       Answers_Spent = tmp_team_Answers_Spent,
                       Total_Score = new_Total_Score
            ),
            values$df_plot)
          }
        

        
  })#Observe event
  

  
}#server fun end

# Run the application
shinyApp(ui = ui, server = server)
