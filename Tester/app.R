#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        numericInput(inputId = 'NoTeams', label = 'Number of teams',value = 1,min = 1, max = 6),
        #uiOutput("SelectTeamRadio"),
        uiOutput('SelectTeamRadio'),
        verbatimTextOutput("teams")
        
      ),
      mainPanel(
        uiOutput("UIsetTeams")
      )
      
      # Show a plot of the generated distribution

   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  values <- reactive({
    sapply(1:(input$NoTeams), function(i) {
      req(input[[ paste0("team_", i)]]);
      input[[  paste0("team_", i)]]})
    
  }) 
   
   output$UIsetTeams <- renderUI({
      # generate bins based on input$bins from ui.R
     lapply(1:input$NoTeams,function(iter){
       column(12/input$NoTeams,
              textInput(inputId = paste0("team_", iter), label = paste0("Name of team", iter),value = paste0("PIK",rpois(1,19)))
       )
     })
   })
   
   #UI for RadioButtons
   output$SelectTeamRadio <- renderUI({
     #req(input$team_1)
     #get names

       radioButtons(inputId = "Cur_Team", label = 'Current team', choices = values())

   })
   
   
   output$teams <- renderPrint({
     print <- values()
     print
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

