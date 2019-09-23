library(shiny)

ui <- fluidPage(
  checkboxGroupInput(
    inputId = "selected_var",
    label = "Choose variables:",
    choices = c(
      "R" = "r",
      "F" = "f",
      "M" = "m"
    ),
    selected = c("r","f")
  ),
  uiOutput('weights_input'),
  textOutput('score')
)

server <- function(input, output) {
  
  output$weights_input <- renderUI({ 
    req(input$selected_var)
    lapply(1:length(input$selected_var), function(i) {
      numericInput(inputId = paste0(input$selected_var[i],"_weight"), label = input$selected_var[i], min = 0, max = 10, value = 0)
    })
  })
  
  output$score <- renderText({
    req(input$selected_var)
    #selected are the r f m
    selected = input$selected_var
    values = sapply(1:length(input$selected_var), function(i) {
      req(input[[ paste0(input$selected_var[i],"_weight")]]);
      input[[ paste0(input$selected_var[i],"_weight")]]
    })
    values 
    
  })
}

shinyApp(ui,server)