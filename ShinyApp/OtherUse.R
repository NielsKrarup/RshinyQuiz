library(shiny)

ui <- fluidPage(
  fluidRow(
    column(6, 
           tags$h2("Set parameters"),
           numericInput("NoTeams", "Value", value = 3, min = 1, max = 6, step = 1),
           radioButtons(inputId = 'radio1',label = 'Radio1Input',choices = c('ko','abe','mus')),
           radioButtons(inputId = 'radio2',label = 'Radio2Input',choices = c('æble','banan'))
    ),
    column(6,
           uiOutput("ui"),
           textOutput('O1')
    )
  )
)

server <- function(input, output, session) {
  output$ui <- renderUI( {
    tagList(
      h2("Choose Team"),
      numericInput("obs1", "Label1", value = 1, min = 0, max = input$NoTeams, step = 1),
      textInput(inputId = 'text1',label = 'Væl et dyr eller frugt',placeholder = 'ehehe'),
      selectInput(inputId = 'dyrFrugt',label = "vælg ultimativt",choices = c(input$radio1, input$radio2))
    )
  })
  output$O1 <- renderText(
    isolate(input$NoTeams^2)
  )
}

shinyApp(ui, server)

