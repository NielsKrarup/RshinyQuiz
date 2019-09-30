library(shiny)

ui <- fluidPage(
  fluidRow(
    column(6, 
           tags$h2("Set parameters"),
           numericInput("value", "Value", value = 3, min = 1, max = 6, step = 1),
           radioButtons(inputId = 'radio1',label = 'Radio1Input',choices = c('ko','abe','mus'), selected = NA),
           radioButtons(inputId = 'radio2',label = 'Radio2Input',choices = c('æble','banan'), selected = NA),
           actionButton(inputId = 'Submit', label = 'SubmitButton'),           verbatimTextOutput("Validate_Submit")

    ),
    column(6,
           tableOutput('Test_Table')
    )
  )
)

server <- function(input, output, session) {
  
    df <- reactiveValues()

    df$Testdata <- data.frame(ID = character(0L),
                              value = character(0L),
                           Animal = character(0L),
                           Fruit = character(0L))


  
  #Initial Dataframe
new_df <- eventReactive(input$Submit,{
  
  validate(
    need(input$radio1,'HEJ!'),
    need(input$radio2, 'SUP')
  )
  
  



    # #Update of current to whole
    # df$Testdata = rbind(df$Testdata,
    #                  data.frame(ID = input$Submit,
    #                             value = input$value,
    #                             Animal = input$radio1,
    #                             Fruit = input$radio2)
    #                  )
  })
  
  #Current selection
  output$Test_Table = renderTable({
    
    validate(
      need(input$radio1,'HEJ!'),
      need(input$radio2, 'SUP')
    )

    #The current row in df
    data.frame(ID = input$Submit,
               value = input$value,
               Animal = input$radio1,
               Fruit = input$radio2)
    })
  
  #Combined Table
  output$Validate_Submit <- renderText({
    
    test <-  eventReactive(input$Submit,{
       validate(
         need(input$radio1,'HEJ DYR!'),
         need(input$radio2, 'MANGLER FRUGT')
       )
     })
    
    test()

    })
  

}

shinyApp(ui, server)

