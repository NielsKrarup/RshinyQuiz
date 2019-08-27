#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("The Quiz"),
  h1(paste0("Julefrokost ", format(
    as.Date(Sys.Date(), format = "%d/%m/%Y"), "%Y"
  ))),
  strong(format(Sys.time(), format = "%H:%M:%S")),
  
  # Sidebar with a slider input for number of bins
  fluidRow(
    column(12,
           h1("Hold1"),
           checkboxInput("Check_val1l", "Noobs?")),
    column(
      3,
      passwordInput("a1l", 'Question 1, Left Interval', placeholder = 'CryptoPassWord'),
      conditionalPanel(
        condition = "input.a1l != '' ",
        passwordInput("a1l_2nd", "Question 1, 2nd try, Left Interval", placeholder = 'CryptoPassWord'),
        conditionalPanel(
          condition = "input.a1l_2nd != '' ",
          passwordInput("a1l_3rd", "Question 1, 3rd try, Left Interval", placeholder = 'CryptoPassWord')
        )
      )
    ),
    column(
      3,
      passwordInput("a1r", 'Question 1, Left Interval', placeholder = 'CryptoPassWord'),
      conditionalPanel(
        condition = "input.a1l != '' ",
        passwordInput("a1r_2nd", "Question 1, 2nd try, Right Interval", placeholder = 'CryptoPassWord'),
        conditionalPanel(
          condition = "input.a1l_2nd != '' ",
          passwordInput("a1l_3rd", "Question 1, 3rd try, Left Interval", placeholder = 'CryptoPassWord')
        )
        
      )
    ),
    column(6,
           # Show a plot of the generated distribution
           textOutput(outputId = "text1"),
           tableOutput("distPlot3"))
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  output$distPlot1 <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(
      x,
      breaks = bins,
      col = 'darkgray',
      border = 'white',
      main = input$a1
    )
  })
  output$distPlot2 <- renderPlot({
    plot(cars, pch = input$ans2)
  })
  output$distPlot3 <- renderTable({
    head(iris, n = input$size)
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
