#Hello
library(shiny)
ui <- fluidPage(
  titlePanel("Bins output"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins1", "Number of bins:", 1, 10, 5),
      sliderInput("bins2", "Number of bins:", -100, 0, -50)
    ),
    mainPanel(
      textOutput("binsOutput")
    )
  )
)
server <- function(input, output) {
  output$binsOutput <- renderText({
    result <- "Bins output:"
    for (i in seq(2)) {
      result <- c(result, input[[paste0("bins", as.character(i))]])
    }
    result
  })
}
shinyApp(ui = ui, server = server)