library(shiny)
library(imola)

ui <- gridPage(
  div(
    actionButton(inputId = "button1", label = "Button 1"),
    actionButton(inputId = "button2", label = "Button 2"),
    textOutput(outputId = "changing_text")
  )
)

server <- function(input, output, session) {

  observeEvent(input$button2, {
    ouput$changing_text <- renderText({
      paste0("You've clicked Button One ", input$button1, " times.")
    })
  })
}

shinyApp(ui = ui, server = server)
