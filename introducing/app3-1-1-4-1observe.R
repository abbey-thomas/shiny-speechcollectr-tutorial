library(shiny)
library(imola)

ui <- gridPage(
  div(
    actionButton(inputId = "button1", label = "Button 1"),
    textOutput(outputId = "changing_text")
  )
)

server <- function(input, output, session) {
  observe({

    text <- paste0("You've clicked Button One ", input$button1, " times.")

    output$changing_text <- renderText({
      text
    })
  })
}

shinyApp(ui = ui, server = server)
