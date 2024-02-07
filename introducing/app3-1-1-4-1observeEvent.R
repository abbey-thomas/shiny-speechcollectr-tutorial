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

    text <- paste0("You've clicked Button One ", input$button1, " times.")

    output$changing_text <- renderText({
      text
    })
  })
}

shinyApp(ui = ui, server = server)
