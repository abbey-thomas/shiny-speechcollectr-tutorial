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
  rvs <- reactiveValues(trial_n = 1)

  observeEvent(input$button2, {
    rvs$trial_n <- rvs$trial_n + 1
  })

  observe({
    text <- paste0("The current trial number is ", rvs$trial_n)

    output$changing_text <- renderText({
      text
    })
  })
}

shinyApp(ui = ui, server = server)
