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
    output$changing_text <- renderText({
      paste0("You have completed trial #", rvs$trial_n)
    })
  })
}

shinyApp(ui = ui, server = server)
