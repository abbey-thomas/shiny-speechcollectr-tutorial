library(speechcollectr)
library(imola)
library(shiny)
library(shinyjs)

# Download sample questions from speechcollectr.
write.csv(data("qualifications"), "www/qualifications.csv", row.names = FALSE)



ui <- gridPage(
  useShinyjs(),
  div(
    actionButton(inputId = "btn", "Start"),
    checkUI(id = "example", title = "Speech Experiment",
            type = "participant"),
    textOutput("confirmation"))

)
server <- function(input, output, session) {
  answer <- checkServer(id = "example",
                        trigger = reactive(input$btn),
                        questionFile = "www/qualifications.csv",
                        outFile = NULL,
                        returnVals = c("eighteen"))

  observeEvent(input$btn, {
    hide("btn")
  })
}

shinyApp(ui = ui, server = server)
