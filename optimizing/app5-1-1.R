# Run these lines one time before running this script.
# You may need to adjust the path to the working directory based on your current working directory.
# setwd("optimizing")
# Download sample questions from speechcollectr.
# data("qualifications")
# write.csv(qualifications, "qualifications.csv", row.names = FALSE)

library(speechcollectr)
library(imola)
library(shiny)
library(shinyjs)

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
                        questionFile = "qualifications.csv",
                        outFile = NULL,
                        returnVals = c("eighteen"))

  observeEvent(input$btn, {
    hide("btn")
  })
}

shinyApp(ui = ui, server = server)
