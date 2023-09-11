library(shiny)
library(shinyjs)
library(speechcollectr)
library(imola)

ui <- gridPage(
  div(
    actionButton("begin", "BEGIN"),
    recordUI(id ="rec"),
    disabled(actionButton("submit", "SUBMIT"))
  )
)

server <- function(input, output, session) {
  recording <- recordServer(id ="rec",
                            trigger = reactive(input$begin),
                            outPrefix = paste0("www/rec_samp"))

  observeEvent(input$begin, {
    disable("begin")
  })

  observe({
    req(recording()$file)
    enable("submit")
  })

  observeEvent(input$submit, {
    disable("submit")
    result <- evalWavServer(wave = recording()$file,
                            tries = 3)
    observeEvent(result(), {
      # If the recording is not good enough, but the participant
      # has had fewer 2 attempts
      # the recording interface will remain visible,
      # and the participant can overwrite the file with by creating a new recording.

      # Otherwise...
      if (result() != "warn") {
        hide("rec")
      }

      # If the recording is of sufficient quality,
      # let the participant record a new file.
      if (result() == "pass") {
        enable("begin")
      }

      # If all attempts have been used (result == "fail")
      # then keep everything hidden/disabled.
    })
  })
}

shinyApp(ui = ui, server = server)
