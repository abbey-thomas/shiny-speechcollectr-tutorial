# Run these lines one time before running this script.
# You may need to adjust the path to the working directory based on your current working directory.
# setwd("optimizing")
# dir.create("www")

library(shiny)
library(shinyjs)
library(speechcollectr)

# Build UI----
ui <- fluidPage(

  ## Get background javascript ready----
  useShinyjs(),
  useRecorder(),

  actionButton("next_trial", "NEXT"),

  ## Setup page layout----
  hidden(div(id = "trialDiv",
             style = "text-align:center;",

             hidden(div(id = "textDiv",
                        h4("Please record yourself reading this sentence aloud:"),
                        h2(textOutput(outputId = "read_this")))),

             ## Create the buttons for controlling the recording----
             actionButton(inputId = "start",
                          label = "start"),

             ### Hide the stop button until user clicks start----
             hidden(actionButton(inputId = "stop",
                                 label = "stop"))
  ))
)

# The Server function----
server <- function(input, output, session) {
  ## Create an object that will count trials----
  rvs <- reactiveValues(trial_n = 0)

  ## When the participant clicks "NEXT"...
  observeEvent(input$next_trial, {
    ### Increase the trial number
    rvs$trial_n <- rvs$trial_n + 1

    ### Show the recording interface
    showElement("trialDiv")

    ### Hide the next button
    hide("next_trial")
  })

  ## When the start button is clicked----
  observeEvent(input$start, {

    ### Start the recording----
    startRec(readyId = "ready")

    ### Disable the start button----
    disable("start")

    ### Show the stop button----
    delay(500, showElement("stop"))
  })

  ## When the user gives permission to record....
  observeEvent(input$ready, {

    ### Show the text they should read
    showElement("textDiv")
    output$read_this <- renderText({paste0("This is recording ",
                                           rvs$trial_n, ".")})
  })


  ## When the user clicks stop----
  observeEvent(input$stop, {

    ### Stop recording----
    stopRec(filename = paste0("rec", rvs$trial_n, ".wav"),
            finishedId = "done")

    ### Enable the start button----
    enable("start")

    ### Hide the stop button----
    hide("stop")
    hide("textDiv")
  })

  ## Once the wav file exists...
  observeEvent(input$done, {
    ### Evaluate the recording----
    evalWavServer(wave = paste(input$done))
  })

  ## Once the wav file has been evaluated...
  observeEvent(input[["evalWav-result"]], {

    ### If the recording is of sufficient quality...
    if (input[["evalWav-result"]] == "pass") {

      #### Hide the recording interface
      hide("trialDiv")

      #### And show the "next" button so that the participant can record the next file
      showElement("next_trial")
    }
  })
}

# Run the application----
shinyApp(ui = ui, server = server)
