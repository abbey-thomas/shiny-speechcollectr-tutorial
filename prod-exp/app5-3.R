# New additions are commented----

# Load the necessary packages----
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(imola)
library(speechcollectr)
library(dplyr)

# Build UI----
ui <- gridPage(
  tags$head(tags$style(HTML("div { text-align:center; }"))),
  useShinyjs(),
  useRecorder(),

  gridPanel(id = "main",
            areas = list(
              default = c(
                "header header header",
                ". content .")),
            gap = "10px",
            rows = c("auto 1fr"),
            columns = list(
              default = c("10% 80% 10%"),
              sm = c("5% 90% 5%")),
            header =  div(id = "progressDiv",
                          style = "background-color:lightgray;",
                          progressBar(id = "progress",
                                      value = 0,
                                      total = 20,
                                      title = "Trials Completed:")),
            content = div(id = "contentDiv",
                          style = "margin:auto;",
                          div(id = "entryDiv",
                              h4("Welcome to..."),
                              h1("Emotional Voices"),
                              p("A simple example experiment."),

                              ## Add a place for returning participants to enter their identification number----
                              textInput(inputId = "returning",
                                        label = "Have you started this experiment previously?
                                        If so, enter your identification number in the box below
                                        to continue from where you left off before clicking 'ENTER EXPERIMENT'."),

                              actionButton(inputId = "enter",
                                           label = "Enter Experiment")
                          ),
                          consentUI(id = "consent",
                                    title = "Do you consent to participate?"
                          ),
                          surveyUI(id = "survey",
                                   questionFile = "www/demographics.csv",
                                   title = "Tell us about yourself..."
                          ),
                          hidden(div(id = "blockDiv",
                                     uiOutput("instruct")
                          )),
                          hidden(div(id = "trialDiv",
                                     style = "height:200px;width:250px;",
                                     div(id = "stim_area",
                                         style = "background-color:lightgray;
                                                      height:100px; width:250px;",
                                         uiOutput("stim_word")),
                                     hidden(actionButton("trial_btn",
                                                         label = "",
                                                         style = "width:250px"))
                          )),
                          hidden(div(id = "endDiv",
                                     h1("Task complete!"),
                                     h4("Thank you for your participation.")
                          ))
            )
  )
)

# The Server function----
server <- function(input, output, session) {
  rvs <- reactiveValues(trial_n = 1)

  observeEvent(input$enter, {
    rvs$pin <- pinGen(reactive = FALSE)

    # Handle the returning user----
    if (isTruthy(input$returning)) {

      # Does their trial number file exist?----
      if (file.exists(paste0("www/outputs/trial_n", input$returning, ".rds"))) {

        # If yes, create the reactive values----
        rvs$pin <- input$returning
        rvs$stimuli <- read.csv(paste0("www/outputs/stimuli", input$returning, ".csv"))
        rvs$trial_n <- as.numeric(readRDS(paste0("www/outputs/trial_n", input$returning, ".rds")))

        # Send them to the block intro----
        showElement("blockDiv")
        showModal(modalDialog(title = "Welcome Back!",
                              p("Click the button below to pick up from the last trial you completed."),
                              footer = actionButton("begin_practice", "READY")))
        updateProgressBar(session = session,
                          id = "progress",
                          value = rvs$trial_n,
                          total = 20)
      } else {

        # If they entered an invalid id#, then create one, and show it to the new user.----
        rvs$pin <- pinGen(reactive = FALSE)
        rvs$stimuli <- randomStim(dataFile = "www/stimuli.csv",
                                  what = "both",
                                  blockCol = "emotion",
                                  trialCol = "word",
                                  blocksSameOrd = FALSE,
                                  n_practice = 5,
                                  outFile = paste0("www/outputs/stimuli", rvs$pin, ".csv"))
        showModal(modalDialog(title = "Invalid ID Number",
                              h5(paste0("Your new participant identification number is ", rvs$pin,
                                        ". Please make a note of this number!
                                        You may use it to pause and resume the experiment if necessary.")),
                              footer = modalButton("I have noted my new ID number."))
        )
        consentServer(
          id = "consent",
          result = "hide",
          cons2rec = TRUE,
          agreeId = "agree"
        )
      }
    } else {
      # If they didn't enter a pin, then create one, and show it to the new user.----
      rvs$pin <- pinGen(reactive = FALSE)
      rvs$stimuli <- randomStim(dataFile = "www/stimuli.csv",
                                what = "both",
                                blockCol = "emotion",
                                trialCol = "word",
                                blocksSameOrd = FALSE,
                                n_practice = 5,
                                outFile = paste0("www/outputs/stimuli", rvs$pin, ".csv"))

      showModal(modalDialog(title = "Welcome!",
                            h5(paste0("Your participant identification number is ", rvs$pin,
                                      ". Please make a note of this number!
                                        You may use it to pause and resume the experiment if necessary.")),
                            footer = modalButton("I have noted my ID number.")))
      consentServer(
        id = "consent",
        result = "hide",
        cons2rec = TRUE,
        agreeId = "agree"
      )
    }
    hide("entryDiv")
  })

  observeEvent(input$agree, {
    surveyServer(id = "survey",
                 questionFile = "www/demographics.csv",
                 notListedLab = "Not listed:",
                 outFile = paste0("www/outputs/demographics", rvs$pin, ".csv"),
                 result = "hide")
  })

  observeEvent(input[["survey-submit"]], {
    showModal(modalDialog(title = "Time to Begin the Experiment!",
                          p("In this experiment, you'll be reading and audio recording words in different emotions.
                          This first round is a practice round, so you'll see instructions throughout this round in popups like this one.
                            Click the 'READY' button below to begin."),
                          footer = actionButton("begin_practice", "READY")))

  })

  observeEvent(input$begin_practice, {
    removeModal()
    showElement("blockDiv")

    if (rvs$trial_n == 1) {
      showModal(modalDialog(
        p("At the beginning of each round, you'll see instructions telling you which emotion you should use when you read the words that appear.
            When you click the emoticon button on this page, recording will begin and the first word will appear.
        Each time a new word appears, audio recording will begin automatically. When you finish reading a word,
        click the emoticon button to stop recording and proceed to the next word.
          You will read and record 5 words in each set."),
        footer = modalButton("Okay")
      ))
    }

    output$instruct <- renderUI({
      tagList(
        h4("For this round, please read the words that appear in a"),
        em(h1(rvs$stimuli$emotion[rvs$trial_n])),
        h4("tone of voice."),
        br(),
        em(h5(paste0("Click the ", rvs$stimuli$emotion[rvs$trial_n],
                     " emoticon button below to view the first word."))),
        actionButton("begin_trials",
                     label = icon(rvs$stimuli$icon[rvs$trial_n],
                                  class = "fa-4x"))
      )
    })

  })

  observeEvent(input$begin_trials, {
    hide("blockDiv")
    showElement("trialDiv")
    startRec(readyId = "ready")

    updateActionButton(session = session,
                       inputId = "trial_btn",
                       label = "",
                       icon = icon(rvs$stimuli$icon[rvs$trial_n], class = "fa-3x"))

    output$stim_word <- renderUI({
      h1(rvs$stimuli$word[rvs$trial_n])
    })
  })

  observeEvent(input$ready, {
    # Delay the appearance of the word to be read by 500 ms after recording begins
    delay(500, showElement("stim_word"))
    delay(1500, showElement("trial_btn"))
  })

  observeEvent(input$trial_btn, {
    stopRec(filename = paste0("www/outputs/rec", rvs$pin, "_", rvs$trial_n, ".wav"),
            finishedId = "done")

    # Save the trial number to a file----
    saveRDS(rvs$trial_n, paste0("www/outputs/trial_n", rvs$pin, ".rds"))


  })

  observeEvent(input$done, {

    updateProgressBar(session = session,
                      id = "progress",
                      value = rvs$trial_n,
                      total = 20)

    if (rvs$trial_n < nrow(rvs$stimuli)) {
      if (rvs$stimuli$block[rvs$trial_n] == rvs$stimuli$block[rvs$trial_n + 1]) {

        # If this is the first trial, then check recording quality
        if (rvs$trial_n == 1) {
          evalWavServer(wave = paste(input$done))

        # Otherwise continue as before...
        } else {
          rvs$trial_n <- rvs$trial_n+1
          hide("trial_btn")
          hide("stim_word")

          startRec(readyId = "ready")
        }
      } else {
        hide("trialDiv")
        hide("trial_btn")
        showElement("blockDiv")
        if (rvs$trial_n == 5) {
          showModal(modalDialog(title = "Practice round complete!",
                                p("Are you ready to begin the next round?"),
                                footer = modalButton("READY!")))
        }
        rvs$trial_n <- rvs$trial_n+1
      }
    } else {
      hide("trialDiv")
      showElement("endDiv")
    }
  })

  ## Once the wav file has been evaluated...
  observeEvent(input[["evalWav-result"]], {

    ### If the recording is of sufficient quality...
    if (input[["evalWav-result"]] == "pass") {

      #### proceed with the next trial
      rvs$trial_n <- rvs$trial_n+1
      hide("trial_btn")
      hide("stim_word")
    }

    #### Whether they passed or not, start recording again...
    #### But give participant time to close the alert before starting recording again...
    delay(1500, startRec(readyId = "ready"))
  })
}
shinyApp(ui = ui, server = server)
