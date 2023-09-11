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
                              actionButton(inputId = "enter",
                                           label = "Enter Experiment")
                          ),
                          hidden(div(id = "consentDiv",
                                     consentUI(id = "consent",
                                               title = "Do you consent to participate?")
                          )),
                          hidden(div(id = "surveyDiv",
                                     surveyUI(id = "survey",
                                              questionFile = "www/demographics.csv",
                                              title = "Tell us about yourself...")
                          )),

                          ## Add a container for the introduction to each block----
                          hidden(div(id = "blockDiv",
                                     uiOutput("instruct")
                          )),

                          ## Add a container for the trial interface----
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

                          ## Add an interface to let the participant know they've finished the task----
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
    rvs$stimuli <- randomStim(dataFile = "www/stimuli.csv",
                              what = "both",
                              blockCol = "emotion",
                              trialCol = "word",
                              blocksSameOrd = FALSE,
                              n_practice = 5,
                              outFile = paste0("www/outputs/stimuli", rvs$pin, ".csv"))
    hide("entryDiv")
    showElement("consentDiv")
  })

  consent <- consentServer(
    id = "consent",
    result = "hide",
    cons2rec = TRUE
  )

  observeEvent(consent$agree, {
    showElement("surveyDiv")

    surveyServer(id = "survey",
                 questionFile = "www/demographics.csv",
                 notListedLab = "Not listed:",
                 outFile = paste0("www/outputs/demographics", rvs$pin, ".csv"),
                 result = "hide")
  })

  ## Show instructions when participant submits the demographic survey----
  observeEvent(input[["survey-submit"]], {
    showModal(modalDialog(title = "Time to Begin the Experiment!",
                          p("In this experiment, you'll be reading and audio recording words in different emotions.
                          This first round is a practice round, so you'll see instructions throughout this round in popups like this one.
                            Click the 'READY' button below to begin."),
                          footer = actionButton("begin_practice", "READY")))

  })

  ## Show the block introduction----
  observeEvent(input$begin_practice, {
    removeModal()
    showElement("blockDiv")

    ## If this is the first trial, give some more instructions----
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

    ## Tell the participant what attitude to use----
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

  ## What to do when participant clicks the button on the block introduction----
  observeEvent(input$begin_trials, {
    hide("blockDiv")
    showElement("trialDiv")

    ## Show the written stimulus for participant to read----
    output$stim_word <- renderUI({
      h1(rvs$stimuli$word[rvs$trial_n])
    })

    ## Make sure the action button is labeled with the emoticon corresponding to the current target emotion----
    updateActionButton(session = session,
                       inputId = "trial_btn",
                       label = "",
                       icon = icon(rvs$stimuli$icon[rvs$trial_n], class = "fa-3x"))

    delay(1500, showElement("trial_btn"))
  })

  ## What to do when participant clicks the button to stop recording----
  observeEvent(input$trial_btn, {

    ## Update the progress bar to match # of trials completed----
    updateProgressBar(session = session,
                      id = "progress",
                      value = rvs$trial_n,
                      total = 20)

    ## What to do if there are more trials (or not)----
    if (rvs$trial_n < nrow(rvs$stimuli)) {

      ## What to do if the block won't change on this round----
      if (rvs$stimuli$block[rvs$trial_n] == rvs$stimuli$block[rvs$trial_n + 1]) {

        ## Increase trial number----
        rvs$trial_n <- rvs$trial_n+1
        hide("trial_btn")
        hide("stim_word")

        ## Show the next word and then the stop button after a delay----
        delay(500, showElement("stim_word"))
        delay(2000, showElement("trial_btn"))

      } else {
        hide("trialDiv")
        hide("trial_btn")
        showElement("blockDiv")

        ## If this is the end of the first block of trials, show one more set of instructions----
        if (rvs$trial_n == 5) {
          showModal(modalDialog(title = "Practice round complete!",
                                p("Are you ready to begin the next round?"),
                                footer = modalButton("READY!")))
        }
        rvs$trial_n <- rvs$trial_n+1
      }
    } else {
      ## If all trials are complete, let the participant know----
      hide("trialDiv")
      show("endDiv")
    }
  })
}
shinyApp(ui = ui, server = server)
