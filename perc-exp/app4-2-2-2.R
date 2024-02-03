# Load the necessary packages----
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(imola)
library(speechcollectr)
library(dplyr)
library(timeR)

my_options <- options(digits.secs = 3)

# Make a vector of emotions that we can use throughout the app
# We'll randomize it for each participant
emos <- sample(c("happy", "sad", "angry", "neutral"), 4)

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
                                      total = 17,
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
                          hidden(div(id = "trialDisplay1",
                                     uiOutput("play_ui"))),
                          hidden(div(id = "trialDisplay3",
                                     uiOutput("conf_question"),
                                     rateUI(id = "confidence",
                                            type = "slider", n_scales = 1),
                                     disabled(actionButton(inputId = "conf_submit",
                                                           label = "SUBMIT")))),
                          hidden(div(id = "endDiv",
                                     h1("Task complete!"),
                                     h4("Thank you for your participation.")
                          ))
            )
  ),
  hidden(gridPanel(id = "trialDisplay2",
                   areas = list(
                     default = c("emo1 emo2",
                                 "emo3 emo4")),
                   lapply(emos, function(i){
                     emo_btn <- disabled(actionButton(inputId = i,
                                                      label = i,
                                                      style = "width:100%;height:100%;
                                       margin:0px;font-size:30px;
                                       border-radius:0px;"))
                     return(emo_btn)
                   })))
)

# The Server function----
server <- function(input, output, session) {
  rvs <- reactiveValues(trial_n = 1)

  observeEvent(input$enter, {
    rvs$pin <- pinGen(reactive = FALSE)
    rvs$stimuli <- randomStim(dataFile = "www/stimuli.csv",
                              what = "trial",
                              trialCol = "filename",
                              blockCol = NULL,
                              blocksSameOrd = FALSE,
                              n_blocks = 1,
                              n_perBlock = 16,
                              n_practice = 1,
                              outFile = paste0("www/outputs/stimuli", rvs$pin, ".csv"))
    hide("entryDiv")
    showElement("consentDiv")
  })

  consent <- consentServer(
    id = "consent",
    result = "hide",
    cons2rec = FALSE
  )

  observeEvent(consent$agree, {
    showElement("surveyDiv")

    surveyServer(id = "survey",
                 questionFile = "www/demographics.csv",
                 notListedLab = "Not listed:",
                 outFile = paste0("www/outputs/demographics", rvs$pin, ".csv"),
                 result = "hide")
  })

  observeEvent(input[["survey-submit"]], {
    showElement("trialDisplay1")

    showModal(modalDialog(title = "Time to begin the experimental task!",
                          p("The first trial is just for practice,
                            so instructions will appear in popups like this one for each step.
                            The first step is to click the 'Play Recording' button on this page."),
                          footer = actionButton("begin_trials", "Got it!")))
  })

  observeEvent(input$begin_trials, {
    removeModal()

    output$play_ui <- renderUI({
      playBttn(inputId = "play",
               label = "Play Recording",
               icon = NULL,
               src = paste0("perc_stim/", rvs$stimuli$filename[rvs$trial_n]),
               audioId = paste0("audio", rvs$trial_n))
    })
  })

  observeEvent(input$play, {
    conf$ratings <- NULL
    rvs$emo_sel <- NULL
    hide("trialDisplay1")
    hide("main")
    showElement("trialDisplay2")

    lapply(emos, function(i){
      delay(as.numeric(rvs$stimuli$duration[rvs$trial_n]),
            enable(i))
    })

    delay(as.numeric(rvs$stimuli$duration[rvs$trial_n]),
          rvs$start <- Sys.time())
  })

  lapply(emos, function(i){
    observeEvent(input[[i]], {
      rvs$emo_sel <- i
      rvs$end <- Sys.time()

      output$conf_question <- renderUI({
        tagList(
          h5("Considering only the recording you just heard, "),
          h3(paste0("how confident are you that the talker's tone of voice was ",
                    rvs$emo_sel, "?"))
        )
      })

      hide("trialDisplay2")
      showElement("main")
      showElement("trialDisplay3")
    })
  })

  conf <- rateServer(id = "confidence",
                     type = "slider",
                     trigger = reactive(rvs$emo_sel),
                     choices = list(c("Not at all confident",
                                      "Extremely confident")))

  observeEvent(conf$ratings, {
    enable("conf_submit")
  })

  observeEvent(input$conf_submit, {
    updateProgressBar(session = session,
                      id = "progress",
                      value = rvs$trial_n,
                      total = 17)

    lapply(emos, function(j){
      disable(j)
    })
    disable("conf_submit")
    hide("trialDisplay3")

    trial_data <- list(trial_n = rvs$trial_n,
                       emo_id = rvs$emo_sel,
                       confidence = conf$ratings,
                       rt = as.numeric(difftime(rvs$end, rvs$start, units = "secs"))
    )
    saveRDS(trial_data, paste0("www/outputs/trial", rvs$pin, "_", rvs$trial_n, ".rds"))

    if (rvs$trial_n < nrow(rvs$stimuli)) {
      rvs$trial_n <- rvs$trial_n+1
      showElement("trialDisplay1")
    } else {
      showElement("endDiv")
    }
  })
}
shinyApp(ui = ui, server = server)
