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

  ## set up the grid----
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

            ## Fill in the content----
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

                          ## Add the consent interface (initially hidden)----
                          hidden(div(id = "consentDiv",
                                     consentUI(id = "consent",
                                               title = "Do you consent to participate?",
                                               cons2rec = FALSE
                                     )
                          )),

                          ## Add interface for collecting demographics----
                          hidden(div(id = "surveyDiv",
                                     surveyUI(id = "survey",
                                              questionFile = "www/demographics.csv",
                                              title = "Tell us about yourself...")
                          ))
            )
  )
)

# The Server function----
server <- function(input, output, session) {

  ## Create the object for holding reactive values----
  rvs <- reactiveValues(trial_n = 1)

  ## Tell the app what to do when a participant clicks enter----
  observeEvent(input$enter, {
    ### Generate id number
    rvs$pin <- pinGen(reactive = FALSE)
    ### Randomize stimuli
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

  ## Build the consent server----
  consent <- consentServer(
    id = "consent",
    result = "hide",
    cons2rec = TRUE
  )

  ## What to do when participant consents----
  observeEvent(consent$agree, {
    showElement("surveyDiv")

    surveyServer(id = "survey",
                 questionFile = "www/demographics.csv",
                 notListedLab = "Not listed:",
                 outFile = paste0("www/outputs/demographics", rvs$pin, ".csv"),
                 result = "hide")
  })
}
shinyApp(ui = ui, server = server)
