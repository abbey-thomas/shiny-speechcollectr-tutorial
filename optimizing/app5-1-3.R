library(speechcollectr)
library(imola)
library(shiny)

## First use wwwPrep() to get the data for the type of headphone screen you want.
## NOTE: Do NOT put this command in your UI!
# Run it once before testing your app to create the www folder.
wwwPrep(HugginsPitchScreen = TRUE)

# Now build the app.
ui <- gridPage(
  div(
    headphoneTestUI(id = "headphone_test", type = "huggins"),
    textOutput("done")
  )
)

server <- function(input, output, session) {
  # Give the reactive value output a name, so it can be checked and accessed later.
  phones <- headphoneTestServer(id = "headphone_test", type = "huggins",
                                n_trials = 6, threshold = 4, n_attempts = 2)
  observe({
    req(phones())
    if (phones() == 1) {
      output$done <- renderText("Headphone test successfully completed!")
    }
  })
}
shinyApp(ui = ui, server = server)
