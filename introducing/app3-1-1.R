library(shiny)
library(shinyjs)
library(imola)

ui <- gridPage(
  useShinyjs(),
  div(id = "page1",
      actionButton(inputId = "button1",
                   label = "Button 1")),
  hidden(div(id = "page2",
             actionButton(inputId = "button2",
                          label = "Button 2"))),
  hidden(div(id = "page3",
             actionButton(inputId = "button3",
                          label = "The End!")))
)

server <- function(input, output, session) {

  observeEvent(input$button1, {
    hide("page1")
    showElement("page2")
  })

  observeEvent(input$button2, {
    hide("page2")
    showElement("page3")
  })

  observeEvent(input$button3, {
    hide("page3")
  })
}

shinyApp(ui = ui, server = server)
