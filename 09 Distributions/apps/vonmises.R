library(shiny)
library(ggplot2)
library(circular)

ui <- fluidPage(

  tags$head(
    tags$style(HTML("
      body {
        background-color: white;
        font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
      }
      .control-label {
        font-weight: 400;
      }
      h2 {
        font-weight: 400;
        margin-bottom: 25px;
      }
    "))
  ),

  fluidRow(
    column(
      width = 8, offset = 2,

      h2("von Mises Distribution Explorer"),

      sliderInput("kappa",
                  "Concentration (κ)",
                  min = 0,
                  max = 20,
                  value = 2,
                  step = 0.1),

      plotOutput("vmPlot", height = "450px"),

      br(),

      textOutput("info")
    )
  )
)

server <- function(input, output) {

  output$vmPlot <- renderPlot({

    theta <- seq(-pi, pi, length.out = 1000)

    dens <- dvonmises(
      circular(theta),
      mu = circular(0),
      kappa = input$kappa
    )

    ggplot(data.frame(theta, dens), aes(theta, dens)) +
      geom_line(linewidth = 1.2) +
      geom_vline(xintercept = 0,
                 linetype = "dashed",
                 linewidth = 0.8) +
      scale_x_continuous(
        breaks = c(-pi, -pi/2, 0, pi/2, pi),
        labels = c("-π", "-π/2", "0", "π/2", "π")
      ) +
      labs(
        x = "Angle",
        y = "Density",
        subtitle = paste("κ =", input$kappa,
                         "| Mean direction = 0")
      ) +
      theme_minimal(base_size = 16)
  })

  output$info <- renderText({
    if (input$kappa == 0) {
      "κ = 0 gives a uniform distribution on the circle."
    } else {
      paste("Higher κ increases concentration around the mean direction.")
    }
  })
}

shinyApp(ui, server)
