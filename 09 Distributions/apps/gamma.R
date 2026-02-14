library(shiny)
library(ggplot2)

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
      .well {
        background-color: #f8f9fa;
        border: none;
        box-shadow: none;
      }
    "))
  ),

  fluidRow(
    column(
      width = 8, offset = 2,

      h2("Gamma Distribution Explorer"),

      sliderInput("shape",
                  "Shape",
                  min = 0.5,
                  max = 20,
                  value = 2,
                  step = 0.1),

      sliderInput("scale",
                  "Scale",
                  min = 0.1,
                  max = 50,
                  value = 1,
                  step = 0.1),

      plotOutput("gammaPlot", height = "450px"),

      br(),

      fluidRow(
        column(6, textOutput("mean")),
        column(6, textOutput("variance"))
      )
    )
  )
)

server <- function(input, output) {

  output$gammaPlot <- renderPlot({

    x <- seq(0,
             qgamma(0.999,
                    shape = input$shape,
                    scale = input$scale),
             length.out = 1000)

    y <- dgamma(x,
                shape = input$shape,
                scale = input$scale)

    ggplot(data.frame(x, y), aes(x, y)) +
      geom_line(linewidth = 1.2) +
      geom_vline(
        xintercept = input$shape * input$scale,
        linetype = "dashed",
        linewidth = 0.8
      ) +
      labs(
        x = "x",
        y = "Density",
        subtitle = paste(
          "Shape =", input$shape,
          "| Scale =", input$scale
        )
      ) +
      theme_minimal(base_size = 16)
  })

  output$mean <- renderText({
    paste("Mean:", round(input$shape * input$scale, 3))
  })

  output$variance <- renderText({
    paste("Variance:", round(input$shape * input$scale^2, 3))
  })
}

shinyApp(ui, server)
