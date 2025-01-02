
library(shiny)
library(wordcloud2)
library(dplyr)

ui <- fluidPage(
  titlePanel("Journal Key Bigrams Dashboard"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("word_count", "Number of Words", 
                  min = 1, 
                  max = nrow(top_100_bigrams_journal),
                  step = 1,
                  value = min(100, nrow(top_100_bigrams_journal))),
      selectInput("color", "Color Scheme", 
                  choices = c("random-light", "random-dark"), 
                  selected = "random-light"),
      sliderInput("size", "Word Cloud Size", 
                  min = 0.3, max = 0.8, value = 0.5),
      selectInput("shape", "Word Cloud Shape", 
                  choices = c("circle", "cardioid", "diamond", "star", "triangle", "pentagon"), 
                  selected = "circle"),
      sliderInput("grid", "Word Spacing", 
                  min = 0, max = 20, value = 5),
      sliderInput("rotate", "Rotation Ratio", 
                  min = 0, max = 1, value = 0.5)
    ),
    mainPanel(
      tags$style(
        type = "text/css",
        "#wordcloud_plot {height: 600px !important; width: 1000px !important;}"
      ),
      wordcloud2Output("wordcloud_plot")
    )
  )
)

server <- function(input, output) {
  output$wordcloud_plot <- renderWordcloud2({
    filtered_words <- top_100_bigrams_journal %>%
      select(bigrams, n) %>%
      slice_head(n = input$word_count)
    
    wordcloud2(
      data = filtered_words, 
      size = input$size, 
      color = input$color, 
      backgroundColor = "black", 
      shape = input$shape,
      gridSize = input$grid,
      rotateRatio = input$rotate
    )
  })
}

shinyApp(ui = ui, server = server)

