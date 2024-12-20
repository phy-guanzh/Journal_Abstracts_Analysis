library(shiny)
library(dplyr)
library(echarts4r)

# 示例数据
top_100_words_journal <- data.frame(
  word = c("model", "paper", "time", "system", "base", "approach", "propose", "method", "result", "simulation"),
  n = c(7032, 3029, 2872, 2631, 2558, 2522, 2396, 2337, 2202, 2096)
)

top_100_bigrams_journal <- data.frame(
  bigrams = c("machine learning", "deep learning", "case study", "data analysis", "research model"),
  n = c(500, 450, 300, 350, 400)
)

# UI 部分
ui <- fluidPage(
  titlePanel("Journal Key Words and Bigrams Dashboard"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("word_count", "Number of Words", 
                  min = 1, 
                  max = 10,
                  step = 1,
                  value = 5),
      selectInput("color", "Color Scheme", 
                  choices = c("blue", "green", "red", "purple"), 
                  selected = "blue")
    ),
    mainPanel(
      fluidRow(
        column(6, echarts4rOutput("wordcloud_plot_words", height = "400px")),
        column(6, echarts4rOutput("wordcloud_plot_bigrams", height = "400px"))
      )
    )
  )
)

# Server 部分
server <- function(input, output) {
  
  # 单词词云
  output$wordcloud_plot_words <- renderEcharts4r({
    filtered_words <- top_100_words_journal %>%
      slice_head(n = input$word_count)  # 筛选数据
    
    filtered_words %>%
      e_charts(filtered_bigrams$word) %>%
      e_cloud(size = n, color = input$color) %>%
      e_title("Top Words Cloud")
  })
  
  # 双词组合词云
  output$wordcloud_plot_bigrams <- renderEcharts4r({
    filtered_bigrams <- top_100_bigrams_journal %>%
      slice_head(n = input$word_count)  # 筛选数据
    
    filtered_bigrams %>%
      e_charts(filtered_bigrams$bigrams) %>%
      e_cloud(size = n, color = input$color) %>%
      e_title("Top Bigrams Cloud")
  })
}

shinyApp(ui = ui, server = server)

ggplot(data=unigrams_tfidf  ,
       aes(x=reorder(word, -tf_idf), y=tf_idf)) +
  geom_bar(stat="identity", color = violet, fill = violet) +
  theme_minimal()+
  labs(
    title = title_name,
    x = "",
    y = "tf-idf values"
  )+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 8),  
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    axis.line = element_line(arrow = arrow(length = unit(0.3, "cm"), type = "closed"))
  )