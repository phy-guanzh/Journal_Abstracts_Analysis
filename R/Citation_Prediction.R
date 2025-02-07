library(shiny)
library(tidyr)
library(dplyr)
library(tidytext)
library(textstem)
library(ggplot2)
library(ggpubr)
library(topicmodels)
library(tidyverse)
library(stm)
library(data.table)

prepare_input_data <- function(input_data, model_features) {
  
  input_data <- input_data[, model_features]
  return(input_data)
}


lm_model <- readRDS("./models/best_lm_model.rds")
rf_model <- readRDS("./models/best_rf_model.rds")
xgb_model <- readRDS("./models/best_xgb_model.rds")
dtm_model <- readRDS("./models/dtm_model.rds")
lda_model <- readRDS("./models/lda_model_2000_2022_k10.rds")
pca_model <- readRDS("./models/pca_model.rds")

ui <- fluidPage(
  titlePanel("Citation Prediction"),
  sidebarLayout(
    sidebarPanel(
      numericInput("year", "Year of Publication:", 2023),
      numericInput("pages", "Number of Pages:", 10),
      numericInput("views", "Number of Views:", 0),
      numericInput("altmetric", "Number of Altmetric:", 0),
      numericInput("author_count", "Number of Authors:", 3),
      selectInput(
        "journal", "Journal Name:",
        choices = c("Health Systems", "Journal of Simulation", "Journal of the Operational Research Society")
      ),
      textAreaInput("title", "Title:",  rows = 2, value = "Efficiency ranking of decision making units in data envelopment analysis by using TOPSIS-DEA method"),
      textAreaInput("abstract", "Abstract:",  rows = 6, value = "Productivity growth of institutions of higher education is of interest for two main reasons: education is an important factor for productivity growth of the economy, and in countries where higher education is funded by the public sector, accountability of resource use is of key interest. Educational services consist of teaching, research and the “third mission” of dissemination of knowledge to the society at large. A bootstrapped Malmquist productivity change index is used to calculate productivity development for Norwegian institutions of higher education over the 10-year period 2004–2013. The confidence intervals from bootstrapping allow part of the uncertainty of point estimates stemming from sample variation to be revealed. The main result is that the majority of institutions have had a positive productivity growth over the total period. However, when comparing with growth in labour input, the impact on productivity varies a lot."),
      
      
      actionButton("predict", "Predict")
    ),
    mainPanel(
      h3("Predicted Citations"),
      verbatimTextOutput("lm_output"),
      plotOutput("prediction_plot", height = "200px"),
      verbatimTextOutput("lda_topics"),
      verbatimTextOutput("lda_terms"),
      verbatimTextOutput("stm_topics"),
      verbatimTextOutput("stm_terms"),
      verbatimTextOutput("text_note"),
      verbatimTextOutput("text_link")
    )
  )
)



server <- function(input, output) {
  observeEvent(input$predict, {
    req(input$abstract)  
    set.seed(42)
    input_title_abstract <- paste0(input$title, " - ", input$abstract)
    input_title_abstract <- lemmatize_strings(input_title_abstract)
    input_token <- tibble(
      row_id = 1,  
      title_abstract = input_title_abstract,
      year = input$year,
      journal = input$journal
    )  %>%
      unnest_tokens(output = word, input = title_abstract, token = "ngrams", n = 2) %>% 
      separate(word, into = c("word1", "word2"), sep = " ") %>%  
      filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word) %>% 
      filter(!grepl("\\b\\d+\\b", word1), !grepl("\\b\\d+\\b", word2)) %>% 
      unite("bigram", word1, word2, sep = "-")
    
    input_token_dt <- as.data.table(input_token)
    input_token_dt <- input_token_dt[, .(text = paste(bigram, collapse = " ")), by = row_id]
    input_token_dt |> as.data.frame()
    print(input_token)
    processed_new_doc <- textProcessor(
      documents = input_token_dt$text,
      metadata = data.frame(year = input$year, journal = input$journal),
      customstopwords = NULL
    )
    
    prep_new <- prepDocuments(processed_new_doc$documents, processed_new_doc$vocab, processed_new_doc$meta, lower.thresh = 1)
    docs_new <- prep_new$documents
    vocab_new <- prep_new$vocab
    meta_new <- prep_new$meta
    
    
    aligned_new_doc <- alignCorpus(
      old.vocab = dtm_model$vocab,
      new = processed_new_doc
    )
    
    new_doc_theta <- fitNewDocuments(
      model = dtm_model,
      documents = aligned_new_doc$documents
    )$theta
    
    journal_token_bi <- tibble(row_id = 1, title_abstract = input_title_abstract) %>% 
      select(row_id,title_abstract) %>% 
      unnest_tokens(output = bigrams, title_abstract, token = "ngrams", n = 2) %>% 
      filter(!grepl("\\b\\d+\\b", bigrams)) %>% 
      separate(bigrams, into = c("word1", "word2"), sep = " ")   %>% 
      filter(!(word1 %in% stop_words$word) & !(word2 %in% stop_words$word)) %>% unite(bigrams, word1, word2, sep = " ")
    
    dtm <- journal_token_bi %>%
      count(row_id, bigrams) %>%
      cast_dtm(document = row_id, term = bigrams, value = n)
    
    topic_probabilities_lda <- posterior(lda_model, dtm)
    
    
    input_data <- data.frame(
      year = input$year,
      pages = input$pages,
      views = input$views,
      altmetric = input$altmetric,
      author_count = input$author_count,
      Health_Systems = ifelse(input$journal == "Health Systems", 1, 0),
      Journal_of_Simulation = ifelse(input$journal == "Journal of Simulation", 1, 0),
      row_id = 1,
      Journal_of_the_Operational_Research_Society = ifelse(input$journal == "Journal of the Operational Research Society", 1, 0),
      Topic_STM_1 = new_doc_theta[1],
      Topic_STM_2 = new_doc_theta[2],
      Topic_STM_3 = new_doc_theta[3],
      Topic_STM_4 = new_doc_theta[4],
      Topic_STM_5 = new_doc_theta[5],
      Topic_STM_6 = new_doc_theta[6],
      Topic_STM_7 = new_doc_theta[7],
      Topic_STM_8 = new_doc_theta[8],
      Topic_STM_9 = new_doc_theta[9],
      Topic_STM_10 = new_doc_theta[10],
      Topic_LDA_1 = topic_probabilities_lda$topics[1],
      Topic_LDA_2 = topic_probabilities_lda$topics[2],
      Topic_LDA_3 = topic_probabilities_lda$topics[3],
      Topic_LDA_4 = topic_probabilities_lda$topics[4],
      Topic_LDA_5 = topic_probabilities_lda$topics[5],
      Topic_LDA_6 = topic_probabilities_lda$topics[6],
      Topic_LDA_7 = topic_probabilities_lda$topics[7],
      Topic_LDA_8 = topic_probabilities_lda$topics[8],
      Topic_LDA_9 = topic_probabilities_lda$topics[9],
      Topic_LDA_10 = topic_probabilities_lda$topics[10]
    )
    print(colnames(model.matrix(~., data = input_data)))
    
    #input_data <- input_data[, colnames(model.matrix(~., data = input_data))]
    
    print(xgb_model$feature_names)
    
    
    input_pca <- predict(pca_model, newdata = input_data) |> as.data.frame()
    
    input_pca <-  input_pca[, 1:23]
    
    output$lm_output <- renderText({
      paste("Linear Model Prediction:", round(predict(lm_model, input_pca), 2), "\n",
            "Random Forest Prediction:", round(predict(rf_model, input_data), 2), "\n",
            "XGBoost Prediction:", round(predict(xgb_model, as.matrix(input_data[,xgb_model$feature_names])), 2))
    })
    
    #output$rf_output <- renderText({
    #  paste("Random Forest Prediction:", round(predict(rf_model, input_data), 2))
    #})
    
    #output$xgb_output <- renderText({
    #  paste("XGBoost Prediction:", round(predict(xgb_model, as.matrix(input_data[,xgb_model$feature_names])), 2))
    #})
    
    output$prediction_plot <- renderPlot({
      lm_prediction <- 0
      lm_prediction <- round(predict(lm_model, input_pca), 2)
      rf_prediction <- round(predict(rf_model, input_data), 2)
      xgb_prediction <- round(predict(xgb_model, as.matrix(input_data[, xgb_model$feature_names])), 2)
      
      
      prediction_data <- data.frame(
        Model = c("Linear Model", "Random Forest", "XGBoost"),
        Prediction = c(lm_prediction, rf_prediction, xgb_prediction)
      )
      
      
      ggplot(prediction_data, aes(x = Model, y = Prediction, fill = Model)) +
        geom_bar(stat = "identity", width = 0.7, alpha = 0.8) +
        labs(
          title = "Citations Predictions",
          x = "Model",
          y = "Predicted Citations"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          legend.position = "none"
        )
      
    })
    top_terms_lda <- terms(lda_model, 5) %>% as.data.frame()
    top_terms_stm <- labelTopics(dtm_model, n = 5)$prob %>% t() %>% as.data.frame()
    
    highest_topic_lda <- which.max(new_doc_theta)
    highest_topic_stm <- which.max(topic_probabilities_lda$topics)
    
    
    output_text <- "your paper topic probabilities from LDA:\n"
    output$lda_topics <- renderText({
      paste(output_text,"\n",
            "Topic A:", round(new_doc_theta[1], 4), "\t",
            "Topic B:", round(new_doc_theta[2], 4), "\n",
            "Topic C:", round(topic_probabilities_lda$topics[3], 4), "\t",
            "Topic D:", round(new_doc_theta[4], 4), "\n",
            "Topic E:", round(new_doc_theta[5], 4), "\t",
            "Topic F:", round(new_doc_theta[6], 4), "\n",
            "Topic G:", round(new_doc_theta[7], 4), "\t",
            "Topic H:", round(new_doc_theta[8], 4), "\n",
            "Topic I:", round(new_doc_theta[9], 4), "\t",
            "Topic J:", round(new_doc_theta[10], 4), "\n",
            "...")
    })
    
    output$lda_terms <- renderTable({
      as.data.frame(top_terms_lda)
    })
    
    output_text1 <- "your paper topic probabilities from STM:\n"
    output$stm_topics <- renderText({
      paste(output_text1,"\n",
            "Topic 1:", round(topic_probabilities_lda$topics[1], 4), "\t",
            "Topic 2:", round(topic_probabilities_lda$topics[2], 4), "\n",
            "Topic 3:", round(topic_probabilities_lda$topics[3], 4), "\t",
            "Topic 4:", round(topic_probabilities_lda$topics[4], 4), "\n",
            "Topic 5:", round(topic_probabilities_lda$topics[5], 4), "\t",
            "Topic 6:", round(topic_probabilities_lda$topics[6], 4), "\n",
            "Topic 7:", round(topic_probabilities_lda$topics[7], 4), "\t",
            "Topic 8:", round(topic_probabilities_lda$topics[8], 4), "\n",
            "Topic 9:", round(topic_probabilities_lda$topics[9], 4), "\t",
            "Topic 10:", round(topic_probabilities_lda$topics[10], 4), "\n",
            "...")
    })
    
    output$lda_terms <- renderText({
      paste0("The highest probability topic terms from LDA includes: ", paste(top_terms_lda[, highest_topic_lda], collapse = ", "),"...")
    })
    output$stm_terms <- renderText({
      paste0("The highest probability topic terms from STM includes: ", paste(top_terms_stm[, highest_topic_stm], collapse = ", "),"...")
    })
    
    output$note <- renderText({
      "NOTE: The key words are from the highest probility topics from LDA/STM models, more information can be found in the paper:"
    })
    
    output$link <- renderText({
      HTML('<a href="https://github.com/phy-guanzh/Paper_Journal_Classification/blob/main/Where_Will_Your_Paper_Go__Predicting_Journal_Submission_and_Citations_with_Topic_Models.pdf" target="_blank"> Where Will Your Paper Go? Predicting Journal Submission and Citations with Topic Models</a>')
    })
    
  })
}

shinyApp(ui = ui, server = server)
