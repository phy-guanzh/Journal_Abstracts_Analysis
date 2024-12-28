library(tidyr)
library(dplyr)
library(tidytext)
library(wordcloud2)
library(textstem)
library(ggplot2)
library(ggrepel)
library(topicmodels)

cleaning_plot_Uni <- function(journal_token, year) {
      #cleaning
      journal_token <- journal_token %>% rename(index_paper = row_id)
      data("stop_words")
      journal_token_cleaned <- journal_token %>% anti_join(stop_words, by = c("word" = "word"))
      journal_token_cleaned <- journal_token_cleaned %>% filter(!grepl("\\b\\d+\\b", word))
      top_100_words_journal <- slice_head(journal_token_cleaned %>% count(word) %>% arrange(desc(n)), n = 100) 
      
      ifelse(length(year) > 1,
             title_name <- paste("Top 10 Frequency Unigrams in", year[1], "-", year[length(year)]),
             title_name <- paste("Top 10 Frequency Unigrams in", year[1])
      )
      
      #plot
      p <- ggplot(data=top_100_words_journal  %>% slice_head(n = 10) ,
             aes(x=reorder(word, -n), y=n)) +
        geom_bar(stat="identity", color = dark_blue, fill = blue) +
        theme_minimal()+
        labs(
          title = title_name,
          x = "",
          y = "Frequency"
        )+
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", size = 8),  
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 12),
          panel.grid = element_blank(),
          axis.text.x = element_text(size = 5),
          axis.line = element_line(arrow = arrow(length = unit(0.15, "cm"), type = "closed"))
        )
      
      return(p)
}


cleaning_plot_Bi <- function(journal_token_bi, year) {
      journal_token_bi <- journal_token_bi %>% rename(index_paper = row_id)
      data("stop_words")
      journal_token_bi_cleaned <- journal_token_bi %>% filter(!grepl("\\b\\d+\\b", bigrams))
      bigrams_separated <- journal_token_bi_cleaned %>% separate(bigrams, into = c("word1", "word2"), sep = " ")
      bigrams_separated_remove_stop <- bigrams_separated %>% filter(!(word1 %in% stop_words$word) & !(word2 %in% stop_words$word)) %>% unite(bigrams, word1, word2, sep = " ")
      bigrams_separated_cleaned <- bigrams_separated_remove_stop  %>% count(bigrams) %>% arrange(desc(n)) %>% slice_head(n = 100)
      
      ifelse(length(year)>1,
            title_name <- paste("Top 10 Frequency Bigrams in", year[1], "-", year[length(year)]),
            title_name <- paste("Top 10 Frequency Bigrams in", year[1])
      )
      ggplot(data=bigrams_separated_cleaned  %>% slice_head(n = 10) ,
             aes(x=reorder(bigrams, -n), y=n)) +
        geom_bar(stat="identity", color = red, fill = bright_red) +
        theme_minimal()+
        labs(
          title = title_name,
          x = "",
          y = "Frequency"
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
}

tfidf_plot_Bi <- function(journal_token_bi, year){
  journal_token_bi <- journal_token_bi %>% rename(index_paper = row_id)
  data("stop_words")
  journal_token_bi_cleaned <- journal_token_bi %>% filter(!grepl("\\b\\d+\\b", bigrams))
  bigrams_separated <- journal_token_bi_cleaned %>% separate(bigrams, into = c("word1", "word2"), sep = " ")
  bigrams_separated_remove_stop <- bigrams_separated %>% filter(!(word1 %in% stop_words$word) & !(word2 %in% stop_words$word)) %>% unite(bigrams, word1, word2, sep = " ")
  bigrams_separated_cleaned <- bigrams_separated_remove_stop  %>% count(bigrams) %>% arrange(desc(n))
  
  ifelse(length(year) > 1,
         title_name <- paste("Top 20 Tf-Idf Bigrams terms in ", year[1], "-", year[length(year)]),
         title_name <- paste("Top 20 Tf-Idf Bigrams terms in ", year[1])
  )
  
  bigrams_tfidf <- bigrams_separated_remove_stop %>%
    count(index_paper, bigrams) %>%        
    bind_tf_idf(bigrams, index_paper, n) %>%  
    arrange(desc(tf_idf)) %>% slice_head(n = 20)
  
  ifelse(year[1] == 2000, index_max <- 100,index_max <- 4000)
  
  ggplot(bigrams_tfidf, aes(x = bigrams, y = index_paper, size = tf_idf)) +
    geom_point(alpha = 0.6, color = violet) +
    geom_text_repel(aes(label = ifelse(tf_idf > 0.3, bigrams, "")), size = 2) +
    labs(
      title = title_name,
      x = "Bigrams",
      y = "Paper Index"
    ) +
    theme_minimal()+
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 8),  
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10),
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 90, hjust = 1, size = 5)
    )+
    ylim(0, index_max)
  
  
}

tfidf_plot_Uni <- function(journal_token, year){
  
  journal_token <- journal_token %>% rename(index_paper = row_id)
  data("stop_words")
  journal_token_cleaned <- journal_token %>% anti_join(stop_words, by = c("word" = "word"))
  journal_token_cleaned <- journal_token_cleaned %>% filter(!grepl("\\b\\d+\\b", word))
  
  ifelse(length(year) > 1,
         title_name <- paste("Top 20 Tf-Idf Unigrams in", year[1], "-", year[length(year)]),
         title_name <- paste("Top 20 Tf-Idf Unigrams in", year[1])
  )
  
  unigrams_tfidf <- journal_token_cleaned %>%
    count(index_paper, word) %>%        
    bind_tf_idf(word, index_paper, n) %>%  
    arrange(desc(tf_idf)) %>% slice_head(n = 20)
  
  
  ggplot(unigrams_tfidf, aes(x = word, y = index_paper, size = tf_idf)) +
    geom_point(alpha = 0.6, color = violet2) +
    geom_text_repel(aes(label = ifelse(tf_idf > 0.3, word, "")), size = 2) +
    labs(
      title = title_name,
      x = "Word",
      y = "Paper Index"
    ) +
    theme_minimal()+
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 8),  
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10),
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 90, hjust = 1, size = 5)
    )
  
}



LDA_analysis <- function(groups, journal_data, k_values){

  for (group in groups) {
    
    journal_token <- journal_data %>% 
      filter(year %in% group) %>%
      select(row_id,title_abstract) %>%
      unnest_tokens(output = word, title_abstract, token = "words") %>%  
      anti_join(stop_words, by = c("word" = "word"))   %>% 
      filter(!grepl("\\b\\d+\\b", word)) 
    
    print(journal_token)
   
    dtm <- journal_token %>%
      count(row_id, word) %>%
      cast_dtm(document = row_id, term = word, value = n)
    
   
    for (k in k_values) {
    
      lda_model <- LDA(dtm, k = k, method = "Gibbs", control = list(seed = 42))
      
    
      perplexity_score <- perplexity(lda_model, dtm)
      
     
      lda_evaluation[[paste0("Group_", paste(group, collapse = "_"), "_K_", k)]] <- list(
        group = group,
        k = k,
        model = lda_model,
        perplexity = perplexity_score
      )
    }
  }
  return(lda_evaluation)
}


tf_idf_table_bi <- function(journal_token_bi){
  
  #journal_token_bi <- journal_token_bi %>% rename(row_id = row_id)
  data("stop_words")
  journal_token_bi_cleaned <- journal_token_bi %>% filter(!grepl("\\b\\d+\\b", bigrams))
  bigrams_separated <- journal_token_bi_cleaned %>% separate(bigrams, into = c("word1", "word2"), sep = " ")
  bigrams_separated_remove_stop <- bigrams_separated %>% filter(!(word1 %in% stop_words$word) & !(word2 %in% stop_words$word)) %>% unite(bigrams, word1, word2, sep = " ")
  bigrams_separated_cleaned <- bigrams_separated_remove_stop  %>% count(bigrams) %>% arrange(desc(n))
  
  bigrams_tfidf <- bigrams_separated_remove_stop %>%
    count(row_id, bigrams) %>%        
    bind_tf_idf(bigrams, row_id, n) %>%  
    arrange(desc(tf_idf))
  
  return(bigrams_tfidf)
  
}

tf_idf_table <- function(journal_token){
  
  journal_token <- journal_token %>% rename(index_paper = row_id)
  data("stop_words")
  journal_token_cleaned <- journal_token %>% anti_join(stop_words, by = c("word" = "word"))
  journal_token_cleaned <- journal_token_cleaned %>% filter(!grepl("\\b\\d+\\b", word))
  
  unigrams_tfidf <- journal_token_cleaned %>%
    count(index_paper, word) %>%        
    bind_tf_idf(word, index_paper, n) %>%  
    arrange(desc(tf_idf)) %>% head(200)
  
  return(unigrams_tfidf)
  
}