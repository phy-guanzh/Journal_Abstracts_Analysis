library(tidyr)
library(dplyr)
library(tidytext)
library(textstem)
library(topicmodels)


LDA_analysis_bi <- function(groups, journal_data, k_values){
  
  for (group in groups) {
    
    journal_token_bi <- journal_data %>%
      filter(year %in% group) %>% 
      select(row_id,title_abstract) %>% 
      unnest_tokens(output = bigrams, title_abstract, token = "ngrams", n = 2) %>% 
      filter(!grepl("\\b\\d+\\b", bigrams)) %>% 
      separate(bigrams, into = c("word1", "word2"), sep = " ")   %>% 
      filter(!(word1 %in% stop_words$word) & !(word2 %in% stop_words$word)) %>% unite(bigrams, word1, word2, sep = " ")
    
    
    dtm <- journal_token_bi %>%
      count(row_id, bigrams) %>%
      cast_dtm(document = row_id, term = bigrams, value = n)
    
    
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
