library(tidyr)
library(dplyr)
library(tidytext)
library(textstem)
library(topicmodels)

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



get_lda_model <- function(group_name, k_value, lda_evaluation) {
  # 将 group_name 转换为整数向量
  group_vector <- as.integer(unlist(strsplit(group_name, "_")))
  
  # 找到符合条件的索引
  model_index <- which(sapply(lda_evaluation, function(x) {
    identical(x$group, group_vector) && x$k == k_value
  }))
  
  # 提取模型
  if (length(model_index) == 1) {
    return(lda_evaluation[[model_index]]$model)
  } else if (length(model_index) == 0) {
    stop("No matching model found!")
  } else {
    stop("Multiple matches found!")
  }
}

library(tidytext)
library(ggplot2)


plot_topic_terms <- function(lda_model, group_name, k_value) {
  # 转换模型结果为数据框
  topic_terms <- tidy(lda_model, matrix = "beta")  # "beta" 是主题-词分布矩阵
  
  # 获取每个主题的前 10 个词
  top_terms <- topic_terms %>%
    group_by(topic) %>%
    slice_max(beta, n = 10) %>%
    ungroup() %>%
    mutate(term = reorder_within(term, beta, topic))
  
  # 绘制条形图
  ggplot(top_terms, aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free", ncol = 4) +
    coord_flip() +
    labs(
      title = paste("Top 10 Terms in Each Topic (Group:", group_name, ", k =", k_value, ")"),
      x = "Terms", y = "Beta"
    ) +
    scale_x_reordered() +
    theme_minimal()
}