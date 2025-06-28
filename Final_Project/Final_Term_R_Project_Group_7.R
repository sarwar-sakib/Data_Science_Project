if (!require(rvest)) install.packages("rvest"); library(rvest)
if (!require(NLP)) install.packages("NLP"); library(NLP)
if (!require(tm)) install.packages("tm"); library(tm)
if (!require(SnowballC)) install.packages("SnowballC"); library(SnowballC)
if (!require(textclean)) install.packages("textclean"); library(textclean)
if (!require(textstem)) install.packages("textstem"); library(textstem)
library(textclean)

url_data <- read.csv("G:/#AIUB 8th/Final_Term_R_Project/urls.csv", stringsAsFactors = FALSE)

print(url_data)

scraped_df <- data.frame(
  headline = character(),
  date = character(),
  articleData = character(),
  cleaned_text = character(),
  stringsAsFactors = FALSE
)

for (i in 1:nrow(url_data)) {
  
  current_url <- url_data$url[i]
  
  webpage <- tryCatch(read_html(current_url), error = function(e) NA)
  if (is.na(webpage)) next
  
  heading <- html_node(webpage, ".dfvxux")
  headtext <- if (!is.na(heading)) html_text(heading, trim = TRUE) else NA
  
  date_node <- html_node(webpage, ".IvNnh")
  dateText <- if (!is.na(date_node)) html_text(date_node, trim = TRUE) else NA
  
  paragraph_nodes <- html_nodes(webpage, "#main-content .hxuGS")
  pText <- html_text(paragraph_nodes, trim = TRUE)
  full_text <- paste(pText, collapse = " ")
  
  scraped_df <- rbind(scraped_df, data.frame(
    headline = headtext,
    date = dateText,
    articleData = full_text,
    cleaned_text = NA,  
    stringsAsFactors = FALSE
  ))
}
scraped_df <- scraped_df[!is.na(scraped_df$headline) & !is.na(scraped_df$date), ]

write.csv(scraped_df,"G:/#AIUB 8th/Final_Term_R_Project/scraped_data.csv", row.names = FALSE )
View(scraped_df)


for (i in 1:nrow(scraped_df)) {
  
  text <- scraped_df$articleData[i] 
  
  text <- tolower(text)    
  text <- removeNumbers(text)   
  text <- replace_number(text)
  text <- removePunctuation(text) 
  text <- gsub("[[:punct:]]", "", text)
  text <- stripWhitespace(text)   
  text <- iconv(text, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  text <- gsub("http\\S+|www\\S+", "", text)
  text <- gsub("<[^>]+>", "", text)
  
  text <- replace_emoji(text)           
  
  text <- replace_contraction(text)         
  
  tokens <- unlist(strsplit(text, "\\s+"))  
  
  tokens <- tokens[!(tokens %in% stopwords("en"))]
  
  tokens <- wordStem(tokens, language = "en")
  tokens <- lemmatize_words(tokens)
  
  cleaned_text <- paste(tokens, collapse = " ")
  
  scraped_df$cleaned_text[i] <- cleaned_text
}

View(scraped_df)

updated_corpus_df <- data.frame( 
  updated_corpus= scraped_df$cleaned_text, 
  stringsAsFactors = FALSE
)

write.csv(updated_corpus_df,"G:/#AIUB 8th/Final_Term_R_Project/updated_corpus_df.csv", row.names = FALSE )

scraped_df <- read.csv("G:/#AIUB 8th/Final_Term_R_Project/scraped_data.csv")
updated_corpus_df <- read.csv("G:/#AIUB 8th/Final_Term_R_Project/updated_corpus_df.csv", header = TRUE)


install.packages("RColorBrewer")
install.packages("topicmodels")
install.packages("tidytext")
install.packages("ggplot2")

library(RColorBrewer)
library(topicmodels)
library(tidytext)
library(dplyr)
library(ggplot2)

text_data <- updated_corpus_df$updated_corpus

my_custom_stopwords <- c("news", "bbc", "say", "others", "example", "another", "etc",  "like", "one" )
stop_word_pattern <- paste0("\\b", my_custom_stopwords, "\\b", collapse = "|")
articles_text_cleaned_custom <- gsub(stop_word_pattern, " ", text_data, ignore.case = TRUE) # ignore.case is good practice
text_data <- stringr::str_squish(articles_text_cleaned_custom)

corpus <- VCorpus(VectorSource(text_data))
inspect(corpus[1])

dtm <- DocumentTermMatrix(corpus, control = list(wordLengths = c(3, Inf)))

print(dtm)
dtm_cleaned <- removeSparseTerms(dtm, sparse = 0.99)
print(dtm_cleaned)

row_totals <- apply(dtm, 1, sum)
dtm <- dtm[row_totals > 0, ]

if (nrow(dtm) == 0) stop("DTM is empty. Check your input text.")

dtm_matrix <- as.matrix(dtm_cleaned)
print("Document-Term Matrix")
View(dtm_matrix)

num_topics <- 5
lda_model <- LDA(dtm_cleaned, k = num_topics, control = list(seed = 1234))

top_terms_per_topic <- terms(lda_model, 10)
print(top_terms_per_topic)

lda_tidy <- tidy(lda_model, matrix = "beta")

top_terms <- lda_tidy %>%
  group_by(topic) %>%
  slice_max(order_by = beta, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(topic, -beta)

print("Top terms per topic:")
print(top_terms, n = 50)

doc_topic_proportions  <- tidy(lda_model, matrix = "gamma")

dominant_topics <- doc_topic_proportions  %>%
  group_by(document) %>%
  slice_max(order_by = gamma, n = 1) %>%
  ungroup()

print(dominant_topics)

topic_names <- c(
  "Climate & Environment",       
  "Human Life, Culture & Travel",    
  "Conflict of Russia & Ukrain",      
  "Gaza-Israel Conflict",     
  "Technology, Economy, Innovation & Governance"    
)

top_terms_named <- top_terms %>%
  mutate(topic_name = factor(topic_names[topic], levels = topic_names))

ggplot(top_terms_named, aes(x = reorder(term, beta), y = beta, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic_name, scales = "free") +
  coord_flip() +
  labs(
    title = "Top Terms per Topic",
    x = "Terms",
    y = "Importance (Beta)"
  ) +
  theme_minimal(base_size = 12)


topic_counts <- dominant_topics %>%
  count(topic) %>%
  arrange(desc(n))

ggplot(topic_counts, aes(x = topic_names[topic], y = n, fill =topic_names[topic])) +
  geom_bar(stat = "identity") +
  labs(
    title = "Number of Documents per Dominant Topic",
    x = "Topic",
    y = "Number of Documents",
    fill = "Topic"
  ) +
  theme_minimal() 

install.packages("wordcloud")
library(wordcloud)


create_single_topic_wordcloud <- function(lda_model, topic_id, num_words = 30) {
  
  par(mfrow = c(1, 1), mar = c(5.1, 4.1, 4.1, 2.1))
  topic_terms_probs <- posterior(lda_model)$terms[topic_id, ]
  sorted_terms <- sort(topic_terms_probs, decreasing = TRUE)
  top_n_terms <- head(sorted_terms, num_words)
  colors_palette <- brewer.pal(8, "Dark2")
  
  wordcloud(
    words = names(top_n_terms), 
    freq = top_n_terms * 1000,   
    min.freq = 0,               
    max.words = num_words,      
    random.order = FALSE,       
    colors = colors_palette,    
    main = paste("Topic", topic_id, "Word Cloud") 
  )
}

create_single_topic_wordcloud(lda_model, 1, num_words = 30)
create_single_topic_wordcloud(lda_model, 2, num_words = 30)
create_single_topic_wordcloud(lda_model, 3, num_words = 30)
create_single_topic_wordcloud(lda_model, 4, num_words = 30)
create_single_topic_wordcloud(lda_model, 5, num_words = 30)


install.packages("LDAvis")
library(LDAvis)

phi <- posterior(lda_model)$terms
theta <- posterior(lda_model)$topics
vocab <- colnames(phi)
doc_length <- rowSums(as.matrix(dtm_cleaned))
term_freq <- colSums(as.matrix(dtm_cleaned))

json <- createJSON(phi = phi, 
                   theta = theta,
                   vocab = vocab,
                   doc.length = doc_length,
                   term.frequency = term_freq,
                   topic.names = topic_names
)

serVis(json)


ggplot(top_terms, aes(x = topic_names[topic], y = reorder(term, beta), fill = beta)) +
  geom_tile(color = "blue") +
  scale_fill_viridis_c() +
  labs(title = "Heatmap of Top Terms per Topic", x = "Topic", y = "Terms") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))
