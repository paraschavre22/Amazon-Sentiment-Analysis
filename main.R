# Author1 - Paras_Vikas_Chavre



install.packages("tidyverse")
install.packages("rvest")
install.packages("robotstxt")
install.packages("stringr")
install.packages("C50")
install.packages("SnowballC")
install.packages("C50")
install.packages("wordcloud")
install.packages("wordcloud2")
install.packages("ggraph")
install.packages("igraph")
install.packages("cooccur")
install.packages("visNetwork")
install.packages("textplot")
install.packages("reshape2")


library(tidyverse)
library(rvest)
library(robotstxt)
library(stringr)
library(xml2)
library(XML)
library(ggplot2)
library(ggraph)
library(dplyr)
library(robotstxt)
library(cld2)
library(wordcloud)
library(C50)
library(tidytext)
library(SnowballC)
library(wordcloud2)
library(udpipe)
library(igraph)
library(cooccur)
library(visNetwork)
library(textplot)
library(ggplot2)

#robotstxt------------------------------



rtxt_text <- get_robotstxt("https://www.amazon.co.uk/")
rtxt_parase <- parse_robotstxt(rtxt_text)
names(rtxt_parase)
table(rtxt_parase$permissions$useragent, rtxt_parase$permissions$field)

rtxt_list <- robotstxt("https://www.amazon.co.uk/")
rtxt_list$bots
rtxt_list$permissions
rtxt_list$check(paths = "/", bot = "*")
paths_allowed("/", "https://www.amazon.co.uk/", bot = "*")

# identify yourself --------------------------------

stringr::str_c("Paras Chavre", "paraschavre77@gmail.com", "web scraping exercises (University)",
               R.version$platform, R.version$version.string, sep = ", ") %>%
  httr::user_agent() %>%
  httr::set_config()

url <- "https://www.amazon.co.uk/"
browser_session <- session(url = url)
browser_session$response$request$options$useragent


#-----------------------------------------------------

link <- "https://www.amazon.co.uk/Sony-MDRZX310L-AE-Foldable-Headphones-Metallic-Blue/dp/B00I3LUYNG/ref=cm_cr_arp_d_product_top?ie=UTF8"
html <- read_html(link)


title_html <- html_nodes(html, "span#productTitle")
title <- html_text(title_html)
trimws(title)

ProdDetails <- html %>% html_nodes("table#productDetails_techSpec_section_1") %>% 
  html_table(header = T) %>% .[[1]]
ProdDetails



ProdDetails2 <- html %>% html_nodes("table#productDetails_detailBullets_sections1") %>% 
  html_table(header = T) %>% .[[1]]
ProdDetails2

price_html <- html_nodes(html, "Span#apexPriceToPay")
price <- html_text(price_html)
head(price)
trimws(price)


Rating_count <- html_node(html,"#acrCustomerReviewText")
Rcount <- html_text(Rating_count)
head(Rcount)


Fdelivery <- html_nodes(html,"#mir-layout-DELIVERY_BLOCK-slot-PRIMARY_DELIVERY_MESSAGE_LARGE .a-text-bold")
delivery <- html_text(Fdelivery)
delivery

rate_html <- html_nodes(html, 'span#acrPopover')

rate <- html_text(rate_html)

rate <- str_replace_all(rate, "[\r\n\t]" , "")
rate <- str_trim(rate)
head(rate)



AmazonProdDetails = distinct(data.frame(Title = unlist(title),Price = unlist(title),Rcount= unlist(Rcount),Delivery= unlist(delivery),Rate = unlist(rate),stringsAsFactors = FALSE))
AmazonProdDetails
View(AmazonProdDetails)

write.csv(AmazonProdDetails,"AmazonProd.csv")


amazon_reviews <- function(id, page) {
  
  url <- paste0("https://www.amazon.co.uk/product-reviews/",
                id, "/?pageNumber=", page)
  html <- read_html(url)
  
  # Review title (UK and not-UK)
  title = html %>%
    html_elements("[class='a-size-base a-link-normal review-title a-color-base review-title-content a-text-bold']") %>%
    html_text2()
  
  title = title %>%
    c(html %>%
        html_elements("[class='a-size-base review-title a-color-base review-title-content a-text-bold']") %>%
        html_text2())
  
  # Review text (the same for UK and not-UK)
  text = html %>%
    html_elements("[class='a-size-base review-text review-text-content']") %>%
    html_text2()
  
  # Review stars (UK and not-UK)
  star = html %>%
    html_elements("[data-hook='review-star-rating']") %>%
    html_text2()
  
  star = star %>%
    c(html %>%
        html_elements("[data-hook='cmps-review-star-rating']") %>%
        html_text2())
  
  # Return a tibble
  tibble(title, text, star, page = page) %>%
    return()
}

id = "B00I3LUYNG"
page = 1:50
data = map_df(page, ~amazon_reviews(id = "B00I3LUYNG", page = .))

data$doc_id = 1:nrow(data)
save(data, file = "data.rda")
load("data.rda")
data
View(data)

#Data Cleaning and Pre- Processing




data$title_lang = detect_language(data$title)
data$text_lang = detect_language(data$text)

table(Text = data$text_lang, Title = data$title_lang, useNA = "always")
data = data %>%
  filter(text_lang == "en")
View(data)


#Score/ To Extract a numeric score from the "Stars" string


data = data %>%
  mutate(score = as.numeric(substring(star, 1, 1)))

summary(data$score)


data %>%
  count(score) %>%
  mutate(p = round(n/sum(n), 2))


data %>%
  ggplot(aes(x = score)) + geom_bar(aes(y = (..count..)), fill = "steelblue") +
  labs(title = "Amazon reviews' stars", subtitle = "Headphone",
       x = "Stars", y = "Number of comments") + theme_bw() +
  theme(plot.title = element_text(color = "steelblue", size = 12,
                                  face = "bold"), plot.subtitle = element_text(color = "steelblue2"))


data = data %>%
  mutate(star_sent = ifelse(score >= 4, "positive", "negative"))
data %>%
  count(star_sent) %>%
  mutate(p = n/sum(n))


data$nchar = str_length(data$text)
ggplot(data, aes(x = star_sent, y = nchar, fill = star_sent)) +
  geom_boxplot() + theme_bw() + scale_fill_manual(values = c("steelblue",
                                                             "skyblue"))





#Tokenization 


install.packages("tidytext")

install.packages("wordcloud")
library(wordcloud)



tidy_text <- data %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)



tidy_stem <- tidy_text %>% 
  mutate(word = wordStem(word))

install.packages("udpipe")

tidy_lemma <- udpipe(data, "english-gum")

tidy_lemma = tidy_lemma %>%
  mutate(stem = wordStem(token)) %>%
  tibble()
tidy_lemma

tidy_lemma %>%
  select(token, lemma, stem)

#additional Data Processing and Visualization 
data %>% 
  bind_rows() %>%
  unnest_tokens(output = "word", input = "text", token = "words") %>%
  count(word) %>%
  filter(!(word %in% c("headphone","headphones"))) %>%
  anti_join(tidytext::stop_words, by = "word") -> word_tb
dev.off()
wordcloud::wordcloud(words = word_tb$word, freq = word_tb$n)


install.packages("C50")
library(C50)


word.freq.table <- data %>% 
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  count(word, sort = TRUE)
word.freq.table

word.freq.table %>% 
  filter(n>1) %>%
  with(wordcloud(word, n,
                 scale = c(3,0.2),
                 colors = brewer.pal(8, "Dark2")))
dev.off()

# Visualization 
par("mar")
par(mar=c(1,1,1,1))

#Unigram

tidy_stem %>%
  count(word) %>%
  slice_max(n, n = 10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = word)) + geom_bar(stat = "identity",
                                               fill = "skyblue") + xlab(NULL) + labs(title = "Most common stems in reviews",
                                                                                     y = "Stems count") + theme(legend.position = "none", plot.title = element_text(color = "steelblue",
                                                                                                                                                                    size = 12, face = "bold")) + coord_flip() + theme_bw()
tidy_stem %>%
  group_by(star_sent) %>%
  count(word) %>%
  group_by(star_sent) %>%
  slice_max(n, n = 10, with_ties = F) %>%
  mutate(star_sent = as.factor(star_sent), word = reorder_within(word,
                                                                 n, star_sent)) %>%
  ggplot(aes(word, n, fill = star_sent)) + geom_col(show.legend = FALSE) +
  facet_wrap(~star_sent, scales = "free_y") + coord_flip() +
  labs(title = "Most common stems in positive/negative reviews",
       y = NULL, x = "N") + scale_x_reordered() + theme(legend.position = "none",
                                                        plot.title = element_text(color = "steelblue", "skyblue")) +
  scale_fill_manual(values = c("steelblue", "skyblue")) + theme_bw()



tidy_stem %>%
  group_by(star_sent) %>%
  count(word, sort = T) %>%
  mutate(prop = n/sum(n)) %>%
  select(star_sent, word, prop) %>%
  pivot_wider(names_from = star_sent, values_from = prop) %>%
  arrange(positive, negative) %>%
  ggplot(aes(positive, negative)) + geom_jitter(alpha = 0.5,
                                                size = 2.5, width = 0.25, height = 0.25, colour = "steelblue") +
  geom_text(aes(label = word), check_overlap = T, vjust = 0) +
  scale_x_log10(labels = scales::percent_format()) + scale_y_log10(labels = scales::percent_format()) +
  geom_abline(color = "red") + theme_bw()



word_ratios <- tidy_stem %>%
  count(word, star_sent) %>%
  group_by(word) %>%
  filter(sum(n) >= 10) %>%
  ungroup() %>%
  pivot_wider(names_from = star_sent, values_from = n, values_fill = 0) %>%
  mutate_if(is.numeric, list(~(. + 1)/(sum(.) + 1))) %>%
  mutate(logratio = log(positive/negative)) %>%
  arrange(desc(logratio))


word_ratios %>%
  group_by(logratio < 0) %>%
  slice_max(abs(logratio), n = 15) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) + geom_col(show.legend = FALSE) +
  coord_flip() + ylab("log odds ratio (Positive/Negative)") +
  scale_fill_manual(name = "", labels = c("Positive", "Negative"),
                    values = c("skyblue", "steelblue")) + theme_bw()




#additional Data Processing and Visualization 
data %>% 
  bind_rows() %>%
  unnest_tokens(output = "word", input = "text", token = "words") %>%
  count(word) %>%
  filter(!(word %in% c("headphone","headphones"))) %>%
  anti_join(tidytext::stop_words, by = "word") -> word_tb

wordcloud::wordcloud(words = word_tb$word, freq = word_tb$n)



word.freq.table <- data %>% 
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  count(word, sort = TRUE)
word.freq.table

word.freq.table %>% 
  filter(n>1) %>%
  with(wordcloud(word, n,
                 scale = c(5,0.5),
                 colors = brewer.pal(8, "Dark2")))

tidy_stem %>%
  count(word) %>%
  with(wordcloud(scale = c(5, 0.7), word, n, max.words = 100,
                 min.freq = 2, random.order = F, rot.per = 0.15, colors = brewer.pal(8,
                                                                                     "Paired")))

library(wordcloud2)
frame = tidy_text %>%
  count(word, sort = T)
frame = data.frame(word = frame$word, freq = frame$n)

wordcloud2(frame, color = "skyblue")
# -------------------------------------------------

# Biagrams

tidy_big_stem <- data %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  mutate(word1 = wordStem(word1)) %>%
  mutate(word2 = wordStem(word2))

bigram_counts = tidy_big_stem %>%
  count(word1, word2, sort = TRUE)


bigram_graph <- bigram_counts %>%
  filter(n >= 2) %>%
  graph_from_data_frame()


set.seed(9265)

a <- grid::arrow(type = "closed", length = unit(0.15, "inches"))

ggraph(bigram_graph, layout = "fr") + geom_edge_link(aes(edge_alpha = n),
                                                     show.legend = FALSE, arrow = a, end_cap = circle(0.07, "inches")) +
  geom_node_point(color = "skyblue", size = 5) + geom_node_text(aes(label = name),
                                                                vjust = 1, hjust = 1) + theme_void()
# Biagram using POS

cooc <- cooccurrence(tidy_lemma$lemma, relevant = tidy_lemma$upos %in%
                       c("NOUN", "ADJ"), skipgram = 1)
head(cooc)


wordnetwork <- head(cooc, 15)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") + geom_edge_link(aes(width = cooc,
                                                        edge_alpha = cooc), edge_colour = "skyblue") + geom_node_text(aes(label = name),
                                                                                                                      col = "darkblue", size = 4) + theme_void() + labs(title = "Words following one another",
                                                                                                                                                                        subtitle = "Nouns & Adjective")

# Co-Occurency using POS

cooc <- cooccurrence(x = subset(tidy_lemma, upos %in% c("NOUN",
                                                        "ADJ")), term = "lemma", group = c("doc_id"))
head(cooc)


wordnetwork <- head(cooc, 30)
wordnetwork <- graph_from_data_frame(wordnetwork)
ggraph(wordnetwork, layout = "fr") + geom_edge_link(aes(width = cooc,
                                                        edge_alpha = cooc), edge_colour = "skyblue") + geom_node_text(aes(label = name),
                                                                                                                      col = "darkblue", size = 4) + theme(legend.position = "none") +
  theme_void() + labs(title = "Cooccurrences within documents",
                      subtitle = "Nouns & Adjective")


# Dependency Parsing

textplot_dependencyparser(tidy_lemma %>%
                            filter(doc_id == "1" & sentence_id == "1"))





# Sentiment Analysis

# Dictionary Based Analysis

# The tidy approach

bing = get_sentiments("bing")
data_bing = tidy_text %>%
  select(doc_id, word) %>%
  inner_join(bing) %>%
  count(doc_id, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(bing = positive - negative)
data_bing
data = data %>%
  left_join(data_bing %>%
              select(doc_id,sentiment))
dev.off()
hist(data$bing, col = "lightblue", main = "Sentiment distribution - tidy(Bing)")

summary(data$bing)

bing_word_counts <- tidy_text %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

dev.off()

bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 5, with_ties = F) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) + geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") + labs(x = "Contribution to sentiment",
                                                   y = NULL) + theme_bw() + scale_fill_manual(values = c("steelblue",
                                                                                                         "skyblue"))
#AFINN lexicon Approach
install.packages("rlang")
library(rlang)
L = get_sentiments("afinn")

afinn <- tidy_text %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(doc_id = row_number() %/% 10) %>% 
  summarise(sentiment = sum(score))
 
afinn
data = data %>%
  left_join(afinn %>%
              select(doc_id, sentiment))

hist(data$sentiment, col = "lightblue", main = "Sentiment distribution - tidy (afinn)")

#NRC lexicon Approach

get_sentiments("nrc")

nrc <- tidy_text %>% 
  inner_join(get_sentiments("nrc") %>% filter(sentiment %in% c("positive","negative"))) %>%
  mutate(method = "NRC") %>% 
  count(method, doc_id = row_number() %/% 10, sentiment) %>%
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)
nrc  

data = data %>%
  left_join(nrc %>%
              select(doc_id,sentiment))
dev.off()

hist(data$sentiment, col = "lightblue", main = "Sentiment distribution - tidy(nrc)")

summary(data$sentiment)

nrc_word_counts <- tidy_text %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
nrc_word_counts

nrc_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 5, with_ties = F) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) + geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") + labs(x = "Contribution to sentiment",
                                                   y = NULL) + theme_bw() + scale_fill_manual(values = c("#CC0000", "#006600", "#669999", "#00CCCC", 
                                                                                                         "#660099", "#CC0066", "#FF9999", "#FF9900", 
                                                                                                         "black", "black", "black", "black", "black"))

# Bing

bing<- tidy_text %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(doc_id = row_number()%/% 10, sentiment) %>% 
  spread(sentiment,  n , fill = 0) %>% 
  mutate(sentiment = positive - negative) %>% 
  mutate(method = "BING")
bing




# Combining all the lexicon created

bind_rows(afinn, bing, nrc) %>% 
  ggplot(aes(doc_id ,sentiment ,positive,negative ,fill = method)) +
  geom_col(show.legend = FALSE)+
  facet_wrap(~method , ncol = 3, scales = "free_y")



# The udpide approach

data_udpipe <- udpipe(data, "english-gum")

scores <- txt_sentiment(x = data_udpipe, term = "lemma", polarity_terms = bing_dict,
                        polarity_negators = c("not"), polarity_amplifiers = c("very"),
                        n_before = 2, n_after = 0, constrain = F)
data$udpipe = scores$overall$sentiment_polarity

hist(data$udpipe, col = "lightblue", main = "Sentiment distribution - udpipe")
summary(data$udpipe)
 
par(mfrow = c(1, 3))
hist(scale(data$bing), col = "lightblue", main = "Sentiment distribution - bing")
hist(scale(data$sentiment), col = "lightblue", main = "Sentiment distribution - nrc")
hist(scale(data$udpipe), col = "lightblue", main = "Sentiment distribution - udpipe")

