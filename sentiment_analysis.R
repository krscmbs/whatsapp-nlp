library(rwhatsapp)
library(tidyverse)
library(tidytext)
library(lubridate)
library(kableExtra)
library(RColorBrewer)
library(knitr)
library(ggimage)
library(tm)
library(SnowballC)
library(wordcloud)
library(syuzhet)
library(ggplot2)
library(rvest)
library(textdata)
library(wordcloud)
library(wordcloud2)
library(devtools)
library(scales)
devtools::install_github("lchiffon/wordcloud2")

data <- rwa_read('whatsapp_data.txt')

#### Clean Dates ####

data <- data %>%
  mutate(day = date(time)) %>%
  mutate(month = case_when(
      day >= dmy(01122019) & day <= dmy(31122019) ~ 'December 2019',
      day >= dmy(01012020) & day <= dmy(31012020) ~ 'January 2020',
      day >= dmy(01022020) & day <= dmy(29022020) ~ 'February 2020',
      day >= dmy(01032020) & day <= dmy(31032020) ~ 'March 2020',
      day >= dmy(01042020) & day <= dmy(30042020) ~ 'April 2020',
      day >= dmy(01052020) & day <= dmy(31052020) ~ 'May 2020',
      day >= dmy(01062020) & day <= dmy(30062020) ~ 'June 2020',
      day >= dmy(01072020) & day <= dmy(31072020) ~ 'July 2020',
      day >= dmy(01082020) & day <= dmy(31082020) ~ 'August 2020',
      day >= dmy(01092020) & day <= dmy(30092020) ~ 'September 2020',
      day >= dmy(01102020) & day <= dmy(31102020) ~ 'October 2020',
      day >= dmy(01112020) ~ 'November 2020')) %>% 
  mutate(month = factor(month)) %>% 
  filter(!is.na(author))

#### Filter Omitted Media ####

data$MediaOmitted <- with(data, if_else(text == '<Media omitted>', 1, 0))
data <- subset(data, data$MediaOmitted == 0)

#### Authors ####

#New ID

data$id <- row.names(data)

#Authors

levels(data$author)[2] <- 'Sinéad'
levels(data$author)[1] <- 'Kris'

#### Split emoji list ####

data <- data %>%
  separate(col = emoji, into = LETTERS[1:10], sep = ", ")

data <- rename(data, emoji_1 = A)
data <- rename(data, emoji_2 = B)
data <- rename(data, emoji_3 = C)
data <- rename(data, emoji_4 = D)
data <- rename(data, emoji_5 = E)
data <- rename(data, emoji_6 = `F`)
data <- rename(data, emoji_7 = G)
data <- rename(data, emoji_8 = H)
data <- rename(data, emoji_9 = I)
data <- rename(data, emoji_10 = J)

data <- data %>%
  separate(col = emoji_name, into = LETTERS[1:10], sep = ", ")

data <- rename(data, emoji_name_1 = A)
data <- rename(data, emoji_name_2 = B)
data <- rename(data, emoji_name_3 = C)
data <- rename(data, emoji_name_4 = D)
data <- rename(data, emoji_name_5 = E)
data <- rename(data, emoji_name_6 = `F`)
data <- rename(data, emoji_name_7 = G)
data <- rename(data, emoji_name_8 = H)
data <- rename(data, emoji_name_9 = I)
data <- rename(data, emoji_name_10 = J)

#### Join to data ####

# data <- left_join(data, emoji_sentiment_score, by = c("emoji_1" = "emoji"))
# data <- left_join(data, emoji_sentiment_score, by = c("emoji_2" = "emoji"))
# data <- left_join(data, emoji_sentiment_score, by = c("emoji_3" = "emoji"))

#Recode

data$emoji_1[data$emoji_1 == 'NULL'] <- NA
data$emoji_name_1[data$emoji_name_1 == 'NULL'] <- NA

#### Clean emoji splits ####

data$emoji_1 <- gsub("c(", "", data$emoji_1, fixed = TRUE)
data$emoji_1 <- gsub('"', "", data$emoji_1, fixed = TRUE)
data$emoji_2 <- gsub('")', "", data$emoji_2, fixed = TRUE)
data$emoji_2 <- gsub('"', "", data$emoji_2, fixed = TRUE)
data$emoji_3 <- gsub('")', "", data$emoji_3, fixed = TRUE)
data$emoji_3 <- gsub('"', "", data$emoji_3, fixed = TRUE)
data$emoji_4 <- gsub('")', "", data$emoji_4, fixed = TRUE)
data$emoji_4 <- gsub('"', "", data$emoji_4, fixed = TRUE)
data$emoji_5 <- gsub('")', "", data$emoji_5, fixed = TRUE)
data$emoji_5 <- gsub('"', "", data$emoji_5, fixed = TRUE)
data$emoji_6 <- gsub('")', "", data$emoji_6, fixed = TRUE)
data$emoji_6 <- gsub('"', "", data$emoji_6, fixed = TRUE)
data$emoji_7 <- gsub('")', "", data$emoji_7, fixed = TRUE)
data$emoji_7 <- gsub('"', "", data$emoji_7, fixed = TRUE)
data$emoji_8 <- gsub('")', "", data$emoji_8, fixed = TRUE)
data$emoji_8 <- gsub('"', "", data$emoji_8, fixed = TRUE)
data$emoji_9 <- gsub('")', "", data$emoji_9, fixed = TRUE)
data$emoji_9 <- gsub('"', "", data$emoji_9, fixed = TRUE)
data$emoji_10 <- gsub('")', "", data$emoji_10, fixed = TRUE)
data$emoji_10 <- gsub('"', "", data$emoji_10, fixed = TRUE)

data$emoji_name_1 <- gsub("c(", "", data$emoji_name_1, fixed = TRUE)
data$emoji_name_1 <- gsub('"', "", data$emoji_name_1, fixed = TRUE)
data$emoji_name_2 <- gsub('")', "", data$emoji_name_2, fixed = TRUE)
data$emoji_name_2 <- gsub('"', "", data$emoji_name_2, fixed = TRUE)
data$emoji_name_3 <- gsub('")', "", data$emoji_name_3, fixed = TRUE)
data$emoji_name_3 <- gsub('"', "", data$emoji_name_3, fixed = TRUE)
data$emoji_name_4 <- gsub('")', "", data$emoji_name_4, fixed = TRUE)
data$emoji_name_4 <- gsub('"', "", data$emoji_name_4, fixed = TRUE)
data$emoji_name_5 <- gsub('")', "", data$emoji_name_5, fixed = TRUE)
data$emoji_name_5 <- gsub('"', "", data$emoji_name_5, fixed = TRUE)
data$emoji_name_6 <- gsub('")', "", data$emoji_name_6, fixed = TRUE)
data$emoji_name_6 <- gsub('"', "", data$emoji_name_6, fixed = TRUE)
data$emoji_name_7 <- gsub('")', "", data$emoji_name_7, fixed = TRUE)
data$emoji_name_7 <- gsub('"', "", data$emoji_name_7, fixed = TRUE)
data$emoji_name_8 <- gsub('")', "", data$emoji_name_8, fixed = TRUE)
data$emoji_name_8 <- gsub('"', "", data$emoji_name_8, fixed = TRUE)
data$emoji_name_9 <- gsub('")', "", data$emoji_name_9, fixed = TRUE)
data$emoji_name_9 <- gsub('"', "", data$emoji_name_9, fixed = TRUE)
data$emoji_name_10 <- gsub('")', "", data$emoji_name_10, fixed = TRUE)
data$emoji_name_10 <- gsub('"', "", data$emoji_name_10, fixed = TRUE)

#### String Cleaning ####

#Add whitespace

data$sentiment_text <- str_pad(data$text, width = 1, side = "right")

#Replace in strings

data$sentiment_text <- if_else(is.na(data$emoji_1) == TRUE, data$text, str_replace(data$text, data$emoji_1, paste0(" ", data$emoji_name_1)))
data$sentiment_text <- if_else(is.na(data$emoji_2) == TRUE, data$sentiment_text, str_replace(data$sentiment_text, data$emoji_2, paste0(" ", data$emoji_name_2)))
data$sentiment_text <- if_else(is.na(data$emoji_3) == TRUE, data$sentiment_text, str_replace(data$sentiment_text, data$emoji_3, paste0(" ", data$emoji_name_3)))
data$sentiment_text <- if_else(is.na(data$emoji_4) == TRUE, data$sentiment_text, str_replace(data$sentiment_text, data$emoji_4, paste0(" ", data$emoji_name_4)))
data$sentiment_text <- if_else(is.na(data$emoji_5) == TRUE, data$sentiment_text, str_replace(data$sentiment_text, data$emoji_5, paste0(" ", data$emoji_name_5)))
data$sentiment_text <- if_else(is.na(data$emoji_6) == TRUE, data$sentiment_text, str_replace(data$sentiment_text, data$emoji_6, paste0(" ", data$emoji_name_6)))
data$sentiment_text <- if_else(is.na(data$emoji_7) == TRUE, data$sentiment_text, str_replace(data$sentiment_text, data$emoji_7, paste0(" ", data$emoji_name_7)))
data$sentiment_text <- if_else(is.na(data$emoji_8) == TRUE, data$sentiment_text, str_replace(data$sentiment_text, data$emoji_8, paste0(" ", data$emoji_name_8)))
data$sentiment_text <- if_else(is.na(data$emoji_9) == TRUE, data$sentiment_text, str_replace(data$sentiment_text, data$emoji_9, paste0(" ", data$emoji_name_9)))
data$sentiment_text <- if_else(is.na(data$emoji_10) == TRUE, data$sentiment_text, str_replace(data$sentiment_text, data$emoji_10, paste0(" ", data$emoji_name_10)))

#### Create Corpus ####

#Get word count list

data$wordcloud_text <- str_replace_all(data$text,"[^[:graph:]]", " ")
text <- Corpus(VectorSource(data$wordcloud_text))

#Clean corpus

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
text <- tm_map(text, toSpace, "/")
text <- tm_map(text, toSpace, "@")
text <- tm_map(text, toSpace, "\\|")
text <- tm_map(text, function(x) iconv(text$content, "UTF-8", "UTF-8", sub = ""))
#text <- tm_map(text, function(x) iconv(enc2utf8(text$content), sub = ""))
text <- tm_map(text, content_transformer(tolower))
text <- tm_map(text, removeNumbers)
text <- tm_map(text, removeWords, stopwords("english"))
text <- tm_map(text, removeWords, c("what", 
                                    "just", 
                                    "get",
                                    "its",
                                    "ill",
                                    "now",
                                    "want",
                                    "see",
                                    "the",
                                    "but",
                                    "why",
                                    "when",
                                    "where",
                                    "ive",
                                    "https",
                                    "http",
                                    "www",
                                    "com",
                                    "co"))
text <- tm_map(text, removePunctuation)
text <- tm_map(text, stripWhitespace)
#text <- tm_map(text, stemDocument)

words <- TermDocumentMatrix(text)
words <- as.matrix(words)
words <- sort(rowSums(words), decreasing = TRUE)
words <- data.frame(word = names(words), freq = words)

#### Wordclouds ####

wordcloud2(words, figPath = "silhouette_3.jpg")

#Split by author

sinead$wordcloud_text <- str_replace_all(sinead$text,"[^[:graph:]]", " ")
sinead_text <- Corpus(VectorSource(sinead$wordcloud_text))
sinead_text <- tm_map(sinead_text, toSpace, "/")
sinead_text <- tm_map(sinead_text, toSpace, "@")
sinead_text <- tm_map(sinead_text, toSpace, "\\|")
sinead_text <- tm_map(sinead_text, function(x) iconv(sinead_text$content, "UTF-8", "UTF-8", sub = ""))
sinead_text <- tm_map(sinead_text, function(x) iconv(enc2utf8(sinead_text$content), sub = ""))
sinead_text <- tm_map(sinead_text, content_transformer(tolower))
sinead_text <- tm_map(sinead_text, removeNumbers)
sinead_text <- tm_map(sinead_text, removeWords, stopwords("english"))
sinead_text <- tm_map(sinead_text, removeWords, c("what", 
                                    "just", 
                                    "get",
                                    "its",
                                    "ill",
                                    "now",
                                    "want",
                                    "see",
                                    "the",
                                    "but",
                                    "why",
                                    "when",
                                    "where",
                                    "ive",
                                    "https",
                                    "http",
                                    "www",
                                    "com",
                                    "co"))
sinead_text <- tm_map(sinead_text, removePunctuation)
sinead_text <- tm_map(sinead_text, stripWhitespace)

sinead_words <- TermDocumentMatrix(sinead_text)
sinead_words <- as.matrix(sinead_words)
sinead_words <- sort(rowSums(sinead_words), decreasing = TRUE)
sinead_words <- data.frame(sinead_words = names(sinead_words), freq = sinead_words)

kris$wordcloud_text <- str_replace_all(kris$text,"[^[:graph:]]", " ")
kris_text <- Corpus(VectorSource(kris$wordcloud_text))
kris_text <- tm_map(kris_text, toSpace, "/")
kris_text <- tm_map(kris_text, toSpace, "@")
kris_text <- tm_map(kris_text, toSpace, "\\|")
kris_text <- tm_map(kris_text, function(x) iconv(kris_text$content, "UTF-8", "UTF-8", sub = ""))
kris_text <- tm_map(kris_text, function(x) iconv(enc2utf8(kris_text$content), sub = ""))
kris_text <- tm_map(kris_text, content_transformer(tolower))
kris_text <- tm_map(kris_text, removeNumbers)
kris_text <- tm_map(kris_text, removeWords, stopwords("english"))
kris_text <- tm_map(kris_text, removeWords, c("what", 
                                                  "just", 
                                                  "get",
                                                  "its",
                                                  "ill",
                                                  "now",
                                                  "want",
                                                  "see",
                                                  "the",
                                                  "but",
                                                  "why",
                                                  "when",
                                                  "where",
                                                  "ive",
                                                  "https",
                                                  "http",
                                                  "www",
                                                  "com",
                                                  "co"))
kris_text <- tm_map(kris_text, removePunctuation)
kris_text <- tm_map(kris_text, stripWhitespace)

kris_words <- TermDocumentMatrix(kris_text)
kris_words <- as.matrix(kris_words)
kris_words <- sort(rowSums(kris_words), decreasing = TRUE)
kris_words <- data.frame(kris_words = names(kris_words), freq = kris_words)

#### Sentiment and Emotion Analysis ####

#Unique words

labels <- c("Sinéad","Kris")
values <- c(6151,9426)
unique_count <- data.frame(labels, values)

unique_words_plot <- unique_count %>% ggplot(aes(y = values, x = labels)) + 
  geom_bar(stat = "identity", width = 0.95, color = "black", fill = c("#600f6b", "#03860a")) + 
  theme_transparent() + 
  scale_y_continuous(label = comma, limits = c(0,10000)) + 
  ggtitle("Unique Words Used") + 
  theme(axis.title.x = element_blank(),
        axis.line.y = element_line(colour = "black"),
        axis.line.x = element_line(colour = "black"),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(color = "black", size = 10, vjust = 0),
        axis.text.y = element_text(color = "black", size = 10, angle = 45),
        plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))

unique_words_plot

ggplot(aes(fill = Emotion, y = percent, x = author)) + 
  geom_bar(position = "fill", stat = "identity", width = 1, color = "black") + 
  geom_text(aes(label = paste0(sprintf("%1.1f", percent*100), "%")),
            position = position_stack(vjust = 0.5), colour = "black", size = 2.5) + 
  scale_y_continuous(labels = percent_format()) + 
  labs(x = "Author", y = "Frequency") + 
  coord_flip() + 
  theme_transparent() + 
  guides(fill = guide_legend(ncol = 4)) + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(color = "black", angle = 90, size = 6, vjust = -10, hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_text(vjust = 0.5, size = 8),
        legend.text = element_text(size = 8),
        legend.box.margin = margin(-15,-15,0,-15))

emotion_plot

#Text sentiment (three ways)

syuzhet_vector <- get_sentiment(data$sentiment_text, method = "syuzhet")
head(syuzhet_vector)
summary(syuzhet_vector)
syuzhet_score <- data.frame(syuzhet_vector)
syuzhet_score$id <- row.names(syuzhet_score)

bing_vector <- get_sentiment(data$sentiment_text, method = "bing")
head(bing_vector)
summary(bing_vector)
bing_score <- data.frame(bing_vector)
bing_score$id <- row.names(bing_score)

afinn_vector <- get_sentiment(data$sentiment_text, method = "afinn")
head(afinn_vector)
summary(afinn_vector)
afinn_score <- data.frame(afinn_vector)
afinn_score$id <- row.names(afinn_score)

data <- left_join(data, syuzhet_score, by = "id")
data <- left_join(data, bing_score, by = "id")
data <- left_join(data, afinn_score, by = "id")

#Emotion classification

emotion <- get_nrc_sentiment(data$sentiment_text)
head(emotion,10)

emotion <- data.frame(emotion)
emotion$id <- row.names(emotion)

data <- left_join(data, emotion, by = "id")

#Sense check emotions barplot

emotion_t <- data.frame(t(emotion))

emotion_t_rowsums <- data.frame(rowSums(emotion_t))
names(emotion_t_rowsums)[1] <- "count"
emotion_t_rowsums <- cbind("sentiment" = rownames(emotion_t_rowsums), emotion_t_rowsums)
rownames(emotion_t_rowsums) <- NULL
emotion_t_rowsums_2 <- emotion_t_rowsums[1:8,]

quickplot(sentiment, data = emotion_t_rowsums_2, weight = count, geom = "bar", fill = sentiment, ylab = "count") + 
  ggtitle("Chat Emotions")

#Author groups

sinead <- subset(data, data$author == "Sinéad")
kris <- subset(data, data$author == "Kris")

#Emotion by group

sinead_emotion <- dplyr::select(sinead, anger, anticipation, disgust, fear, joy, sadness, surprise, trust)
sinead_emotion <- na.omit(sinead_emotion)
sinead_emotion_t <- data.frame(t(sinead_emotion))
sinead_emotion_t_rowsums <- data.frame(rowSums(sinead_emotion_t))
names(sinead_emotion_t_rowsums)[1] <- "count"
sinead_emotion_t_rowsums <- cbind("sentiment" = rownames(sinead_emotion_t_rowsums), sinead_emotion_t_rowsums)
rownames(sinead_emotion_t_rowsums) <- NULL
sinead_emotion_t_rowsums <- sinead_emotion_t_rowsums[1:8,]
sinead_emotion_t_rowsums <- rename(sinead_emotion_t_rowsums, sinead = count)

kris_emotion <- dplyr::select(kris, anger, anticipation, disgust, fear, joy, sadness, surprise, trust)
kris_emotion <- na.omit(kris_emotion)
kris_emotion_t <- data.frame(t(kris_emotion))
kris_emotion_t_rowsums <- data.frame(rowSums(kris_emotion_t))
names(kris_emotion_t_rowsums)[1] <- "count"
kris_emotion_t_rowsums <- cbind("sentiment" = rownames(kris_emotion_t_rowsums), kris_emotion_t_rowsums)
rownames(kris_emotion_t_rowsums) <- NULL
kris_emotion_t_rowsums <- kris_emotion_t_rowsums[1:8,]
kris_emotion_t_rowsums <- rename(kris_emotion_t_rowsums, kris = count)

emotion_by_author <- full_join(sinead_emotion_t_rowsums, kris_emotion_t_rowsums, by = "sentiment")
emotion_by_author <- gather(emotion_by_author, key = "author", value = "count", 2:3)
emotion_by_author$author[emotion_by_author$author == "sinead"] <- "Sin?ad"
emotion_by_author$author[emotion_by_author$author == "kris"] <- "Kris"
emotion_by_author$sentiment <- str_to_title(emotion_by_author$sentiment)

emotion_plot <- emotion_by_author %>%
  rename(Emotion = sentiment) %>%
  group_by(author) %>%
  mutate(percent = count/sum(count)) %>%
  ggplot(aes(fill = Emotion, y = percent, x = author)) + 
  geom_bar(position = "fill", stat = "identity", width = 1, color = "black") + 
  geom_text(aes(label = paste0(sprintf("%1.1f", percent*100), "%")),
            position = position_stack(vjust = 0.5), colour = "black", size = 2.5) + 
  scale_y_continuous(labels = percent_format()) + 
  labs(x = "Author", y = "Frequency") + 
  coord_flip() + 
  theme_transparent() + 
  guides(fill = guide_legend(ncol = 4)) + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(color = "black", angle = 90, size = 6, vjust = -10, hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_text(vjust = 0.5, size = 8),
        legend.text = element_text(size = 8),
        legend.box.margin = margin(-15,-15,0,-15))

emotion_plot

#Sentiment by group

syuzhet_sentiment_by_author <- data %>%
  group_by(author) %>%
  summarise(negative = sum(if_else(syuzhet_vector < 0, 1, 0), na.rm = TRUE)
            ,positive = sum(if_else(syuzhet_vector > 0, 1, 0), na.rm = TRUE)
            ,neutral = sum(if_else(syuzhet_vector == 0, 1, 0), na.rm = TRUE))

syuzhet_sentiment_by_author <- gather(syuzhet_sentiment_by_author, key = "sentiment", value = "count", 2:4)

syuzhet_sentiment_by_author$author[syuzhet_sentiment_by_author$author == "sinead"] <- "Sinéad"
syuzhet_sentiment_by_author$author[syuzhet_sentiment_by_author$author == "kris"] <- "Kris"
syuzhet_sentiment_by_author$sentiment[syuzhet_sentiment_by_author$sentiment == "positive"] <- "Positive"
syuzhet_sentiment_by_author$sentiment[syuzhet_sentiment_by_author$sentiment == "neutral"] <- "Neutral"
syuzhet_sentiment_by_author$sentiment[syuzhet_sentiment_by_author$sentiment == "negative"] <- "Negative"

bing_sentiment_by_author <- data %>%
  group_by(author) %>%
  summarise(negative = sum(if_else(bing_vector < 0, 1, 0), na.rm = TRUE)
            ,positive = sum(if_else(bing_vector > 0, 1, 0), na.rm = TRUE)
            ,neutral = sum(if_else(bing_vector == 0, 1, 0), na.rm = TRUE))

bing_sentiment_by_author <- gather(bing_sentiment_by_author, key = "sentiment", value = "count", 2:4)

bing_sentiment_by_author$author[bing_sentiment_by_author$author == "sinead"] <- "Sinéad"
bing_sentiment_by_author$author[bing_sentiment_by_author$author == "kris"] <- "Kris"
bing_sentiment_by_author$sentiment[bing_sentiment_by_author$sentiment == "positive"] <- "Positive"
bing_sentiment_by_author$sentiment[bing_sentiment_by_author$sentiment == "neutral"] <- "Neutral"
bing_sentiment_by_author$sentiment[bing_sentiment_by_author$sentiment == "negative"] <- "Negative"

afinn_sentiment_by_author <- data %>%
  group_by(author) %>%
  summarise(negative = sum(if_else(afinn_vector < 0, 1, 0), na.rm = TRUE)
            ,positive = sum(if_else(afinn_vector > 0, 1, 0), na.rm = TRUE)
            ,neutral = sum(if_else(afinn_vector == 0, 1, 0), na.rm = TRUE))

afinn_sentiment_by_author <- gather(afinn_sentiment_by_author, key = "sentiment", value = "count", 2:4)

afinn_sentiment_by_author$author[afinn_sentiment_by_author$author == "sinead"] <- "Sinéad"
afinn_sentiment_by_author$author[afinn_sentiment_by_author$author == "kris"] <- "Kris"
afinn_sentiment_by_author$sentiment[afinn_sentiment_by_author$sentiment == "positive"] <- "Positive"
afinn_sentiment_by_author$sentiment[afinn_sentiment_by_author$sentiment == "neutral"] <- "Neutral"
afinn_sentiment_by_author$sentiment[afinn_sentiment_by_author$sentiment == "negative"] <- "Negative"

sentiment_plot <- afinn_sentiment_by_author %>%
  rename(Sentiment = sentiment) %>%
  group_by(author) %>%
  mutate(percent = count/sum(count)) %>%
  ggplot(aes(fill = Sentiment, y = percent, x = author)) + 
  geom_bar(position = "fill", stat = "identity", width = 1, color = "black") + 
  geom_text(aes(label = paste0(sprintf("%1.1f", percent*100), "%")),
            position = position_stack(vjust = 0.5), colour = "black") + 
  scale_y_continuous(labels = percent_format()) + 
  scale_fill_manual(values = c("#ec0426","#f3f733","#23c40e")) +
  labs(x = "Author", y = "Frequency") + 
  coord_flip() + 
  theme_transparent() + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(color = "black", angle = 90, size = 10, vjust = -3, hjust = 0.5))

sentiment_plot