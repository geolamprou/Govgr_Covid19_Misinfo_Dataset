#  Misinformation during the pandemic Covid19 in Greece - The Dataset: "Govgr_Covid19_Misinfo"

#Libraries

library(ggplot2)
library(tidyr)
library(dplyr)
library(syuzhet)
library(readxl)
library(ggwordcloud)
library(tm)
library(wordcloud2)
library(ggthemes)
library(paletteer)
library(rlang)
library(RColorBrewer)


# Data
setwd("C:/Users/USER/Downloads/covid19_mythoi")

data <- read_xlsx("covid19_missinformation_gov_gr_en.xlsx")


summary(data)

# Sentiment Analysis

sentemces_to_analyze <- as.character(data$myths)


get_nrc_sentiment(char_v = 'happy', language = 'english')
get_nrc_sentiment(char_v = 'fear', language = 'english')

review_emotion <- get_nrc_sentiment(sentemces_to_analyze)

review_sentiment <- cbind(data$myths, review_emotion)

colnames(review_emotion) <- c("Anger", "Anticipation", "Disgust", "Fear", "Joy", "Sadness", "Surprise", "Trust", "Negative", "Positive") 


review_emotion_sums <- colSums(review_emotion)

df_review_emotion_sums <- data.frame(Emotions = c("Anger", "Anticipation", "Disgust", "Fear", "Joy", "Sadness", "Surprise", "Trust", "Negative", "Positive"),
                                     Sums = review_emotion_sums)

colnames(df_review_emotion_sums) <- c("Emotions", "Sum")

df_review_emotion_sums_2 <- df_review_emotion_sums %>%
  mutate((`Sum` / sum(`Sum`)) * 100)

colnames(df_review_emotion_sums_2) <- c("Emotions", "Sum", "Percent")
  
df_review_emotion_sums_2$Percent <- round(df_review_emotion_sums_2$Percent, 2)
df_review_emotion_sums_2$Percent <- paste(df_review_emotion_sums_2$Percent, "%")

ggplot(df_review_emotion_sums_2[-10,],aes(x= `Emotions`, y=`Sum`, fill = `Emotions`))+
  geom_bar(stat='identity', position = 'dodge')+
  ggtitle("Sentiment Analysis") +
  xlab('Emotions')+
  ylab('Frequency')+
  theme(
    plot.title = element_text(hjust = 0.5)
  )+
  geom_text(aes(label = `Percent`), position = position_dodge(width = 0.9), vjust = -0.5, colour = "black", size = 6)+
  theme_fivethirtyeight()+
  scale_fill_manual(values = stata_pal("s2color")(9))

# Wordcloud of myths

myths <- data %>%
  select(myths)

docs <- Corpus(VectorSource(myths))

# use tm to remove punctuation, stopwords, case etc. 
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)

docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))

# create table of word counts
dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

df_without_covid <- df %>%
  filter(df$word != 'covid' & df$word != 'coronavirus')

wordcloud2(slice_max(df_without_covid, order_by = freq, n=200), size=0.8, 
           color='random-dark')

# Statistics about the main dataset

subject <- data %>%
  dplyr::select(`subject`)%>%
  count(`subject`)

ggplot(subject, aes(y= `subject`, x=n, fill = `subject`))+
  geom_bar(stat='identity', position = 'dodge')+
  ggtitle("Subjects of Sentences;") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )+
  geom_text(aes(label = n), position = position_dodge(width = 0.9), hjust = -0.3, colour = "black", size = 7)+
  theme_fivethirtyeight()+
  scale_fill_manual(values = stata_pal("s2color")(length(unique(subject$`subject`))))

aim <- data %>%
  dplyr::select(`aim`) %>%
  count(`aim`)

ggplot(aim, aes(y= `aim`, x=n))+
  geom_bar(stat='identity', position = 'dodge', fill="#2d6d66")+
  ggtitle("Target of Sentences") +
  xlab("n") +
  ylab("")+
  geom_text(aes(label = n), position = position_dodge(width = 0.9), hjust = -0.3, colour = "black", size = 5)+
  theme_fivethirtyeight()

vaccine <- data %>%
  select(`subject`, `date`, `vacc_timing`) %>%
  group_by(`subject`) %>%
  filter(`subject` == 'Vaccine')

vaccine_time <- vaccine %>%
  group_by(`vacc_timing`) %>%
  count(`vacc_timing`)

vaccine_time_color <- c('orange3', 'green4')

ggplot(vaccine_time, aes(x=`vacc_timing`, y=n, fill = `vacc_timing`))+
  geom_bar(stat = 'identity', position = 'dodge')+
  ggtitle(" Timing Relative to Vaccine Discovery") +
  xlab("Timing") +
  ylab("n") +
  geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = -0.5, colour = "black", size = 6) +
  theme_fivethirtyeight()+
  scale_fill_manual(values = stata_pal("s2color")(2))

misinfo_time <- data %>%
  select(id, `date`) %>%
  group_by(`date`) %>%
  count(`date`)

ggplot(misinfo_time, aes(x=`date`, y=n))+
  geom_point(stat = 'identity', size=2)+
  geom_line(stat = 'identity', color='red2', size=1, linetype = 'dashed')+
  ggtitle("Date of Senttences' Recording on covid19.gov.gr") +
  xlab("Date") +
  ylab("n") +
  ylim(-1,20)+
  geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = -0.5, colour = "black", size = 7)+
  geom_text(aes(label = `date`), position = position_dodge(width = 0.9), vjust=2, colour = "green4", size = 6) +
  theme_fivethirtyeight()




# data_for_links

data <- read_xlsx("phsm_database_04032024_greece.xlsx", col_names = TRUE)

# Data Processing and Transformations

data$`Start of measure` <- as.Date.POSIXct(data$`Start of measure`)
data$`End of measure` <- as.Date.POSIXct(data$`End of measure`)
data$`Date of access` <- as.Date.POSIXct(data$`Date of access`)


data_for_links <- data %>%
  dplyr::select(`Entry ID`,Source, Links, `Date of access`)


data_for_links$Source[data_for_links$Source == 1] <- 'Govermental Sources'
data_for_links$Source[data_for_links$Source == 2] <- 'Articles from Mass Media'
data_for_links$Source[data_for_links$Source == 3] <- 'WHO Websites'
colnames(data_for_links) <- c('Entry ID','Sources','Links', 'Date_of_access') 

# Distribution of Measures' Records Sources

g7 <- ggplot(data_for_links, aes(x= `Sources`, fill = `Sources` ))+
  geom_bar(stat='count', position = 'dodge')+
  ggtitle("Distribution of Covid-19 Protectio Measures Records' Sources") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )+
  xlab("Type of Sources") +
  ylab("n") +
  geom_text(stat = 'count', aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.3, colour = "black", size = 6) +
  theme_fivethirtyeight()+
  scale_fill_manual(values = stata_pal("s2color")(3))
g7


# sites

measures_sites <- read_xlsx('measures_sites.xlsx')

g10 <- ggplot(measures_sites, aes(y=`Row Labels`, x=`Count of Sites`)) +
  geom_bar(stat='identity', position = 'dodge', fill = '#2d6d66')+
  ggtitle("Websites Distribution") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )+
  geom_text(aes(label = `Count of Sites`), position = position_dodge(width = 0.9), hjust = -0.8, colour = "black", size = 6) +
  theme_fivethirtyeight()+
  scale_fill_paletteer_d("ggthemes::Classic_Blue_Red_12")

g10
