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
#setwd("C:/Users/USER/Downloads/covid19_mythoi")

data <- read_xlsx("Govgr_Covid19_Misinfo_greek.xlsx")


summary(data)

# Begin the emotion analysis

sentemces_to_analyze <- as.character(data$Myths)


get_nrc_sentiment(char_v = 'happy', language = 'english')
get_nrc_sentiment(char_v = 'fear', language = 'english')

review_emotion <- get_nrc_sentiment(sentemces_to_analyze)

review_sentiment <- cbind(data$Myths, review_emotion)

colnames(review_emotion) <- c("Θυμός", "Προσμονή", "Αποτροπιασμός", "Φόβος", "Χαρά", "Λύπη", "Έκπληξη", "Εμπιστοσύνη", "Αρνητικότητα", "Θετικότητα") 


review_emotion_sums <- colSums(review_emotion)

df_review_emotion_sums <- data.frame(Emotions = c("Θυμός", "Προσμονή", "Αποτροπιασμός", "Φόβος", "Χαρά", "Λύπη", "Έκπληξη", "Εμπιστοσύνη", "Αρνητικότητα", "Θετικότητα"),
                                     Sums = review_emotion_sums)

colnames(df_review_emotion_sums) <- c("Συναισθήματα", "Άθροισμα")

df_review_emotion_sums_2 <- df_review_emotion_sums %>%
  mutate((`Άθροισμα` / sum(`Άθροισμα`)) * 100)

colnames(df_review_emotion_sums_2) <- c("Συναισθήματα", "Άθροισμα", "Ποσοστό")
  
df_review_emotion_sums_2$Ποσοστό <- round(df_review_emotion_sums_2$Ποσοστό, 2)
df_review_emotion_sums_2$Ποσοστό <- paste(df_review_emotion_sums_2$Ποσοστό, "%")

ggplot(df_review_emotion_sums_2[-10,],aes(x= `Συναισθήματα`, y=`Άθροισμα`, fill = `Συναισθήματα`))+
  geom_bar(stat='identity', position = 'dodge')+
  ggtitle("Συναισθήματα που προκύπτουν από τις Προτάσεις Παραπληροφόρησης") +
  xlab('Συναισθήματα')+
  ylab('Συχνότητα')+
  theme(
    plot.title = element_text(hjust = 0.5)
  )+
  geom_text(aes(label = `Ποσοστό`), position = position_dodge(width = 0.9), vjust = -0.5, colour = "black", size = 6)+
  theme_fivethirtyeight()+
  scale_fill_manual(values = stata_pal("s2color")(9))

# Statistics about the main dataset

subject <- data %>%
  dplyr::select(`subject`)%>%
  count(`sunject`)

ggplot(subject, aes(y= `subject`, x=n, fill = `subject`))+
  geom_bar(stat='identity', position = 'dodge')+
  ggtitle("Που αναφέρονται οι Προτάσεις Παραπληροφόρησης;") +
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
  ggtitle("Οι Προτάσεις Παραπληροφόρησης είναι σχετικές με την/τον:") +
  xlab("n") +
  ylab("")+
  geom_text(aes(label = n), position = position_dodge(width = 0.9), hjust = -0.3, colour = "black", size = 5)+
  theme_fivethirtyeight()

vaccine <- data %>%
  select(`subject`, `sate`, `vacc_timing`) %>%
  group_by(`subject`) %>%
  filter(`subject` == 'Εμβόλιο')

vaccine_time <- vaccine %>%
  group_by(`vacc_timing`) %>%
  count(`vacc_timing`)

vaccine_time_color <- c('orange3', 'green4')

ggplot(vaccine_time, aes(x=`vacc_timing`, y=n, fill = `vacc_timing`))+
  geom_bar(stat = 'identity', position = 'dodge')+
  ggtitle(" Χρονική Τοποθέτηση Προτάσεων σχετικές με τα Εμβόλια κατά της Covid-19") +
  xlab("Χρονική Τοποθέτηση") +
  ylab("n") +
  geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = -0.5, colour = "black", size = 7) +
  theme_fivethirtyeight()+
  scale_fill_manual(values = stata_pal("s2color")(2))

misinfo_time <- data %>%
  select(id, `date`) %>%
  group_by(`date`) %>%
  count(`date`)

ggplot(misinfo_time, aes(x=`date`, y=n))+
  geom_point(stat = 'identity', size=2)+
  geom_line(stat = 'identity', color='red2', size=1, linetype = 'dashed')+
  ggtitle("Καταγραφή των Προτάσεων Παραπληροφόρησης στην υπηρεσία ¨Μύθοι για Τον Covid-19¨ στο gov.gr") +
  xlab("Ημερομηνία Καταγραφής") +
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


data_for_links$Source[data_for_links$Source == 1] <- 'Κυβερνητικές Πηγές'
data_for_links$Source[data_for_links$Source == 2] <- 'Άρθρα από Μ.Μ.Ε.'
data_for_links$Source[data_for_links$Source == 3] <- 'Ιστοσελίδες ΠΟΥ'
colnames(data_for_links) <- c('Entry ID','Πηγές','Links', 'Ημερομηνία Πρόσβασης') 

# Distribution of Measures' Records Sources

g7 <- ggplot(data_for_links, aes(x= `Πηγές`, fill = `Πηγές` ))+
  geom_bar(stat='count', position = 'dodge')+
  ggtitle("Κατανομή των Πηγών για την Δημιουργία της Βάσης Δεδομένων του ΠΟΥ για τα Μέτρα Προστασίας στην Ελλάδα") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )+
  xlab("Τύπος Δεδομένων") +
  ylab("n") +
  geom_text(stat = 'count', aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.3, colour = "black", size = 6) +
  theme_fivethirtyeight()+
  scale_fill_manual(values = stata_pal("s2color")(3))
g7


# sites

measures_sites <- read_xlsx('measures_sites_from_phsm_database_04032024_greece.xlsx')

measures_sites <- measures_sites %>%
  filter(`Count of Sites` > 6)



g10 <- ggplot(measures_sites, aes(y=`Row Labels`, x=`Count of Sites`)) +
  geom_bar(stat='identity', position = 'dodge', fill = '#2d6d66')+
  ggtitle("Κατανομή Ιστοσελίδων") +
  theme(
    plot.title = element_text(hjust = 0.5)
  )+
  xlim(0, 170)+
  geom_text(aes(label = `Count of Sites`), position = position_dodge(width = 0.9), hjust = -0.8, colour = "black", size = 6) +
  theme_fivethirtyeight()+
  scale_fill_paletteer_d("ggthemes::Classic_Blue_Red_12")

g10
