#libraries
library(webshot)
library(htmlwidgets)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)
library(tidyverse)
library(udpipe) # creating dtm
library(topicmodels)
library(tidytext)
library(stringr)
library(ggplot2)
library(dplyr)

mainDir <- "C:/Users/dokha/OneDrive/Dokumente/SCHATSI"
subDir <- "outputDirectory"
dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
path <- paste(mainDir, "/", subDir, "/", sep="", collapse = NULL)

myfun <- function(x) {
  if(is.numeric(x)){ 
    ifelse(is.na(x), x, paste0(round(x*100L, 1), "%")) 
  } else x 
}

#parametrization
min.freq <- 50
length <- 1
k <- 6
#load data
data <- SCHATSI_terms

#pre-processing of data
dataSet0 <- data
dataSet1 <- subset(data, data[3] >= min.freq)
##filter words that are shorter than 2 characters
dataSet1$Length <- str_count(dataSet1$term)
dataSet1 <- subset(dataSet1, dataSet1$Length > length)
data1 <- dataSet1
data1 <- data1[!grepl("et", data1$term),]
data1 <- data1[!grepl("al", data1$term),]
data1 <- data1[!grepl("et al", data1$term),]
##prepare for wordcloud
dataSet0 <- dataSet0[2:3]
dataSet0 <- aggregate(dataSet0$`term count`, by=list(Category=dataSet0$term), FUN=sum)

dataSet1 <- dataSet1[2:3]
dataSet1 <- aggregate(dataSet1$`term count`, by=list(Category=dataSet1$term), FUN=sum)

subSet0 <- subset(dataSet0, dataSet0[2] >= min.freq)
subSet1 <- subset(dataSet1, dataSet1[2] >= min.freq)
##remove digits in term array
subSet1 <- subSet1 %>% filter(!grepl("[[:digit:]]", Category))
subSet1 <- subSet1[!grepl("et", subSet1$Category),]
subSet1 <- subSet1[!grepl("al", subSet1$Category),]
subSet1 <- subSet1[!grepl("et al", subSet1$Category),]
#generate wordcloud
wordCloudPlain0 <- wordcloud2(data=subSet0, size=1.6, color='random-light')
wordCloudPlainFile0 <- paste(path, "wordCloudPlain0", ".html", sep ="", collpase=NULL)
saveWidget(wordCloudPlain0,wordCloudPlainFile0,selfcontained = F)

wordCloudPlain1 <- wordcloud2(data=subSet1, size=1.6, color='random-light')
wordCloudPlainFile1 <- paste(path, "wordCloudPlain1", ".html", sep ="", collpase=NULL)
saveWidget(wordCloudPlain1,wordCloudPlainFile1,selfcontained = F)
#create DTM
x <- data.frame(doc_id = c(1, 1, 2, 3, 4), 
                term = c("A", "C", "Z", "X", "G"), 
                freq = c(1, 5, 7, 10, 0))

data_frame <- setNames(data1, c("doc_id","term","freq"))
dtm <- document_term_matrix(data_frame)

ap_lda <- LDA(dtm, k = k, control = list(seed = 1234))
ap_lda

ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

file <- paste(path, "topics.jpeg", sep="", collapse = NULL) 
png(file)
ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()
dev.off()

ap_top_terms[3] <- ap_top_terms[3] %>% mutate_each(funs(myfun))
ap_top_terms_file <- paste(path, "topics.csv", sep="", collapse = NULL)
write.table(ap_top_terms, file=ap_top_terms_file,sep = ";", col.names = T, row.names = F)

ap_documents <- tidy(ap_lda, matrix = "gamma")

ap_documents_file <- paste(path, "topicAllocation.csv", sep="", collapse = NULL)
ap_documents[3] <- ap_documents[3] %>% mutate_each(funs(myfun))
write.table(ap_documents, file=ap_documents_file,sep = ";", col.names = T, row.names = F)
