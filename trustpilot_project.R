library(readr)
library(data.table)
library(tidyverse)
library(hms)
library(dplyr)
library(ggplot2)
library(assertive)
library(lubridate)
library(stringr)
library(forcats)
library(tidyr)
library(naniar)
library(anytime)
library(viridis)
library(tidytext)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(widyr)
library(topicmodels)
library(purrr)
library(factoextra)
library(NbClust)
library(remotes)
library(cld2)
library(vcd)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)


###
#Data input
FB_data <- read_csv("facebook.csv")


###
###Data cleaning and manipulation for sentiment analysis
#Prepare data for sentiment analysis by tokenizing, removing stop words
#Try out different lexicons and join to data frame

df <- data.frame(print(is.na(FB_data)))
df <- df %>% filter(Name == TRUE, Time == TRUE, Num_Reviews == TRUE, Title == TRUE, Content == TRUE, Rating == TRUE)
# Seems that there are not missing values found
rm(df)

#Data cleaning and manipulation for text analysis
FB_text <- FB_data %>% select(-X1)

# creating an id column
FB_text <- FB_text %>% mutate(id = row_number())

# combing title and content columns into one
FB_text$review <- paste(FB_text$Title, "-", FB_text$Content) 
#this deals with the issue that I want to analyze both what is in the title and within the review itself; also some text only come with a title but no input into the content field or vice versa;
#this potentially may skew the results in the topic modeling and LDA but I don't think it would be too significant; a good alternative is to do a separate 
#analysis for both when going into topic modeling but for sentiment analysis I don't think this is a limitation

# remove special characters, punctuation & numbers
FB_text$review <- gsub("[^A-Za-z]", " ", FB_text$review) 

# drop observations that are only empty strings
FB_text <- FB_text[FB_text$review!="",]

# tokenization and removing stop words
custom <- add_row(stop_words, word = c("facebook","fb","FB","Facebook", "facebok", "facebook.com", "facebook.i", "fb's", "fcebook"), lexicon = "custom")

FB_text <- FB_text %>%  
  unnest_tokens(output = "word", token = "words", input = review, drop = FALSE) %>%  
  anti_join(custom)

# removing unnecessary columns
FB_text <- FB_text %>% select(-c(Name, Title, Content))


# Sentiment analysis

#Afinn
FB_text_afinn <- FB_text %>%   
  inner_join(get_sentiments("afinn")) 

# creating sentiments total score per review
FB_text_sent <- FB_text %>%   
  inner_join(get_sentiments("afinn")) %>%   
  group_by(id) %>%   
  mutate(afinn_sentiment = sum(value))%>%
  arrange(afinn_sentiment)

# graphing afinn_sentiment as a dependent variable
ggplot(FB_text_sent, aes(id, afinn_sentiment)) + 
  geom_point(alpha=0.2)+
  geom_smooth(method="lm")

ggplot(FB_text_sent, aes(Num_Reviews, afinn_sentiment)) + 
  geom_point(alpha=0.2)
# some potential outliers

ggplot(FB_text_sent, aes(Rating, afinn_sentiment)) + 
  geom_point(alpha=0.2)+
  geom_smooth(method="lm")
# these results do not make logical sense, how can a review of 1 rating have a very high afinn_sentiment rating that is almost 100, or how can a rating of 5 review have an afinn_sentiment of lower than -75?
weird_rating_1 <- FB_text_sent %>% filter(Rating ==1 & afinn_sentiment>50)
weird_rating_1 <- weird_rating_1 %>% select(review) %>% 
  distinct(review) %>% group_by(review, id) %>% 
  ungroup %>% distinct(review)

wr1_detected_language <- weird_rating_1 %>% 
  sapply(., map_chr, detect_language) %>% 
  data.frame(check.names = FALSE)
#da = danish is the language; using google translate string of the comment

wr1_string <- as.character(weird_rating_1$review)
print(wr1_string)
#this review is in another language with some english words mixed in, the english words are positive (thus the sentiment score) but it seems like
#review is negative; in any case I would not want to analyze this further
#reading the first few lines of the translated string via goolge transalate it seems like the review is negative when written in dutch but also
#uses english words that give most of the time a positive sentiment thus it would be inappropriate to have this review in the final analysis 

weird_rating_5 <- FB_text_sent %>% filter(Rating ==5 & afinn_sentiment< (-50))
#seems like a spam review, one that uses the text for expressing a negative sentiment towards the company but leaving the review positive; i think 
#this violates what Trustpilot is trying to prevent on their website, so a system to monitor these odd reviews would be a nice addition by their dev
#team

# removing extreme values for afinn_sentiment
# restricting number of reviews from 25 to 200; these are the reviewers that have 25 to 200 reviews on trustpilot and these are not extreme values
# for the afinn_sentiment which we saw caused problems with the ratings
no_outliers <- FB_text_sent %>% filter(afinn_sentiment <= 25 & afinn_sentiment>=-50)
no_outliers <- no_outliers %>% filter(Num_Reviews>=25 & Num_Reviews<=200)

#old reviews for DV
mean(FB_text_sent$afinn_sentiment)
sd(FB_text_sent$afinn_sentiment)
# mean = -4.28
# sd = 13.03
# these reviews I can possibly trust because they are more or less clustered around the actual mean and the number of reviews left by the account on 
# trustpilot indicates that they may potentially be an actual user that uses trustpilot to review companies

#cleaned subset of reviews for DV
mean(no_outliers$afinn_sentiment)
sd(no_outliers$afinn_sentiment)
# mean = -4.26
# sd = 8.62
# the mean did not change that much but the sd did as expected 

#cleaned subset of reviews for another variable
mean(no_outliers$Rating)
sd(no_outliers$Rating)
# mean = 2.11
# sd = 1.56

ggplot(no_outliers, aes(Num_Reviews, afinn_sentiment)) + 
  geom_point(alpha=0.2)

ggplot(no_outliers, aes(Rating, afinn_sentiment)) + 
  geom_point(alpha=0.2)+
  geom_smooth(method="lm")
# the problem still persists with the rating, despite the filtering out extreme values in the afinn_sentiment and Num_Reviews variable categories

no_outliers <- no_outliers %>% select(-c(word, value))
no_outliers <- distinct(no_outliers)
nrow(no_outliers)
summary(no_outliers)
# at this point there are 159 reviews
# filter the data so that for reviews of rating (1,2), there are only afinn_sentiment scores that are in the range of (-5 to negative extreme)
# filter the data so that for reviews of rating (3), there are only afinn_sentiment scores that are in the range of (-5 to 5)
# filter the data so that for reviews of rating (4,5), there are only afinn_sentiment scores that are in the range of (5 to positive extreme)

negative_rating <- no_outliers %>% filter(Rating %in% c(1:2), afinn_sentiment %in% c(-6:-26))
neutral_rating <- no_outliers %>% filter(Rating %in% 3, afinn_sentiment %in% c(-5:5))
positive_rating <- no_outliers %>% filter(Rating %in% c(4:5), afinn_sentiment %in% c(6:13))

sentiment_analysis_reviews <- negative_rating %>% 
  full_join(neutral_rating) %>% 
  full_join(positive_rating)

# Final dataset used 
ggplot(sentiment_analysis_reviews, aes(Rating, afinn_sentiment)) + 
  geom_point(alpha=0.2)+
  geom_smooth(method="lm")
# this is a more cleaner sample of reviews in my opinion, that allows for easier digesting of the data because there are fewer reviews to read and analyze
# also the inappropriate ratings were adjusted in accordance with sentiment analysis sentiment rating
# each review content seems to be unique in nature, so the risk of spam reviews is removed
# each reviewer has left a significant amount of reviews on trustpilot
# since anyone can post on trustpilot

rm(FB_text, FB_text_afinn, negative_rating, neutral_rating, no_outliers, positive_rating, weird_rating_1, weird_rating_5, wr1_detected_language, wr1_string)

# creating 3 binary variables (positive, neutral, negative) to add more for more variable for cluster analysis
sentiment_analysis_reviews <- sentiment_analysis_reviews %>%	
  mutate( 
    negative = ifelse(afinn_sentiment %in% c(-6:-26), 1, 0),
    neutral = ifelse(afinn_sentiment %in% c(-5:5), 1, 0),
    positive = ifelse(afinn_sentiment %in% c(6:13), 1, 0))

sentiment_analysis_reviews <- data.frame(sentiment_analysis_reviews) 



###
# Data preparation for cluster analysis

cluster_analysis <- sentiment_analysis_reviews %>% select(-c(id,review))

cluster_analysis <- cluster_analysis %>% mutate(Time=as.numeric(Time))

# standardizing the variables
cluster_analysis <- cluster_analysis %>% mutate(Time=(Time-mean(Time))/sd(Time),
                                                Num_Reviews=(Num_Reviews-mean(Num_Reviews))/sd(Num_Reviews),
                                                Rating=(Rating-mean(Rating))/sd(Rating),
                                                afinn_sentiment=(afinn_sentiment-mean(afinn_sentiment))/sd(afinn_sentiment),
                                                negative=(negative-mean(negative))/sd(negative),
                                                neutral=(neutral-mean(neutral))/sd(neutral),
                                                positive=(positive-mean(positive))/sd(positive))

# cluster analysis

# Elbow method
fviz_nbclust(cluster_analysis, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(cluster_analysis, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")

# Gap statistic method
set.seed(123)
fviz_nbclust(cluster_analysis, kmeans, method = "gap_stat")+
  labs(subtitle = "Gap statistic method")

# All three cluster analysis methods indicate that the optimal number of clusters is 2


###
# Topic modeling

# Preparing data for topic modelling
topic_model <- sentiment_analysis_reviews %>%  
  unnest_tokens(output = "word", token = "words", input = review) %>%  
  anti_join(custom) %>%  
  mutate(word = wordStem(word))

topic_model_matrix <- topic_model %>%  
  count(id, word) %>%  
  cast_dtm(document = id, term = word, value = n, weighting = tm::weightTf)


# Topic modeling according to cluster analysis results
topic_model_lda_2 <-
  LDA(topic_model_matrix, k = 2, method = 'Gibbs', control = list(seed = 1111))

# Extract the beta and gamma matrix 
topic_betas_2 <- tidy(topic_model_lda_2, matrix = "beta") #beta = top words by topic
topic_gammas_2 <- tidy(topic_model_lda_2, matrix = "gamma") #gamma = top topic per document

# Betas filtered by topic and most common words
# Topic 1
topic_betas_2 %>%
  filter(topic == 1) %>%
  top_n(30, beta) %>%
  arrange(-beta) %>%
  ggplot(mapping = aes(reorder(term,beta), beta))+
  geom_bar(stat = "identity", fill = "red")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_flip()

t1b2 <- topic_betas_2 %>% filter(topic == 1)

# using the betas median as a min.freq in the wordcloud
summary(t1b2$beta)

# word cloud for topic 1 betas
set.seed(1234) # for reproducibility 
wordcloud(words = t1b2$term, freq = t1b2$beta, max.words=767, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))

# Topic 2
topic_betas_2 %>%
  filter(topic == 2) %>%
  top_n(30, beta) %>%
  arrange(-beta) %>%
  ggplot(mapping = aes(reorder(term,beta), beta))+
  geom_bar(stat = "identity", fill = "red")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_flip()

t2b2 <- topic_betas_2 %>% filter(topic == 2)

# using the betas median as a min.freq in the wordcloud
summary(t2b2$beta)

# wordlocloud for topic 2 betas
set.seed(1234) # for reproducibility 
wordcloud(words = t2b2$term, freq = t2b2$beta, max.words=767, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))

# Gammas filtered by topic 
topic_gammas_2 %>%
  filter(topic == 1) %>%
  arrange(-gamma)

topic_one <- sentiment_analysis_reviews %>% filter(id %in% c(1522,1619,1085,2849,499,222,807,481,864,3258))

topic_gammas_2 %>%
  filter(topic == 2) %>%
  arrange(-gamma)

topic_two <- sentiment_analysis_reviews %>% filter(id %in% c(1008,661,1562,616,523,1074,2915,1531,1462,2504))


###
# Perplexity(measure of how well a probability model fits new data; lower the perplexity, the better; helps find optimal k(number of topics)
sample_size <- floor(0.90 * nrow(topic_model_matrix))
set.seed(1111)
train_ind <- sample(nrow(topic_model_matrix), size = sample_size)
train <- topic_model_matrix[train_ind, ]
test <- topic_model_matrix[-train_ind, ]

values = c()

for(i in c(2:35)){  
  lda_model <- LDA(train, k = i, method = "Gibbs",                   
                   control = list(iter = 25, seed = 1111))  
  values <- c(values, perplexity(lda_model, newdata = test))  
}

plot(c(2:35), values, main="Perplexity for Topics",      
     xlab="Number of Topics", ylab="Perplexity")
# based on perplexity analysis it seems that 7 to 11 topics provide the best value; the more topics I include the more 
# "detailed" each of the topics should become, this may be worth exploring further but for this analysis constricting the number 
# of topics to 7 is enough; there is a greater payoff in the range of 5 to 7 because perplexity decreases until it flattens at 8
# topics, indicating this is a good place to stop

# Topic modeling using using k=7
topic_model_lda_7 <-
  LDA(topic_model_matrix, k = 7, method = 'Gibbs', control = list(seed = 1111))

# Extract the beta and gamma matrix 
topic_betas_7 <- tidy(topic_model_lda_7, matrix = "beta") #beta = top words by topic
topic_gammas_7 <- tidy(topic_model_lda_7, matrix = "gamma") #gamma = top topic per document

# Betas filtered by topic and most common words
topic_betas_7 %>%
  filter(topic == 1) %>%
  top_n(30, beta) %>%
  arrange(-beta) %>%
  ggplot(mapping = aes(reorder(term,beta), beta))+
  geom_bar(stat = "identity", fill = "red")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_flip()

t1b7 <- topic_betas_7 %>% filter(topic == 1)
set.seed(1234) # for reproducibility 
wordcloud(words = t1b7$term, freq = t1b7$beta, max.words=767, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))

topic_betas_7 %>%
  filter(topic == 2) %>%
  top_n(30, beta) %>%
  arrange(-beta) %>%
  ggplot(mapping = aes(reorder(term,beta), beta))+
  geom_bar(stat = "identity", fill = "red")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_flip()

t2b7 <- topic_betas_7 %>% filter(topic == 2)
set.seed(1234) # for reproducibility 
wordcloud(words = t2b7$term, freq = t2b7$beta, max.words=767, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))

topic_betas_7 %>%
  filter(topic == 3) %>%
  top_n(30, beta) %>%
  arrange(-beta) %>%
  ggplot(mapping = aes(reorder(term,beta), beta))+
  geom_bar(stat = "identity", fill = "red")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_flip()

t3b7 <- topic_betas_7 %>% filter(topic == 3)
set.seed(1234) # for reproducibility 
wordcloud(words = t3b7$term, freq = t3b7$beta, max.words=767, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))

topic_betas_7 %>%
  filter(topic == 4) %>%
  top_n(30, beta) %>%
  arrange(-beta) %>%
  ggplot(mapping = aes(reorder(term,beta), beta))+
  geom_bar(stat = "identity", fill = "red")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_flip()

t4b7 <- topic_betas_7 %>% filter(topic == 4)
set.seed(1234) # for reproducibility 
wordcloud(words = t4b7$term, freq = t4b7$beta, max.words=767, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))

topic_betas_7 %>%
  filter(topic == 5) %>%
  top_n(30, beta) %>%
  arrange(-beta) %>%
  ggplot(mapping = aes(reorder(term,beta), beta))+
  geom_bar(stat = "identity", fill = "red")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_flip()

t5b7 <- topic_betas_7 %>% filter(topic == 5)
set.seed(1234) # for reproducibility 
wordcloud(words = t5b7$term, freq = t5b7$beta, max.words=767, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))

topic_betas_7 %>%
  filter(topic == 6) %>%
  top_n(30, beta) %>%
  arrange(-beta) %>%
  ggplot(mapping = aes(reorder(term,beta), beta))+
  geom_bar(stat = "identity", fill = "red")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_flip()

t6b7 <- topic_betas_7 %>% filter(topic == 6)
set.seed(1234) # for reproducibility 
wordcloud(words = t6b7$term, freq = t6b7$beta, max.words=767, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))

topic_betas_7 %>%
  filter(topic == 7) %>%
  top_n(30, beta) %>%
  arrange(-beta) %>%
  ggplot(mapping = aes(reorder(term,beta), beta))+
  geom_bar(stat = "identity", fill = "red")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  coord_flip()

t7b7 <- topic_betas_7 %>% filter(topic == 7)
set.seed(1234) # for reproducibility 
wordcloud(words = t7b7$term, freq = t7b7$beta, max.words=767, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))

# betas (top words per topic) give us some information what the reviews are about but in my opinion it would be better to 


# look the gammas (top topic per review) by reading the review; unfortunately i have not reached this step but provided the code below
# Gammas filtered by topic 
topic_gammas_7 %>%
  filter(topic == 1) %>%
  arrange(-gamma)

topic1 <- sentiment_analysis_reviews %>% filter(id %in% c(523,1619,3258,2476,1324,292,1660,1567,1074,481))

topic_gammas_7 %>%
  filter(topic == 2) %>%
  arrange(-gamma)

topic2 <- sentiment_analysis_reviews %>% filter(id %in% c(1522,1648,1839,1531,2527,1085,3258,864,1355,785))

topic_gammas_7 %>%
  filter(topic == 3) %>%
  arrange(-gamma)

topic3 <- sentiment_analysis_reviews %>% filter(id %in% c(2849,1085,1196,3151,1815,1561,222,3035,1562,1074))

topic_gammas_7 %>%
  filter(topic == 4) %>%
  arrange(-gamma)

topic4 <- sentiment_analysis_reviews %>% filter(id %in% c(1196,661,551,2983,616,1648,481,145,1797,1715))

topic_gammas_7 %>%
  filter(topic == 5) %>%
  arrange(-gamma)

topic5 <- sentiment_analysis_reviews %>% filter(id %in% c(1008,1385,1772,1709,2590,1195,1236,145,1531,864))

topic_gammas_7 %>%
  filter(topic == 6) %>%
  arrange(-gamma)

topic6 <- sentiment_analysis_reviews %>% filter(id %in% c(1008,1562,1355,925,807,2518,3151,1195,222,1567))

topic_gammas_7 %>%
  filter(topic == 7) %>%
  arrange(-gamma)

topic7 <- sentiment_analysis_reviews %>% filter(id %in% c(1381,2915,145,616,2914,3151,1609,661,1815,499))

# compare the reviews with LDA results of k=2 and k=7; this part is very subjective as I will be reading each 10 reviews per topic
# on its own, noting the overall theme that unites these 10 reviews per topic.

q()
