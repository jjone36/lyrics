library(tidyverse)
library(magrittr)
library(httr)
library(rvest)
library(jsonlite)
library(stringr)
library(rebus)
library(tm)
library(tidytext)
library(RColorBrewer)
library(wordcloud2)
library(gridExtra)
library(plotrix)
library(caret)
library(e1071)
library(xgboost)
library(ROCR)


# 1. Scraping the data from the web ####
url = 'https://www.billboard.com/charts/artist-100'

# Scarping the list of Top 100 artists
artists = read_html(url) %>% 
  html_nodes(css = '.chart-number-one__title , .chart-list-item__title-text') %>% 
  html_text()

artists = gsub(pattern = '\n', replacement = '', x = artists)
artists = str_trim(artists)

billboard = tibble(rank = 1:100,
                   artist = artists[1:100])

# deleting K-pop singers as their lyrics aren't English 
billboard = billboard %>%
  filter(!artist %in% c('BTS', 'BAEKHYUN'))


# Scraping song titles and lyrics 
song = tibble(artist = vector(), 
              title = vector())

for(i in 1:nrow(billboard)){
  url_artist_page = paste0('https://genius.com/artists/', billboard$artist[(i)])
  
  song_title = read_html(url_artist_page) %>%
    html_nodes(css = '.mini_card-title') %>% 
    html_text()
  
  df = tibble(artist = replicate(5, billboard$artist[i]),
              title = song_title[1:5])
  song = rbind(song, df)
}


for(i in 1:nrow(billboard)){
  url_artist_page = paste0('https://genius.com/artists/', billboard$artist[i])
  
  url_lyrics = read_html(url_artist_page) %>%
    html_nodes(css = '.mini_card_grid-song a') %>%
    html_attr('href')
  
  song$url[(i*5-4):(i*5)] = url_lyrics[1:5]
}

for(i in 1:nrow(song)){
  song$lyrics[i] = read_html(song$url[i]) %>%
    html_nodes(css = '.lyrics p') %>% 
    html_text()
}


# Scarping the list of Female & Male artists
url = 'https://www.ranker.com/crowdranked-list/best-female-singer-of-the-year'
female_singers = read_html(url) %>% 
  html_nodes(css = '.listItem__title--link') %>%
  html_text()

url2 = 'https://www.ranker.com/list/best-new-female-artists/ranker-music?ref=collections_btm&l=2672226&collectionId=2473'
female_singers2 = read_html(url2) %>% 
  html_nodes(css = '.listItem__title--link') %>%
  html_text()

song$gender = 'Male'
song$gender[song$artist %in% c(female_singers, female_singers2)] = 'Female'
song$gender[song$artist %in% c('Aretha Franklin', 'Ella Mai', 'Lauren Daigle')] = 'Female'

song %>%
  group_by(artist) %>%
  filter(gender == 'Female') %$%
  n_distinct(artist)

song = as.data.frame(song)
song$gender = as.factor(song$gender)


# 2. Text Mining ####
# write.csv(song, file = 'song.csv', row.names = F)
a = song

clean = function(x){
  x = gsub("won't", "will not", x)
  x = gsub("can't", "can not", x)
  x = gsub("n't", " not", x)
  x = gsub("'ll", " will", x)
  x = gsub("'re", " are", x)
  x = gsub("'ve", " have", x)
  x = gsub("'m", " am", x)
  x = gsub("'d", " would", x)
  x = gsub("'s", "", x)
  x = gsub('feelin', 'feeling', x)
  x = gsub('tryin', 'trying', x)
  return(x)
}


song$lyrics = a$lyrics %>%
  gsub(pattern = '\\[[^][]*]', replacement = ' ') %>%
  tolower() %>%
  clean() %>%
  gsub(pattern = '[[:punct:]|[:digit:]]', replacement = ' ')


# 2-1. Unigram tokenization
myStopwords = c('ooh', 'oooh', 'oh', 'uh', 'baby', 'babi', 'bebe', 
                'yeah', 'yeh', 'ye', 'yes', 'ya', 'eh', 'da', 'cardi', 'se',
                'ayy', 'ah', 'yo', 'o', 'bum', 'na', 'la', 'ai', 'ba', 'hey')
slangs = c('fuck', 'fuckin', 'bitch', 'bitches')

song_token = song %>%
  select(title, lyrics, gender) %>%
  unnest_tokens(output = word, input = lyrics) %>%
  filter(!word %in% myStopwords) %>%
  filter(!word %in% slangs) %>%
  anti_join(stop_words, by = 'word') 

head(song_token)
str(song_token)

song_tidy = song_token %>%
  group_by(gender, word) %>%
  count() %>%
  spread(key = gender, value = n) %>%
  mutate(total = Female + Male) %>%
  ungroup () %>%
  arrange(desc(total)) 

# Wordcloud of the most frequent words 
wordcloud2(data = song_tidy[1:100, ], size = 1, color = brewer.pal(8, 'Dark2'))

# Getting the proportion of words for each gender
song_tidy = song_tidy %>% 
  filter(!is.na(Female) & !is.na(Male)) %>%
  mutate(Female_prop = Female / sum(Female),
         Male_prop = Male / sum(Male),
         diff = Female_prop - Male_prop)

# The plot of female Frequent words compared to male
p1 = song_tidy %>%
  arrange(desc(diff)) %>%
  head(n = 10) %>%
  ggplot(aes(x = reorder(word, diff), y = diff)) + 
  geom_col(fill = 'indianred1') +
  coord_flip() +
  labs(x = NULL, y = NULL, title = 'Top 10 Words For Female Singers') +
  theme_bw()

# The plot of male Frequent words compared to female
p2 = song_tidy %>%
  arrange(diff) %>%
  head(n = 10) %>%
  ggplot(aes(x = reorder(word, -diff), y = (-diff))) + 
  geom_col(fill = 'lightseagreen') +
  coord_flip() +
  labs(x = NULL, y = NULL, title = 'Top 10 Words For Male Singers') +
  theme_bw()

grid.arrange(p1, p2)

# 2-2. Bigram tokenization
bigram_token = song %>%
  select(title, lyrics, gender) %>%
  unnest_tokens(output = bigram, input = lyrics, token = 'ngrams', n = 2)

bigram_token = bigram_token %>%
  separate(bigram, into = c('word1', 'word2'), sep = ' ') %>%
  filter(!word1 %in% c(myStopwords, slangs, stop_words$word)) %>%
  filter(!word2 %in% c(myStopwords, slangs, stop_words$word)) %>% 
  filter(word1 != word2) %>%
  unite(col = bigram, word1, word2, sep = ' ') %>%
  filter(!bigram %in% tolower(song$artist))

bigram_tidy = bigram_token %>% 
  group_by(gender, bigram) %>%
  count() %>%
  spread(key = gender, value = n) %>%
  mutate(total = Female + Male) %>%
  arrange(desc(total)) %>%
  ungroup()

# bigram Wordcloud of the most frequent words 
wordcloud2(bigram_tidy[1:50, ], size = .5, color = brewer.pal(8, 'Dark2'), shape = 'circle')

# Getting the proportion of words for each gender
bigram_tidy = bigram_tidy %>% 
  mutate(Female_prop = Female / sum(Female, na.rm = T),
         Male_prop = Male / sum(Male, na.rm = T),
         diff = Female_prop - Male_prop)

# The plot of female Frequent words compared to male
b1 = bigram_tidy %>%
  arrange(desc(diff)) %>%
  head(n = 7) %>%
  ggplot(aes(x = reorder(bigram, diff), y = diff)) + 
  geom_col(fill = 'indianred1') +
  coord_flip() +
  labs(x = NULL, y = NULL, title = 'Top 7 Words For Female Singers') +
  theme_bw()

# The plot of male Frequent words compared to male
b2 = bigram_tidy %>%
  arrange(diff) %>%
  head(n = 7) %>%
  ggplot(aes(x = reorder(bigram, -diff), y = (-diff))) + 
  geom_col(fill = 'lightseagreen') +
  coord_flip() +
  labs(x = NULL, y = NULL, title = 'Top 7 Words For Male Singers') +
  theme_bw()

grid.arrange(b1, b2)

# 3. Sentiment Anaylsis ####
# 3-1. nrc lexicon 
(nrc = get_sentiments(lexicon = 'nrc'))
table(nrc$sentiment)

song_nrc = song_token %>%
  inner_join(nrc) %>%
  group_by(gender, sentiment) %>% 
  count() %>%
  ungroup() 

# The comparison plot of sentiment by nrc
ggplot(song_nrc, aes(x = reorder(sentiment, n), y = n, fill = gender)) + 
  geom_col(show.legend = F) +
  facet_wrap(gender ~.) +
  coord_flip() +
  labs(x = NULL, y = NULL, title = 'Sentiment Analysis by Nrc Lexicon') +
  theme_bw()

# the plot of proportional comparison of sentiment by nrc
song_nrc %>%
  group_by(gender) %>%
  mutate(prop = n / sum(n)) %>%
  arrange(desc(prop)) %>%
  ggplot(aes(x = reorder(sentiment, prop), y = prop, fill = gender)) +
  geom_col(show.legend = F) +
  facet_wrap(gender ~.) +
  coord_flip() +
  labs(x = NULL, y = NULL, title = 'Proportional Comparison for Each Gender') +
  theme_bw() 

# 3-2. afinn lexicon 
(afinn = get_sentiments(lexicon = 'afinn'))

song_afinn = song_token %>%
  inner_join(afinn) %>%
  group_by(title) %>%
  mutate(total_score = sum(score)) %>%
  ungroup() %>%
  arrange(desc(total_score))

# the boxplot of sentiment score by afinn 
song_afinn %>%
  ggplot(aes(x = gender, y = total_score, fill = gender)) + 
  geom_boxplot(show.legend = F) +
  labs(x = 'Gender', y = 'Sentiment Score', title = 'Sentiment Score by Afinn Lexicon') +
  theme_bw()

# filtering outliers
song_afinn %>%
  filter(total_score > -300) %>%
  ggplot(aes(x = gender, y = total_score, fill = gender)) + 
  geom_boxplot() +
  theme_bw()

song_afinn %>%
  group_by(gender) %>%
  summarise(mean_score = mean(total_score)) 

# the histogram of sentimnet score by afinn (filtering outliers)
song_afinn %>%
  filter(total_score > -300) %>%
  ggplot(aes(x = total_score, fill = gender)) + 
  geom_histogram(show.legend = F) + 
  facet_grid(. ~ gender) +
  labs(x = 'Sentiment Score', y = NULL, title = 'The Distribution of Sentiment Score for Each Gender') + 
  theme_bw()    


# 4. Transforming into corpus and document-term matrix ####
# Removing some outliers and joining with song table
song = song_afinn %>%
  filter(total_score > -300) %>%
  select(title, total_score) %>%
  distinct() %>%
  right_join(song, by = 'title')

outlier = which(is.na(song$total_score))

song$title[outlier]
song = song[-outlier, ]

song$gender = factor(song$gender, levels = c('Male', 'Female'), labels = c(0, 1))

# Tranforming the dataset into corpus and then dtm 
song_df = song %>% 
  select(title, lyrics)

corpus = VectorSource(song_df$lyrics) %>% VCorpus()  

corpus = tm_map(corpus, removePunctuation)   
corpus = tm_map(corpus, stripWhitespace)   
corpus = tm_map(corpus, removeNumbers)  
corpus = tm_map(corpus, removeWords, stopwords('english'))    
corpus = tm_map(corpus, stemDocument)   
corpus_cleaned = tm_map(corpus, removeWords, myStopwords)

all_dtm = DocumentTermMatrix(corpus_cleaned)
all_dtm = removeSparseTerms(x = all_dtm, sparse = 0.97)
all_m = as.matrix(all_dtm)
dim(all_m)

all_df = as.data.frame(all_m)
dim(all_df)

# Adding predicting features
dataset = all_df

dataset$sentiment = song$total_score %>% scale()
dataset$length = str_length(song$lyrics) %>% scale()
dataset$nword = str_count(song$lyrics, pattern = one_or_more(WRD)) %>% scale()
dataset$gender = song$gender


# Splitting the dataset into train and test set
randomN = sample(nrow(dataset))
dataset = dataset[randomN, ]

part = createDataPartition(y = dataset$gender, p = 0.7, list = F)
training = dataset[part, ]
testing = dataset[-part, ]


# 5. Fitting the models ####
# 5-1. Logistic 
model_glm = glm(gender ~., data = training, family = binomial(link = 'logit'))

pred_glm = predict(model_glm, testing[, -ncol(testing)], response = 'response') 
pred_glm = ifelse(pred_glm < .5, 0, 1)

actualData = testing$gender 
table(actualData, pred_glm)
mean(actualData == pred_glm) 

# 5-2. SVM
model_svm = svm(gender ~., training, type = 'C-classification', kernel = 'linear')

pred_svm = predict(model_svm, testing[, -ncol(testing)]) 

table(actualData, pred_svm)
mean(actualData == pred_svm)

# 5-3. Gradient Boosting
X = as.matrix(training[, -ncol(training)])
Y = as.matrix(training$gender)

model_xgb = xgboost(data = X, label = Y, 
                    objective = 'binary:logistic',
                    eval_meric = 'auc',
                    nrounds = 1000, 
                    eta = .2, 
                    gamma = .85,
                    print_every_n = 50, 
                    max_depth = 5,
                    early_stopping_rounds = 100)

model_xgb$best_iteration

pred_xgb = predict(object = model_xgb, 
                   newdata = as.matrix(testing[, -ncol(testing)]), type = 'probs')
pred_xgb = ifelse(pred_xgb < .5, 0, 1)

table(actualData, pred_xgb)
mean(actualData == pred_xgb)

# variance importance 
xgb.importance(feature_names = names(X), model = model_xgb) %>% xgb.plot.importance(top_n = 15)

# 5-5. Evaluaion
preds_list = list(pred_glm, pred_svm, pred_xgb)

m = length(preds_list)
actuals_list = rep(list(testing$gender), m)

pred = prediction(labels = actuals_list, predictions = preds_list)
rocs = performance(pred, "tpr", "fpr")
plot(rocs, col = as.list(1:m), main = "Test Set ROC Curves")
legend(x = "bottomright", 
       legend = c("Logisitc", "Random Forest", "SVM"),
       fill = 1:m)



