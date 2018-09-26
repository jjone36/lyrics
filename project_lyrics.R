library(tidyverse)
library(magrittr)
library(RColorBrewer)

#################################################### Web Scraping ####
library(httr)
library(rvest)
library(jsonlite)

url = 'https://www.billboard.com/charts/artist-100'

# Get Top 100 artists
artists = read_html(url) %>% 
  html_nodes(css = '.chart-number-one__title , .chart-list-item__title-text') %>% 
  html_text()

artists = gsub(pattern = '\n', replacement = '', x = artists)
artists = str_trim(artists)

billboard = tibble(rank = 1:100,
                   artist = artists[1:100])

# Get song titles and lyrics 
song = tibble()
for(i in 1:100){
  url_artist_page = paste0('https://genius.com/artists/', billboard$artist[(i)])
  
  song_title = read_html(url_artist_page) %>%
    html_nodes(css = '.mini_card-title') %>% 
    html_text()
  
  df = tibble(artist = replicate(5, billboard$artist[i]),
              title = song_title[1:5])
  song = rbind(song, df)
}

for(i in 1:100){
  url_artist_page = paste0('https://genius.com/artists/', billboard$artist[i])
  
  url_lyrics = read_html(url_artist_page) %>%
    html_nodes(css = '.mini_card_grid-song a') %>% html_attr('href')
  
  song$url[(i*5-4):(i*5)] = url_lyrics[1:5]
}

for(i in 1:500){
  song$lyrics[i] = read_html(song$url[i]) %>%
    html_nodes(css = '.lyrics p') %>% html_text()
}


# Female & Male artists
url = 'https://www.ranker.com/crowdranked-list/best-female-singer-of-the-year'
female_singers = read_html(url) %>% html_nodes(css = '.listItem__title--link') %>% html_text()

url2 = 'https://www.ranker.com/list/best-new-female-artists/ranker-music?ref=collections_btm&l=2672226&collectionId=2473'
female_singers2 = read_html(url2) %>% html_nodes(css = '.listItem__title--link') %>% html_text()

song$sex = 'Male'
song$sex[song$artist %in% c(female_singers, female_singers2)] = 'Female'
song$sex[song$artist %in% c('Aretha Franklin', 'Ella Mai', 'Lauren Daigle')] = 'Female'

song = as.data.frame(song)
song$sex = as.factor(song$sex)

contraction_clean = function(x){
  x = gsub("won't", "will not", x)
  x = gsub("can't", "can not", x)
  x = gsub("n't", " not", x)
  x = gsub("'ll", " will", x)
  x = gsub("'re", " are", x)
  x = gsub("'ve", " have", x)
  x = gsub("'m", " am", x)
  x = gsub("'d", " would", x)
  x = gsub("'s", "", x)
  return(x)
}

specialChars_clean = function(x) gsub("[^a-zA-Z0-9]", " ", x)

song$lyrics = contraction_clean(song$lyrics)
song$lyrics = specialChars_clean(song$lyrics)

song = song %>%
  filter(artist != 'BTS')

#################################################### Text Mining ####
library(tidytext)
library(wordcloud2)
library(plotrix)

# song = billboard_100

myStopwords = c('chorus', 'intro', 'outro', 'hook', 'bridge', 'verse', 
                'ooh', 'oooh', 'oh', 'uh', 'baby', 'babi', 'bebe', 'yeah', 'yeh', 'ye',
                'o', 'bum', 'na', 'la', 'ai', 'ba', 'hey', '1', '2', '3', 'cardi')

song_token = song %>%
  select(title, lyrics, sex) %>%
  unnest_tokens(output = word, input = lyrics) %>%
  filter(!word %in% c(myStopwords)) %>%
  anti_join(stop_words, by = 'word') 

# Wordcloud
head(song_token)
str(song_token)

song_tidy = song_token %>%
  group_by(sex, word) %>%
  count() %>%
  spread(key = sex, value = n) %>%
  mutate(total = Female + Male) %>%
  ungroup () %>%
  arrange(desc(total)) 

wordcloud2(data = song_tidy[1:100, ], size = 1, color = brewer.pal(12, 'Dark2'))

song_tidy = song_tidy %>% 
  filter(!is.na(Female) & !is.na(Male)) %>%
  mutate(Female_prop = Female / sum(Female),
         Male_prop = Male / sum(Male),
         diff = Female_prop - Male_prop) 

song_tidy %>%
  arrange(desc(diff)) %>%
  head(n = 15) %>%
  ggplot(aes(x = reorder(word, diff), y = diff)) + 
  geom_col(fill = 'indianred1') +
  coord_flip() +
  labs(x = NULL, y = 'Female Freq. Words') +
  theme_bw()

song_tidy %>%
  arrange(diff) %>%
  head(n = 15) %>%
  ggplot(aes(x = reorder(word, -diff), y = (-diff))) + 
  geom_col(fill = 'lightseagreen') +
  coord_flip() +
  labs(x = NULL, y = 'Male Freq. Words') +
  theme_bw()

# Bigram tokenization
bigram_token = song %>%
  select(title, lyrics, sex) %>%
  unnest_tokens(output = bigram, input = lyrics, token = 'ngrams', n = 2)

bigram_token = bigram_token %>%
  separate(bigram, into = c('word1', 'word2'), sep = ' ') %>%
  filter(!word1 %in% c(myStopwords, stop_words$word)) %>%
  filter(!word2 %in% c(myStopwords, stop_words$word)) %>% 
  filter(word1 != word2) %>%
  unite(col = bigram, word1, word2, sep = ' ') %>%
  filter(!bigram %in% tolower(song$artist))

bigram_tidy = bigram_token %>% 
  group_by(sex, bigram) %>%
  count() %>%
  spread(key = sex, value = n) %>%
  mutate(total = Female + Male) %>%
  arrange(desc(total))

wordcloud2(bigram_tidy[1:50, ], size = .4, color = brewer.pal(12, 'Dark2'))

#################################################### Sentimental Anaylsis ####
(nrc = get_sentiments(lexicon = 'nrc'))
(afinn = get_sentiments(lexicon = 'afinn'))

# nrc lexicon 
table(nrc$sentiment)

song_nrc = song_token %>%
  inner_join(nrc) %>%
  group_by(sex, sentiment) %>% 
  count() %>%
  ungroup() 

ggplot(song_nrc, aes(x = sentiment, y = n, fill = sex)) + 
  geom_col() +
  facet_wrap(sex ~.) +
  coord_flip() +
  theme_bw()

song_nrc %>%
  group_by(sex) %>%
  mutate(prop = n / sum(n)) %>%
  arrange(desc(prop)) %>%
  ggplot(aes(x = reorder(sentiment, prop), y = prop, fill = sex)) +
  geom_col() +
  facet_wrap(sex~.) +
  coord_flip() +
  labs(x = 'sentiments', y = NULL) +
  theme_bw() 

# afinn lexicon 
song_afinn = song_token %>%
  inner_join(afinn) %>%
  group_by(title) %>%
  mutate(total_score = sum(score)) %>%
  ungroup() %>%
  arrange(desc(total_score))

song_afinn %>%
  ggplot(aes(x = sex, y = total_score, fill = sex)) + 
  geom_boxplot() +
  theme_bw()

song_afinn %>%
  filter(total_score > -500) %>%
  ggplot(aes(x = sex, y = total_score, fill = sex)) + 
  geom_boxplot() +
  theme_bw()

song_afinn %>%
  group_by(sex) %>%
  summarise(mean_score = mean(score)) 

song_afinn %>%
  filter(total_score > -300) %>%
  ggplot(aes(x = total_score, fill = sex)) + 
  geom_histogram() + 
  facet_grid(. ~ sex) +
  theme_bw()    

song = song_afinn %>%
  filter(total_score > -300) %>%
  select(title, total_score) %>%
  distinct() %>%
  right_join(song, by = 'title')

outlier = which(is.na(song$total_score))

song$title[outlier]
song = song[-outlier, ]

saveRDS(object = song, file = 'billboard_100.rds')

#################################################### Classification ####
library(tm)
library(wordcloud)
library(plotrix)
library(caret)
library(randomForest)
library(e1071)
library(xgboost)
library(caTools)

song_df = song %>% 
  select(title, lyrics)

corpus = VectorSource(song_df$lyrics) %>% VCorpus()  

corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removePunctuation)   
corpus = tm_map(corpus, stripWhitespace)   
corpus = tm_map(corpus, removeNumbers)  

corpus = tm_map(corpus, removeWords, stopwords('english'))    
corpus = tm_map(corpus, stemDocument)   

corpus_cleaned = tm_map(corpus, removeWords, myStopwords)

all_dtm = DocumentTermMatrix(corpus_cleaned)
all_dtm = removeSparseTerms(x = all_dtm, sparse = 0.9)
all_m = as.matrix(all_dtm)
dim(all_m)

song$sex = factor(song$sex, 
                  levels = c('Male', 'Female'), 
                  labels = c(0, 1))

all_df = as.data.frame(all_m)
dim(all_df)

all_df$score = song$total_score
all_df$sex = song$sex
dataset = all_df

dataset = dataset %>%
  filter(!is.na(score))

dataset$score = scale(dataset$score)

randomN = sample(nrow(dataset))
dataset = dataset[randomN, ]

part = createDataPartition(y = dataset$sex, p = 0.7, list = F)
training = dataset[part, ]
testing = dataset[-part, ]


## Logistic 
fit_glm = glm(sex ~ ., data = training, family = binomial(link = 'logit'))

pred.prob = predict(fit_glm, testing[, -ncol(testing)], type = 'response') %>% as.data.frame()
pred_glm = ifelse(pred.prob>= 0.5, 1, 0)

actualData = testing$sex
table(actualData, pred_glm)
mean(actualData == pred_glm) 

confusionMatrix(data = factor(pred_glm), reference = actualData)

## Random Forest
hyper_grid = expand.grid(m_try = seq(4, ncol(training), 2),
                         node_size = seq(3, 8, 2),
                         samp_size = nrow(training)*c(.7, .8))

models = list()
for(i in 1:nrow(hyper_grid)){
  
  models[[i]] = randomForest(default~., training, 
                             mtry = hyper_gird$m_try[i],
                             nodesize = hyper_grid$node_size[i],
                             sampsize = hyper_grid$samp_size[i])
  
  err = models[[i]]$err.rate
  hyper_grid$oob_err[i] = err[nrow(err), 'OOB']
  
}
head(hyper_grid)
hyper_grid[which.min(hyper_grid$oob_err), ]
(opt_model = models[[which.min(hyper_grid$oob_err)]])


fit_rf = randomForest(x = training[, -ncol(training)], y = training[, ncol(training)], ntree = 50)
pred_rf = predict(fit_rf, testing[, -ncol(testing)])

table(actualData, pred_rf)
mean(actualData == pred_rf)

confusionMatrix(data = pred_rf, reference = actualData)

## SVM
fit_svm = svm(sex ~., training, type = 'C-classification', kernel = 'linear')
fit_svm2 = svm(sex ~., training, type = 'C-classification', kernel = 'radial')

pred_svm2 = predict(fit_svm2, testing[, -ncol(testing)]) 

table(actualData, pred_svm2)
mean(actualData == pred_svm2)


## XGboost
fit_xg = xgboost(data = as.matrix(training[, -ncol(training)]), label = training$sex, 
                 nrounds = 10)

pred_xg = predict(fit_xg, as.matrix(testing[, -ncol(testing)]))

table(actualData, pred_xg)
mean(actualData == pred_xg)


## Evaluaion
preds_list = list(pred_glm, pred_rf, pred_svm2, pred_xg)

m = length(preds_list)
actuals_list = rep(list(testing$sex), m)

library(ROCR)
pred = prediction(labels = actuals_list, predictions = preds_list)
rocs = performance(pred, "tpr", "fpr")
plot(rocs, col = as.list(1:m), main = "Test Set ROC Curves")
legend(x = "bottomright", 
       legend = c("Logisitc", "Random Forest", "SVM", "Xgboost"),
       fill = 1:m)
