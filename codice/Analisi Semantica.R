library(tm)
library(udpipe)
library(wordcloud)
library(RColorBrewer)

tweets <- readLines('/Users/andreaermellino/desktop/tweetext.txt', encoding = 'UTF-8')
tweets_unico <- paste(tweets, collapse = '')
tweets_unico <- VCorpus(VectorSource(tweets_unico))
inspect(tweets_unico)

tweets_unico <- tm_map(tweets_unico, removeNumbers)
tweets_unico <- tm_map(tweets_unico, removePunctuation)
tweets_unico <- tm_map(tweets_unico, removeWords, stopwords(kind = 'en'))
tweets_unico <- tm_map(tweets_unico, removeWords, stopwords(kind = 'it'))
tweets_unico <- tm_map(tweets_unico, removeWords, stopwords(kind = 'es'))
tweets_unico <- tm_map(tweets_unico, removeWords, stopwords(kind = 'fr'))
tweets_unico <- tm_map(tweets_unico, removeWords, stopwords(kind = 'de'))
tweets_unico <- tm_map(tweets_unico, removeWords, stopwords(kind = 'ru'))
tweets_unico <- tm_map(tweets_unico, removeWords, stopwords(kind = 'pt'))
tweets_unico <- tm_map(tweets_unico, removeWords, stopwords(kind = 'SMART'))
tweets_unico <- tm_map(tweets_unico, stripWhitespace)


dtm <- DocumentTermMatrix(tweets_unico)
findMostFreqTerms(dtm, 20)

mat <- as.matrix(t(dtm))
tot_r <- rowSums(mat)
tot_r_ord <- sort(tot_r, decreasing = TRUE)
nomi <- names(tot_r_ord)[1:10]
nomi_da_eliminare <- c(nomi[4], nomi[6])
tot_r_ord <- tot_r_ord[! names(tot_r_ord) %in% nomi_da_eliminare]
head(tot_r_ord)
barplot(tot_r_ord[1:10], names.arg = names(tot_r_ord[1:10]), col = heat.colors(10), ylab = 'Frequenza', xlab = 'Parole')
wordcloud(words = names(tot_r_ord), freq = tot_r_ord, min.freq = 1500, colors = brewer.pal(8, "Dark2"))
