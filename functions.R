## Code to mine Twitter

##---------------------------------------
## Libraries.
##---------------------------------------

## Extract Tweets
suppressPackageStartupMessages(library(twitteR))
suppressPackageStartupMessages(library(ROAuth))
suppressPackageStartupMessages(library(streamR))
## URLS
suppressPackageStartupMessages(library(RCurl))
## JSON
suppressPackageStartupMessages(library(RJSONIO))
## Data
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(dplyr))
## Strings
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(tau))
suppressPackageStartupMessages(library(tm))
## Strings
suppressPackageStartupMessages(library(FactoMineR))

## ---------------------------------------
## keys & tokens
## ---------------------------------------
access_token        <-
    "235347211-WPBkwLTonKCus2LbIvZm7WJkoUXhwkiKRQn4ONQH"
access_token_secret <-
    "81vTk2pu1Cc424Wx7GcD5MrCNq1zY50K7Ese2D954yy2S"
consumer_key        <-
    "jvIzU7EpCOVHSpKZwIihQweFC"
consumer_secret     <-
    "RtObIJuzJX7fwhHVafLGu3yHsdSacaornwAfddnCAuYWD9O8uh"
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL  <- "https://api.twitter.com/oauth/access_token"
authURL    <- "https://api.twitter.com/oauth/authorize"

## ---------------------------------------
## Handshake
## ---------------------------------------
my_oauth <- OAuthFactory$new(consumerKey=consumer_key,
consumerSecret=consumer_secret, requestURL=requestURL,
accessURL=accessURL, authURL=authURL)
my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))

## ---------------------------------------
## Tweet Stream
## ---------------------------------------
filterStream(file="test.json",
             locations=c(-118.111422, 13.693723,
                         -85.343218, 32.235846), 
             oauth=my_oauth, tweets = 1000)

## ---------------------------------------
## Read in and process results
## ---------------------------------------
tweets <- parseTweets("test.json")

## filter
tweets <- tweets[!is.na(tweets$lon),]
tweets <- tweets[tweets$country == "México",]

## clean
write.csv(tweets, "tweets.csv", row.names = FALSE)
system("./encode.sh tweets.csv")

## read in again
tweets <- read.csv("clean_tweets.csv", stringsAsFactors = FALSE)

## ---------------------------------------
## Clean message
## ---------------------------------------

## Text
text <-
    tweets$text         %>%
    removeNumbers()     %>%
    tolower()           %>%
    removePunctuation() %>%
    str_replace_all("http[[:alpha:]]+","") %>%
    removeWords(stopwords("spanish"))

## Words
words <- str_split(text, " ") %>%
    unlist() %>%
    str_trim()
words <- words[str_length(words) > 2]


## ---------------------------------------
## Word-Doc matrix
## ---------------------------------------

## Words of interest
thresh <- 1
word_count <- plyr::count(words)
words_interest <- filter(word_count, freq > thresh)

## Matrix Structure
term_matrix <- data.frame(matrix(0,
                                nrow = length(unique(text)),
                                ncol = nrow(words_interest)),
                         row.names=unique(text))
colnames(term_matrix) <- words_interest$x

## Matrix Content
for(i in 1:length(text)){
    for(j in 1:ncol(term_matrix)){
        term_matrix[i, j] <- str_count(rownames(term_matrix)[i],
                                      colnames(term_matrix)[j])
    }
}

## ---------------------------------------
## PCA & Clusters
## ---------------------------------------
clusters <- 4
prc_fit  <- prcomp(term_matrix, scale.=T)
eig      <- as.data.frame(prc_fit$x[,1:2])
clus_fit <- kmeans(eig, clusters)
eig$clus <- as.character(clus_fit$cluster)

## Aux functions
## get_coord
get_coord_text <- function(word, eig){
    result     <- list()
    messages   <- rownames(eig)
    inside     <- laply(messages, function(t)t <- str_detect(t, word))
    data       <- eig[inside,]
    data$word  <- rep(word, nrow(data))
    result[[1]] <- data
    result[[2]] <- inside
    result
}

## all_words
words_coords <- c()
aux          <- eig
for(word in words){
    results      <- get_coord_text(word, aux)
    words_coords <- rbind(words_coords, results[[1]])
    aux          <- aux[-results[[2]], ]
}



## Plot
ggplot(eig, aes(x = PC1, y = PC2, col = clus)) + geom_point()
