# this script first queries for news organizations and extracts their tweet ids with #COVID. for each tweet we assign a sentiment score. We then search their replies
# Do not blindly run. Each section has opportunities for error based on internet performance and specific data retrieved. Not robust
# next itteration will move away from AFINN dictionary.


library(tidyr)
library(rtweet)
library(dplyr)
library(ggplot2)
library(textdata)
library(tidytext)
library(widyr)
library(stringr)
library(curl)
options(tibble.width = Inf)

twitter_token <- create_token(
  app = "media_percepticon",
  consumer_key = "6JvIoIc4hoKlVBFlSMt7SzFAH",
  consumer_secret = "QsU9FiryZ9w8JNp9HaY4THtLoHuEcIMkLlrYvnz52AFvLtR3Pj",
  access_token = "433833735-qOLsqVeGHYLueOY7WhT6w1sJhQcUxmkEv0Cm8TJJ",
  access_secret = "0hJBMM8q3YdWt47Ja67PVl0UHF2csNctmQVrq2sUtP11Y",
  set_renv = TRUE)

get_token()
#tostore <- search_tweets(q= '#COVID19 AND -url:amzn.to AND -url:spotify.com AND -url:soundcloud.com AND -url:facebook.com AND -url:zoom.us AND -url:remote.io AND -url:twitter AND -url:zazzle.com AND -url:linkedin.com AND -url:lnkd.in AND -url:instagram AND -url:youtu.be AND -url:youtube.com AND -filter:verified AND -filter:replies AND filter:links AND -filter:news AND -filter:retweets', 

#gather news tweets and user data
NewNewsSources <- search_tweets(q= '#COVID19 AND filter:news AND -filter:retweets', 
                                lang = "en",
                                include_rts = FALSE,
                                n = 10000,
                                geocode = lookup_coords("usa"),
                                parse = TRUE,
                                token = twitter_token,
                                retryonratelimit = TRUE)

NewsSources <- NewNewsSources
#filter out non english sources
NewsSources <- NewsSources[NewsSources$lang == "en",]
#filter out non US country codes
NewsSources <- NewsSources[NewsSources$country_code == "US" | is.na(NewsSources$country_code),]
#filter out small outfits
NewsSources <- NewsSources[NewsSources$statuses_count > 2000 & NewsSources$followers_count > 1000,]
#simplify expanded urls to only have one (if more than one) 
NewsSources$urls_top_simple <- ""
for(i in 1:nrow(NewsSources)) {
  NewsSources[i,]$urls_top_simple <- unlist(NewsSources$urls_expanded_url[[i]][1])
}

#capture URI stems of news for use as definitive source

#create stems of URI from links for tweets with them
uri_stem_generator <- function(x) strsplit(gsub("http://|https://|www\\.", "", x), "/")[[c(1, 1)]]
NewsSources$uri_stem <-  sapply(NewsSources$urls_top_simple, uri_stem_generator)

#unpack redirect uri's

for(i in 1:nrow(NewsSources)) {
  if(NewsSources$uri_stem[i] == "hubs.ly" | 
     NewsSources$uri_stem[i] == "bit.ly" | 
     NewsSources$uri_stem[i] == "tinyurl.com" | 
     NewsSources$uri_stem[i] == "ow.ly" | 
     NewsSources$uri_stem[i] == "buff.ly" |
     NewsSources$uri_stem[i] == "dy.si" |
     NewsSources$uri_stem[i] == "dlvr.it" |
     NewsSources$uri_stem[i] == "is.gd" #|
     #NewsSources$uri_stem[i] == "at.pysc.al"
  ){
    test <- 0
    print(i)
    print(NewsSources$uri_stem[i])
    test <- try(response <- str_extract(grep(curlGetHeaders(NewsSources$urls_top_simple[[i]]), pattern = "Location", value = T), pattern = "http.+"))
    if(test == 0){
      response <- str_extract(grep(curlGetHeaders(NewsSources$urls_top_simple[[i]]), pattern = "Location", value = T), pattern = "http.+")
      if(length(response) == 0){
        
        response <-  str_extract(grep(curlGetHeaders(NewsSources$urls_top_simple[[i]]), pattern = "Link", value = T)[1], pattern = "http.+(?=>;)")
      }
      
      print(response[1])
      NewsSources$urls_top_simple[i] <- response[max(length(response))]
      NewsSources[i,]$uri_stem <- uri_stem_generator(NewsSources[i,]$urls_top_simple)
      #alternate single line
      # stringr::str_extract(grep(curlGetHeaders("www.ard.de"), pattern = "Location", value = T), pattern = "http:*")
      
    } else {
      NewsSources[i,]$uri_stem <- "0"
    }
  }
}



#iterate through each news tweet to identify who replied
#create a list of each status and its relevant id for use in iteration
NewsSources %>%  
  select(status_id, screen_name) -> 
  NameStatusPairs

RepliesToNews2 <- NewsSources[0,]

#for(i in 1:nrow(NameStatusPairs)) {
for(i in 1:100) {
  print(i)
  search_tweets(q= paste("to:", NameStatusPairs[i,]$screen_name," ", "since_id:", NameStatusPairs[i,]$status_id, sep=""), 
                lang = "en",
                include_rts = FALSE,
                n = 500,
                geocode = lookup_coords("usa"),
                parse = TRUE,
                token = twitter_token,
                retryonratelimit = TRUE) -> 
    test
  
  test %>% 
    filter(reply_to_status_id == NameStatusPairs[i,]$status_id) -> 
    test
  
  rbind(RepliesToNews2, test) ->
    RepliesToNews2
}


#-----------------------------------------------------------------------------------
#Sentiment scoring of replies

todaytweets <- RepliesToNews

#filter out non-english post
todaytweets <- todaytweets[todaytweets$lang == "en",]
#filter known non US country codes
todaytweets <- todaytweets[todaytweets$country_code == "US" | is.na(todaytweets$country_code),]


#data cleaning based on the vinete at: https://towardsdatascience.com/twitter-sentiment-analysis-and-visualization-using-r-22e1f70f6967

#remove http elements
todaytweets$stripped_text <- todaytweets$text

todaytweets$stripped_text <- gsub("^[[:space:]]*","",todaytweets$stripped_text) # Remove leading whitespaces
todaytweets$stripped_text <- gsub("[[:space:]]*$","",todaytweets$stripped_text) # Remove trailing whitespaces
todaytweets$stripped_text <- gsub(" +"," ",todaytweets$stripped_text) # Remove extra whitespaces
todaytweets$stripped_text <- gsub("#+", " ", todaytweets$stripped_text) # remove #'s
todaytweets$stripped_text <- gsub("'", "%%", todaytweets$stripped_text) # Replacing apostrophes with %%
todaytweets$stripped_text <- iconv(todaytweets$stripped_text, "latin1", "ASCII", sub="") # Remove emojis/dodgy unicode
todaytweets$stripped_text <- gsub("<(.*)>", "", todaytweets$stripped_text) # Remove pesky Unicodes like <U+A>
todaytweets$stripped_text <- gsub("\\ \\. ", " ", todaytweets$stripped_text) # Replace orphaned fullstops with space
todaytweets$stripped_text <- gsub("  ", " ", todaytweets$stripped_text) # Replace double space with single space
todaytweets$stripped_text <- gsub("%%", "\'", todaytweets$stripped_text) # Changing %% back to apostrophes
todaytweets$stripped_text <- gsub("https(.*)*$", "", todaytweets$stripped_text) # remove tweet URL
todaytweets$stripped_text <- gsub("\\n", "-", todaytweets$stripped_text) # replace line breaks with -
todaytweets$stripped_text <- gsub("--", "-", todaytweets$stripped_text) # remove double - from double line breaks
todaytweets$stripped_text <- gsub("&amp;", "&", todaytweets$stripped_text) # fix ampersand &


#unify terms
todaytweets$stripped_text <- gsub("covid", "covid19", todaytweets$stripped_text, ignore.case = TRUE)
todaytweets$stripped_text <- gsub("coronavirus", "covid19", todaytweets$stripped_text, ignore.case = TRUE)

#remove no value words
todaytweets$stripped_text <- gsub("[0-9]", "", todaytweets$stripped_text, ignore.case = TRUE)
todaytweets$stripped_text <- gsub("trump", "", todaytweets$stripped_text, ignore.case = TRUE)
todaytweets$stripped_text <- gsub("covid19", "", todaytweets$stripped_text, ignore.case = TRUE)
todaytweets$stripped_text <- gsub("covid", "", todaytweets$stripped_text, ignore.case = TRUE)

#create a list of word tokens from our cleaned tweets
#convert to lowercase and remove punctuation
todaytweets_stem <- todaytweets %>%
  select(stripped_text) %>%
  unnest_tokens(word, stripped_text)

todaytweets_stem <- todaytweets_stem %>%
  anti_join(stop_words)

todaytweets_stem %>%
  count(word, sort = TRUE) %>%
  top_n(30) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  theme_classic() +
  labs(x = "Count",
       y = "Unique words",
       title = "unique word counts found")


#sentiments expected through affin scoring

#create a DF of words and their scores by frequency across our data
afinn_today <- todaytweets_stem %>%
  inner_join(get_sentiments("afinn")) %>%
  count(word, value, sort = TRUE) %>%
  ungroup()

#view histogram of tokens grouped by affinity
afinn_today %>%
  group_by(value) %>%
  top_n(15) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = value)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~value >=0, scales = "free_y") +
  labs(title = "Tweet",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip() + theme_bw()



#sentiments expected through bing scoring

bing_today <- todaytweets_stem %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()


bing_today %>%
  group_by(sentiment) %>%
  top_n(30) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Tweet",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip() + theme_bw()


#sentiments expected through nrc scoring
#note from author:
#site as such:
#
#Saif M. Mohammad and Peter Turney. (2013), ``Crowdsourcing a Word-Emotion Association Lexicon.'' Computational Intelligence, 29(3): 436-465.
#article{mohammad13,
#  author = {Mohammad, Saif M. and Turney, Peter D.},
#  title = {Crowdsourcing a Word-Emotion Association Lexicon},
#  journal = {Computational Intelligence},
#  volume = {29},
#  number = {3},
#  pages = {436-465},
#  doi = {10.1111/j.1467-8640.2012.00460.x},
#  url = {https://onlinelibrary.wiley.com/doi/abs/10.1111/j.1467-8640.2012.00460.x},
#  eprint = {https://onlinelibrary.wiley.com/doi/pdf/10.1111/j.1467-8640.2012.00460.x},
#  year = {2013}
#}



nrc_today <- todaytweets_stem %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()


nrc_today %>%
  group_by(sentiment) %>%
  top_n(30) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Tweet",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip() + theme_bw()


#afinn sentiments appear more appropriate for what we are trying to measure

#build a data structure of words and rankings, one row per tweet.
#where score = sum of affinity of tokens
sentiment_afinn <- function(twt) {
  twt_tbl <- tibble(text = twt) %>%
    mutate(
      #remove http elements manualy
      stripped_text = gsub("http\\S+","",text)
    ) %>%
    unnest_tokens(word, stripped_text) %>%
    anti_join(stop_words) %>% #remove stop words
    inner_join(get_sentiments("afinn")) %>% #merge with afinn sentiment
    count(word, score = value, sort = TRUE) %>%
    ungroup() 
  #calculate total score
  sent.score <- case_when(
    nrow(twt_tbl)==0~0, #if there are no words, score is 0
    nrow(twt_tbl)>0~sum(twt_tbl$score)
  )
  ## track tweets with words not on bing list
  zero.type = case_when(
    nrow(twt_tbl)==0~"Type 1", # type 1: no words at all
    nrow(twt_tbl)>0~"Type 2" # type 2: zero means sum of words = 0
  )
  list(score = sent.score, type = zero.type, twt_tbl = twt_tbl)
}

sentiments <- lapply(todaytweets$stripped_text, function(x){sentiment_afinn(x)})

todaytweets$sent_afinn <- NA

#assign scores to each tweet in our data
for(i in 1:length(sentiments)) {
  todaytweets$sent_afinn[i] <- as.integer(unlist(sentiments[[i]][1]))
}

#assign longitude and latitude to data
todaytweets <- lat_lng(todaytweets)


#------------------------------------------------------------------------------------------------------

#write study data frame

replies <- todaytweets[,c("status_id","reply_to_status_id",,"display_text_width","sent_afinn")]
names(replies) <- c("tweet_id","reply_to_status_id","ncar","sent")
head(replies)

news <- NewsSources[,c("status_id","uri_stem","followers_count","statuses_count")]

test <- cbind(replies[0,], news[0,])

#can't dplyr::left_join on characters, and coercion turns to floating point but NA's on integer.. so....
for(i in 1:nrow(replies)) {
  test[i,] <- cbind(replies[i,], news[news$status_id == replies[i,]$reply_to_status_id,])
}


DF$source <- as.numeric(DF$uri_stem)

write.csv(test,"./data/tweets.csv")


#---------------------------------------------------------------------------
 
#This section is for generating plots in rstudio. These are manually scaled and saved as images rather than using ggplot save functionality

 
trial <- todaytweets %>% 
  group_by(uri_stem) %>% 
  summarise(sentiment_afinn = mean(sent_afinn),freq = n())


#visualize affinity performance by news source
trial %>%
  arrange(freq) %>%
  filter(freq > 1) %>%
  ggplot(aes(x = sentiment_afinn, y = uri_stem)) + 
  geom_col() +
  facet_wrap(~sentiment_afinn >0, scales = "free_y") +
  labs(title = "afinn performance", y = "afinn sentiment score", x = "URI") 

#visualize affinity performance by news source
trial %>%
  arrange(freq) %>%
  filter(freq > 5) %>%
  ggplot(aes(x = sentiment_afinn, y = uri_stem)) + 
  geom_col() +
  facet_wrap(~sentiment_afinn >0, scales = "free_y") +
  labs(title = "afinn performance", y = "afinn sentiment score", x = "URI") 


#plot tweets on map
#consider implementing the leaflet map ui ....
#https://rstudio.github.io/leaflet/
## create lat/lng variables using all available tweet and profile geo-location data
## plot state boundaries
par(mar = c(0, 0, 0, 0))
maps::map("state", lwd = .25)
## plot lat and lng points onto state map
with(todaytweets, points(lng, lat, pch = 20, cex = .75, col = rgb(0, .3, .7, .75)))



