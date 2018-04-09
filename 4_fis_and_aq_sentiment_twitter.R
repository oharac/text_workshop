library(tidyverse)
library(stringr)

#install.packages("twitteR")
library(twitteR)

### Change the next lines based on your own consumer_key, consume_secret,
### access_token, and access_secret. I saved my keys in a four-line text file,
### which gets read by the 'scan()' function and then passed to the four
### authorization tokens
twitter_key <- scan('~/github/api_keys/twitter.txt', what = 'character')

consumer_key    <- twitter_key[1]
consumer_secret <- twitter_key[2]
access_token    <- twitter_key[3]
access_secret   <- twitter_key[4]

twitteR::setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

### Who loves fisheries and aquaculture more - iphone users or android users?

fish_tweets_file <- 'data/fis_aq_tweets.csv'
if(!file.exists(fish_tweets_file)) { 
  ### A bit slow so save it for future use...
  tw_fis <- twitteR::searchTwitter('#fisheries', n = 5000, 
                                   # since = '2018-04-05', until = '2018-04-07',
                                   # geocode = '34.4208,-119.6982,200mi',
                                   retryOnRateLimit = 1e3)
  tw_fis_df <- twitteR::twListToDF(tw_fis)
  tw_aq <- twitteR::searchTwitter('#aquaculture', n = 5000, 
                                   # since = '2018-04-05', until = '2018-04-07',
                                   # geocode = '34.4208,-119.6982,200mi',
                                   retryOnRateLimit = 1e3)
  tw_aq_df <- twitteR::twListToDF(tw_aq)
  
  tw_df <- bind_rows(tw_aq_df, tw_fis_df)
  write_csv(tw_df, fish_tweets_file)
} else {
  tw_df <- read_csv(fish_tweets_file)
}

### Set up some devices and hashtags we want to test:
devices <- c('iphone', 'ipad', 'android') %>% 
  paste(collapse = '|')
hashtags <- c('#aquaculture', '#fisheries') %>%
  paste(collapse = '|')

### Note use of POSIX instead of more complicated [^A-Za-z0-9<etc>]
tw_by_phone_df <- tw_df %>%
  mutate(text = str_replace_all(text, '[^[:ascii:]]', '_') %>% 
           tolower(),
         text_clean = str_replace_all(text, '@[^ ]+', '_usr_'),
         text_clean = str_replace_all(text_clean, 'http[^ ]+', '_url_'),
         statusSource = tolower(statusSource)) %>%
  mutate(device = ifelse(str_detect(statusSource, devices), 
                         str_match(statusSource, devices),
                         'other')) %>%
  mutate(hashtag = ifelse(str_detect(text_clean, hashtags), 
                          str_match_all(text_clean, hashtags),
                          'other')) %>%
  unnest(hashtag) %>%
  distinct()

tmp <- tw_by_phone_df %>%
  select(text, text_clean, statusSource, device, hashtag)

### OK now let's do a sentiment analysis
library(tidytext)

sentiments_b <- tidytext::get_sentiments('bing')
sentiments_a <- get_sentiments('afinn')
sentiments_n <- get_sentiments('nrc')
# sentiments_l <- get_sentiments('loughran')

test_df <- tw_by_phone_df %>%
  select(text, text_clean, created, device, hashtag) %>%
  filter(hashtag != 'other') %>%
  tidytext::unnest_tokens(output = word, input = text_clean, token = 'words') %>%
  anti_join(tidytext::stop_words, by = 'word') %>%
  left_join(sentiments_b, by = 'word')
### Check the sentiment assignments by word - some are pretty funny,
### but also shows limitation of the word bank used

score_df <- test_df %>%
  count(device, hashtag, sentiment) %>%
  spread(sentiment, n) %>%
  mutate(score = (positive - negative) - mean(positive - negative, na.rm = TRUE))

ggplot(score_df, aes(x = device, fill = hashtag)) +
  theme_classic() +
  geom_bar(aes(y = score), stat = 'identity', position = 'dodge') +
  labs(y = 'sentiment score') +
  ylim(c(-100, 250)) +
  coord_flip()
