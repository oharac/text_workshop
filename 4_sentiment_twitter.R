library(tidyverse)
library(stringr)

#install.packages("twitteR")
library(twitteR)

### Change the next four lines based on your own consumer_key, consume_secret,
### access_token, and access_secret. 
twitter_key <- scan('~/github/api_keys/twitter.txt', what = 'character')

consumer_key    <- twitter_key[1]
consumer_secret <- twitter_key[2]
access_token    <- twitter_key[3]
access_secret   <- twitter_key[4]

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
tw <- twitteR::searchTwitter('#earthquake', n = 20000, 
                             since = '2018-04-05', until = '2018-04-07',
                             geocode = '34.4208,-119.6982,200mi',
                             retryOnRateLimit = 1e3)
tw_df <- twitteR::twListToDF(tw)

### plot map?
library(sf)
eq_spatial <- tw_df %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  select(text, tw_date = created, latitude, longitude) %>%
  mutate(latitude = as.numeric(latitude),
         longitude = as.numeric(longitude))
eq_sf <- st_as_sf(eq_spatial, coords=c('longitude', 'latitude'))
st_crs(eq_sf) <- 4326

ctrys50m <- rnaturalearth::ne_countries(scale = 50, type = 'countries', 
                                        returnclass = 'sf') %>%
  select(iso_a3, iso_n3, admin)
  
ggplot(eq_sf) +
  # theme_bw() +
  geom_sf(data = ctrys50m, fill = 'grey55', color = 'grey45', size = .25) +
  geom_sf(aes(color = tw_date)) +
  xlim(c(-121, -117)) +
  ylim(c(33, 35.5))
  # ## Search between two dates
# searchTwitter('charlie sheen', since='2011-03-01', until='2011-03-02')

### plot cumulative time series
tw_timeseries <- tw_df %>%
  arrange(created) %>%
  mutate(tw_count = 1:n())
ggplot(tw_timeseries, aes(x = created, y = tw_count)) +
  theme_bw() +
  geom_line() +
  labs(x = 'Tweet date',
       y = 'Cumulative tweets')

### OK let's do a sentiment analysis
library(tidytext)

tweet_text <- tw_df %>%
  select(text, created) %>%
  mutate(text_utf8 = str_replace_all(text, '[^a-zA-Z0-9\\?\\.!#@]+', ' '),
         text_clean = tolower(text_utf8),
         text_clean = str_replace_all(text_clean, '#', ' '),
         text_clean = str_replace_all(text_clean, '\\@[a-z0-9]+ ', ''))

sentiments_b <- get_sentiments('bing')
sentiments_a <- get_sentiments('afinn')
sentiments_n <- get_sentiments('nrc')
# sentiments_l <- get_sentiments('loughran')

tweet_words <- tweet_text %>%
  tidytext::unnest_tokens(output = word, input = text_clean, token = 'words') %>%
  filter(word != 'earthquake') %>% ### don't count the keyword!
  anti_join(stop_words, by = 'word') 

tweet_scores_b <- tweet_words %>%
  inner_join(sentiments_b, by = 'word') %>%
  group_by(sentiment) %>%
  summarize(count = n()) %>% 
  ungroup()

tweet_scores_n <- tweet_words %>%
  inner_join(sentiments_n, by = 'word') %>%
  group_by(sentiment) %>%
  summarize(count = n()) %>% 
  ungroup()

tweet_scores_a <- tweet_words %>%
  inner_join(sentiments_a, by = 'word') %>%
  group_by(text, created) %>%
  summarize(score_text = sum(score, na.rm = TRUE)) %>% 
  ungroup() %>%
  mutate(score_mean = mean(score_text))
