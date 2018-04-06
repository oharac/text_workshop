library(tidyverse)
library(stringr)

library(tidytext)

#install.packages("twitteR")
library(twitteR)

# Change the next four lines based on your own consumer_key, consume_secret, access_token, and access_secret. 
twitter_key <- scan('~/github/api_keys/twitter.txt', what = 'character')

consumer_key    <- twitter_key[1]
consumer_secret <- twitter_key[2]
access_token    <- twitter_key[3]
access_secret   <- twitter_key[4]

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
tw <- twitteR::searchTwitter('#earthquake', n = 100000, 
                             since = '2018-04-03', 
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

ctrys50m <- rnaturalearth::ne_countries(scale = 50, type = 'countries', returnclass = 'sf') %>%
  select(iso_a3, iso_n3, admin)
  
ggplot(eq_sf) +
  # theme_bw() +
  geom_sf(data = ctrys50m, fill = 'grey55', color = 'grey45', size = .25) +
  geom_sf(aes(color = tw_date)) +
  xlim(c(-121, -117)) +
  ylim(c(33, 35.5))
  # ## Search between two dates
# searchTwitter('charlie sheen', since='2011-03-01', until='2011-03-02')

ggplot(tw_df, aes(x = created)) +
  geom_bar()
