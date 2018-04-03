library(tidyverse)
library(stringr)

library(pdftools)
library(tidytext)

hobbit <- pdf_text('pdfs/the_hobbit.pdf')
hobbit_tbl <- tibble(text = hobbit) %>%
  mutate(text = tolower(text)) 

main_text <- hobbit_tbl[18:487, ] %>% 
  mutate(chapter = str_extract(text, '^chapter .+')) %>%
  tidyr::fill(chapter, .direction = 'down') %>%
  mutate(lines = str_split(text, '\n')) %>%
  unnest(lines)

### Examine stop words and sentiments
head(stop_words) ### common words that won't impact analysis
?get_sentiments
sentiments_b <- get_sentiments('bing')
# sentiments_a <- get_sentiments('afinn')
# sentiments_n <- get_sentiments('nrc')
# sentiments_l <- get_sentiments('loughran')

text_words <- main_text %>%
  tidytext::unnest_tokens(output = word, input = lines, token = 'words') %>%
  select(chapter, word) %>%
  anti_join(stop_words, by = 'word') %>%
  inner_join(sentiments_b, by = 'word')

text_scores <- text_words %>%
  count(chapter, sentiment) %>%
  spread(sentiment, n) %>%
  mutate(score = (positive - negative) - mean(positive - negative),
         score_norm = score / (positive + negative))
  
  
hobbit_toc <- hobbit_tbl[7:8, ] %>%
  mutate(ch_title = str_split(text, '\n')) %>%
  unnest(ch_title) %>%
  select(ch_title) %>%
  filter(str_detect(ch_title, '^chapter')) %>%
  mutate(chapter = str_extract(tolower(ch_title), '^ch.+(?=:)'),
         ch_title = str_extract(ch_title, '(?<=:).+'),
         ch_title = tools::toTitleCase(ch_title),
         ch_title = factor(ch_title, levels = ch_title[19:1]))

ch_sentiments <- text_scores %>%
  left_join(hobbit_toc, by = 'chapter')

ggplot(ch_sentiments, aes(x = ch_title)) +
  theme_classic() +
  geom_bar(aes(y = score), stat = 'identity', fill = 'slateblue3') +
  labs(title = 'Sentiment analysis: The Hobbit, by chapter',
       y = 'Sentiment score') +
  coord_flip() +
  theme(axis.title.y = element_blank())
