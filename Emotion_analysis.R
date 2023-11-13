install.packages("tidytext")
install.packages("textdata")
library(tidytext)
library(tidyverse)
library(textdata)
library(sentimentr)
library(ggplot2)
install.packages("sentimentr")

sentiments %>% slice(sample(1:nrow(sentiments)))

sentiments %>% filter(word=='sick')

sentiments = sentiments %>% slice(sample(1:nrow(sentiments)))
sentiments
sentiments %>% filter(sentiment=='superfluous')

library(tidytext)
barth0 = 
  data_frame(file = dir('data/texts_raw/barthelme', full.names = TRUE)) %>%
  mutate(text = map(file, read_lines)) %>%
  transmute(work = basename(file), text) %>%
  unnest(text) 

barth = barth0 %>% 
  mutate(
    text = 
      sapply(
        text, 
        stringi::stri_enc_toutf8, 
        is_unknown_8bit = TRUE,
        validate = TRUE
      )
  ) %>%
  unnest_tokens(
    output = sentence,
    input = text,
    token = 'sentences'
  )

baby = barth %>% 
  filter(work=='baby.txt') %>% 
  mutate(sentence_id = 1:n()) %>%
  unnest_tokens(
    output = word,
    input = sentence,
    token = 'words',
    drop = FALSE
  ) %>%
  ungroup()

baby_sentiment = baby %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(sentence_id, sentence) %>% 
  ungroup()

sentiments %>% slice(sample(1:nrow(sentiments)))

baby_sentiment2 = barth0 %>%
  filter(work=='baby.txt') %>% 
  get_sentences(text) %>% 
  sentiment() %>% 
  drop_na() %>%   # empty lines
  mutate(sentence_id = row_number())


install.packages("devtools")
install.packages("Rtools")
library(devtools)
library(Rtools)
devtools::install_github("ropensci/gutenbergr",force = TRUE)
library(gutenbergr)
gw0 = gutenberg_works(title == "Romeo and Juliet")  # look for something with this title
rnj = gutenberg_download(1513)
rnj = gutenberg_download(gw0$gutenberg_id,mirror ="http://mirror.csclub.uwaterloo.ca/gutenberg/")

rnj_filtered = rnj %>% 
  slice(-(1:49)) %>% 
  filter(!text==str_to_upper(text),            # will remove THE PROLOGUE etc.
         !text==str_to_title(text),            # will remove names/single word lines
         !str_detect(text, pattern='^(Scene|SCENE)|^(Act|ACT)|^\\[')) %>% 
  select(-gutenberg_id) %>% 
  unnest_tokens(sentence, input=text, token='sentences') %>% 
  mutate(sentenceID = 1:n())

stop_words$word[which(stop_words$word %in% sentiments$word)] %>% head(20)

rnj_filtered = rnj_filtered %>% 
  unnest_tokens(output=word, input=sentence, token='words') %>%   
  anti_join(stop_words)

rnj_filtered %>% 
  count(word) %>% 
  arrange(desc(n))

rnj_sentiment = rnj_filtered %>% 
  inner_join(get_sentiments("bing"))
rnj_sentiment

rnj_sentiment_bing = rnj_sentiment %>% 
  filter(lexicon=='bing')
table(rnj_sentiment_bing$sentiment)

rnj_sentiment%>%count(sentiment)

library(tidytext); library(janeaustenr)
austen_books()

austen_books() %>% 
  distinct(book)

nrc_sadness <- get_sentiments("nrc") %>% filter(sentiment == "positive")

ja_book = austen_books() %>% filter(book =="Emma")

ja_book =  ja_book %>%
  mutate(chapter = str_detect(text, regex("^chapter [\\divxlc]", ignore_case = TRUE)),
         line_chapter = cumsum(chapter),
         line_book = row_number()) %>%
  unnest_tokens(word, text)

nrc_bad = get_sentiments("nrc")%>%
  filter(sentiment =="negative")
ja_book_sentiment = ja_book %>% 
  inner_join(get_sentiments("nrc"))%>%
  group_by(chapter) %>%
  count(word,sort =TRUE)

plot_data = ja_book %>%
  inner_join(nrc_bad) %>%
  group_by(chapter, line_book, line_chapter) %>% 
  count() %>%
  group_by(chapter) %>% 
  mutate(negativity = cumsum(n),
         mean_chapter_negativity=mean(negativity)) %>% 
  group_by(line_chapter) %>%
  mutate(mean_line_negativity=mean(n))

plot_data  
