library(tidyverse)
library(tidytext)

seinfeld <- read.csv("scripts.csv", stringsAsFactors = FALSE)

sentiments <- get_sentiments("bing")

text_words <- seinfeld %>% 
    as_tibble() %>% 
    tidytext::unnest_tokens(output = word, input = Dialogue, token = "words") %>% 
    inner_join(sentiments, by = "word") 

seinfeld_plot <- text_words %>% 
    group_by(Season) %>% 
    count(sentiment, EpisodeNo) %>% 
    # filter(Character %in% c("ELAINE", "GEORGE", "JERRY", "KRAMER")) %>% 
    spread(sentiment, n) %>% 
    na.omit() %>% 
    mutate(raw_score = positive - negative,
           positivity = positive/(positive + negative),
           offset_positivity = positivity - .5,
           offset = mean(positive - negative),
           offset_score = raw_score - offset) %>% 
    # mutate(Character = reorder(Character, offset_score)) %>%
    ggplot(aes(x = EpisodeNo, y = offset_positivity)) +
    geom_col() +
    facet_grid(.~ Season)

library(plotly)
ggplotly(seinfeld_plot)
