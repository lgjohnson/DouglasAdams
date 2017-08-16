## ----global_options, include = FALSE-------------------------------------
knitr::opts_chunk$set(
  fig.width = 7, fig.height = 4.5, 
  fig.path = 'Figs/',
  warning = FALSE, message = FALSE,
  tidy = TRUE)

## ----installLibraries, include = FALSE-----------------------------------
require("tidytext") #text processing
require("magrittr") #piping
require("tidyverse") #data science ecosystem
require("stringr") #string processing
require("knitr") #kable tables
require("wordcloud") #word clouds
require("RColorBrewer") #color palettes

## ----readData------------------------------------------------------------
#setwd("~/Documents/Analytics/R/DouglasAdams")
book1 <- readLines("data/HitchHiker.txt")
book1 %<>% tibble(line = 1:length(.), text = .)
c(" ",head(book1$text,20)," ") %>% kable

## ----tokenize------------------------------------------------------------
book1 %<>% mutate(linenumber = row_number(),
                      chapter = cumsum(str_detect(text,regex("Chapter \\d+$",ignore_case = TRUE))))

book1words <- book1 %>% unnest_tokens(word,text)


## ----removeStop----------------------------------------------------------
book1words %<>% anti_join(stop_words)

## ----countWords, results = "hold"----------------------------------------
book1count <- book1words %>% count(word,sort=TRUE)
book1count %>% filter( n > 50) %>% mutate(word = reorder(word,n))%>% ggplot(aes(word,n)) + geom_col() + xlab(NULL)+ coord_flip()
wordcloud(
  words = book1count$word, freq = book1count$n, 
  max.words = 200, 
  random.order = FALSE, rot.per = .35, 
  colors = brewer.pal(8,"Dark2")
)

