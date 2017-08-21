## ----global_options, include = FALSE-------------------------------------
knitr::opts_chunk$set(
  fig.width = 7, fig.height = 3.5,
  tidy.opts=list(width.cutoff=60),
  fig.path = 'Figs/',
  fig.align='center',
  warning = FALSE, message = FALSE,
  tidy = TRUE)

## ----installLibraries, include = FALSE-----------------------------------
require(tidytext) #text processing
require(magrittr) #piping
require(tidyverse) #data science ecosystem
require(stringr) #string processing
require(knitr) #kable tables
require(wordcloud) #word clouds
require(RColorBrewer) #color palettes
require(scales)
require(reshape2)

## ----readData------------------------------------------------------------
#setwd("~/Documents/Analytics/R/DouglasAdams")
for(book in list.files("data")){
  temp <- book %>% 
          paste("data", ., sep = "/") %>% 
          readLines() %>% tibble(text = .) %>% 
          mutate(bookline = row_number(), book = substr(book,9,nchar(book)-4))
  
  assign(book %>% substr(1,5),temp)  
}

c(" ",head(Book1$text,20)," ") %>% kable

## ----tokenize------------------------------------------------------------
Doug <- bind_rows(Book1,Book2,Book3,Book4,Book5)
Doug %<>% mutate(chapter = cumsum(str_detect(text,regex("Chapter \\d+$",ignore_case = TRUE))))

Dougwords <- Doug %>% unnest_tokens(word,text)

## ----removeStop----------------------------------------------------------
Dougwords %<>% anti_join(stop_words, by = "word")

## ----countWords, results = "hold"----------------------------------------
Book1count <- Dougwords %>% filter(book == "The Hitchhiker's Guide to the Galaxy") %>% count(word,sort=TRUE)
Book1count %>% filter( n > 50) %>% mutate(word = reorder(word,n))%>% ggplot(aes(word,n)) + geom_col() + xlab(NULL)+ coord_flip()
wordcloud(
  words = Book1count$word, freq = Book1count$n, 
  max.words = 200, 
  random.order = FALSE, rot.per = .35, 
  colors = brewer.pal(8,"Dark2")
)

## ----PlotWordFreq, fig.width=12, fig.height=18---------------------------
Dougfreq <- Dougwords %>% count(book,word) %>% group_by(book) %>% mutate(proportion = n/ sum(n)) %>% select(-n) %>% spread(book,proportion) %>% gather(book, proportion,`The Restaurant at the End of the Universe`,`So Long, and Thanks for All the Fish`,`Life, the Universe and Everything`,`Mostly Harmless`)


# expect a warning about rows with missing values being removed
ggplot(Dougfreq, aes(x = proportion, y = `The Hitchhiker's Guide to the Galaxy`, color = abs(`The Hitchhiker's Guide to the Galaxy` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~book, nrow = 2, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Hitchhiker's Guide to the Galaxy", x = NULL)

## ----PropCorr, fig.height = 5--------------------------------------------
Dougfreq <- Dougwords %>% count(book,word) %>% group_by(book) %>% mutate(proportion = n/ sum(n)) %>% select(-n) %>% spread(book,proportion) %>% gather(book, proportion,`The Hitchhiker's Guide to the Galaxy`, `The Restaurant at the End of the Universe`,`So Long, and Thanks for All the Fish`,`Life, the Universe and Everything`,`Mostly Harmless`)

getCor <- function(book1,book2){
  cor.test(
    Dougfreq %>% filter(book == book1) %>% select(proportion) %>% pull(),
    Dougfreq %>% filter(book == book2) %>% select(proportion) %>% pull()
  )[["estimate"]]
}


booknames <- unique(Dougfreq$book)
R <- matrix(0,5,5,dimnames = list(c("Hitchhiker","Restaurant","So Long", "Life", "Mostly"),booknames))

for(i in 2:5){
  for(j in 1:(i-1)){
    R[i,j] <- getCor(booknames[i],booknames[j])
  }
}
R <- R + t(R)
diag(R) <- 1

meltR <- melt(R)
ggplot(data = meltR, aes(x = Var1, y = Var2, fill = value)) + geom_tile() + 
  scale_colour_gradient(low = "red", high = "white") +
  labs(title = "Book Correlation Heat Map", x = "", y = "" ) +
  guides(fill = guide_legend(title = "Correlation")) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
  plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, hjust = 1))

## ----sentiment-----------------------------------------------------------
nrcAnger <- sentiments %>% filter(lexicon == "nrc", sentiment == "anger")
nrcAngerCount<- Dougwords %>% inner_join(nrcAnger) %>% count(word, sort = TRUE)
wordcloud(
  words = nrcAngerCount$word, freq = nrcAngerCount$n, 
  max.words = 50, 
  random.order = FALSE, rot.per = .35, 
  colors = brewer.pal(8,"Dark2")
)

## ----ComparisonClouds, fig.height = 5, results = "hold"------------------
Dougwords %>% 
  inner_join(sentiments %>% filter(lexicon == "bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup() %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word,n,fill = sentiment)) +
  geom_col(show.legend = FALSE) + 
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()
Dougwords %>%
  inner_join(sentiments %>% filter(lexicon == "bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)

## ----SentimentTime, fig.height = 9---------------------------------------
Dougsentiment <- Dougwords %>% 
  inner_join(sentiments %>% filter(lexicon == "bing"), by = "word") %>%
  count(book, index = bookline %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(Dougsentiment, aes(index, sentiment, fill = book)) + 
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")

## ----BetweenLexicons-----------------------------------------------------
SoLong <- Dougwords %>% filter(book == "So Long, and Thanks for All the Fish")

afinn <- SoLong %>% 
  inner_join(sentiments %>% filter(lexicon == "AFINN")) %>%
  group_by(index = bookline %/% 80) %>%
  summarise(sentiment = sum(score)) %>%
  mutate(method = "AFINN")

bingNRC <- bind_rows(SoLong %>% inner_join(sentiments %>% filter(lexicon == "bing")) %>% mutate(method = "bing"),
                     SoLong %>% inner_join(sentiments %>% filter(lexicon == "nrc") %>% filter(sentiment %in% c("positive","negative"))) %>% mutate(method = "NRC")
                     ) %>%
  count(method, index = bookline %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

bind_rows(afinn, bingNRC) %>% 
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")
  

## ----comparisonCloud-----------------------------------------------------
nrcvalence <- sentiments %>% 
  filter(lexicon == "nrc", sentiment %in% c("positive", "negative")) %>% 
  count(sentiment)

afinnvalence <- sentiments %>%
  filter(lexicon == "AFINN") %>%
  count(score > 0)

bingvalence <- sentiments %>%
  filter(lexicon == "bing") %>%
  count(sentiment)
to_print <- cbind(nrcvalence$n, afinnvalence$n, bingvalence$n)
dimnames(to_print) <- list(c("Count of Negative Words","Count of Positive Words"), c("nrc","AFINN","bing"))
kable(to_print)

## ----SentimentNRC, fig.height = 9----------------------------------------
Dougsentiment2 <- Dougwords %>% 
  inner_join(sentiments %>% filter(lexicon == "nrc", sentiment %in% c("positive","negative")), by = "word") %>%
  count(book, index = bookline %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(Dougsentiment2, aes(index, sentiment, fill = book)) + 
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")


## ----TermFreqDist, fig.height = 5----------------------------------------
#get counts for each word for each book
book_words <- Dougwords %>% 
  count(book, word, sort = TRUE) %>% 
  ungroup()
#total number of words per book
total_words <- book_words %>% 
  group_by(book) %>% 
  summarize(total = sum(n))
#append to tf per book dataset the total number of words per book
book_words <- left_join(book_words, total_words)

#plot distribution of term frequencies
ggplot(book_words, aes(n/total, fill = book))+
  geom_histogram(show.legend = FALSE) +
  xlim(NA, .0009) + 
  facet_wrap(~book, ncol = 2, scales = "free_y")

## ----Zipf----------------------------------------------------------------
freq_by_rank <- book_words %>%
  group_by(book) %>%
  mutate(rank = row_number(), `term frequency` = n/total)
  
p<- freq_by_rank %>%  
  ggplot(aes(rank, `term frequency`, color = book)) +
  geom_line(size = 1.2, alpha = .8) +
  scale_x_log10() +
  scale_y_log10()
p

## ----ZipfFit-------------------------------------------------------------
OLSfit <- lm(log10(`term frequency`) ~ log10(rank), data = freq_by_rank) %>% coef
p + geom_abline(intercept = OLSfit[1], slope = OLSfit[2], color = "gray50", linetype = 2)

## ----tfidf, results = "hold", fig.height = 5-----------------------------
book_words %<>% bind_tf_idf(word, book, n) 
to_kable <- book_words %>% select(-total) %>% arrange(desc(tf_idf)) %>% top_n(10, tf_idf)
to_kable %<>% mutate(tf = round(tf,4), idf = round(idf,3), tf_idf = round(tf_idf,4))
to_kable %>% kable()
plot_doug <- book_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = word %>% unique %>% rev))
plot_doug %>% 
  top_n(20) %>%
  ggplot(aes(word, tf_idf, fill = book)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()

## ----tfidfGrouped, fig.height = 6----------------------------------------
plot_doug %>%
  group_by(book) %>%
  top_n(7) %>%
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip()


