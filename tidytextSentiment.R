library(tidytext)
sentiments

library(janeaustenr)
library(dplyr)
library(stringr)

tidyBooks <-
  austen_books() %>% group_by(book) %>% mutate(linenumber = row_number(), chapter = cumsum(str_detect(
    text, regex("^chapter [\\divxlc]", ignore_case = TRUE)
  ))) %>%  ungroup() %>% unnest_tokens(word, text)

nrcJoy <- get_sentiments("nrc") %>% filter(sentiment == "joy")

tidyBooks %>% filter(book == "Emma") %>% inner_join(nrcJoy) %>% count(word, sort = TRUE)

library(tidyr)

jaSentiment <- tidyBooks %>% inner_join(get_sentiments("bing")) %>% count(book, index = linenumber %/% 80,sentiment) %>% spread(sentiment,n,fill = 0) %>% mutate(sentiment = positive - negative)

library(ggplot2)
ggplot(data = jaSentiment,mapping = aes(x = index, y = sentiment, fill = book)) + geom_col(show.legend = FALSE) + facet_wrap(~book,ncol = 2,scales = "free_x")

pridePrejudice <-  tidyBooks %>% filter(book == "Pride & Prejudice")
pridePrejudice

afinn <- pridePrejudice %>% inner_join(get_sentiments("afinn")) %>% group_by(index = linenumber%/%80) %>% summarise(sentiment = sum(score)) %>% mutate(method = "AFINN")

bingNrc <- bind_rows(pridePrejudice %>% inner_join(get_sentiments("bing")) %>% mutate(method = "Bing et al."),pridePrejudice %>% inner_join(get_sentiments("nrc") %>% filter(sentiment %in% c("positive","negative"))) %>% mutate(method = "NRC")) %>% count(method,index = linenumber%/%80,sentiment) %>% spread(sentiment,n,fill = 0) %>% mutate(sentiment = positive - negative)

#plotting
bind_rows(afinn,bingNrc) %>% ggplot(aes(x = index, y = sentiment,fill = method)) + geom_col(show.legend = FALSE) + facet_wrap(~method,ncol = 1,scales = "free_y")

bind_rows(afinn,bingNrc) %>% ggplot(aes(x = index, y = sentiment,fill = method)) + geom_col(show.legend = FALSE,position = "identity",alpha = 0.5)

get_sentiments("nrc") %>% filter(sentiment %in% c("positive","negative")) %>% count(sentiment)

get_sentiments("bing") %>% count(sentiment)

bingWordCounts <- tidyBooks %>% inner_join(get_sentiments("bing")) %>% count(word,sentiment,sort = TRUE) %>% ungroup()
bingWordCounts

bingWordCounts %>% group_by(sentiment) %>% top_n(10) %>% ungroup() %>% mutate(word = reorder(word,n)) %>% ggplot(aes(word,n,fill = sentiment)) + geom_col(show.legend = FALSE) + facet_wrap(~sentiment,scales = "free_y") + labs(y = "Contribution to sentiment",x = NULL) + coord_flip()

library(wordcloud)

tidyBooks %>% anti_join(stop_words) %>% count(word) %>% with(wordcloud(word,n,max.words = 100,scale = c(3,0.25)))

library(reshape2)

tidyBooks %>% inner_join(get_sentiments("bing")) %>% count(word,sentiment,sort=TRUE) %>% acast(word~sentiment,value.var = "n",fill = 0) %>% comparison.cloud(colors = c("#F8766D", "#00BFC4"),max.words = 100,scale = c(3,0.5))

PandP_sentences <- data_frame(text = prideprejudice) %>% 
  unnest_tokens(sentence, text, token = "sentences")

PandP_sentences$sentence[2]

austenChapters <- austen_books() %>% group_by(book) %>% unnest_tokens(chapter,text,token="regex",pattern = "Chapter|CHAPTER [\\dIVXLC]") %>% ungroup()
austenChapters %>% group_by(book) %>% summarise(chapters = n())

bingNegative <- get_sentiments("bing") %>% filter(sentiment=="negative")
wordCounts <- tidyBooks %>% group_by(book,chapter) %>% summarise(words = n())

tidyBooks %>% semi_join(bingNegative) %>% group_by(book,chapter) %>% summarise(negativewords = n()) %>% left_join(wordCounts,by = c("book","chapter")) %>%  mutate(ratio = negativewords/words) %>% filter(chapter!=0) %>% top_n(1) %>% ungroup()
