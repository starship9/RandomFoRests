#Practicing tidytext stuff
library(tidytext)
library(tidyverse)

text <- c(
  "Because I could not stop for Death -",
  "He kindly stopped for me -",
  "The Carriage held but just Ourselves -",
  "and Immortality"
)
text

textDF <- data_frame(line = 1:4, text = text)
textDF

#using unnest_tokens()

textDF %>% unnest_tokens(word, text)

library(stringr)
library(janeaustenr)

#loading the original books after grouping and adding new columns
originalBooks <-
  austen_books() %>% group_by(book) %>% mutate(linenumber = row_number(), chapter = cumsum(str_detect(
    text, regex("^chapter [\\divxlc]", ignore_case = TRUE)
  ))) %>% ungroup()
originalBooks

#tokenizing
tidyBooks <- originalBooks %>% unnest_tokens(word, text)
tidyBooks

data(stop_words)

#removing stopwords
tidyBooks <- tidyBooks %>% anti_join(stop_words)

#counting and sorting
tidyBooks %>%  count(word, sort = TRUE)

#plotting
tidyBooks %>% count(word, sort = TRUE) %>% filter(n > 600) %>% mutate(word = reorder(word, n)) %>% ggplot(aes(word, n)) + geom_col() + xlab(NULL) + coord_flip()

library(gutenbergr)

hgWells <- gutenberg_download(c(35, 36, 5230, 159))

tidyWells <-
  hgWells %>% unnest_tokens(word, text) %>% anti_join(stop_words)

tidyWells %>% count(word, sort = TRUE)

bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))
tidyBronte <-
  bronte %>% unnest_tokens(word, text) %>% anti_join(stop_words)

tidyBronte %>% count(word, sort = TRUE)

frequency <-
  bind_rows(
    mutate(tidyBronte, author = "Brontë Sisters"),
    mutate(tidyWells, author = "H.G. Wells"),
    mutate(tidyBooks, author = "Jane Austen")
  ) %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `Brontë Sisters`:`H.G. Wells`)

library(scales)
ggplot(data = frequency,
       mapping = aes(
         x = proportion,
         y = `Jane Austen`,
         color = abs(`Jane Austen` - proportion)
       )) + geom_abline(color = "gray40", lty = 2) + geom_jitter(
         alpha = 0.1,
         size = 2.5,
         width = 0.3,
         height = 0.3
       ) + geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) + scale_x_log10(labels = percent_format()) + scale_y_log10(labels = percent_format()) + scale_color_gradient(limits = c(0, 0.001),
                                                                                                                                                                                        low = "darkslategray4",
                                                                                                                                                                                         high = "gray75") + facet_wrap( ~ author, ncol = 2) + theme(legend.position = "none") + labs(y = "Jane Austen", x = NULL)
#correlating the texts
cor.test(data = frequency[frequency$author == "Brontë Sisters",],~proportion + `Jane Austen`)

cor.test(data = frequency[frequency$author == "H.G. Wells",],~proportion + `Jane Austen`)


