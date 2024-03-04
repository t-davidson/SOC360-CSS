library(tidyverse)
library(gutenbergr)
library(tidytext)

# Getting all english text info
m <- gutenberg_metadata %>%
    filter(language == "en" & has_text == TRUE)

# Finding top collections
top <- m %>% group_by(gutenberg_bookshelf) %>% summarize(n = n()) %>% filter(n >= 50) %>% arrange(desc(n))

# Filtering for sci-fi
scifi <- m %>% filter(gutenberg_bookshelf == "Science Fiction")

# Downloading text
texts <- gutenberg_download(scifi$gutenberg_id)

texts <- texts %>% left_join(m, by = "gutenberg_id") %>%
    drop_na(text)

# Preprocessing
texts.tidy <- texts %>% unnest_tokens(word, text) %>%
    anti_join(stop_words) %>%
    count(title, word) %>%
    group_by(word) %>%
    mutate(total = sum(n),
           text_count = n_distinct(title)) %>%
    ungroup() %>%
    filter(text_count >= 100 & total >= 10) %>%
    filter(!str_detect(word, "^[0-9]+$")) %>%
    bind_tf_idf(word, title, n)

# Creating a DTM
DTM <- texts.tidy %>% cast_dtm(title, word, tf_idf)

print(DTM)
dim(DTM)

# Converting to matrix
DTMd <- as.matrix(DTM)

# Storing matrix
write.csv(DTMd %>% as.data.frame(), "../data/scifi_100.csv", row.names = T)
