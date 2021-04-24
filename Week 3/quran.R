packages=c('dplyr', 'tidyverse', 'tidytext', 'ggplot2', 'ggraph', 'knitr', 'quRan')
for (p in packages){
  if (! require (p,character.only = T)){
    install.packages(p)
  }
  library(p,character.only = T)
}

#Al-Quran Saheeh International Translation
quranES <- quran_en_sahih %>% 
  select(surah_id, 
         ayah_id,
         surah_title_en, 
         surah_title_en_trans, 
         revelation_type, 
         text, 
         surah, 
         ayah, 
         ayah_title)

quranES_unnest <- quranES %>%
  unnest_tokens(word, text)

#remove unnecessary words e.g., "the", "in", "at", etc.
data(stop_words)
quranES_unnest <- quranES_unnest %>%
  anti_join(stop_words)

#Get some stats
sentiment_counts <- quranES_unnest %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
sentiment_counts %>%
  group_by(sentiment) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip() +
  theme(axis.text = element_text( 
    angle = 0, 
    color="blue", 
    size=10))


library(wordcloud)
library(reshape2)

dev.new(width = 1500, height = 1500, unit = "px")
par(bg="black") 
sentiment_counts %>%
  inner_join(get_sentiments("bing")) %>%
  reshape2::acast(word ~ sentiment, value.var = "n", fill = 0)%>%
  comparison.cloud(colors = c("red", "green"),
                 max.words = 100, random.order=FALSE, title.bg.colors = "black", title.colors = "white", title.size = 3) 
