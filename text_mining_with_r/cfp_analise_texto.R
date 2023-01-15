##Autor: Daniel Oliveira Zacarias
##Data: 15/01/2023


# Referências -------------------------------------------------------------

#### https://ptbr.netlify.app/post/tidytext-package/
### http://www.leg.ufpr.br/~walmes/ensino/mintex/tutorials/03-sentimento.html#5_exibi%C3%A7%C3%A3o_das_avalia%C3%A7%C3%B5es_polarizadas
### https://rpubs.com/sumitkumar-00/twitter_sentiment_analysis

# Bibliotecas -------------------------------------------------------------

library(tidyverse)
library(tidytext)
library(tm)

# Carregar Dados ----------------------------------------------------------

bd_raw <- read_csv("result.csv")

bd_raw <- bd_raw %>% 
  select(ownerId, comment) %>% 
  slice(-1)


# Processamento de dados --------------------------------------------------

ig_tokenized <- 
  bd_raw %>% 
  tidytext::unnest_tokens(output = token,
                          input = comment)

ig_tokenized %>% 
  glimpse()

library(stopwords)

stopwords_vec <- stopwords::stopwords(language = "pt")

ig_tk_cleaned <- ig_tokenized %>% 
  filter(!token %in% stopwords_vec) %>%  #remove as stopwords 
  filter(!str_detect(token, "[:digit:]")) #remove os números

library(SnowballC)

ig_stemmed <- ig_tk_cleaned %>% 
  mutate(token = SnowballC::wordStem(token, language = "pt"))

library(lexiconPT)
ls("package:lexiconPT")

sample_n(oplexicon_v3.0, size = 20) %>%
  arrange(polarity)

# Análises ----------------------------------------------------------------

tb_sen <- inner_join(ig_tk_cleaned,
                     oplexicon_v3.0[, c("term", "polarity")],
                     by = c("token" = "term"))

sample_n(tb_sen, size = 20)

tb <- tb_sen %>%
  group_by(ownerId) %>%
  summarise(soma = sum(polarity),
            n = n(),
            sentiment = soma/n)
tb

# Vizualização ------------------------------------------------------------

library(wordcloud)
library(ggplot2)

# Para poder melhorar o dicionário.

# Determina as frequências dos termos de polaridade não nula.
tb_words <- tb_sen %>%
  count(token, polarity, sort = TRUE) %>%
  filter(polarity != 0)

tb_cloud <- tb_words %>%
  spread(key = "polarity", value = "n", fill = 0) %>%
  rename("negativo" = "-1", "positivo" = "1")

tb_cloud

tb <- as.data.frame(tb_cloud[, c("negativo", "positivo")])

rownames(tb) <- tb_cloud$token

head(tb)

comparison.cloud(tb,
                 colors = c("red", "blue"),
                 max.words = min(nrow(tb), 200))



tb_sen %>%
  count(token, polarity, sort=TRUE) %>%
  group_by(polarity) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(n = ifelse(polarity==1, n, -n)) %>%
  arrange(desc(n)) %>%
  ggplot(aes(x=factor(token, levels = rev(unique(token))), y = n, fill = polarity)) + 
  geom_bar(stat= 'identity') +
  labs(y="Frequência de Palavras", x = "") + coord_flip() +
  scale_fill_gradientn(colors = c("red", "gray", "green"), name = "Sentimento")



