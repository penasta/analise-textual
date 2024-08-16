pacman::p_load(xml2,rvest,tidyverse,SentimentAnalysis,lexiconPT,stringi)

lexico <- eval(parse(text = "oplexicon_v3.0")) %>%
  distinct(term, .keep_all = TRUE) %>%
  select(term,polarity)

positivas = lexico %>%
  filter(polarity == 1) %>%
  select(term) %>%
  as.vector() %>%
  unlist()

negativas = lexico %>%
  filter(polarity == -1) %>%
  select(term) %>%
  as.vector() %>%
  unlist()

dict_pt <- SentimentDictionaryBinary(
  # vetor de palavras com conotação positiva
  positivas,
  # vetor de palavras com conotação negativa
  negativas
)

AnaliseSentimentos <- analyzeSentiment(texto,
                                       language="portuguese",
                                       rules=list("pontos"=list(ruleSentiment, dict_pt )))

mean(AnaliseSentimentos$pontos, na.rm = T)
summary(AnaliseSentimentos$pontos)
