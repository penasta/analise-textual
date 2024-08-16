pacman::p_load(udpipe,xml2,rvest,tidyverse,wordcloud,RColorBrewer,wordcloud2,
               tm,igraph,ggraph,tm)

#dl <- udpipe_download_model(language = "portuguese-bosque")

udmodel_ptBosque <- udpipe_load_model(file = "portuguese-bosque-ud-2.5-191206.udpipe")

txt.anotado <- udpipe::udpipe_annotate(udmodel_ptBosque, x = texto) %>%
  as.data.frame()

txt.anotado = txt.anotado %>%
  filter(upos %in% c('PRON',
                     'VERB',
                     'NOUN',
                     'ADJ'
  ))

coocor <- udpipe::cooccurrence(txt.anotado$token, skipgram = 4) # Ajuste de parâmetros: token ou lemma | 0 - 5 skipgram

coocor_filtrado <- coocor[coocor$cooc >= 35, ] # Ajustar o filtro conforme necessidade

freq = txt.anotado %>%
  select(token) %>%
  group_by(token) %>%
  tally() %>%
  mutate(size = log(n + 1))

wordnetwork <- igraph::graph_from_data_frame(coocor_filtrado)
V(wordnetwork)$size <- freq$size[match(V(wordnetwork)$name, freq$token)]

ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "#003366") +
  geom_node_text(aes(label = name, size = size), col = "#006633") +
  theme(legend.position = "none")
