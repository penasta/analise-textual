# Pacotes e funções ----
pacman::p_load(udpipe,xml2,rvest,tidyverse,wordcloud,RColorBrewer,wordcloud2,
               tm,igraph,ggraph,tm,ggrepel,ggplot2,plotly,gapminder,visNetwork,
               networkD3)
`%notin%` = negate(`%in%`)

# Dados ----


# Modelo ----
# dl <- udpipe_download_model(language = "portuguese-br",
#                             udpipe_model_repo = "jwijffels/udpipe.models.ud.2.0")

udmodel<- udpipe_load_model(file = "portuguese-br-ud-2.0-170801.udpipe")

texto = df$mensagem
# rm(df)
# Modelando ----
txt.anotado <- udpipe::udpipe_annotate(udmodel, x = texto) %>%
  as.data.frame()

# Filtrando classes verbais insignificantes ----
txt.anotado = txt.anotado %>%
  filter(upos %in% c('PRON',
                     'VERB',
                     'NOUN',
                     'ADJ'
  ))

txt.anotado = txt.anotado %>%
  filter(upos %notin% c('DET',
                        'SCONJ',
                        'CCONJ',
                        'ADP'
  ))

filtro = "" # Ajustar

# Definindo nº de skipgram e ocorrências desejadas ----
coocor <- udpipe::cooccurrence(txt.anotado$token, skipgram = 4,
                               relevant = txt.anotado$token %notin% filtro) # Ajuste de parâmetros: token ou lemma | 0 - 5 skipgram

library(data.table)
coocor <- as.data.table(txt.anotado)
coocor <- coocor[, cooccurrence(token, skipgram = 4, order = FALSE), by = list(doc_id)]
head(coocor)

coocor_filtrado <- coocor[coocor$cooc >= 6, ] # Ajustar o filtro conforme necessidade

# Criando tabela auxiliar para parâmetros do gráfico ----
freq = txt.anotado %>%
  select(token) %>%
  group_by(token) %>%
  tally() %>%
  mutate(size = log(n + 1))

# Criando e parametrizando o grafo ----
wordnetwork <- igraph::graph_from_data_frame(coocor_filtrado)
V(wordnetwork)$size <- freq$size[match(V(wordnetwork)$name, freq$token)]

########################### Gráfico estático ##################################

image = ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "lightblue") +
  geom_node_text(aes(label = name, size = size), col = "#006633",repel = T) +
  theme_void() +
  theme(legend.position = "none") 

image
ggsave(file="grafo_estatico.png", plot=image, width=12, height=8,units="cm")

image = ggraph(wordnetwork, layout = "fr") +
  geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "lightblue") +
  geom_node_text(aes(label = name, size = size), col = "#006633") +
  theme(legend.position = "none") 

image
ggsave(file="grafo_estatico.png", plot=image, width=12, height=8,units="cm")

############################# Gráfico dinâmico ################################

nodes <- data.frame(id = V(wordnetwork)$name,
                    label = V(wordnetwork)$name,
                    size = V(wordnetwork)$size)

edges <- data.frame(from = as.character(ends(wordnetwork, E(wordnetwork))[,1]), 
                    to = as.character(ends(wordnetwork, E(wordnetwork))[,2]), 
                    width = sqrt(E(wordnetwork)$cooc))

network <- visNetwork(nodes,
                      edges,
                      width = "100%",
                      height = "100vh") %>%
  visEdges(color = list(color = "lightblue",
                        highlight = "lightblue")) %>%
  visNodes(color = list(background = "#006633",
                        border = "#006633",
                        highlight = "#006633")) %>%
  visOptions(highlightNearest = TRUE,
             nodesIdSelection = TRUE) %>%
  visInteraction(multiselect = T,
                 selectable = T,
                 selectConnectedEdges = T) %>%
  visPhysics(solver = "repulsion",
             stabilization = T)
network
#saveNetwork(network, file = "grafo_interativo.html")
