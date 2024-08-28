# Pacotes e funções ----
pacman::p_load(udpipe,xml2,rvest,tidyverse,wordcloud,RColorBrewer,wordcloud2,
               tm,igraph,ggraph,tm,ggrepel,ggplot2,plotly,gapminder,visNetwork,
               networkD3)
`%notin%` = negate(`%in%`)

# Dados ----
source("rdocs/concatenar_textos_zapzap.R")

# Modelo ----
#dl <- udpipe_download_model(language = "portuguese-bosque")

udmodel_ptBosque <- udpipe_load_model(file = "portuguese-bosque-ud-2.5-191206.udpipe")

# Modelando ----
txt.anotado <- udpipe::udpipe_annotate(udmodel_ptBosque, x = texto) %>%
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


# Filtrando termos insignificantes ----
txt.anotado = txt.anotado %>%
  filter(token %notin% c("silva","souza","gilson","roberto","joao","lucia","reginaldo",
                         "paulo","fernando","maria","vanessa","valter","tiago","thomaz",
                         "robson","pablo","josiel","josequias","jones","jonathas","hellen",
                         "helio","edu","edilson","drezao","davi","augusto","anderson",
                         "andre","jose","italo","sergio","rezende","mauro","aria","wande",
                         "gilvan","henrique","andrade","gabriel","marcos","irineu",
                         "elson","zack","null","faria","braziel","neto","campos","co",
                         "nosco","bla","to","pereira","cesar","vilmanevespereira",
                         "sonia","raimundo","billy","marbit","vrsnts","camp","buzaglo",
                         "yuki","sarmento","holanda","cruzeiro","ncampos","master",
                         "rino","stuart","fabio","fvp","cmack","rsrs","jefersonbernardo",
                         "na","um","isso","se","so","me","att","no","ela","tem",
                         "o","dos","os","ja","vao","da","do","foi","ele","dele",
                         "mim","comigo","ate","que","deles","ao","sim","nao","ser",
                         "la","nao","aos","sao","onde","das","ter","se","consigo",
                         "hoje","estava","vou","entrar","tenho","avisem","feito",
                         "mega","esta","estao","eu","sul","catarina","amazonas","bahia",
                         "minas","gerais","rio","janeiro","rj","parte","deste","feiras",
                         "feira","vai","atraves")
  )

# Definindo nº de skipgram e ocorrências desejadas ----
coocor <- udpipe::cooccurrence(txt.anotado$token, skipgram = 5) # Ajuste de parâmetros: token ou lemma | 0 - 5 skipgram

coocor_filtrado <- coocor[coocor$cooc >= 8, ] # Ajustar o filtro conforme necessidade

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

nodes <- data.frame(id = V(wordnetwork)$name, label = V(wordnetwork)$name, size = V(wordnetwork)$size)
edges <- data.frame(from = as.character(ends(wordnetwork, E(wordnetwork))[,1]), 
                    to = as.character(ends(wordnetwork, E(wordnetwork))[,2]), 
                    width = sqrt(E(wordnetwork)$cooc))

network <- visNetwork(nodes, edges, width = "100%", height = "100vh") %>%
  visEdges(color = list(color = "lightblue", highlight = "lightblue")) %>%
  visNodes(color = list(background = "#006633", border = "#006633", highlight = "#006633")) %>%
  visOptions(highlightNearest = TRUE,
             nodesIdSelection = TRUE)

network
#saveNetwork(network, file = "grafo_interativo.html")
