if (!require("pacman")) install.packages("pacman")
pacman::p_load(xml2,rvest,tidyverse,wordcloud,RColorBrewer,wordcloud2,tm,purrr,spacyr,igraph,tidygraph,ggraph)

link_site <- "C:\\Users\\toled\\Documents\\R_testes\\dados2\\messages_2.html"
page <- read_html(link_site)

mensagens <- page %>% 
  html_nodes("div.text") %>%
  html_text()

mensagens = mensagens %>% str_to_lower(.) %>%
  str_replace_all(., "\n", " ") %>%
  str_replace_all(., "\"", " ") %>%
  str_replace_all(., "https://\\S+", " ") %>%
  str_replace_all(., "http://\\S+", " ") %>%
  str_replace_all(., "t.me/", " ") %>%
  str_replace_all(., "🇧🇷", " ") %>%
  str_trim(.)

mensagens <- mensagens[mensagens != ""]
mensagens <- mensagens[mensagens != " "]
mensagens <- mensagens[mensagens != "  "]
mensagens <- mensagens[!str_detect(mensagens, "saved by @download_it_bot")]
mensagens <- mensagens[-1]
#mensagens
# padroes <- c(
#   "\n", 
#   "eu quero bolsonaro elegível 🇧🇷🇧🇷", 
#   "Photo                    Not included, change data exporting settings to download.", 
#   "https://\\S+", 
#   "\\d{2}:\\d{2}", 
#   "E ", 
#   "next messages", 
#   "\\d{2}\\.\\d{1} KB", 
#   "\\d{3}x\\d{3}", 
#   "pic.twitter.com\\S+", 
#   "fileNot included", 
#   "http://\\S+", 
#   "\\d{2} \\w{3,9} \\d{4}", 
#   "(Veja o Vídeo)", 
#   "Next messages", 
#   " Channel «Bolsonaro Presidente» createdChannel photo changed E", 
#   "this message", 
#   "jair m. bolsonaro (@jairmessiasbolsonaro)", 
#   "E\\d{3}x\\d{3}", 
#   "E\\d{4}x\\d{3}", 
#   "E\\d{3}x\\d{4}", 
#   "E\\d{4}x\\d{4}", 
#   "\\d{1}\\.\\d{1} MB", 
#   "\\d{1}\\.\\d{1} MB", 
#   "\\d{1}\\.\\d{1} kb", 
#   "\\d{2}\\.\\d{1} kb", 
#   "\\d{3}\\.\\d{1} kb", 
#   "january", 
#   "february", 
#   "march", 
#   "april", 
#   "may", 
#   "june", 
#   "july", 
#   "august", 
#   "september", 
#   "october", 
#   "november", 
#   "december", 
#   "photonot", 
#   "change", 
#   "data", 
#   "exporting", 
#   "settings", 
#   "download", 
#   "2019", 
#   "download.", 
#   "photo", 
#   "not", 
#   "included,", 
#   ".\\d{3}x\\d{3},", 
#   ",\\d{3}x\\d{3}.", 
#   ",\\d{3}x\\d{3},", 
#   ".\\d{3}x\\d{3}.", 
#   ".\\d{4}x\\d{4},", 
#   ",\\d{4}x\\d{4}.", 
#   ",\\d{4}x\\d{4},", 
#   ".\\d{4}x\\d{4}.", 
#   ".\\d{2}.\\d{2}.", 
#   ":\\d{2}", 
#   "2020", 
#   " to ", 
#   "previous", 
#   "(veja o vídeo)", 
#   "“", 
#   "‘", 
#   "'", 
#   "’", 
#   "”", 
#   "\"", 
#   "/", 
#   "jair m. bolsonaro (@jairmessiasbolsonaro)", 
#   "\\d{2} x \\d{2}", 
#   "\\d{2}", 
#   "\\d{3}", 
#   "\\d{4}"
# )
# 
# for (padrao in padroes) {
#   df$Mensagem <- str_replace_all(df$Mensagem, padrao, " ")
# }

df = as_tibble(mensagens)
colnames(df) <- 'Mensagem'

df <- df %>% mutate(Mensagem = str_trim(Mensagem))
#df <- df %>% filter(nchar(Mensagem) >= 8)
df <- df %>% filter(Mensagem != "")


# criar matriz de adjacencias
get_adjacent_list <- function(edge_list) {
  gtools::combinations(length(edge_list), 2, edge_list)  
}

# # Executar apenas 1 vez
# spacyr::spacy_install()
# spacy_download_langmodel("pt_core_news_sm")

spacy_initialize(model="pt_core_news_sm")
data = df$Mensagem

#data = append(data,df$Mensagem)

entities <- spacy_extract_entity(data)
#entities

# Filtrar apenas entidades cujo tipo são pessoas ou organizações:
filtered_entities <- entities %>%
  filter(ent_type=='ORG'| ent_type=='PER')
#filtered_entities = entities

edges <- filtered_entities %>%
  group_by(doc_id) %>%
  summarise(entities = paste(text, collapse = ",")) %>% 
  pull(entities) %>% 
  str_split(",") %>% 
  map(~unique(unlist(.x))) %>% 
  .[map_dbl(., length) != 1]

adjacent_matrix <- map_dfr(edges, ~ as.data.frame(get_adjacent_list(.x))) %>% 
  as_tibble() %>% 
  set_names(c('item1', 'item2'))

# Padronizar entidades
adjacent_matrix <- adjacent_matrix %>% 
  mutate_all(~.x %>% 
               str_replace_all("— jair m. bolsonaro", "Jair Bolsonaro") %>% 
               str_replace_all("@jairmessiasbolsonaro", "Jair Bolsonaro") %>% 
               str_replace_all("jair bolsonaro1", "Jair Bolsonaro") %>% 
               str_replace_all("jair bolsonaroe", "Jair Bolsonaro") %>% 
               str_replace_all("maria do rosáriochina", "Maria do Rosário") %>%
               str_replace_all("pteonyx lorenzoni", "Onyx Lorenzoni") %>%
               str_replace_all("tenie marcelo freixo", "Marcelo Freixo") %>%
               str_replace_all("abraham weintraub denuncia", "Abraham Weintraub") %>%
               str_replace_all("alessandro empareda davi", "Senador Delegado Alessandro") %>%
               str_replace_all("carlos mirandateni", "Carlos Miranda") %>%
               str_replace_all("deltane bolsonaro", "Deltan e Bolsonaro") %>%
               str_replace_all("debatee pf", "Debate e PF") %>%
               str_replace_all("desfile9", "Desfile") %>%
               str_replace_all("macrone bolsonaro arrebenta", "Macron e Bolsonaro arrebenta") %>%
               str_replace_all("mais!8", "Mais") %>%
               str_replace_all("maracanã9", "Maracanã") %>%
               str_replace_all("moraes", "alexandre de moraes") %>%
               str_replace_all("zavascki", "teori zavaski") %>%
               str_replace_all("zavascki", "teori zavaski") %>%
               str_replace_all("teori", "teori zavaski") %>%
               str_replace_all("zavascki", "teori zavaski") %>%
               str_replace_all("supremo tribunal federal", "stf") %>%
               str_replace_all("elon elon", "elon musk") %>%
               str_replace_all("“teori zavaski zavaski", "teori zavaski") %>%
               str_replace_all("musk", "elon musk") %>%
               str_replace_all("musk", "elon") %>%
               str_replace_all("@elonelon elon for exposing", "elon") %>%
               str_replace_all("alexandre de alexandre de moraes", "alexandre de moraes") %>%
               str_replace_all("marcel van", "Marcel van Hatten")
             )




# remover residuos
# {
entities_to_drop <- c("Assine", "Google Podcasts", "Spotify", "Focus do",
                      "Focus", "Segundo", "Ninguém", "Haverá", "G1",
                      "Começa", "LEIA", "R$", "Considera", "Caixa Aqui")

weighted_edgelist <- adjacent_matrix %>%
#     filter_at(1:2, ~ !.x %in% entities_to_drop) %>% 
     group_by(item1, item2) %>%
     summarise(n=n()) %>% 
     ungroup()#  %>% filter(n>1) 

# Instanciar objeto das setas
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

# Definir pesos conforme numero de ocorrencias
subt <- weighted_edgelist

# Instanciar objeto dos vertices
vert <- subt %>% 
  tidyr::gather(item, word, item1, item2) %>%
  group_by(word) %>% 
  summarise(n = sum(n))

# Obter componentes para colorir os clusters do grafo
tidy_graph_components <- subt  %>%
  select(item1, item2) %>% 
  as.matrix() %>%
  graph.edgelist(directed = FALSE)  %>%
  as_tbl_graph() %>% 
  activate("edges") %>% 
  # definir pesos como numero de ocorrencias
  mutate(weight = subt$n) %>% 
  activate("nodes") %>% 
  # obter clusters:
  mutate(component = as.factor(tidygraph::group_edge_betweenness()))
# outros tipos de agrupamentos:
# tidygraph.data-imaginist.com/reference/group_graph.html 

# Atualizar vertice para incluir grupos
vert <- vert %>% 
  left_join( as.data.frame(activate(tidy_graph_components, "nodes")) %>% 
               rename(word = name))

set.seed(42)
subt %>%
  graph_from_data_frame(vertices = vert) %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = log(n), edge_width = log(n)), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches'), color = "yellow") +
  geom_node_point() + 
  geom_node_text(aes(label = name, size = n, alpha = n, color = component),# color = "#EAFF00",
                 repel = TRUE, point.padding = unit(0.2, "lines"),
                 show.legend = F) +
  scale_size(range = c(2,10)) +
  scale_alpha(range = c(0.5,1))+ 
  theme_minimal() + 
  theme(
    panel.background = element_rect(fill = "#2D2D2D"),
    legend.key = element_rect(fill = "#2D2D2D")
  ) +
  theme_graph(background = "black")
