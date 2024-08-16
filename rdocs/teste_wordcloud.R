if (!require("pacman")) install.packages("pacman")
pacman::p_load(xml2,rvest,tidyverse,wordcloud,RColorBrewer,wordcloud2,tm)

link_site <- "C:\\Users\\toled\\Documents\\R_testes\\dados2\\messages.html"
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
mensagens

df = as_tibble(mensagens)
colnames(df) <- 'mensagem'

df$mensagens <- df$mensagem
df2 <- as_tibble(str_subset(df$mensagens,pattern="entrou usando o link de convite deste grupo",negate=T))
df2 <- as_tibble(str_subset(df2$value,pattern="Arquivo de mídia oculto",negate=T))
df2 <- as_tibble(str_subset(df2$value,pattern="https",negate=T))
df2 <- as_tibble(str_subset(df2$value,pattern="criou o grupo",negate=T))
df2 <- as_tibble(str_subset(df2$value,pattern="mudou o nome para",negate=T))
df2 <- as_tibble(str_subset(df2$value,pattern="Mensagem apagada",negate=T))
df2 <- as_tibble(str_subset(df2$value,pattern="Seu código de segurança com",negate=T))

lista <- list()
for (i in 1:nrow(df2)){
  lista[[i]] <- df2[i,1]
}
vetor <- unlist(lista)

# Nuvem de palavras:

docs <- Corpus(VectorSource(vetor))

docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))

dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 
df <- data.frame(word = names(words),freq=words)

filtro <- c('que','para','com','isso','tem','por','uma','pra','esse','mais',
            'dos','das','essa','tá','nas','nem','sem','aos','sobre','aí','pois',
            'este','esse','dos','ela','pra', 'está', 'mas',"vamos","vai",
            "estão","nossa","foi","nosso","nos","aqui","ainda","meu","ter",
            "porque","nossos","são","vão","você","sua","seu","acima","seja",
            "temos","será","sera","mesmo","pelo","só","so","então","entao",
            "umas","como","dele","fazendo","galera","alguém","alguem","assim",
            "número","numero","qualquer","ser","ficar","fazer","ja","já","nossas",
            "manter","estou","cara","nesse","olha","sair","passar","esses","estas",
            "dar","pela","esta","apenas","pro","souza","silva","chegar","dessa",
            "roberto","outra","sendo","estava","gilson","josé","durante","marcus",
            "marcos","desse","vou","tirar","era","teve","vocês","voces","ontem",
            "também","tbm","tambem","atraves","através","única","unica","costa",
            "consegue","as","às","ás","vem","vêm","null","não","ele","la","lá",
            "sim","boa","estamos","agora","hoje","dia","muito","quem","até",
            "pode","bom","nome","quando","coisa",
            'data','change','exporting','settings','included','download×','ephotonot','download',"pinned","may","photonot","“a","“não","february",
            "photonot","“a","“não") # adicionar mais conforme necessidade

rm(df2,docs,dtm,lista,matrix,i,vetor,words)
gc()

df <- df %>% filter(!(word %in% filtro)) %>% # Aplicando filtros
  filter(freq >5)

wordcloud(words = df$word, freq = df$freq, min.freq = 2,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))
