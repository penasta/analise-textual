if (!require("pacman")) install.packages("pacman")
p_load(readr,stringr,tidyverse,lubridate,wordcloud,RColorBrewer,wordcloud2,tm)

df <- read_csv("C:/Users/toled/Documents/R_testes/logs whatsapp/Conversa do WhatsApp com DEMOCRACIA III.txt")

df <- as_tibble(df)
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
            "pode","bom","nome","quando","coisa") # adicionar mais conforme necessidade

rm(df2,docs,dtm,lista,matrix,filtro,i,vetor,words)
gc()

df <- df %>% filter(!(word %in% filtro)) %>% # Aplicando filtros
  filter(freq >9)


wordcloud(words = df$word, freq = df$freq, min.freq = 2,
          max.words=45, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

#write.table(filtro,"filtradas.txt",row.names = F,col.names = F)
