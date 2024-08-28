pacman::p_load(quanteda)

# termos a serem buscados
termos.vetor= c("democracia"
                )

texto %>% 
  # precisamos primeiro tokenizar
  tokens%>% 
  # rodando a função de palavras chave em contexto
  kwic(., 
       # termos a serem buscados. Pode ser um termo ou um vetor
       termos.vetor, 
       # quantas palavras devem ser mostradas ao redor
       3, 
       # Para pegar tanto palavras minúsculas como as em maiúsculo. 
       case_insensitive = TRUE)

rm(termos.vetor)
