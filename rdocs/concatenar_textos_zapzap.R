pacman::p_load(tidyverse,stringi,tm)

texto <- readLines("dados/est.txt")
texto <- sub(".*: ", "", texto)
texto <- tm::removeWords(texto, stopwords("pt"))
texto <- stri_trans_general(texto, "Latin-ASCII")

texto <- texto %>%
  str_to_lower() %>%
  str_replace_all("\n", " ") %>%
  str_replace_all("\"", " ") %>%
  str_replace_all("https://\\S+", " ") %>%
  str_replace_all("http://\\S+", " ") %>%
  str_replace_all("t.me/", " ") %>%
  str_replace_all("🇧🇷", " ") %>%
  str_replace_all("\\*", " ") %>%
  str_replace_all("[[:punct:]]", " ") %>%
  str_replace_all("\\b[0-9]+\\b", " ") %>%
  str_replace_all("\\s+", " ") %>%
  str_replace_all("\\+", " ") %>%
  gsub("[^\x01-\x7F]", "", .) %>%
  .[. != ""] %>%
  .[nzchar(trimws(.))] %>%
  str_trim()

padroes <- c("<midia oculta>",
             "mensagem apagada",
             "seu codigo de seguranca com .* mudou toque para saber mais",
             "k{2,}",
             "a{2,}")
texto <- texto[!grepl(paste(padroes, collapse = "|"), texto)]
texto <- gsub("\\b\\w\\b", "", texto)
texto <- texto[!grepl("seu codigo de seguranca", texto)]
texto <- texto[texto != ""]
texto <- tm::removeWords(texto, stopwords("pt"))
texto <- gsub("\\s+", " ", texto)
texto <- trimws(texto)
texto <- texto[nchar(texto) > 2]
texto <- texto[!grepl("codigo seguranca mudou toque saber", texto, ignore.case = TRUE)]
texto <- gsub("<mensagem editada>", "", texto)
texto <- texto[!grepl("entrou usando link convite deste grupo", texto, ignore.case = TRUE)]
texto <- texto[!grepl("codigo seguranca", texto, ignore.case = TRUE)]
texto <- texto[!grepl("entrou neste grupo atraves comunidade", texto, ignore.case = TRUE)]
texto <- texto[!grepl("mudou configuracoes grupo permitir", texto, ignore.case = TRUE)]
texto <- texto[!grepl("mudou configuracoes desse grupo permitir", texto, ignore.case = TRUE)]
texto <- texto[!grepl("mudou configuracoes permitir", texto, ignore.case = TRUE)]

texto
rm(padroes)
