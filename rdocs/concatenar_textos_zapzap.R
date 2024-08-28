pacman::p_load(tidyverse,stringi,tm)

texto <- readLines("dados/data.txt") %>%
  sub(".*: ", "", .) %>%
  removeWords(stopwords("pt")) %>%
  stri_trans_general("Latin-ASCII") %>%
  tolower() %>%
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
  str_trim() %>%
  .[!grepl(paste(c("<midia oculta>", "mensagem apagada", "seu codigo de seguranca com .* mudou toque para saber mais", "k{2,}", "a{2,}"), collapse = "|"), .)] %>%
  gsub("\\b\\w\\b", "", .) %>%
  .[!grepl("seu codigo de seguranca", .)] %>%
  .[!grepl("atualizou duracao mensagens", .)] %>%
  .[!grepl("desativou mensagens", .)] %>%
  .[!grepl("<arquivo midia oculto>", .)] %>%
  .[. != ""] %>%
  removeWords(stopwords("pt")) %>%
  gsub("\\s+", " ", .) %>%
  trimws() %>%
  .[nchar(.) > 2] %>%
  .[!grepl("codigo seguranca mudou toque saber", ., ignore.case = TRUE)] %>%
  gsub("<mensagem editada>", "", .) %>%
  .[!grepl("entrou usando link convite deste grupo", ., ignore.case = TRUE)] %>%
  .[!grepl("codigo seguranca", ., ignore.case = TRUE)] %>%
  .[!grepl("entrou neste grupo atraves comunidade", ., ignore.case = TRUE)] %>%
  .[!grepl("mudou configuracoes grupo permitir", ., ignore.case = TRUE)] %>%
  .[!grepl("mudou configuracoes desse grupo permitir", ., ignore.case = TRUE)] %>%
  .[!grepl("mudou configuracoes permitir", ., ignore.case = TRUE)]

#texto
