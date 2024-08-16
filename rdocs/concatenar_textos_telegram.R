pacman::p_load(xml2,rvest,tidyverse,stringi)

page <- read_html("dados/messages14.html")

mensagens <- page %>% 
  html_nodes("div.text") %>%
  html_text()

texto <- page %>% 
  html_nodes("div.text") %>%
  html_text()

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

#cofre = texto
#cofre = append(cofre,texto)
#texto = cofre
#rm(mensagens,page,cofre)

