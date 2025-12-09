library(rvest)

arquivos <- list.files(path = "dados/2001", pattern = "\\.html$", full.names = TRUE)

vetor <- c()

for (link_site in arquivos) {
  page <- read_html(link_site)
  
  mensagens <- page %>% 
    html_nodes("div.text") %>%
    html_text()
  
  vetor <- append(vetor, mensagens)
}

texto <- vetor

rm(page, link_site, vetor, mensagens,arquivos)

texto = texto %>%
  str_to_lower(.) %>%
  str_replace_all(., "\n", " ") %>%
  str_replace_all(., "\"", " ") %>%
  str_replace_all(., "https://\\S+", " ") %>%
  str_replace_all(., "http://\\S+", " ") %>%
  str_replace_all(., "t.me/", " ") %>%
  str_replace_all(., "🇧🇷", " ") %>%
  str_trim(.)

texto <- texto[texto != ""]
texto <- texto[texto != " "]
texto <- texto[texto != "  "]
texto <- texto[!str_detect(texto, "saved by @download_it_bot")]
texto <- texto[-1]

texto <- stri_trans_general(texto, "Latin-ASCII")

texto <- texto %>%
  str_replace_all("\\*", " ") %>%
  str_replace_all("[[:punct:]]", " ") %>%
  str_replace_all("\\b[0-9]+\\b", " ") %>%
  str_replace_all("\\s+", " ") %>%
  str_replace_all("\\+", " ") %>%
  gsub("[^\x01-\x7F]", "", .) %>%
  .[. != ""] %>%
  .[nzchar(trimws(.))] %>%
  str_trim()

head(texto)

df = as_tibble(texto)
colnames(df) <- 'mensagem'

df = unique(df)
