pacman::p_load(tidyverse,stringi,tm)

# Filtrando termos insignificantes ----
filtro = c("silva","souza","gilson","roberto","joao","lucia","reginaldo",
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
           "feira","vai","atraves","alguma","coisa","av","paulista",
           "zello","pra","todo","cada","atender","vossos","peitos",
           "bracos","hora","duracao","neste","momento","estrategicos",
           "pontos","mi","nome","amigo","nobre","redes","sociais",
           "fotos","videos","coisa","alguma","homens","mulheres",
           "km","vc","ai","cima","dar","todas","ola","assim","algum",
           "carros","som","outros","pras","agosto","tarde","dias",
           "desse","deixe","faz","tudo","santo","confira","oficial",
           "instagram","usam","noite","boa","dia","todos","link",
           "vamos","aqui","estaremos","grupos","sobre","agora","links",
           "geral","pessoal","fazer","pix","telegram","contamos",
           "pode","dinheiro","fazendo","regionais","pedimos","ir",
           "whatsapp","live","porque",'cristo','alguem','chat',
           'estados','municipios','estaduais','somente','atencao',
           'abram','casa','horas','canal','favor','add','aplicativo',
           'busque','enquanto','definido','referente','quartas',
           'confira','data','continue','precisa','ainda',
           'ninguem','usam','regiao','quer','horario','oficial',
           'ira','toda')

texto <- texto %>%
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

substituir_palavras <- function(texto, filtro) {
  for (palavra in filtro) {
    texto <- gsub(paste0("\\b", palavra, "\\b"), "", texto)
  }
  return(texto)
}

texto <- substituir_palavras(texto, filtro)

texto <- texto %>%
  sub(".*: ", "", .) %>%
  removeWords(stopwords("pt")) %>%
  str_replace_all("\n", " ") %>%
  str_replace_all("\"", " ") %>%
  str_replace_all("\\*", " ") %>%
  str_replace_all("[[:punct:]]", " ") %>%
  str_replace_all("\\b[0-9]+\\b", " ") %>%
  str_replace_all("\\s+", " ") %>%
  str_replace_all("\\+", " ") %>%
  gsub("[^\x01-\x7F]", "", .) %>%
  .[. != ""] %>%
  .[nzchar(trimws(.))] %>%
  str_trim() %>%
  .[. != ""] %>%
  removeWords(stopwords("pt")) %>%
  gsub("\\s+", " ", .) %>%
  trimws() %>%
  .[nchar(.) > 2]

#texto
