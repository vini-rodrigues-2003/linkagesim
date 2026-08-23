# Padronizacao de nomes, datas, idade e CNS

#' Normaliza texto para comparacao
#'
#' Converte para minusculas, translitera acentos para ASCII, remove pontuacao e
#' colapsa espacos. Strings vazias viram \code{NA}.
#'
#' @param x vetor de caracteres.
#' @return Vetor de caracteres normalizado, com \code{NA} onde o resultado seria vazio.
#' @examples
#' ln_normalizar_texto(c("Jose Magalhaes", "MARIA  D'AVILA", ""))
#' @export
# [FIX-1] A versão anterior chamava iconv(x, "latin1", "UTF-8") antes da
# transliteração. Em uma string que já está em UTF-8 isso produz sequências
# inválidas, e o gsub seguinte APAGA a letra acentuada em vez de convertê-la:
# "magalhães" virava "magalhes" e não "magalhaes". Aqui a transliteração é
# feita direto pelo stringi, que já lida com o encoding declarado.
ln_normalizar_texto <- function(x) {
  x <- stri_trans_general(as.character(x), "Latin-ASCII")
  x <- stri_trans_tolower(x)
  x <- stri_replace_all_regex(x, "[^a-z0-9 ]", " ")
  x <- stri_trim_both(stri_replace_all_regex(x, "\\s+", " "))
  x[x == ""] <- NA_character_
  x
}

#' Converte datas em multiplos formatos para IDate
#'
#' Identifica o layout da data por expressao regular ancorada antes de
#' converter, evitando a ambiguidade de \code{as.Date()}, que aceita
#' \code{"15-03-2010"} sob o formato \code{"\%Y-\%m-\%d"} e devolve o ano 15.
#'
#' Formatos reconhecidos: dd/mm/aaaa, dd-mm-aaaa, dd.mm.aaaa, aaaa-mm-dd,
#' aaaa/mm/dd, ddmmaaaa e aaaammdd. Sufixo de hora e descartado.
#'
#' @param x vetor de caracteres com as datas.
#' @param min_ano,max_ano janela de plausibilidade; fora dela o valor vira \code{NA}.
#' @return Vetor \code{IDate}.
#' @examples
#' ln_parse_data(c("15-03-2010", "2010-03-15", "15/03/2010", "15032010"))
#' @export
# [FIX-2] ESTE É O BUG PRINCIPAL DO SCRIPT ORIGINAL.
# A função antiga percorria uma lista de formatos com as.Date(x, format = f)
# e ficava com o primeiro que não retornasse NA. Acontece que as.Date/strptime
# são tolerantes: "%Y-%m-%d" aplicado a "15-03-2010" NÃO falha — ele lê
# ano = 15, mês = 03, dia = 20 e devolve 0015-03-20 silenciosamente.
# Como "%Y-%m-%d" vinha ANTES de "%d-%m-%Y" na lista, todas as datas gravadas
# como dd-mm-aaaa (15% dos registros gerados) eram lidas com o ano errado.
#
# A versão nova identifica o layout por expressão regular ancorada, monta a
# string ISO e só então converte, sem ambiguidade. Datas fora da janela
# plausível viram NA em vez de entrarem no linkage com ano absurdo.
ln_parse_data <- function(x, min_ano = 1900L,
                          max_ano = as.integer(format(Sys.Date(), "%Y"))) {
  x <- trimws(as.character(x))
  x <- sub("[T ][0-9:.]+$", "", x)              # descarta hora, se houver
  x[x %in% c("", "NA", "NULL", "null", "-", "//", "00/00/0000", "0000-00-00")] <- NA_character_

  iso <- rep(NA_character_, length(x))
  pega <- function(rx) !is.na(x) & is.na(iso) & grepl(rx, x)

  i <- pega("^[0-9]{2}/[0-9]{2}/[0-9]{4}$")
  iso[i] <- paste0(substr(x[i], 7, 10), "-", substr(x[i], 4, 5), "-", substr(x[i], 1, 2))
  i <- pega("^[0-9]{2}-[0-9]{2}-[0-9]{4}$")
  iso[i] <- paste0(substr(x[i], 7, 10), "-", substr(x[i], 4, 5), "-", substr(x[i], 1, 2))
  i <- pega("^[0-9]{2}\\.[0-9]{2}\\.[0-9]{4}$")
  iso[i] <- paste0(substr(x[i], 7, 10), "-", substr(x[i], 4, 5), "-", substr(x[i], 1, 2))
  i <- pega("^[0-9]{4}-[0-9]{2}-[0-9]{2}$")
  iso[i] <- x[i]
  i <- pega("^[0-9]{4}/[0-9]{2}/[0-9]{2}$")
  iso[i] <- chartr("/", "-", x[i])

  # 8 dígitos: ddmmaaaa ou aaaammdd — desempate por plausibilidade
  i <- pega("^[0-9]{8}$")
  if (any(i)) {
    v  <- x[i]
    a1 <- as.integer(substr(v, 1, 4)); m1 <- as.integer(substr(v, 5, 6)); d1 <- as.integer(substr(v, 7, 8))
    d2 <- as.integer(substr(v, 1, 2)); m2 <- as.integer(substr(v, 3, 4)); a2 <- as.integer(substr(v, 5, 8))
    ok_iso <- a1 >= min_ano & a1 <= max_ano & m1 %in% 1:12 & d1 %in% 1:31
    ok_dmy <- a2 >= min_ano & a2 <= max_ano & m2 %in% 1:12 & d2 %in% 1:31
    iso[i] <- fifelse(ok_dmy & !ok_iso,
                      paste0(substr(v, 5, 8), "-", substr(v, 3, 4), "-", substr(v, 1, 2)),
               fifelse(ok_iso,
                      paste0(substr(v, 1, 4), "-", substr(v, 5, 6), "-", substr(v, 7, 8)),
                      NA_character_))
  }

  out <- as.IDate(iso, format = "%Y-%m-%d")
  a <- year(out)
  out[!is.na(out) & (a < min_ano | a > max_ano)] <- NA
  out
}

#' Idade em anos a partir de campo codificado no padrao DATASUS
#'
#' Em varios extratos do OpenDataSUS a idade vem codificada, com o primeiro
#' digito indicando a unidade (1 hora, 2 dia, 3 mes, 4 ano). A funcao detecta a
#' codificacao e converte para anos completos.
#'
#' @param x vetor de idades, cru.
#' @param codificado \code{TRUE}, \code{FALSE} ou \code{NA} para detectar sozinho.
#' @return Vetor inteiro de idades em anos completos.
#' @examples
#' ln_idade_anos(c(4008, 4012, 3011))   # codificado
#' ln_idade_anos(c(8, 12, 45))          # ja em anos
#' @export
# [FIX-3] O script original filtrava `nu_idade_paciente >= 8 & <= 16` sem
# verificar a codificação. Em vários extratos do OpenDataSUS o campo é
# codificado (1º dígito = unidade: 1 hora, 2 dia, 3 mês, 4 ano). Se for esse
# o caso, o filtro antigo devolve praticamente zero registro de 8 a 16 anos.
# Esta função detecta e converte; use `ln_idade_anos(x, codificado = NA)`
# para deixar a detecção automática.
ln_idade_anos <- function(x, codificado = NA) {
  x <- suppressWarnings(as.integer(x))
  if (is.na(codificado)) {
    codificado <- mean(x >= 4000 & x <= 4130, na.rm = TRUE) > 0.5
  }
  if (!codificado) return(x)
  unidade <- x %/% 1000L
  valor   <- x %%  1000L
  fifelse(unidade == 4L, valor,
   fifelse(unidade %in% c(1L, 2L, 3L), 0L, NA_integer_))
}

#' Limpa e valida o Cartao Nacional de Saude
#'
#' Mantem apenas digitos, exige 15 caracteres e, opcionalmente, aplica a
#' validacao modulo 11.
#'
#' @param x vetor de caracteres ou numerico com os CNS.
#' @param validar aplicar a validacao modulo 11.
#' @return Vetor de caracteres com o CNS limpo, ou \code{NA} quando invalido.
#' @export
ln_limpar_cns <- function(x, validar = TRUE) {
  y <- gsub("[^0-9]", "", as.character(x))
  y[nchar(y) != 15L] <- NA_character_
  if (validar) {
    ok <- which(!is.na(y))
    if (length(ok)) {
      z <- y[ok]
      soma <- integer(length(z))
      # [PERF] soma posicao a posicao: 15 vetores inteiros em vez de um
      # strsplit/unlist de 15 x N elementos (que estourava a memoria em 3 milhoes)
      for (i in 1:15) soma <- soma + as.integer(substr(z, i, i)) * (16L - i)
      y[ok[soma %% 11L != 0L]] <- NA_character_
    }
  }
  y
}

#' Primeiro e ultimo token de um nome
#'
#' @param x vetor de nomes ja normalizados por \code{ln_normalizar_texto()}.
#' @return Vetor de caracteres com o primeiro (\code{ln_primeiro_nome}) ou o
#'   ultimo (\code{ln_ultimo_nome}) token.
#' @rdname tokens_nome
#' @export
ln_primeiro_nome <- function(x) sub(" .*$", "", x)

#' @rdname tokens_nome
#' @export
ln_ultimo_nome   <- function(x) sub("^.* ", "", x)
