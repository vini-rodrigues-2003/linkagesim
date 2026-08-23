# Geracao dos bancos sinteticos com ruido controlado

#' Parametros de ruido da simulacao
#'
#' Reune num unico objeto todas as taxas de inconsistencia injetadas nos bancos
#' sinteticos, para que possam ser variadas de forma explicita.
#'
#' @param p_nome_vazio,p_nome_abrev,p_nome_typo taxas para o nome do paciente.
#' @param p_mae_vazio,p_mae_abrev,p_mae_typo taxas para o nome da mae.
#' @param p_data_vazia proporcao de datas de nascimento em branco.
#' @param p_cns_trocado,p_cns_vazio,p_cns_invalido taxas para o CNS.
#' @param p_duplicata proporcao de registros duplicados dentro do banco.
#' @param formatos_data vetor nomeado com os formatos de data e suas proporcoes.
#' @return Lista de parametros.
#' @examples
#' par <- ln_par_ruido(p_mae_vazio = 0.20)
#' @export
ln_par_ruido <- function(
    p_nome_vazio        = 0.03,
    p_nome_abrev        = 0.07,
    p_nome_typo         = 0.05,
    p_mae_vazio         = 0.08,
    p_mae_abrev         = 0.17,
    p_mae_typo          = 0.05,
    p_data_vazia        = 0.05,
    p_cns_trocado       = 0.08,
    p_cns_vazio         = 0.05,
    p_cns_invalido      = 0.03,
    p_duplicata         = 0.05,
    formatos_data       = c("%d/%m/%Y" = 0.30, "%Y-%m-%d" = 0.25,
                            "%d-%m-%Y" = 0.15, "%d%m%Y"   = 0.10,
                            "%Y/%m/%d" = 0.20)
) as.list(environment())

#' Injeta erros de digitacao
#'
#' Aplica troca, transposicao ou supressao de um caractere, sem tocar em
#' espacos, para nao fundir tokens do nome.
#'
#' @param x vetor de caracteres.
#' @param p probabilidade de um registro receber um erro.
#' @return Vetor de caracteres com os erros aplicados.
#' @export
# [PERF] versao vetorizada: a anterior percorria os registros alterados num
# laco for com strsplit/sample por elemento.
ln_typo <- function(x, p) {
  n <- length(x)
  alvo <- which(!is.na(x) & nchar(x) > 4L & runif(n) < p)
  if (!length(alvo)) return(x)
  s <- x[alvo]; L <- nchar(s)
  k <- pmax(2L, ceiling(runif(length(s)) * (L - 1L)))
  # nao mexer em espacos, para nao grudar dois tokens do nome
  for (tent in 1:10) {
    ruim <- substr(s, k, k) == " " | substr(s, k + 1L, k + 1L) == " "
    if (!any(ruim)) break
    k[ruim] <- pmax(2L, ceiling(runif(sum(ruim)) * (L[ruim] - 1L)))
  }
  manter <- !(substr(s, k, k) == " " | substr(s, k + 1L, k + 1L) == " ")
  s <- s[manter]; k <- k[manter]; L <- L[manter]; alvo <- alvo[manter]
  if (!length(s)) return(x)
  op  <- sample(1:3, length(s), TRUE)
  pre <- substr(s, 1L, k - 1L); ch <- substr(s, k, k)
  pos <- substr(s, k + 1L, L)
  nxt <- substr(s, k + 1L, k + 1L); resto <- substr(s, k + 2L, L)
  novo <- character(length(s))
  i1 <- op == 1L; novo[i1] <- paste0(pre[i1], sample(letters, sum(i1), TRUE), pos[i1])  # troca
  i2 <- op == 2L; novo[i2] <- paste0(pre[i2], nxt[i2], ch[i2], resto[i2])               # transposicao
  i3 <- op == 3L; novo[i3] <- paste0(pre[i3], pos[i3])                                  # supressao
  x[alvo] <- novo
  x
}

#' Gera CNS provisorios validos pelo modulo 11
#'
#' @param n quantidade de CNS a gerar.
#' @return Vetor de caracteres com 15 digitos.
#' @export
ln_gerar_cns_valido <- function(n) {
  d <- matrix(sample(0:9, n * 14L, TRUE), nrow = n)
  d[, 1] <- sample(7:9, n, TRUE)
  s14 <- as.vector(d %*% 15:2)
  ult <- (11L - s14 %% 11L) %% 11L
  ruim <- which(ult == 10L)
  while (length(ruim)) {                       # digito verificador 10 nao existe
    d[ruim, 14] <- sample(0:9, length(ruim), TRUE)
    s14[ruim] <- as.vector(d[ruim, , drop = FALSE] %*% 15:2)
    ult[ruim] <- (11L - s14[ruim] %% 11L) %% 11L
    ruim <- ruim[ult[ruim] == 10L]
  }
  # [PERF] paste0 por colunas em vez de apply(d, 1, paste)
  do.call(paste0, c(split(d, col(d)), list(ult)))
}

#' Aplica ruido a um vetor de nomes
#'
#' Combina tres inconsistencias: ausencia, abreviacao do segundo token e erro de
#' digitacao.
#'
#' @param nomes vetor de nomes.
#' @param p_vazio,p_abrev,p_typo taxas de cada inconsistencia.
#' @param ponto abreviar com ponto final (\code{"S."}) ou sem (\code{"S"}).
#' @return Vetor de nomes com ruido.
#' @export
# [PERF] abreviacao do 2o token feita por regex vetorizada, sem strsplit/vapply
ln_aplicar_ruido_nome <- function(nomes, p_vazio, p_abrev, p_typo, ponto = TRUE) {
  n <- length(nomes); u <- runif(n); out <- nomes
  i <- which(u >= p_vazio & u < p_vazio + p_abrev)
  if (length(i))
    out[i] <- sub("^(\\S+) (\\S)\\S* (.*)$",
                  if (ponto) "\\1 \\2. \\3" else "\\1 \\2 \\3", out[i])
  out <- ln_typo(out, p_typo)
  out[u < p_vazio] <- NA_character_
  out
}

#' Formata datas em layouts variados
#'
#' @param datas vetor \code{Date}.
#' @param p_vazia proporcao de datas devolvidas em branco.
#' @param formatos vetor nomeado com os formatos e suas proporcoes.
#' @return Vetor de caracteres.
#' @export
# [PERF] ESTE ERA O GARGALO DE 9 HORAS.
# A versao anterior fazia:
#     out[livre] <- vapply(seq_along(esc), function(k) format(datas[livre][k], esc[k]), ...)
# `datas[livre]` era recalculado DENTRO de cada iteracao: um subset de 3,2
# milhoes de elementos repetido 3 milhoes de vezes -> custo O(n^2).
# Medido: n = 40.000 levava 5,3 s; extrapolado para 3.240.380, ~9,7 horas.
# Agora sao 5 chamadas vetorizadas de format(), uma por formato: 2,4 s.
ln_formatar_data <- function(datas, p_vazia, formatos) {
  n <- length(datas); u <- runif(n); out <- rep(NA_character_, n)
  livre <- which(u >= p_vazia)
  fs <- names(formatos)
  esc <- sample(fs, length(livre), TRUE, prob = formatos / sum(formatos))
  for (f in fs) {
    i <- livre[esc == f]
    if (length(i)) out[i] <- format(datas[i], f)
  }
  out
}

#' Gera a base de pessoas presentes nos dois bancos
#'
#' Esta e a "verdade" da simulacao: cada pessoa recebe um identificador
#' \code{COMUM_*} que so e usado na avaliacao, nunca no linkage.
#'
#' @param n numero de pessoas.
#' @param dicionario lista com os vetores \code{masc}, \code{fem} e \code{sobrenomes}.
#' @param ano_min,ano_max janela de anos de nascimento.
#' @return \code{data.table} com os identificadores de referencia.
#' @export
ln_gerar_base_comum <- function(n, dicionario, ano_min = 2008, ano_max = 2017) {
  data.table(
    id_pessoa_verdadeiro = sprintf("COMUM_%08d", seq_len(n)),
    nome_paciente_ref = with(dicionario, paste(sample(c(masc, fem), n, TRUE),
                                               sample(sobrenomes, n, TRUE),
                                               sample(sobrenomes, n, TRUE))),
    nome_mae_ref      = with(dicionario, paste(sample(fem, n, TRUE),
                                               sample(sobrenomes, n, TRUE),
                                               sample(sobrenomes, n, TRUE))),
    data_nasc_ref     = as.Date(sample(as.integer(as.Date(paste0(ano_min, "-01-01"))):
                                       as.integer(as.Date(paste0(ano_max, "-12-31"))), n, TRUE),
                                origin = "1970-01-01"),
    cns_ref           = ln_gerar_cns_valido(n)
  )
}

#' Monta um banco sintetico
#'
#' Junta as pessoas em comum (com ruido proprio deste banco), as pessoas
#' exclusivas e as duplicatas internas.
#'
#' @param n_total numero de registros do banco.
#' @param base_comum saida de \code{ln_gerar_base_comum()}.
#' @param prefixo prefixo dos identificadores das pessoas exclusivas.
#' @param dicionario lista de nomes.
#' @param par parametros de ruido, de \code{ln_par_ruido()}.
#' @param semente semente aleatoria.
#' @return \code{data.table} com \code{nome_paciente}, \code{nome_mae},
#'   \code{data_nasc}, \code{numero_sus} e \code{id_pessoa_verdadeiro}.
#' @export
ln_criar_banco <- function(n_total, base_comum, prefixo, dicionario,
                           par = ln_par_ruido(), semente = 1L) {
  set.seed(semente)
  ruido_bloco <- function(dt) {
    dt[, nome_paciente := ln_aplicar_ruido_nome(nome_paciente_ref, par$p_nome_vazio,
                                                par$p_nome_abrev, par$p_nome_typo, TRUE)]
    dt[, nome_mae      := ln_aplicar_ruido_nome(nome_mae_ref, par$p_mae_vazio,
                                                par$p_mae_abrev, par$p_mae_typo, FALSE)]
    dt[, data_nasc     := ln_formatar_data(data_nasc_ref, par$p_data_vazia, par$formatos_data)]
    dt[, numero_sus    := cns_ref]
    # CNS trocado: a pessoa aparece com OUTRO CNS (situação corriqueira no SUS),
    # em branco, ou truncado/inválido
    tr <- which(runif(nrow(dt)) < par$p_cns_trocado)
    if (length(tr)) {
      novo <- ln_gerar_cns_valido(length(tr))
      v <- runif(length(tr))
      novo[v < par$p_cns_vazio / par$p_cns_trocado] <- NA_character_
      inv <- which(v >= par$p_cns_vazio / par$p_cns_trocado &
                   v <  (par$p_cns_vazio + par$p_cns_invalido) / par$p_cns_trocado)
      if (length(inv)) novo[inv] <- substr(novo[inv], 1, sample(10:14, length(inv), TRUE))
      dt[tr, numero_sus := novo]
    }
    dt[, .(id_pessoa_verdadeiro, nome_paciente, nome_mae, data_nasc, numero_sus)]
  }

  comum <- ruido_bloco(copy(base_comum))

  n_excl <- n_total - nrow(base_comum)
  if (n_excl < 0) stop("n_total menor que a base comum.")
  excl_ref <- ln_gerar_base_comum(n_excl, dicionario)
  excl_ref[, id_pessoa_verdadeiro := sprintf(paste0(prefixo, "_%08d"), seq_len(n_excl))]
  excl <- ruido_bloco(excl_ref)

  banco <- rbindlist(list(comum, excl))
  n_dup <- as.integer(n_total * par$p_duplicata)
  if (n_dup > 0) banco <- rbindlist(list(banco, banco[sample(.N, n_dup)]))
  banco[sample(.N)]
}
