# Identificacao de pessoas dentro de cada banco

#' Padroniza os campos de identificacao
#'
#' @param dt \code{data.table} de entrada.
#' @param col_nome,col_mae,col_data,col_cns nomes das colunas de origem.
#' @param validar_cns aplicar a validacao modulo 11 ao CNS.
#' @return \code{data.table} com as colunas padronizadas \code{nome_n},
#'   \code{mae_n}, \code{dn}, \code{cns} e os tokens de nome.
#' @export
ln_preparar <- function(dt, col_nome = "nome_paciente", col_mae = "nome_mae",
                        col_data = "data_nasc", col_cns = "numero_sus",
                        validar_cns = TRUE) {
  dt <- copy(dt)
  dt[, nome_n := ln_normalizar_texto(get(col_nome))]
  dt[, mae_n  := ln_normalizar_texto(get(col_mae))]
  dt[, dn     := ln_parse_data(get(col_data))]
  dt[, cns    := ln_limpar_cns(get(col_cns), validar = validar_cns)]
  dt[, `:=`(nome_1 = ln_primeiro_nome(nome_n), nome_u = ln_ultimo_nome(nome_n),
            mae_1  = ln_primeiro_nome(mae_n),  mae_u  = ln_ultimo_nome(mae_n))]
  dt[]
}

#' Agrupa registros do mesmo individuo dentro de um banco
#'
#' Usa o CNS quando existe; na falta dele, a chave nome + mae + data de
#' nascimento, que so e formada quando os tres componentes estao presentes.
#' Registros sem nenhuma chave recebem identificador proprio, em vez de serem
#' fundidos entre si.
#'
#' @param dt saida de \code{ln_preparar()}.
#' @return \code{data.table} com a coluna \code{id_paciente}.
#' @export
# [FIX-5] A versão antiga montava
#   chave <- paste(nome, mae, data)
# sem tratar ausências. Com nome, mãe e data faltando, paste() devolve " NA"
# para TODOS esses registros, que passavam a ser tratados como a MESMA pessoa
# e eram eliminados na deduplicação. Além disso, o merge de propagação usava
# allow.cartesian = TRUE e podia multiplicar linhas.
# Agora: a chave só existe quando os três componentes existem; a propagação do
# CNS é feita por lookup 1:1 (primeiro CNS por chave), sem produto cartesiano.
ln_identificar_pessoas <- function(dt) {
  dt <- copy(dt)
  dt[, chave_forte := fifelse(!is.na(nome_n) & !is.na(mae_n) & !is.na(dn),
                              paste(nome_n, mae_n, dn), NA_character_)]
  lk <- dt[!is.na(cns) & !is.na(chave_forte), .(cns_prop = cns[1L]), by = chave_forte]
  dt <- lk[dt, on = "chave_forte"]
  dt[, id_final := fifelse(!is.na(cns), cns,
                    fifelse(!is.na(cns_prop), cns_prop, chave_forte))]
  # [PERF] so os registros sem nenhuma chave recebem identificador proprio
  sozinhos <- which(is.na(dt$id_final))
  if (length(sozinhos)) dt[sozinhos, id_final := paste0("SOZINHO_", sozinhos)]
  dt[, id_paciente := .GRP, by = id_final]
  dt[, c("cns_prop", "id_final") := NULL]
  dt[]
}

#' Reduz o banco a um registro por pessoa
#'
#' Para cada campo, aproveita o primeiro valor nao ausente entre os registros da
#' pessoa, de modo que replicas completem lacunas umas das outras.
#'
#' @param dt saida de \code{ln_identificar_pessoas()}.
#' @return \code{data.table} com uma linha por \code{id_paciente}.
#' @export
# [FIX-6] O linkage passa a ser feito pessoa x pessoa, e não linha x linha.
# Isso (a) evita que duplicatas e doses inflem artificialmente o número de
# pares, (b) permite que a resolução 1:1 faça sentido e (c) recupera campos
# vazios de um registro usando a réplica que estiver preenchida.
# [PERF] a versao anterior agrupava com uma funcao R por grupo
# (dt[, .(primeiro_ok(...)), by = id_paciente]), o que custa uma avaliacao do
# interpretador para cada uma dos ~3 milhoes de pessoas. Agora e um ordena +
# desduplica por coluna: 4 passadas vetorizadas.
ln_perfil_pessoa <- function(dt) {
  cols <- c("nome_n", "mae_n", "dn", "cns")
  perfil <- dt[, .(n_registros = .N, id_pessoa_verdadeiro = id_pessoa_verdadeiro[1L]),
               keyby = id_paciente]
  for (cl in cols) {
    dt[, .falta := is.na(get(cl))]
    setorder(dt, id_paciente, .falta)                 # nao-NA primeiro dentro da pessoa
    v <- dt[!duplicated(id_paciente), c("id_paciente", cl), with = FALSE]
    setkey(v, id_paciente)
    perfil[v, (cl) := get(paste0("i.", cl))]
  }
  dt[, .falta := NULL]
  perfil[, `:=`(nome_1 = ln_primeiro_nome(nome_n), nome_u = ln_ultimo_nome(nome_n),
                mae_1  = ln_primeiro_nome(mae_n),  mae_u  = ln_ultimo_nome(mae_n))]
  perfil[]
}
