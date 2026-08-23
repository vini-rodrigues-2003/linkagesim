# Comparacao e escore Fellegi-Sunter

#' Pesos de concordancia e discordancia de Fellegi-Sunter
#'
#' @param m probabilidade de concordancia dado que o par e verdadeiro.
#' @param u probabilidade de concordancia por acaso.
#' @return Vetor nomeado com \code{mais} (log2(m/u)) e \code{menos}
#'   (log2((1-m)/(1-u))).
#' @export
ln_pesos <- function(m, u) c(mais = log2(m / u), menos = log2((1 - m) / (1 - u)))

#' Parametros do escore Fellegi-Sunter
#'
#' @param m_nome,u_nome,m_mae,u_mae,m_data,u_data,m_cns,u_cns probabilidades m e
#'   u de cada campo.
#' @param corte_nome,corte_mae,corte_data,corte_cns similaridade a partir da
#'   qual a concordancia e considerada total; abaixo dela a contribuicao e
#'   interpolada linearmente ate o peso de discordancia.
#' @return Lista de parametros.
#' @export
ln_par_escore <- function(
    m_nome = 0.92, u_nome = 0.004,
    m_mae  = 0.88, u_mae  = 0.008,
    m_data = 0.93, u_data = 1 / 3650,
    m_cns  = 0.85, u_cns  = 1e-6,
    corte_nome = 0.85, corte_mae = 0.85, corte_data = 0.80, corte_cns = 0.60
) as.list(environment())

#' Compara os pares candidatos e calcula o escore
#'
#' Campos ausentes contribuem com zero: ausencia nao e discordancia. Um CNS
#' diferente e indicio contrario moderado, nao veto, o que corresponde a
#' realidade do SUS, onde a mesma pessoa costuma ter mais de um CNS.
#'
#' @param cand pares candidatos, de \code{ln_gerar_candidatos()}.
#' @param A,B tabelas de pessoas.
#' @param par parametros do escore, de \code{ln_par_escore()}.
#' @return \code{data.table} com as similaridades por campo, \code{escore} e
#'   \code{n_campos}.
#' @export
ln_comparar <- function(cand, A, B, par = ln_par_escore()) {
  setkey(A, id_paciente); setkey(B, id_paciente)
  x <- cbind(cand,
    A[cand$ida, .(nome_a = nome_n, mae_a = mae_n, dn_a = dn, cns_a = cns,
                  id_verd_a = id_pessoa_verdadeiro)],
    B[cand$idb, .(nome_b = nome_n, mae_b = mae_n, dn_b = dn, cns_b = cns,
                  id_verd_b = id_pessoa_verdadeiro)])

  x[, s_nome := fifelse(!is.na(nome_a) & !is.na(nome_b),
                        1 - stringdist(nome_a, nome_b, method = "jw", p = 0.1), NA_real_)]
  x[, s_mae  := fifelse(!is.na(mae_a) & !is.na(mae_b),
                        1 - stringdist(mae_a, mae_b, method = "jw", p = 0.1), NA_real_)]
  x[, s_data := {
      igual <- fifelse(!is.na(dn_a) & !is.na(dn_b), as.numeric(dn_a == dn_b), NA_real_)
      inv <- !is.na(igual) & igual == 0 &
             mday(dn_a) == month(dn_b) & month(dn_a) == mday(dn_b) & year(dn_a) == year(dn_b)
      fifelse(inv, 0.85, igual)                 # dia/mês trocados: concordância parcial
  }]
  x[, s_cns := fifelse(!is.na(cns_a) & !is.na(cns_b),
                fifelse(cns_a == cns_b, 1,
                 fifelse(stringdist(cns_a, cns_b, method = "dl") <= 1, 0.6, 0)), NA_real_)]

  contrib <- function(s, w, corte) {
    g <- pmin(pmax((s - corte) / (1 - corte), 0), 1)
    fifelse(is.na(s), 0, w["menos"] + g * (w["mais"] - w["menos"]))
  }
  w_nome <- ln_pesos(par$m_nome, par$u_nome); w_mae <- ln_pesos(par$m_mae, par$u_mae)
  w_data <- ln_pesos(par$m_data, par$u_data); w_cns <- ln_pesos(par$m_cns, par$u_cns)

  x[, escore := contrib(s_nome, w_nome, par$corte_nome) +
                contrib(s_mae,  w_mae,  par$corte_mae)  +
                contrib(s_data, w_data, par$corte_data) +
                contrib(s_cns,  w_cns,  par$corte_cns)]
  x[, n_campos := rowSums(cbind(!is.na(s_nome), !is.na(s_mae), !is.na(s_data), !is.na(s_cns)))]
  x[]
}

#' Decide os pares e resolve para 1:1
#'
#' @param cmp saida de \code{ln_comparar()}.
#' @param limiar ponto de corte do escore. Veja \code{ln_calibrar_limiar()}: o
#'   valor depende do tamanho dos bancos.
#' @param min_campos numero minimo de campos efetivamente comparados.
#' @return \code{data.table} com os pares aceitos.
#' @export
# [FIX-9] O script antigo aceitava TODOS os pares acima do limiar, inclusive
# vários candidatos para a mesma pessoa. Aqui cada pessoa do SINAN fica com o
# melhor par disponível na VACINA e vice-versa (guloso por escore decrescente).
ln_decidir <- function(cmp, limiar = 14, min_campos = 2L) {
  ok <- cmp[n_campos >= min_campos & escore >= limiar]
  setorder(ok, -escore)
  ok <- ok[!duplicated(ida)][!duplicated(idb)]
  ok[]
}
