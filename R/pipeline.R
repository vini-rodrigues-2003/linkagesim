# Execucao de ponta a ponta

#' Executa o linkage de ponta a ponta
#'
#' Padroniza, identifica pessoas dentro de cada banco, gera candidatos por
#' blocking multi-passo, pontua e decide.
#'
#' @param banco_a,banco_b \code{data.table} com as colunas de identificacao.
#' @param limiar ponto de corte do escore.
#' @param verbose imprimir o progresso.
#' @param passos passos de blocking.
#' @param par_escore parametros do escore.
#' @return Lista com \code{pares}, \code{comparacoes}, \code{pessoas_a},
#'   \code{pessoas_b}, \code{registros_a} e \code{registros_b}.
#' @examples
#' \dontrun{
#' res <- ln_pipeline(base_sinan, base_vacina, limiar = 18)
#' ln_avaliar(res$pares, res$pessoas_a, res$pessoas_b)
#' }
#' @export
ln_pipeline <- function(banco_a, banco_b, limiar = 14, verbose = TRUE,
                        passos = ln_passos_padrao(), par_escore = ln_par_escore()) {
  if (verbose) cat("[1/5] Padronizando campos...\n")
  A <- ln_preparar(banco_a); B <- ln_preparar(banco_b)

  if (verbose) cat("[2/5] Identificando pessoas dentro de cada banco...\n")
  A <- ln_identificar_pessoas(A); B <- ln_identificar_pessoas(B)
  pA <- ln_criar_chaves(ln_perfil_pessoa(A))
  pB <- ln_criar_chaves(ln_perfil_pessoa(B))
  if (verbose) cat(sprintf("      pessoas: A = %s | B = %s\n",
                           format(nrow(pA), big.mark = ","), format(nrow(pB), big.mark = ",")))

  if (verbose) cat("[3/5] Blocking multi-passo...\n")
  cand <- ln_gerar_candidatos(pA, pB, passos, verbose = verbose)
  if (verbose) cat(sprintf("      candidatos \u00fanicos: %s\n", format(nrow(cand), big.mark = ",")))

  if (verbose) cat("[4/5] Comparando e pontuando (Fellegi-Sunter)...\n")
  cmp <- ln_comparar(cand, pA, pB, par_escore)

  if (verbose) cat("[5/5] Decidindo pares (resolu\u00e7\u00e3o 1:1)...\n")
  pares <- ln_decidir(cmp, limiar)

  list(pares = pares, comparacoes = cmp, pessoas_a = pA, pessoas_b = pB,
       registros_a = A, registros_b = B)
}
