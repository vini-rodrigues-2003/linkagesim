#' linkagesim: simulacao e validacao de record linkage para bases do SUS
#'
#' Gera bancos sinteticos com inconsistencias controladas a partir de uma base
#' de pessoas conhecida, executa o relacionamento probabilistico e mede
#' sensibilidade, valor preditivo positivo e F1 contra a verdade conhecida.
#'
#' O fluxo tipico e:
#' \enumerate{
#'   \item \code{\link{ln_gerar_base_comum}} e \code{\link{ln_criar_banco}} para
#'     montar os dois bancos sinteticos;
#'   \item \code{\link{ln_pipeline}} para executar o linkage;
#'   \item \code{\link{ln_avaliar}} e \code{\link{ln_calibrar_limiar}} para medir
#'     o desempenho e escolher o ponto de corte.
#' }
#'
#' @keywords internal
#' @import data.table
#' @importFrom stringi stri_trans_general stri_trans_tolower stri_replace_all_regex stri_trim_both
#' @importFrom stringdist stringdist
#' @importFrom stats complete.cases median runif
"_PACKAGE"

# silencia as notas do R CMD check sobre variaveis criadas por referencia no
# data.table (elas nao existem no escopo lexico, so dentro de j)
utils::globalVariables(c(
  "nome_n", "mae_n", "dn", "cns", "nome_1", "nome_u", "mae_1", "mae_u",
  "chave_forte", "cns_prop", "id_final", "id_paciente", ".falta", ".N", ".I", ".GRP",
  "k_cns", "k_dn", "k_anomes", "k_nome", "k_mae", "k_mae1", "ida", "idb",
  "N.a", "N.b", "pares", "nome_a", "nome_b", "mae_a", "mae_b", "dn_a", "dn_b",
  "cns_a", "cns_b", "s_nome", "s_mae", "s_data", "s_cns", "escore", "n_campos",
  "id_verd_a", "id_verd_b", "id_pessoa_verdadeiro", "nome_paciente_ref",
  "nome_mae_ref", "data_nasc_ref", "cns_ref", "nome_paciente", "nome_mae",
  "data_nasc", "numero_sus", "n_registros", "ppv", "sensibilidade", "f1", "limiar",
  "idade_anos", "i.x", ".", "..ks"
))
