# Geracao de pares candidatos

#' Passos de blocking padrao
#'
#' Cada passo exige apenas dois campos concordantes, e o par entra na comparacao
#' se sobreviver a qualquer um deles. Assim e preciso destruir dois campos ao
#' mesmo tempo para perder uma pessoa.
#'
#' @return Lista nomeada de vetores de chaves.
#' @export
ln_passos_padrao <- function() list(
  P1_cns          = "k_cns",
  P2_data_nome    = c("k_dn", "k_nome"),
  P3_data_mae     = c("k_dn", "k_mae"),
  P4_nome_mae     = c("k_nome", "k_mae"),
  P5_anomes_nome  = c("k_anomes", "k_nome", "k_mae1")
)

#' Cria as chaves de bloco
#'
#' @param p \code{data.table} de pessoas, saida de \code{ln_perfil_pessoa()}.
#' @return O mesmo \code{data.table} com as colunas \code{k_*}.
#' @export
ln_criar_chaves <- function(p) {
  p[, k_cns    := cns]
  p[, k_dn     := as.character(dn)]
  p[, k_anomes := substr(as.character(dn), 1, 7)]
  p[, k_nome   := fifelse(!is.na(nome_n), paste0(substr(nome_1, 1, 4), "|", substr(nome_u, 1, 4)), NA_character_)]
  p[, k_mae    := fifelse(!is.na(mae_n),  paste0(substr(mae_1, 1, 4), "|", substr(mae_u, 1, 4)),  NA_character_)]
  p[, k_mae1   := substr(mae_1, 1, 3)]
  p[]
}

#' Gera os pares candidatos pela uniao dos passos de blocking
#'
#' @param A,B tabelas de pessoas com as chaves criadas por \code{ln_criar_chaves()}.
#' @param passos lista de passos, de \code{ln_passos_padrao()}.
#' @param max_pares_bloco teto de pares por chave; blocos maiores (nomes muito
#'   frequentes) sao ignorados naquele passo e avisados.
#' @param verbose imprimir o progresso.
#' @return \code{data.table} com as colunas \code{ida} e \code{idb}.
#' @export
ln_gerar_candidatos <- function(A, B, passos = ln_passos_padrao(),
                                max_pares_bloco = 5e5, verbose = TRUE) {
  saida <- vector("list", length(passos))
  for (j in seq_along(passos)) {
    ks <- passos[[j]]
    a <- A[stats::complete.cases(A[, ..ks]), c("id_paciente", ks), with = FALSE]
    b <- B[stats::complete.cases(B[, ..ks]), c("id_paciente", ks), with = FALSE]
    setnames(a, "id_paciente", "ida"); setnames(b, "id_paciente", "idb")

    # teto de segurança: blocos absurdos (nomes muito frequentes) são separados
    tam <- merge(a[, .N, by = ks], b[, .N, by = ks], by = ks, suffixes = c(".a", ".b"))
    tam[, pares := as.numeric(N.a) * as.numeric(N.b)]
    grandes <- tam[pares > max_pares_bloco]
    if (nrow(grandes) && verbose)
      cat(sprintf("    %s: %d bloco(s) acima do teto ignorado(s) neste passo\n",
                  names(passos)[j], nrow(grandes)))
    if (nrow(grandes)) {
      a <- a[!grandes, on = ks]; b <- b[!grandes, on = ks]
    }
    r <- merge(a, b, by = ks, allow.cartesian = TRUE)[, .(ida, idb)]
    if (verbose) cat(sprintf("    %s: %s pares candidatos\n",
                             names(passos)[j], format(nrow(r), big.mark = ",")))
    saida[[j]] <- r
  }
  unique(rbindlist(saida))
}
