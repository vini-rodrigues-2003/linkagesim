# Metricas e calibracao do limiar

#' Metricas de validacao da simulacao
#'
#' @param pares saida de \code{ln_decidir()}.
#' @param A,B tabelas de pessoas.
#' @param prefixo_comum expressao regular que identifica as pessoas presentes
#'   nos dois bancos.
#' @return \code{data.table} com sensibilidade, valor preditivo positivo e F1.
#' @export
ln_avaliar <- function(pares, A, B, prefixo_comum = "^COMUM_") {
  esperados <- intersect(A[grepl(prefixo_comum, id_pessoa_verdadeiro), unique(id_pessoa_verdadeiro)],
                         B[grepl(prefixo_comum, id_pessoa_verdadeiro), unique(id_pessoa_verdadeiro)])
  certos <- pares[id_verd_a == id_verd_b]
  vp <- uniqueN(certos$id_verd_a); fp <- nrow(pares) - nrow(certos)
  sens <- vp / length(esperados); ppv <- nrow(certos) / max(nrow(pares), 1)
  data.table(esperados = length(esperados), pares = nrow(pares),
             verdadeiros = vp, falsos_positivos = fp,
             sensibilidade = round(100 * sens, 2), ppv = round(100 * ppv, 2),
             f1 = round(100 * 2 * sens * ppv / (sens + ppv), 2))
}

#' Explica onde os pares verdadeiros se perderam
#'
#' Separa as perdas ocorridas no blocking (o par nunca chegou a ser comparado)
#' das ocorridas no escore (foi comparado, mas ficou abaixo do limiar).
#'
#' @param pares saida de \code{ln_decidir()}.
#' @param cmp saida de \code{ln_comparar()}.
#' @param A,B tabelas de pessoas.
#' @param prefixo_comum expressao regular das pessoas em comum.
#' @return \code{data.table} com a contagem por origem da perda.
#' @export
ln_diagnosticar_perdas <- function(pares, cmp, A, B, prefixo_comum = "^COMUM_") {
  esperados <- intersect(A[grepl(prefixo_comum, id_pessoa_verdadeiro), unique(id_pessoa_verdadeiro)],
                         B[grepl(prefixo_comum, id_pessoa_verdadeiro), unique(id_pessoa_verdadeiro)])
  achados <- pares[id_verd_a == id_verd_b, unique(id_verd_a)]
  perdidos <- setdiff(esperados, achados)
  em_cand <- cmp[id_verd_a == id_verd_b & id_verd_a %in% perdidos]
  data.table(
    perdidos_total = length(perdidos),
    perdidos_no_blocking = length(perdidos) - uniqueN(em_cand$id_verd_a),
    perdidos_no_escore   = uniqueN(em_cand$id_verd_a),
    escore_mediano_perdido = round(median(em_cand$escore), 2)
  )
}

#' Escolhe o ponto de corte do escore
#'
#' O escore Fellegi-Sunter e uma razao de verossimilhanca e nao incorpora a
#' chance a priori de dois registros quaisquer serem a mesma pessoa. Essa chance
#' cai quando os bancos crescem, porque as comparacoes possiveis crescem com
#' \code{N_a * N_b} enquanto os pares verdadeiros crescem so com \code{N}. Na
#' pratica o limiar sobe \code{log2(N_a * N_b)}: de um teste em um terco da
#' escala para a base inteira o espaco de comparacao cresce nove vezes, ou seja
#' 3,17 pontos de escore. Um limiar calibrado num subconjunto nao pode ser
#' transportado para a base inteira sem essa correcao.
#'
#' @param cmp saida de \code{ln_comparar()}.
#' @param A,B tabelas de pessoas.
#' @param limiares valores a testar.
#' @param criterio \code{"f1"}, \code{"vpp"} ou \code{"sensibilidade"}.
#' @param alvo valor minimo exigido quando o criterio e \code{"vpp"} ou
#'   \code{"sensibilidade"}.
#' @param min_campos numero minimo de campos comparados.
#' @return Lista com o \code{limiar} escolhido, a \code{grade} completa e a
#'   funcao \code{ajuste_escala()}, que projeta o mesmo corte para bancos de
#'   outro tamanho.
#' @export
ln_calibrar_limiar <- function(cmp, A, B, limiares = seq(4, 30, by = 1),
                               criterio = c("f1", "vpp", "sensibilidade"),
                               alvo = 99, min_campos = 2L) {
  criterio <- match.arg(criterio)
  grade <- rbindlist(lapply(limiares, function(L)
    cbind(limiar = L, ln_avaliar(ln_decidir(cmp, L, min_campos), A, B))))
  escolhido <- switch(criterio,
    f1            = grade[which.max(f1), limiar],
    vpp           = { ok <- grade[ppv >= alvo]
                      if (!nrow(ok)) NA_real_ else ok[which.max(sensibilidade), limiar] },
    sensibilidade = { ok <- grade[sensibilidade >= alvo]
                      if (!nrow(ok)) NA_real_ else ok[which.max(ppv), limiar] })
  list(limiar = escolhido, grade = grade[],
       ajuste_escala = function(na_novo, nb_novo, na_atual = nrow(A), nb_atual = nrow(B))
         escolhido + log2((as.numeric(na_novo) * nb_novo) / (as.numeric(na_atual) * nb_atual)))
}
