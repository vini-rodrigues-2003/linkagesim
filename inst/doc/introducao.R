## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----setup--------------------------------------------------------------------
library(linkagesim)

## -----------------------------------------------------------------------------
dicionario <- list(
  masc       = c("Joao", "Pedro", "Carlos", "Lucas", "Rafael"),
  fem        = c("Maria", "Ana", "Julia", "Camila", "Beatriz"),
  sobrenomes = c("Silva", "Santos", "Souza", "Lima", "Costa")
)

base <- ln_gerar_base_comum(n = 2000, dicionario)
head(base, 3)

## -----------------------------------------------------------------------------
par <- ln_par_ruido(p_mae_vazio = 0.08, p_nome_typo = 0.05)
str(par[1:6])

## -----------------------------------------------------------------------------
sinan  <- ln_criar_banco(20000, base, "SINAN_EXCL",  dicionario, par, semente = 43)
vacina <- ln_criar_banco(60000, base, "VACINA_EXCL", dicionario, par, semente = 42)
head(sinan[, .(nome_paciente, nome_mae, data_nasc)], 5)

## -----------------------------------------------------------------------------
res <- ln_pipeline(sinan, vacina, limiar = 14, verbose = FALSE)
ln_avaliar(res$pares, res$pessoas_a, res$pessoas_b)

## -----------------------------------------------------------------------------
ln_diagnosticar_perdas(res$pares, res$comparacoes, res$pessoas_a, res$pessoas_b)

## -----------------------------------------------------------------------------
cal <- ln_calibrar_limiar(res$comparacoes, res$pessoas_a, res$pessoas_b,
                          criterio = "f1")
cal$limiar
cal$grade[limiar %in% 10:20]

## -----------------------------------------------------------------------------
cal$ajuste_escala(nrow(res$pessoas_a) * 10, nrow(res$pessoas_b) * 10)

