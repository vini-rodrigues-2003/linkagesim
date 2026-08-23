test_that("o ruido de digitacao nao funde tokens do nome", {
  set.seed(1)
  x <- rep("Maria Aparecida Souza", 2000)
  y <- ln_typo(x, 0.9)
  expect_true(all(lengths(strsplit(y, " ")) == 3L))
})

test_that("as taxas de ruido injetadas sao as pedidas", {
  set.seed(2)
  par <- ln_par_ruido(p_nome_vazio = 0.10, p_mae_vazio = 0.20, p_data_vazia = 0.15)
  dic <- list(masc = c("Joao", "Pedro"), fem = c("Maria", "Ana"),
              sobrenomes = c("Silva", "Souza", "Lima"))
  base <- ln_gerar_base_comum(200, dic)
  b <- ln_criar_banco(20000, base, "EXCL", dic, par = par, semente = 1L)
  # tolerancia absoluta: testthat 3 interpreta `tolerance` como relativa
  expect_lt(abs(mean(is.na(b$nome_paciente)) - 0.10), 0.015)
  expect_lt(abs(mean(is.na(b$nome_mae))      - 0.20), 0.015)
  expect_lt(abs(mean(is.na(b$data_nasc))     - 0.15), 0.015)
})
