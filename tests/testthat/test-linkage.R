dic <- list(
  masc = c("Joao","Pedro","Carlos","Lucas","Rafael","Gabriel","Bruno","Felipe","Diego","Thiago"),
  fem  = c("Maria","Ana","Julia","Camila","Fernanda","Beatriz","Larissa","Bruna","Carla","Helena"),
  sobrenomes = c("Silva","Santos","Souza","Lima","Costa","Alves","Gomes","Dias","Rocha","Pinto"))

test_that("o pipeline recupera a maioria dos pares verdadeiros", {
  set.seed(7)
  base <- ln_gerar_base_comum(1000, dic)
  S <- ln_criar_banco(10000, base, "SINAN_EXCL",  dic, semente = 43L)
  V <- ln_criar_banco(30000, base, "VACINA_EXCL", dic, semente = 42L)
  res <- ln_pipeline(S, V, limiar = 14, verbose = FALSE)
  m <- ln_avaliar(res$pares, res$pessoas_a, res$pessoas_b)
  expect_gt(m$sensibilidade, 95)
  expect_gt(m$ppv, 90)
})

test_that("campo ausente nao e tratado como discordancia", {
  # dois registros identicos, um deles sem o nome da mae: o par tem de sobreviver
  A <- data.table::data.table(nome_paciente = "Maria Silva Souza", nome_mae = "Ana Lima Costa",
                              data_nasc = "10/05/2011", numero_sus = ln_gerar_cns_valido(1),
                              id_pessoa_verdadeiro = "COMUM_1")
  B <- data.table::copy(A)[, nome_mae := NA_character_]
  res <- ln_pipeline(A, B, limiar = 14, verbose = FALSE)
  expect_equal(nrow(res$pares), 1L)
})

test_that("a resolucao 1:1 nao devolve a mesma pessoa duas vezes", {
  set.seed(11)
  base <- ln_gerar_base_comum(500, dic)
  S <- ln_criar_banco(5000, base, "SINAN_EXCL",  dic, semente = 3L)
  V <- ln_criar_banco(15000, base, "VACINA_EXCL", dic, semente = 4L)
  res <- ln_pipeline(S, V, limiar = 14, verbose = FALSE)
  expect_false(any(duplicated(res$pares$ida)))
  expect_false(any(duplicated(res$pares$idb)))
})

test_that("o limiar calibrado sobe com o tamanho dos bancos", {
  set.seed(13)
  base <- ln_gerar_base_comum(800, dic)
  S <- ln_criar_banco(8000, base, "SINAN_EXCL",  dic, semente = 5L)
  V <- ln_criar_banco(24000, base, "VACINA_EXCL", dic, semente = 6L)
  res <- ln_pipeline(S, V, limiar = 14, verbose = FALSE)
  cal <- ln_calibrar_limiar(res$comparacoes, res$pessoas_a, res$pessoas_b)
  # nove vezes mais comparacoes -> log2(9) = 3,17 pontos a mais
  expect_equal(cal$ajuste_escala(nrow(res$pessoas_a) * 3, nrow(res$pessoas_b) * 3),
               cal$limiar + log2(9), tolerance = 0.01)
})
