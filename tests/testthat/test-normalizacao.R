test_that("datas dd-mm-aaaa nao sao lidas como ano de dois digitos", {
  # regressao do bug principal: as.Date("15-03-2010", "%Y-%m-%d") devolve
  # "0015-03-20" sem erro nem aviso
  expect_equal(as.character(ln_parse_data("15-03-2010")), "2010-03-15")
  expect_equal(as.character(ln_parse_data("03/05/2010")), "2010-05-03")
  expect_equal(as.character(ln_parse_data("2010-05-03")), "2010-05-03")
  expect_equal(as.character(ln_parse_data("2010/05/03")), "2010-05-03")
  expect_equal(as.character(ln_parse_data("15032010")),   "2010-03-15")
  expect_equal(as.character(ln_parse_data("20100315")),   "2010-03-15")
})

test_that("datas implausiveis ou vazias viram NA", {
  expect_true(is.na(ln_parse_data("")))
  expect_true(is.na(ln_parse_data("00/00/0000")))
  expect_true(is.na(ln_parse_data("31/02/2010")))
  expect_true(is.na(ln_parse_data("15/03/1850")))
  expect_true(is.na(ln_parse_data("banana")))
})

test_that("acentos sao transliterados e nao apagados", {
  # regressao: iconv(x, "latin1", "UTF-8") fazia "magalhaes" virar "magalhes"
  expect_equal(ln_normalizar_texto("Magalh\u00e3es"), "magalhaes")
  expect_equal(ln_normalizar_texto("Jos\u00e9 Ant\u00f4nio"), "jose antonio")
  expect_equal(ln_normalizar_texto("MARIA  D'AVILA"), "maria d avila")
  expect_true(is.na(ln_normalizar_texto("")))
})

test_that("idade codificada no padrao DATASUS e convertida", {
  expect_equal(ln_idade_anos(rep(4008L, 10)), rep(8L, 10))
  expect_equal(ln_idade_anos(c(8L, 12L, 45L)), c(8L, 12L, 45L))
  expect_equal(ln_idade_anos(3011L, codificado = TRUE), 0L)
})

test_that("CNS invalido pelo modulo 11 e descartado", {
  cns <- ln_gerar_cns_valido(100)
  expect_equal(sum(!is.na(ln_limpar_cns(cns))), 100L)
  expect_true(is.na(ln_limpar_cns("1234")))
  expect_true(is.na(ln_limpar_cns("123456789012345")))
})

test_that("tokens de nome", {
  expect_equal(ln_primeiro_nome("maria silva santos"), "maria")
  expect_equal(ln_ultimo_nome("maria silva santos"), "santos")
})
