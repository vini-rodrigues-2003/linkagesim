#===============================================================================
# PIPELINE COMPLETO — filtro etário -> bancos sintéticos -> linkage -> validação
# Substitui o script "Filtro_idade_ate_linkage.R"
#
# Este arquivo contém APENAS a execução. Toda a lógica está em
# linkagesim_funcoes.R, para que depois vire o R/ do pacote.
#===============================================================================

source("linkagesim_funcoes.R")   # ajuste o caminho se necessário

#-------------------------------------------------------------------------------
# 0. CONFIGURAÇÃO
#-------------------------------------------------------------------------------
CFG <- list(
  pasta        = "C:/Renata/Doutorado_e_EpiSUS/Saúde_Pública_FMRP/Belíssimo/Projeto_vacina_dengue/Doutorado/Script/",
  idade_min    = 8L,
  idade_max    = 16L,
  ano_nasc_min = 2008L,     # 8-16 anos em 2024 ou 2025
  ano_nasc_max = 2017L,
  overlap      = 0.10,      # fração de pessoas presentes nos dois bancos
  limiar       = 14,        # ponto de corte do escore Fellegi-Sunter
  semente      = 2026L
)
p <- function(...) file.path(CFG$pasta, ...)
set.seed(CFG$semente)

#-------------------------------------------------------------------------------
# 1. FILTRO DE FAIXA ETÁRIA NOS BANCOS REAIS
#-------------------------------------------------------------------------------
cat("=== 1. FILTRO ETÁRIO ===\n")

vacina_real <- fread(p("dg_2024_2025_completo.csv"))
cat("VACINA original:", format(nrow(vacina_real), big.mark = ","), "registros\n")

# [FIX-3] nu_idade_paciente pode vir codificado no padrão DATASUS
# (1º dígito = unidade: 1 hora, 2 dia, 3 mês, 4 ano). ln_idade_anos detecta
# sozinho e converte; se detectar errado, force com codificado = TRUE/FALSE.
vacina_real[, idade_anos := ln_idade_anos(nu_idade_paciente)]
cat("Distribuição de idade após decodificação:\n")
print(summary(vacina_real$idade_anos))

vacina_filtrada <- vacina_real[idade_anos >= CFG$idade_min & idade_anos <= CFG$idade_max]
cat("VACINA filtrada:", format(nrow(vacina_filtrada), big.mark = ","), "registros\n")
if (nrow(vacina_filtrada) == 0) stop("Filtro etário zerou a base — confira a codificação de nu_idade_paciente.")
fwrite(vacina_filtrada, p("dg_2024_2025_8a16anos.csv"))

sinan_real <- fread(p("sinan_2024_2025_completo.csv"))
cat("SINAN original:", format(nrow(sinan_real), big.mark = ","), "registros\n")
sinan_filtrado <- sinan_real[ANO_NASC >= CFG$ano_nasc_min & ANO_NASC <= CFG$ano_nasc_max]
cat("SINAN filtrado:", format(nrow(sinan_filtrado), big.mark = ","), "registros\n")
fwrite(sinan_filtrado, p("sinan_2024_2025_8a16anos.csv"))

rm(vacina_real, sinan_real); gc()

#-------------------------------------------------------------------------------
# 2. GERAÇÃO DOS IDENTIFICADORES SINTÉTICOS
#-------------------------------------------------------------------------------
# [FIX-11] n_vacina e n_sinan são lidos dos arquivos, não mais fixados no código.
# No script antigo eram constantes (3.240.380 / 1.055.110) que só valiam para
# uma extração específica; se o arquivo mudasse, o bind_cols colava nomes de
# uma pessoa em cima do registro de outra sem avisar.
cat("\n=== 2. BANCOS SINTÉTICOS ===\n")
n_vacina <- nrow(vacina_filtrada)
n_sinan  <- nrow(sinan_filtrado)
n_comum  <- as.integer(min(n_vacina, n_sinan) * CFG$overlap)
cat(sprintf("n_vacina = %s | n_sinan = %s | pessoas em comum = %s\n",
            format(n_vacina, big.mark = ","), format(n_sinan, big.mark = ","),
            format(n_comum, big.mark = ",")))

dicionario <- list(
  masc = c("Joao","Pedro","Carlos","Marcos","Paulo","Rafael","Lucas","Bruno","Gabriel","Felipe",
           "Rodrigo","Fernando","Andre","Diego","Thiago","Eduardo","Ricardo","Daniel","Gustavo","Vitor",
           "Marcelo","Fabio","Alexandre","Guilherme","Leonardo","Matheus","Vinicius","Arthur","Henrique",
           "Caio","Igor","Samuel","Davi","Miguel","Enzo","Bernardo","Heitor","Lorenzo","Nicolas","Murilo",
           "Otavio","Antonio","Francisco","Jose","Luiz","Jorge","Sergio","Roberto","Claudio","Anderson"),
  fem  = c("Maria","Ana","Julia","Beatriz","Larissa","Camila","Fernanda","Patricia","Aline","Bruna",
           "Carla","Daniela","Eliane","Fabiana","Gabriela","Helena","Isabela","Joana","Karina","Luciana",
           "Mariana","Natalia","Paula","Rafaela","Sabrina","Tatiane","Vanessa","Amanda","Bianca","Carolina",
           "Debora","Elisa","Flavia","Giovana","Heloisa","Ingrid","Jessica","Leticia","Manuela","Nicole",
           "Alice","Laura","Sophia","Valentina","Cecilia","Eloa","Livia","Marina","Rebeca","Yasmin"),
  sobrenomes = c("Silva","Santos","Oliveira","Souza","Rodrigues","Ferreira","Alves","Pereira","Lima","Gomes",
                 "Costa","Ribeiro","Martins","Carvalho","Almeida","Lopes","Soares","Fernandes","Vieira","Barbosa",
                 "Rocha","Dias","Nascimento","Andrade","Moreira","Nunes","Marques","Machado","Mendes","Freitas",
                 "Cardoso","Ramos","Goncalves","Santana","Teixeira","Araujo","Correia","Cavalcanti","Monteiro",
                 "Moura","Campos","Cunha","Pinto","Duarte","Miranda","Reis","Sales","Farias","Braga","Aguiar")
)

par_ruido <- ln_par_ruido()   # altere aqui para testar cenários mais/menos sujos

base_comum <- ln_gerar_base_comum(n_comum, dicionario,
                                  ano_min = CFG$ano_nasc_min, ano_max = CFG$ano_nasc_max)

nomes_vacina <- ln_criar_banco(n_vacina, base_comum, "VACINA_EXCL", dicionario,
                               par = par_ruido, semente = 42L)
nomes_sinan  <- ln_criar_banco(n_sinan,  base_comum, "SINAN_EXCL",  dicionario,
                               par = par_ruido, semente = 43L)

fwrite(nomes_vacina, p("nomes_ficticios_vacina_8a16anos.csv"))
fwrite(nomes_sinan,  p("nomes_ficticios_sinan_8a16anos.csv"))
cat("Bancos sintéticos gravados.\n")

#-------------------------------------------------------------------------------
# 3. COLAGEM DOS IDENTIFICADORES SINTÉTICOS NOS REGISTROS REAIS
#-------------------------------------------------------------------------------
# [FIX-12] A colagem continua sendo lado a lado, mas agora é feita com
# data.table e com verificação explícita de tamanho. As linhas extras do banco
# real (para acompanhar as duplicatas sintéticas) são sorteadas com reposição,
# como antes, porém a semente é a mesma do restante do pipeline.
cat("\n=== 3. COLAGEM COM OS REGISTROS REAIS ===\n")

colar <- function(nomes, reais, semente) {
  set.seed(semente)
  falta <- nrow(nomes) - nrow(reais)
  if (falta > 0) reais <- rbind(reais, reais[sample(.N, falta, replace = TRUE)])
  if (falta < 0) reais <- reais[sample(.N, nrow(nomes))]
  stopifnot(nrow(nomes) == nrow(reais))
  cbind(nomes, reais)
}

base_vacina <- colar(nomes_vacina, vacina_filtrada, 123L)
base_sinan  <- colar(nomes_sinan,  sinan_filtrado,  456L)

fwrite(base_vacina, p("base_vacina_ficticia_para_teste_8a16anos.csv"))
fwrite(base_sinan,  p("base_sinan_ficticia_para_teste_8a16anos.csv"))
cat("Bases de teste gravadas.\n")

rm(vacina_filtrada, sinan_filtrado, nomes_vacina, nomes_sinan); gc()

#-------------------------------------------------------------------------------
# 4. LINKAGE
#-------------------------------------------------------------------------------
cat("\n=== 4. LINKAGE ===\n")
res <- ln_pipeline(base_sinan, base_vacina, limiar = CFG$limiar, verbose = TRUE)

#-------------------------------------------------------------------------------
# 5. VALIDAÇÃO
#-------------------------------------------------------------------------------
cat("\n=== 5. VALIDAÇÃO ===\n")
metricas <- ln_avaliar(res$pares, res$pessoas_a, res$pessoas_b)
print(metricas)

cat("\nOnde os pares verdadeiros que faltaram se perderam:\n")
print(ln_diagnosticar_perdas(res$pares, res$comparacoes, res$pessoas_a, res$pessoas_b))

cat("\nVarredura de limiar (para escolher o ponto de corte):\n")
varredura <- rbindlist(lapply(seq(6, 22, by = 2), function(L)
  cbind(limiar = L, ln_avaliar(ln_decidir(res$comparacoes, L), res$pessoas_a, res$pessoas_b))))
print(varredura)

#-------------------------------------------------------------------------------
# 6. SAÍDAS
#-------------------------------------------------------------------------------
cat("\n=== 6. GRAVANDO RESULTADOS ===\n")

# pares pessoa-a-pessoa
fwrite(res$pares, p("linkage_pares_8a16anos.csv"), sep = ";")

# registros do SINAN com a marcação de pareado / não pareado
sinan_reg <- copy(res$registros_a)
sinan_reg[, pareado := id_paciente %in% res$pares$ida]
fwrite(sinan_reg[pareado == FALSE], p("sinan_nao_pareados_8a16anos.csv"), sep = ";")

vacina_reg <- copy(res$registros_b)
vacina_reg[, pareado := id_paciente %in% res$pares$idb]
fwrite(vacina_reg[pareado == FALSE], p("vacina_nao_pareados_8a16anos.csv"), sep = ";")

fwrite(varredura, p("linkage_varredura_limiar_8a16anos.csv"), sep = ";")
fwrite(metricas,  p("linkage_metricas_8a16anos.csv"), sep = ";")

cat("\nPROCESSAMENTO CONCLUÍDO.\n")
cat(sprintf("Sensibilidade: %.2f%% | VPP: %.2f%% | F1: %.2f%%\n",
            metricas$sensibilidade, metricas$ppv, metricas$f1))

#-------------------------------------------------------------------------------
# LEMBRETE
#-------------------------------------------------------------------------------
# id_pessoa_verdadeiro existe apenas na simulação e NÃO entra em nenhuma etapa
# do algoritmo — só nas funções ln_avaliar() e ln_diagnosticar_perdas().
# Ao rodar sobre dados reais, remova essas duas chamadas.
