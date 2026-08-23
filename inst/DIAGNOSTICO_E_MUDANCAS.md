# Por que a sensibilidade travava em ~65% — diagnóstico e correções

Diagnóstico feito reproduzindo o gerador e o algoritmo de linkage do script
original em escala reduzida (30.000 registros no SINAN, 90.000 na vacina,
3.000 pessoas realmente em comum), com R 4.3.3.

---

## 1. O que estava acontecendo

Não era um bug único. O algoritmo exigia que **quatro condições fossem
verdadeiras ao mesmo tempo**:

```
mesmo ano de nascimento  E  mesma 1ª letra do nome  E  jw(nome) ≤ 0,2  E  jw(mãe) ≤ 0,2
```

Como o gerador injeta ruído em cada um desses campos de forma independente,
cada inconsistência derruba o par verdadeiro sozinha. Medindo par a par,
sobre as 3.000 pessoas realmente comuns:

| Motivo da perda | % dos pares verdadeiros perdidos |
|---|---|
| Um dos dois bancos sem data de nascimento (estratos "com data"/"sem data" nunca se cruzam) | 9,60% |
| Ano de nascimento divergente após a leitura da data | **24,20%** |
| Primeira letra do nome divergente ou nome em branco | 6,50% |
| jw(nome do paciente) > 0,2 | 6,80% |
| jw(nome da mãe) > 0,2 | 14,53% |
| **Teto de sensibilidade do desenho original** | **52,90%** |

Rodando a regra antiga de ponta a ponta sobre os mesmos dados:
**sensibilidade 53,07%** e **VPP 10,82%** — ou seja, quase 9 em cada 10 pares
devolvidos estavam errados. A sensibilidade que você observa é o mesmo
fenômeno; o valor exato varia com a semente e com as proporções do banco real.

---

## 2. O bug mais grave: leitura das datas

`parse_data_multiformato()` testava formatos em sequência e ficava com o
primeiro que não devolvesse `NA`:

```r
formatos <- c("%d/%m/%Y %H:%M:%S", "%Y-%m-%d %H:%M:%S",
              "%d/%m/%Y", "%Y-%m-%d", "%d-%m-%Y", "%Y/%m/%d", ...)
```

O problema é que `as.Date()` / `strptime()` são tolerantes:

```r
as.Date("15-03-2010", format = "%Y-%m-%d")
#> "0015-03-20"     # ano 15, mês 03, dia 20 — sem erro, sem aviso
```

Como `"%Y-%m-%d"` vinha **antes** de `"%d-%m-%Y"`, toda data gravada como
`dd-mm-aaaa` (15% dos registros, pelo próprio gerador) era lida com o ano
errado. Medido: **16,38% das datas preenchidas foram lidas incorretamente**, e
nenhuma virou `NA` — o erro passava silencioso até o blocking.

Como o formato é sorteado de forma independente em cada banco, a mesma pessoa
aparecia com `ano = 2010` de um lado e `ano = 15` do outro, e o bloco
`ano + letra` nunca as colocava juntas.

**Correção:** `ln_parse_data()` identifica o layout por expressão regular
ancorada, monta a string ISO e só então converte. Datas fora da janela
plausível viram `NA` em vez de entrar no linkage com ano absurdo.

---

## 3. Os outros seis problemas corrigidos

| # | Problema | Correção |
|---|---|---|
| 1 | `clean_string()` chamava `iconv(x, "latin1", "UTF-8")` em texto que já estava em UTF-8. Resultado: `"magalhães"` → `"magalhes"` (a letra acentuada era **apagada**, não transliterada) | `ln_normalizar_texto()` translitera direto com `stringi` |
| 2 | Blocking de passo único (`ano + 1ª letra`): qualquer falha em um desses dois campos eliminava o par antes da comparação. Além disso gera ~260 blocos gigantes — com 1,05 M × 3,24 M são mais de 10¹⁰ comparações, e os blocos que estouravam a memória eram engolidos por um `tryCatch` que apenas imprimia o erro | Cinco passos independentes, cada um exigindo só **dois** campos. O par entra na comparação se sobreviver a **qualquer** passo. Cobertura medida: 99,5% dos pares verdadeiros |
| 3 | Decisão por conjunção rígida de limiares. Ausência era tratada como discordância: `jw("", "maria silva") = 1` | Escore Fellegi-Sunter: cada campo soma `log2(m/u)` quando concorda e `log2((1−m)/(1−u))` quando discorda, e soma **zero** quando está ausente. CNS diferente vira indício contrário moderado, não veto — o que corresponde à realidade do SUS, onde a mesma pessoa costuma ter mais de um CNS |
| 4 | `chave_deterministica <- paste(nome, mae, data)` sem tratar ausências: com os três campos vazios, `paste()` devolve `" NA"` para todos, e esses registros viravam "a mesma pessoa" e eram apagados na deduplicação. O `merge` de propagação usava `allow.cartesian = TRUE` e podia multiplicar linhas | Chave só existe com os três componentes presentes; propagação do CNS por lookup 1:1 |
| 5 | Linkage feito linha a linha, com duplicatas e doses inflando os pares; e todos os candidatos acima do limiar eram aceitos | Linkage pessoa × pessoa, com resolução 1:1 gulosa pelo maior escore. Campos vazios de um registro são completados pelas réplicas |
| 6 | `n_vacina` e `n_sinan` fixos no código (3.240.380 / 1.055.110). Se a extração mudasse, o `bind_cols` colaria os dados de uma pessoa sobre o registro de outra sem avisar | Contagens lidas dos arquivos, com `stopifnot()` no tamanho |
| 7 | `nu_idade_paciente` filtrado como se fosse idade em anos, sem checar a codificação DATASUS (1º dígito = unidade) | `ln_idade_anos()` detecta e converte, e o pipeline aborta se o filtro zerar a base |

Também foi acrescentado ao gerador o ruído que os comentários prometiam mas o
código não produzia: **erros de digitação caractere a caractere** (troca,
transposição e supressão de letra). E o CNS sintético agora é válido pelo
módulo 11, para que a validação do CNS faça sentido no teste.

---

## 4. Resultado

Mesmos dados, mesma semente, agora **com** os erros de digitação (cenário mais
difícil que o original):

| | Sensibilidade | VPP | F1 |
|---|---|---|---|
| Regra antiga | 53,07% | 10,82% | — |
| Pipeline novo (limiar 14) | **98,03%** | **99,32%** | 98,67% |

Varredura do limiar (para escolher o ponto de corte com a curva na mão):

| Limiar | Pares | Sensibilidade | VPP | F1 |
|---|---|---|---|---|
| 8  | 3.355 | 99,03% | 88,55% | 93,50% |
| 10 | 3.138 | 98,80% | 94,46% | 96,58% |
| 12 | 3.051 | 98,53% | 96,89% | 97,70% |
| **14** | 2.961 | **98,03%** | **99,32%** | **98,67%** |
| 16 | 2.932 | 97,40% | 99,66% | 98,52% |
| 18 | 2.916 | 97,13% | 99,93% | 98,51% |
| 20 | 2.807 | 93,57% | 100,00% | 96,68% |

Das 59 pessoas ainda não encontradas no limiar 14: 22 se perderam no blocking
(dois ou mais campos destruídos ao mesmo tempo) e 37 ficaram logo abaixo do
corte, com escore mediano 11,8.

---

## 4-bis. Correção de desempenho (v2.1)

A v2.0 rodava em minutos no teste de 30 mil registros e travava por horas nos
3,2 milhões reais. O culpado era `ln_formatar_data()`:

```r
out[livre] <- vapply(seq_along(esc), function(k) format(datas[livre][k], esc[k]), character(1))
```

`datas[livre]` era recalculado **dentro** de cada iteração — um subset de 3,2
milhões de elementos, repetido 3 milhões de vezes. Custo O(n²). Medido:

| n | tempo |
|---|---|
| 5.000 | 0,27 s |
| 10.000 | 0,43 s |
| 20.000 | 1,50 s |
| 40.000 | 5,32 s |
| **3.240.380 (extrapolado)** | **~9,7 h** |

Corrigido para 5 chamadas vetorizadas de `format()`, uma por formato de data.
Outros quatro pontos foram vetorizados junto, todos marcados com `[PERF]`:

| Função | Antes | Depois (3,24 M) |
|---|---|---|
| `ln_formatar_data` | ~9,7 h | 2,4 s |
| `ln_typo` | laço `for` com `strsplit` por elemento | 0,7 s (junto com o ruído de nome) |
| `ln_gerar_cns_valido` | `apply(d, 1, paste)` linha a linha | 20,6 s |
| `ln_limpar_cns` | `strsplit`+`unlist` de 15 × N elementos (~2,8 GB) | 5,2 s |
| `ln_normalizar_texto` | `gsub` do R base | 26,1 s |
| `ln_perfil_pessoa` | uma avaliação do interpretador por pessoa (~3 M grupos) | ordena + desduplica por coluna |

Medição de ponta a ponta em 1/3 da escala real (1.080.126 × 351.703), num
container de 1 núcleo e 3 GB:

| Etapa | Tempo |
|---|---|
| Gerar os dois bancos sintéticos | 16 s |
| Padronizar campos | 43 s |
| Identificar pessoas e montar perfis | 20 s |
| Blocking (138.951 candidatos) | 4 s |
| Comparar, pontuar e decidir | 1 s |
| **Total** | **~85 s**, pico de 1,3 GB |

Resultado nessa escala: sensibilidade 97,97%, VPP 99,28%. Na escala cheia
espera-se algo entre 5 e 10 minutos e cerca de 4 GB de pico.


---

## 4-ter. Execução na escala real e escolha do limiar

Resultado na base completa (1.055.775 pessoas no SINAN × 3.242.450 na vacina,
105.511 realmente em comum):

| Limiar | Pares | Sensibilidade | VPP | F1 |
|---|---|---|---|---|
| 10 | 128.619 | 98,74% | 81,00% | 89,00% |
| 12 | 114.027 | 98,53% | 91,17% | 94,71% |
| 14 | 105.531 | 98,02% | 98,00% | 98,01% |
| 16 | 103.333 | 97,21% | 99,26% | 98,23% |
| **18** | 102.261 | **96,82%** | **99,90%** | **98,33%** |
| 20 | 98.155 | 93,03% | 100,00% | 96,39% |

**O limiar ótimo depende do tamanho dos bancos.** O escore Fellegi-Sunter é uma
razão de verossimilhança: mede o quanto a concordância favorece "mesma pessoa",
mas não incorpora a chance *a priori* de dois registros quaisquer serem a mesma
pessoa. Essa chance cai quando as bases crescem, porque o número de comparações
possíveis cresce com `N_a × N_b` enquanto o número de pares verdadeiros cresce
apenas com `N`.

Na prática, o limiar precisa subir `log2(N_a × N_b)`:

| | Espaço de comparação | Limiar ótimo |
|---|---|---|
| Teste em 1/3 da escala | 3,80 × 10¹¹ | 14 |
| Base completa | 3,42 × 10¹² (9× maior) | 14 + log2(9) = **17,2 ≈ 18** |

E é exatamente onde o F1 da base completa tem o pico. Por isso um limiar
calibrado num subconjunto **não** pode ser transportado para a base inteira sem
essa correção.

`ln_calibrar_limiar()` faz a varredura e devolve o corte por três critérios
(máximo F1, VPP mínimo ou sensibilidade mínima), junto com a função
`ajuste_escala()` que projeta o mesmo corte para bancos de outro tamanho.


---

## 5. Arquivos

- `linkagesim_funcoes.R` — toda a lógica, em funções sem estado, prontas para
  virar o `R/` do pacote. Cada correção está marcada com `[FIX-n]` no ponto
  onde foi feita.
- `roda_pipeline_completo.R` — apenas a execução: filtro etário → geração dos
  bancos sintéticos → colagem nos registros reais → linkage → validação.

## 6. Caminho para o pacote

O corte natural dos arquivos, quando for empacotar:

| Arquivo do pacote | Funções |
|---|---|
| `R/normalizacao.R` | `ln_normalizar_texto`, `ln_parse_data`, `ln_limpar_cns`, `ln_idade_anos`, `ln_primeiro_nome`, `ln_ultimo_nome` |
| `R/simulacao.R` | `ln_par_ruido`, `ln_typo`, `ln_gerar_cns_valido`, `ln_aplicar_ruido_nome`, `ln_formatar_data`, `ln_gerar_base_comum`, `ln_criar_banco` |
| `R/deduplicacao.R` | `ln_preparar`, `ln_identificar_pessoas`, `ln_perfil_pessoa` |
| `R/blocking.R` | `ln_passos_padrao`, `ln_criar_chaves`, `ln_gerar_candidatos` |
| `R/escore.R` | `ln_pesos`, `ln_par_escore`, `ln_comparar`, `ln_decidir` |
| `R/avaliacao.R` | `ln_avaliar`, `ln_diagnosticar_perdas`, `ln_calibrar_limiar` |
| `R/pipeline.R` | `ln_pipeline` |

Dois pontos que valem virar teste automatizado (`testthat`) desde já, porque
foram exatamente os que quebraram em silêncio:

1. `ln_parse_data("15-03-2010")` tem que devolver `2010-03-15`.
2. `ln_normalizar_texto("Magalhães")` tem que devolver `"magalhaes"`.
