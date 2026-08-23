# linkagesim

Simulação e validação de *record linkage* para bases do SUS.

O pacote gera bancos sintéticos com inconsistências controladas a partir de uma
base de pessoas conhecida, executa o relacionamento probabilístico e mede
sensibilidade, valor preditivo positivo e F1 **contra a verdade conhecida** —
que é o que permite calibrar o algoritmo antes de aplicá-lo aos dados reais.

Desenvolvido para relacionar o SINAN (dengue) ao registro de imunizações, mas as
funções não dependem dessas bases.

## Instalação

```r
# install.packages("remotes")
remotes::install\_github("vini-rodrigues-2003/linkagesim")
```

## Uso

```r
library(linkagesim)

dicionario <- list(
  masc       = c("Joao", "Pedro", "Carlos", "Lucas", "Rafael"),
  fem        = c("Maria", "Ana", "Julia", "Camila", "Beatriz"),
  sobrenomes = c("Silva", "Santos", "Souza", "Lima", "Costa")
)

# 1. base de pessoas que existem nos dois bancos (a "verdade")
base <- ln\_gerar\_base\_comum(n = 5000, dicionario)

# 2. dois bancos sintéticos, cada um com seu próprio ruído
sinan  <- ln\_criar\_banco( 50000, base, "SINAN\_EXCL",  dicionario, semente = 43)
vacina <- ln\_criar\_banco(150000, base, "VACINA\_EXCL", dicionario, semente = 42)

# 3. linkage
res <- ln\_pipeline(sinan, vacina, limiar = 14)

# 4. validação e escolha do ponto de corte
ln\_avaliar(res$pares, res$pessoas\_a, res$pessoas\_b)
cal <- ln\_calibrar\_limiar(res$comparacoes, res$pessoas\_a, res$pessoas\_b,
                          criterio = "vpp", alvo = 99.5)
cal$limiar
```

O ruído injetado é todo parametrizado em `ln\_par\_ruido()`: ausências,
abreviações, erros de digitação, formatos de data heterogêneos, CNS divergentes
e duplicatas internas.

## Como funciona

**Blocking multi-passo.** Cinco passos independentes, cada um exigindo apenas
dois campos concordantes (CNS; data + nome; data + mãe; nome + mãe;
ano-mês + nome + mãe). O par entra na comparação se sobreviver a *qualquer* um
deles, de modo que é preciso destruir dois campos ao mesmo tempo para perder uma
pessoa.

**Escore de Fellegi-Sunter.** Cada campo soma `log2(m/u)` quando concorda e
`log2((1-m)/(1-u))` quando discorda, e soma **zero** quando está ausente em um
dos lados. Ausência não é discordância. Um CNS diferente é indício contrário
moderado, não veto — o que corresponde à realidade do SUS, onde a mesma pessoa
costuma ter mais de um CNS.

**Resolução 1:1.** Cada pessoa de um banco fica com o melhor par disponível no
outro, por escore decrescente.

## O limiar depende do tamanho dos bancos

O escore é uma razão de verossimilhança: mede o quanto a concordância favorece
"mesma pessoa", mas não incorpora a chance *a priori* de dois registros
quaisquer serem a mesma pessoa. Essa chance cai quando as bases crescem, porque
as comparações possíveis crescem com `N\_a × N\_b` enquanto os pares verdadeiros
crescem apenas com `N`.

Na prática o limiar sobe `log2(N\_a × N\_b)`:

|Escala|Espaço de comparação|Limiar ótimo|
|-|-|-|
|Um terço da base|3,80 × 10¹¹|14|
|Base completa (9× maior)|3,42 × 10¹²|14 + log2(9) = 17,2 ≈ **18**|

`ln\_calibrar\_limiar()` devolve, junto com o corte escolhido, a função
`ajuste\_escala()` que faz essa projeção. **Não transporte um limiar calibrado em
um subconjunto para a base inteira sem essa correção.**

## Desempenho

Base completa: 1.055.775 pessoas no SINAN × 3.242.450 na vacina, 105.511
realmente em comum.

|Limiar|Sensibilidade|VPP|F1|
|-|-|-|-|
|14|98,02%|98,00%|98,01%|
|16|97,21%|99,26%|98,23%|
|**18**|**96,82%**|**99,90%**|**98,33%**|
|20|93,03%|100,00%|96,39%|

Qual corte usar depende do desfecho. Num estudo de efetividade vacinal, um falso
positivo classifica como vacinado quem não foi — viés de exposição, que puxa a
estimativa para o nulo. Um falso negativo tira o caso da análise: custa poder,
mas não enviesa da mesma forma. Isso costuma favorecer o corte mais alto.

## Limitações conhecidas

O dicionário de nomes é sorteado de forma uniforme. Nomes brasileiros reais são
muito mais concentrados — "Maria Silva Santos" aparece milhares de vezes,
"Yasmin Cavalcanti Aguiar" quase nunca. Como o `u` (probabilidade de
concordância por acaso) varia enormemente entre esses dois casos, o VPP medido
aqui é uma média que não representa nenhum dos extremos. Sortear nomes com
frequências reais e ponderar a concordância pela frequência do nome é o próximo
passo.

## Licença

MIT

