# linkagesim 0.1.0

Primeira versao.

Reescrita do script de simulacao original, cuja sensibilidade estava em torno de
53% com valor preditivo positivo de 11%. As correcoes estao marcadas no codigo
com `[FIX-n]` e `[PERF]`:

* `ln_parse_data()` identifica o formato da data por expressao regular ancorada.
  A versao anterior percorria formatos com `as.Date()`, que aceita
  `"15-03-2010"` sob `"%Y-%m-%d"` e devolve o ano 15 sem erro nem aviso — 16,4%
  das datas eram lidas incorretamente.
* `ln_normalizar_texto()` translitera acentos em vez de apaga-los.
* Blocking passou de um passo (`ano + 1a letra`) para cinco passos
  independentes, cada um exigindo apenas dois campos.
* A decisao passou de conjuncao de limiares para escore de Fellegi-Sunter, em
  que campo ausente contribui com zero em vez de ser tratado como discordancia.
* O linkage passou a ser pessoa a pessoa, com resolucao 1:1.
* `ln_calibrar_limiar()` escolhe o ponto de corte e corrige-o pela escala dos
  bancos.
* Vetorizacao de `ln_formatar_data()`, que tinha custo quadratico e levava cerca
  de 9,7 horas em 3,2 milhoes de registros; agora leva 2,4 segundos.

Desempenho na base completa (1.055.775 x 3.242.450 pessoas, 105.511 em comum):
sensibilidade 96,8% e valor preditivo positivo 99,9% no limiar 18.
