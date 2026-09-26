# ============================================================
# 02_DESENHO_DESTINO.R
# Pergunta: o MESMO produto, exportado para os EUA, caiu mais
# do que quando exportado para outros destinos? E isso só
# acontece com produtos tarifados (não com os controle)?
# Rode 00_preparar_dados.R antes deste.
# ============================================================

library(dplyr)
library(fixest)
library(lubridate)

pasta <- "C:/Users/csalv/OneDrive/Desktop/TCC_2/Comex_data"
classificacao <- readRDS(file.path(pasta, "classificacao_sh4.rds"))
dados_comex   <- readRDS(file.path(pasta, "base_comex_rs_tempo.rds"))

# Amostra: TODOS os destinos agora (não só EUA), tira a Seção 232
df <- dados_comex %>%
  left_join(classificacao, by = "SH4") %>%
  mutate(grupo = coalesce(grupo, "controle")) %>%
  filter(grupo != "secao_232") %>%
  mutate(log_export = log(export_total))

# ------------------------------------------------------------
# A IDEIA
#
# Em vez de comparar produto tarifado x produto controle (como
# no script anterior), aqui a gente compara o MESMO produto
# vendido pra EUA x vendido pros demais países, mês a mês. Isso
# funciona como um "controle" ainda mais limpo: qualquer coisa
# que seja característica só do produto (safra, sazonalidade,
# qualidade) afeta os EUA e os outros destinos igual, e some na
# comparação. O que sobra é só o efeito de ser vendido
# especificamente para os EUA.
#
# Fazemos essa comparação DUAS VEZES, separadamente:
#   - só com os produtos tarifados
#   - só com os produtos controle
# Se a tarifa é a explicação, o desvio (EUA x outros destinos)
# deve aparecer forte nos tarifados e não aparecer nos controle -
# essa segunda regressão funciona como um teste de placebo.
# ------------------------------------------------------------

modelo_destino <- function(dados) {
  feols(
    log_export ~ eua * (pos1 + pos_free + pos2) | SH4 + pais + data,
    data    = dados,
    cluster = ~SH4
  )
}
# "| SH4 + pais + data": tira o nível médio de cada produto (SH4),
# de cada destino (pais) e de cada mês (data) - sobra só o desvio
# específico dos EUA, em cada período.

m_tarifado <- modelo_destino(df %>% filter(grupo == "tarifado_2025"))
m_controle <- modelo_destino(df %>% filter(grupo == "controle"))

etable(m_tarifado, m_controle,
       headers = c("Só tarifados", "Só controle"),
       keep    = "eua",
       digits  = 4)

# ------------------------------------------------------------
# COMO LER A TABELA
#
#   "eua x pos1" = quanto a exportação pros EUA (em relação aos
#   demais destinos) mudou durante a tarifa, comparado ao período
#   base (antes de ago/2025).
#
# O QUE ESPERAR:
#   - Coluna "Só tarifados": coeficiente NEGATIVO e significativo
#     em pos1 - os produtos tarifados desviaram menos exportação
#     pros EUA durante a vigência da tarifa. Esperado.
#   - Coluna "Só controle": coeficiente PRÓXIMO DE ZERO e SEM
#     significância (sem estrela) em pos1 - produtos que nunca
#     foram tarifados não deveriam mostrar desvio nenhum do
#     destino EUA. Esse é o teste de placebo: se aqui também
#     desse negativo e forte, seria sinal de que o que a coluna
#     anterior captou não é o efeito da tarifa, e sim algo comum
#     a todos os produtos (frete mais caro em geral, recessão nos
#     EUA etc.).
#   - Em pos_free (tarifa suspensa), o coeficiente dos tarifados
#     deveria enfraquecer (chegar mais perto de zero) - é outra
#     checagem de que o efeito realmente vem da tarifa.
#
# Conversão pra %: 100 * (exp(coeficiente) - 1). Ex.: -0,41 vira
# aproximadamente -34%.
# ------------------------------------------------------------

cat("\nSó tarifados, variação %:\n")
print(round(100 * (exp(coef(m_tarifado)) - 1), 1))
cat("\nSó controle, variação %:\n")
print(round(100 * (exp(coef(m_controle)) - 1), 1))

# ------------------------------------------------------------
# GRÁFICO (event study): em vez de só 3 períodos, estima um
# coeficiente PARA CADA MÊS, sempre comparado a jul/2025 (o
# último mês sem tarifa). Serve pra ver duas coisas na imagem:
#   1) ANTES de ago/2025 (mês 0): os pontos devem ficar perto de
#      zero, sem tendência - se já estivessem caindo antes da
#      tarifa existir, o resultado não seria causado por ela.
#   2) DEPOIS de ago/2025: os pontos devem cair visivelmente
#      abaixo de zero (só no grupo tarifado).
# ------------------------------------------------------------

df_ev <- df %>%
  mutate(tempo = as.integer(interval(as.Date("2025-07-01"), data) %/% months(1)))

es_tarifado <- feols(
  log_export ~ i(tempo, eua, ref = -1) | SH4 + pais + data,
  data    = df_ev %>% filter(grupo == "tarifado_2025"),
  cluster = ~SH4
)

iplot(es_tarifado,
      main = "Desvio do destino EUA, mês a mês (jul/2025 = referência)",
      xlab = "Meses relativos a jul/2025", ylab = "Coeficiente e IC 95%")
abline(v = c(6, 11), lty = 2, col = "red")  # linhas: Suprema Corte (fev/26) e nova tarifa (ago/26)
