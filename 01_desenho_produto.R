# ============================================================
# 01_DESENHO_PRODUTO.R
# Pergunta: dentro do mercado americano, os produtos tarifados
# exportaram menos do que os produtos controle, depois da tarifa?
# Rode 00_preparar_dados.R antes deste.
# ============================================================

library(dplyr)
library(fixest)

pasta <- "C:/Users/csalv/OneDrive/Desktop/TCC_2/Comex_data"
classificacao <- readRDS(file.path(pasta, "classificacao_sh4.rds"))
dados_comex   <- readRDS(file.path(pasta, "base_comex_rs_tempo.rds"))

# Amostra: só exportação para os EUA, tira a Seção 232 (não varia no período)
df <- dados_comex %>%
  filter(pais == 249) %>%
  left_join(classificacao, by = "SH4") %>%
  mutate(grupo = coalesce(grupo, "controle")) %>%
  filter(grupo != "secao_232") %>%
  mutate(
    log_export = log(export_total),                  # log, não o valor bruto (explico abaixo)
    tarifado   = as.integer(grupo == "tarifado_2025")
  )

# ------------------------------------------------------------
# O MODELO
#
#   tarifado * (pos1 + pos_free + pos2)
#
# é um "diferenças-em-diferenças": compara a variação das
# exportações de quem foi tarifado com a variação de quem não
# foi, separadamente em cada período (tarifa valendo, tarifa
# suspensa, nova tarifa). "| SH4 + data" são os efeitos fixos:
# tiram de cada produto seu nível médio próprio (SH4) e tiram
# de cada mês o que afetou todo mundo igual naquele mês (data,
# ex: câmbio) - assim o que sobra pra explicar é só a diferença
# entre tarifados e controle.
#
# A variável é log(exportação), não a exportação em R$/US$ direto,
# porque os produtos têm escalas muito diferentes (um exporta
# US$ 500 mil, outro US$ 50 milhões) - em log, o coeficiente vira
# direto uma variação PERCENTUAL, e dá pra comparar produtos de
# tamanhos diferentes na mesma regressão.
# ------------------------------------------------------------

m_produto <- feols(
  log_export ~ tarifado * (pos1 + pos_free + pos2) | SH4 + data,
  data    = df,
  cluster = ~SH4    # erro-padrão agrupado por produto (mais correto quando
)                   # os mesmos 502 produtos aparecem repetidos mês a mês

summary(m_produto)

# ------------------------------------------------------------
# COMO LER O RESULTADO
#
# O coeficiente está em log, então converte pra % assim:
#   100 * (exp(coeficiente) - 1)
# Ex.: coeficiente -0,71  ->  -51%  (queda de 51% nas exportações)
#
# O QUE ESPERAR:
#   - Sinal NEGATIVO em tarifado:pos1: é o que a teoria prevê -
#     uma tarifa encarece o produto lá fora, então espera-se
#     menos exportação do grupo tarifado, em relação ao controle,
#     durante a vigência da tarifa (pos1).
#   - Sinal deve ENFRAQUECER (ficar mais perto de zero) em
#     pos_free: nesse período a tarifa foi suspensa, então se a
#     queda em pos1 realmente veio da tarifa, ela deveria
#     desaparecer aqui. Se pos_free continuar tão negativo quanto
#     pos1, é sinal de alerta (pode ser que a diferença entre os
#     grupos não seja só a tarifa, mas outra coisa que os
#     distingue - ex: sazonalidade diferente).
#   - Estrelas (*, **, ***) do lado do coeficiente = significância
#     estatística. Duas ou três estrelas = a chance de esse
#     resultado ser só coincidência (ruído da amostra) é baixa
#     (< 5% ou < 1%).
# ------------------------------------------------------------

cat("\nVariação percentual estimada:\n")
print(round(100 * (exp(coef(m_produto)) - 1), 1))

# ------------------------------------------------------------
# R² e Within R² (aparecem no fim do summary):
#
#   R² comum: quanto da variação total do log(exportação) o
#   modelo INTEIRO explica, incluindo os efeitos fixos de produto
#   e mês. Costuma vir alto (aqui, ~0,78) só porque "cada produto
#   tem seu próprio nível médio" (SH4) já explica muita coisa -
#   isso não quer dizer que a tarifa tenha um efeito forte.
#
#   Within R²: quanto do que sobra DEPOIS de tirar os efeitos
#   fixos (ou seja, só a parte que varia dentro de cada produto
#   ao longo do tempo) é explicado pela tarifa. Esse número é
#   baixo por natureza (aqui, ~0,001) porque a tarifa é só UM
#   entre inúmeros fatores que fazem a exportação de um produto
#   variar mês a mês (câmbio, demanda, clima etc.) - o R² baixo
#   não invalida o resultado, só mostra que a tarifa não é a
#   única coisa que mexe com as exportações (o que é óbvio e
#   esperado).
# ------------------------------------------------------------
