# ============================================================
# 00_PREPARAR_DADOS.R
# Só isso: ler os dados e marcar quais produtos foram tarifados.
# Rode este primeiro. Os outros dois arquivos usam o que ele salva.
# ============================================================

library(readxl)
library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(lubridate)

pasta_listas <- "C:/Users/csalv/OneDrive/Desktop/TCC_2/Listas_ncm"
pasta_comex  <- "C:/Users/csalv/OneDrive/Desktop/TCC_2/Comex_data"
pasta_saida  <- "C:/Users/csalv/OneDrive/Desktop/TCC_2/Comex_data"

# ------------------------------------------------------------
# PARTE 1: quais produtos (SH4) foram tarifados?
#
# As listas do governo usam o código NCM (8 dígitos). A base de
# exportação usa SH4 (só os 4 primeiros dígitos do NCM). Por isso
# a gente sempre corta o NCM para pegar o SH4.
# ------------------------------------------------------------

ler_lista <- function(arquivo, sheet = 1, skip = 0) {
  read_excel(file.path(pasta_listas, arquivo), sheet = sheet,
             col_types = "text", skip = skip) %>%
    rename(NCM = 1) %>%
    transmute(SH4 = str_sub(str_pad(NCM, 8, "left", "0"), 1, 4)) %>%
    distinct(SH4)
}

sh4_tarifado_2025 <- ler_lista("Lista_ncm_13_10_2025.xlsx", sheet = 1, skip = 1)
sh4_secao_232     <- ler_lista("Lista_ncm_16_04_2026.xlsx")

# Cada SH4 recebe UM rótulo:
#   "tarifado_2025" = estava na lista de produtos atingidos
#   "secao_232"     = tem regime tarifário próprio (aço, alumínio, veículos),
#                      não varia no período -> fica de fora da análise
#   "controle"      = todo o resto (não foi tarifado)
classificacao <- bind_rows(sh4_tarifado_2025, sh4_secao_232) %>%
  distinct(SH4) %>%
  mutate(grupo = case_when(
    SH4 %in% sh4_secao_232$SH4     ~ "secao_232",
    SH4 %in% sh4_tarifado_2025$SH4 ~ "tarifado_2025",
    TRUE                            ~ "controle"
  ))

saveRDS(classificacao, file.path(pasta_saida, "classificacao_sh4.rds"))

# ------------------------------------------------------------
# PARTE 2: exportações mensais do RS, por produto (SH4) e destino
# ------------------------------------------------------------

arquivos <- c("EXP_2021_MUN.csv", "EXP_2022_MUN.csv", "EXP_2023_MUN.csv",
              "EXP_2024_MUN.csv", "EXP_2025_MUN.csv", "EXP_2026_MUN.csv")

dados_comex <- map_dfr(arquivos, function(arq) {
  read_delim(file.path(pasta_comex, arq), delim = ";", show_col_types = FALSE) %>%
    filter(SG_UF_MUN == "RS") %>%
    select(CO_ANO, CO_MES, SH4, CO_PAIS, VL_FOB)
}) %>%
  transmute(
    ano   = as.integer(CO_ANO),
    mes   = as.integer(CO_MES),
    SH4   = str_pad(as.character(SH4), 4, "left", "0"),
    pais  = as.integer(CO_PAIS),
    valor = as.numeric(VL_FOB)
  ) %>%
  group_by(ano, mes, SH4, pais) %>%
  summarise(export_total = sum(valor, na.rm = TRUE), .groups = "drop") %>%
  filter(export_total > 0)   # remove meses sem exportação (não dá pra tirar log de zero)

# ------------------------------------------------------------
# PARTE 3: marcar os períodos e o destino EUA
#
#   pos1     = tarifa de 2025 valendo   (ago/2025 a jan/2026)
#   pos_free = tarifa suspensa          (fev/2026 a jun/2026)
#   pos2     = nova tarifa de 2026      (a partir de ago/2026)
#   eua      = 1 se o destino é os Estados Unidos, 0 caso contrário
# ------------------------------------------------------------

dados_comex <- dados_comex %>%
  mutate(
    data     = ymd(paste(ano, mes, "01", sep = "-")),
    pos1     = as.integer(between(data, as.Date("2025-08-01"), as.Date("2026-01-01"))),
    pos_free = as.integer(between(data, as.Date("2026-02-01"), as.Date("2026-06-01"))),
    pos2     = as.integer(data >= as.Date("2026-08-01")),
    eua      = as.integer(pais == 249)
  )

saveRDS(dados_comex, file.path(pasta_saida, "base_comex_rs_tempo.rds"))

message("Pronto. Produtos por grupo:")
print(table(classificacao$grupo))
