# =============================================================================
# Nowcasting: correção do atraso de notificação
#
# NOTA SOBRE O PACOTE SUGERIDO
# ----------------------------
# O pacote nmecsys/nowcasting (FGV/IBRE) é "Predicting Economic Variables using
# Dynamic Factor Models": serve para nowcast de PIB e séries macroeconômicas com
# dados de frequência mista. Ele não modela atraso de notificação, e foi
# removido do CRAN em 25/05/2022 (depende do pacote arquivado 'matlab').
#
# O que o gráfico de casos/incidência precisa é nowcasting EPIDEMIOLÓGICO:
# estimar quantos casos de cada semana ainda vão ser digitados. Isso é feito
# com o triângulo de notificação (reporting triangle), que é o método usado
# pelo InfoGripe/Fiocruz e o implementado aqui.
#
# MÉTODO
# ------
# 1. Para cada caso, calcula-se o atraso em semanas entre a semana dos primeiros
#    sintomas (DT_SIN_PRI) e a data de digitação no SIVEP (DT_DIGITA).
# 2. Nas semanas já "fechadas" (mais antigas que `max_atraso`), estima-se a
#    proporção acumulada de casos digitados com até d semanas de atraso: p_d.
# 3. Para as últimas semanas, o observado é apenas a fração p_d do total:
#       estimado = observado / p_d
# 4. O intervalo vem da variância binomial de uma proporção conhecida:
#       Var(N) = n (1 - p) / p^2
#
# Se a base não tiver DT_DIGITA, a função devolve NULL e o painel usa a média
# móvel como alternativa, deixando isso explícito na legenda.
# =============================================================================

#' Início (domingo) da semana epidemiológica de uma data
inicio_semana_epi <- function(d) {
  d <- as.Date(d)
  d - (lubridate::wday(d) - 1)
}

#' Nowcasting por triângulo de notificação.
#'
#' @param dados data.frame com DT_SIN_PRI e DT_DIGITA
#' @param semanas_corrigir quantas semanas finais recebem correção
#' @param max_atraso horizonte do atraso, em semanas, usado para estimar p_d
#' @param treino_semanas janela de semanas fechadas usada para estimar p_d
#' @param data_referencia data da extração (padrão: maior DT_DIGITA da base)
#'
#' @return lista com `tabela` (uma linha por semana corrigida), `p` (proporção
#'   acumulada por atraso), `erro_relativo` e `disponivel`
nowcast_atraso <- function(dados,
                           semanas_corrigir = 4,
                           max_atraso = 8,
                           treino_semanas = 104,
                           data_referencia = NULL) {

  vazio <- list(disponivel = FALSE, tabela = NULL, p = NULL,
                erro_relativo = NA_real_, motivo = NULL)

  if (is.null(dados) || nrow(dados) == 0) {
    vazio$motivo <- "sem dados no período"
    return(vazio)
  }
  if (!all(c("DT_SIN_PRI", "DT_DIGITA") %in% names(dados))) {
    vazio$motivo <- "a base não tem a data de digitação (DT_DIGITA)"
    return(vazio)
  }

  d <- dados %>%
    dplyr::filter(!is.na(DT_SIN_PRI), !is.na(DT_DIGITA), DT_DIGITA >= DT_SIN_PRI) %>%
    dplyr::mutate(
      sem_ini = inicio_semana_epi(DT_SIN_PRI),
      atraso  = as.integer(floor(as.numeric(inicio_semana_epi(DT_DIGITA) - sem_ini) / 7))
    ) %>%
    dplyr::filter(atraso >= 0)

  if (nrow(d) < 50) {
    vazio$motivo <- "registros insuficientes com data de digitação"
    return(vazio)
  }

  ref <- data_referencia %||% max(d$DT_DIGITA, na.rm = TRUE)
  ref_sem <- inicio_semana_epi(ref)

  # ----- 1. proporção acumulada de notificação por atraso ---------------------
  # só semanas já fechadas (passou tempo suficiente para o atraso máximo)
  limite_fechada <- ref_sem - (max_atraso + 1) * 7
  inicio_treino  <- limite_fechada - treino_semanas * 7

  treino <- d %>% dplyr::filter(sem_ini <= limite_fechada, sem_ini >= inicio_treino)
  if (nrow(treino) < 50) {
    treino <- d %>% dplyr::filter(sem_ini <= limite_fechada)
  }
  if (nrow(treino) < 50) {
    vazio$motivo <- "histórico insuficiente para estimar o atraso"
    return(vazio)
  }

  tot_treino <- nrow(treino)
  p <- vapply(0:max_atraso, function(k) sum(treino$atraso <= k) / tot_treino, numeric(1))
  p <- cummax(pmin(pmax(p, 1e-6), 1))   # monótona e dentro de (0, 1]
  names(p) <- as.character(0:max_atraso)

  # ----- 2. aplica a correção nas últimas semanas -----------------------------
  obs <- d %>%
    dplyr::count(sem_ini, name = "observado") %>%
    dplyr::arrange(sem_ini)

  semanas_alvo <- obs %>%
    dplyr::filter(sem_ini <= ref_sem) %>%
    dplyr::slice_tail(n = semanas_corrigir)

  if (nrow(semanas_alvo) == 0) {
    vazio$motivo <- "nenhuma semana recente para corrigir"
    return(vazio)
  }

  tabela <- semanas_alvo %>%
    dplyr::mutate(
      semanas_decorridas = as.integer(as.numeric(ref_sem - sem_ini) / 7),
      d_usado = pmin(semanas_decorridas, max_atraso),
      p_acum  = p[as.character(d_usado)],
      estimado = observado / p_acum,
      variancia = observado * (1 - p_acum) / (p_acum^2),
      ic_inf = pmax(observado, estimado - 1.96 * sqrt(variancia)),
      ic_sup = estimado + 1.96 * sqrt(variancia),
      fator = estimado / observado,
      SE  = as.integer(lubridate::epiweek(sem_ini)),
      ano = as.integer(lubridate::epiyear(sem_ini))
    )

  ultima <- tabela[nrow(tabela), ]
  erro_rel <- if (ultima$estimado > 0) {
    100 * (ultima$ic_sup - ultima$estimado) / ultima$estimado
  } else NA_real_

  list(
    disponivel = TRUE,
    tabela = tabela,
    p = p,
    max_atraso = max_atraso,
    data_referencia = ref,
    erro_relativo = erro_rel,
    fator_ultima = ultima$fator,
    motivo = NULL
  )
}

#' Frase para o subtítulo do gráfico.
nowcast_subtitulo <- function(nc, semanas_corrigir) {
  if (is.null(nc) || !isTRUE(nc$disponivel)) {
    motivo <- if (is.null(nc)) "indisponível" else (nc$motivo %||% "indisponível")
    return(paste0("Nowcasting não aplicado (", motivo, ")."))
  }
  paste0(
    "Nowcasting: últimas ", semanas_corrigir, " semanas corrigidas pelo atraso de ",
    "notificação (referência ", format(nc$data_referencia, "%d/%m/%Y"),
    "). Erro estimado na última semana: ±", formatar_dec(nc$erro_relativo, 1),
    "%; fator de correção ", formatar_dec(nc$fator_ultima, 2), "x."
  )
}
