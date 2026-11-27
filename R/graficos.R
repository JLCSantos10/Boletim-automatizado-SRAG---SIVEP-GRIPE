# =============================================================================
# Versão interativa (plotly) dos gráficos do boletim
#
# Regra: os gráficos do Word continuam sendo os ggplot de funcoes_boletim.R.
# Aqui eles são apenas convertidos, para que eixos, escalas, cores e rótulos
# sejam exatamente os mesmos nos dois lugares.
#
# Duas exceções precisam ser montadas direto em plotly, porque o ggplotly não
# suporta o recurso usado:
#   - casos + incidência  -> eixo y secundário (sec_axis)
#   - Rt                  -> composição de painéis do patchwork
# =============================================================================

suppressPackageStartupMessages({
  library(plotly)
})

CONFIG_PLOTLY <- list(
  displaylogo = FALSE,
  locale = "pt-BR",
  toImageButtonOptions = list(format = "png", scale = 2),
  modeBarButtonsToRemove = c("select2d", "lasso2d", "autoScale2d",
                             "hoverClosestCartesian", "hoverCompareCartesian")
)

# plotly::config() nao aceita splice de lista; este atalho aplica a configuracao
cfg_plotly <- function(p) do.call(plotly::config, c(list(p), CONFIG_PLOTLY))

#' Converte um ggplot para plotly preservando o desenho original.
#' A legenda (caption) do ggplot é reposta como anotação, porque o ggplotly a descarta.
para_plotly <- function(g, altura = NULL, legenda_embaixo = FALSE) {
  if (is.null(g)) return(NULL)

  legenda <- tryCatch(g$labels$caption, error = function(e) NULL)
  subtitulo <- tryCatch(g$labels$subtitle, error = function(e) NULL)

  p <- tryCatch(
    plotly::ggplotly(g, tooltip = c("x", "y", "fill", "colour", "label"), height = altura),
    error = function(e) NULL
  )
  if (is.null(p)) return(NULL)

  margem <- list(l = 70, r = 70, b = 70, t = if (is.null(subtitulo)) 30 else 56)
  if (!is.null(legenda) && nzchar(legenda)) margem$b <- margem$b + 34

  p <- p %>%
    plotly::layout(
      margin = margem,
      hovermode = "closest",
      legend = if (legenda_embaixo) {
        list(orientation = "h", y = -0.25, x = 0)
      } else {
        list(orientation = "v", y = 0.5, yanchor = "middle")
      }
    )

  anotacoes <- list()
  if (!is.null(subtitulo) && nzchar(subtitulo)) {
    anotacoes[[length(anotacoes) + 1L]] <- list(
      x = 0, y = 1.045, xref = "paper", yref = "paper", xanchor = "left",
      text = gsub("\n", " ", subtitulo), showarrow = FALSE,
      font = list(size = 11, color = "#555555")
    )
  }
  if (!is.null(legenda) && nzchar(legenda)) {
    anotacoes[[length(anotacoes) + 1L]] <- list(
      x = 0, y = -0.22, xref = "paper", yref = "paper", xanchor = "left",
      text = gsub("\n", " ", legenda), showarrow = FALSE,
      font = list(size = 10, color = "#666666")
    )
  }
  if (length(anotacoes) > 0) p <- plotly::layout(p, annotations = anotacoes)

  cfg_plotly(p)
}

# -----------------------------------------------------------------------------
# Casos + incidência + nowcasting (montado direto em plotly: eixo y duplo)
# Mesmas cores do ggplot: barras #1f77b4, incidência vermelha, nowcast preto tracejado.
# -----------------------------------------------------------------------------
pl_incidencia <- function(B, I, nc = NULL, altura = NULL) {

  if (!B$tem_populacao) {
    return(pl_aviso(paste0("Incidência não calculada: ", B$motivo_sem_populacao, ".")))
  }

  df <- I$incidencia
  nw <- dados_nowcast_semana(B, I, nc)
  sem <- B$semanas
  passo <- max(1, ceiling(nrow(sem) / 30))
  idx <- seq(1, nrow(sem), by = passo)

  p <- plotly::plot_ly(height = altura) %>%
    plotly::add_bars(
      data = df, x = ~semana_idx, y = ~casos, name = "Casos",
      color = I("#1f77b4"), marker = list(color = "#1f77b4"), width = 0.9,
      hovertemplate = paste0("SE %{customdata}<br>%{y} casos<extra></extra>"),
      customdata = df$semana_rotulo
    ) %>%
    plotly::add_lines(
      data = df, x = ~semana_idx, y = ~inc, name = "Incidência", yaxis = "y2",
      # color = I(): sem isso o plotly aplica a paleta automática por cima e a
      # linha sai laranja, diferente do vermelho usado no boletim em Word
      color = I("red"), line = list(color = "red", width = 2.5),
      hovertemplate = paste0("SE %{customdata}<br>%{y:.1f} /100 mil<extra></extra>"),
      customdata = df$semana_rotulo
    )

  if (nrow(nw) > 0) {
    p <- p %>%
      plotly::add_ribbons(
        data = nw, x = ~semana_idx, ymin = ~ic_inf, ymax = ~ic_sup,
        name = "IC 95% do nowcasting", color = I("rgba(60,60,60,0.18)"),
        line = list(width = 0, color = "rgba(60,60,60,0)"),
        fillcolor = "rgba(60,60,60,0.18)", hoverinfo = "skip", showlegend = FALSE
      ) %>%
      plotly::add_lines(
        data = nw, x = ~semana_idx, y = ~estimado, name = "Casos corrigidos (nowcasting)",
        color = I("black"), line = list(color = "black", width = 2.5, dash = "dash"),
        hovertemplate = paste0("Corrigido: %{y:.0f} casos<br>",
                               "observado: %{customdata:.0f}<extra></extra>"),
        customdata = nw$observado
      )
  }

  subtitulo <- if (is.null(nc)) NULL else nowcast_subtitulo(nc, nrow(nw))

  p %>%
    plotly::layout(
      xaxis = list(title = "Semana epidemiológica",
                   tickmode = "array", tickvals = sem$semana_idx[idx],
                   ticktext = sem$semana_rotulo[idx], tickangle = -45),
      yaxis = list(title = "Número de casos", rangemode = "tozero"),
      yaxis2 = list(title = "Incidência por 100 mil hab.", overlaying = "y",
                    side = "right", rangemode = "tozero",
                    titlefont = list(color = "red"), tickfont = list(color = "red"),
                    showgrid = FALSE),
      legend = list(orientation = "h", y = -0.28, x = 0),
      margin = list(l = 70, r = 80, b = 110, t = if (is.null(subtitulo)) 30 else 60),
      annotations = if (is.null(subtitulo)) NULL else list(list(
        x = 0, y = 1.06, xref = "paper", yref = "paper", xanchor = "left",
        text = subtitulo, showarrow = FALSE, font = list(size = 11, color = "#555555")
      )),
      hovermode = "x unified"
    ) %>%
    cfg_plotly()
}

# -----------------------------------------------------------------------------
# Rt: um painel por território (substitui o patchwork)
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# Rt de UMA localidade: uma faceta por virus.
# O agrupamento e sempre por localidade; o virus e selecao, nao agrupamento.
# -----------------------------------------------------------------------------
pl_rt_local <- function(B, RT, local = NULL, colunas = 2, altura = NULL) {

  d <- if (is.null(RT) || nrow(RT) == 0) NULL
       else if (is.null(local)) RT
       else dplyr::filter(RT, local == !!local)

  if (is.null(d) || nrow(d) == 0) {
    return(pl_aviso(paste0(
      "Sem série suficiente para estimar o Rt aqui: são necessários ao menos ",
      RT_MIN_DIAS, " dias e ", RT_MIN_CASOS, " casos por vírus."), altura))
  }

  virus <- unique(d$virus)
  colunas <- max(1, min(colunas, length(virus)))
  linhas <- ceiling(length(virus) / colunas)

  paineis <- lapply(virus, function(v) {
    dd <- d %>% dplyr::filter(virus == v) %>% dplyr::arrange(data)
    ult <- dd %>% dplyr::filter(casos_janela >= RT_MIN_CASOS_JAN) %>%
      dplyr::slice_max(data, n = 1, with_ties = FALSE)

    p <- plotly::plot_ly(dd) %>%
      plotly::add_ribbons(
        x = ~data, ymin = ~low, ymax = ~high, color = I("rgba(174,199,232,0.45)"),
        line = list(width = 0, color = "rgba(174,199,232,0)"),
        fillcolor = "rgba(174,199,232,0.45)", showlegend = FALSE, hoverinfo = "skip") %>%
      plotly::add_lines(
        x = ~data, y = ~Rt, color = I("#1f77b4"), line = list(width = 2),
        showlegend = FALSE, name = v,
        hovertemplate = paste0("<b>", v, "</b><br>%{x|%d/%m/%Y}",
                               "<br>Rt %{y:.2f}<br>IC 95%: %{customdata}<extra></extra>"),
        customdata = paste0(formatar_dec(dd$low, 2), " - ", formatar_dec(dd$high, 2))) %>%
      plotly::add_segments(
        x = min(dd$data), xend = max(dd$data), y = 1, yend = 1, color = I("red"),
        line = list(color = "red", dash = "dash", width = 1),
        showlegend = FALSE, hoverinfo = "skip")

    if (nrow(ult) > 0) {
      p <- p %>% plotly::add_annotations(
        x = 0.02, y = 0.96, xref = "paper", yref = "paper",
        text = sprintf("<b>Rt %.2f</b> (SE %02d)", ult$Rt, ult$SE),
        showarrow = FALSE, xanchor = "left", yanchor = "top",
        bgcolor = "rgba(255,255,255,0.75)", bordercolor = "#cccccc",
        font = list(size = 11, color = "black"))
    }

    p %>% plotly::layout(
      annotations = list(list(x = 0.5, y = 1.06, xref = "paper", yref = "paper",
                              text = v, showarrow = FALSE,
                              font = list(size = 12, color = "#222222"))),
      xaxis = list(title = ""), yaxis = list(title = "", rangemode = "tozero"))
  })

  altura_total <- altura %||% max(320, 290 * linhas)

  p <- plotly::subplot(paineis, nrows = linhas, shareX = FALSE, shareY = FALSE,
                       margin = c(0.035, 0.035, 0.07, 0.07)) %>%
    plotly::layout(showlegend = FALSE,
                   margin = list(t = 46, b = 46, l = 54, r = 20))

  # altura direto no layout: passar height= em layout() esta depreciado
  p$x$layout$height <- altura_total
  cfg_plotly(p)
}

# -----------------------------------------------------------------------------
# Mensagem quando não há o que desenhar
# -----------------------------------------------------------------------------
pl_aviso <- function(mensagem, altura = NULL) {
  plotly::plot_ly(height = altura) %>%
    plotly::layout(
      xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
      annotations = list(list(
        x = 0.5, y = 0.5, xref = "paper", yref = "paper",
        text = paste(strwrap(mensagem, 70), collapse = "<br>"),
        showarrow = FALSE, font = list(size = 14, color = "#555555")
      )),
      margin = list(t = 20, b = 20)
    ) %>%
    cfg_plotly()
}

# -----------------------------------------------------------------------------
# Atalhos: mesma assinatura dos gráficos ggplot, devolvendo plotly
# -----------------------------------------------------------------------------
pl_casos_por_ano   <- function(B, I, h = NULL) para_plotly(graf_casos_por_ano(B, I), h)
pl_virus_total     <- function(B, I, h = NULL) para_plotly(graf_virus_total(B, I), h)
pl_virus_semana    <- function(B, I, h = NULL) para_plotly(graf_virus_semana(B, I), h, TRUE)
pl_virus_fe        <- function(B, I, h = NULL) para_plotly(graf_virus_fe(B, I), h, TRUE)
pl_hospitais       <- function(B, I, h = NULL) para_plotly(graf_hospitais(B, I), h)
pl_faixa_etaria    <- function(B, I, h = NULL) para_plotly(graf_faixa_etaria(B, I), h)
pl_raca_cor        <- function(B, I, h = NULL) para_plotly(graf_raca_cor(B, I), h)
pl_bebes           <- function(B, I, h = NULL) para_plotly(graf_bebes(B, I), h)
pl_virus_bebes     <- function(B, I, h = NULL) para_plotly(graf_virus_bebes(B, I), h, TRUE)
pl_ventilacao      <- function(B, I, h = NULL) para_plotly(graf_ventilacao(B, I), h)
pl_obitos          <- function(B, I, h = NULL) para_plotly(graf_obitos(B, I), h)
pl_internacao      <- function(B, I, h = NULL) para_plotly(graf_internacao(B, I), h)
pl_evolucao        <- function(B, I, h = NULL) para_plotly(graf_evolucao(B, I), h)
pl_diagrama        <- function(B, D, h = NULL) para_plotly(graf_diagrama(B, D), h)
#' Diagrama por municipio: a altura acompanha o numero de facetas.
pl_diagrama_mun <- function(B, DM, legenda = "", colunas = 3, h = NULL) {
  altura <- h %||% altura_diagrama_municipio(DM, colunas)
  para_plotly(graf_diagrama_municipio(B, DM, legenda = legenda, colunas = colunas), altura)
}

#' Altura em pixels para a grade de facetas do diagrama por municipio.
altura_diagrama_municipio <- function(DM, colunas = 3) {
  linhas <- linhas_diagrama_municipio(DM, colunas)
  max(340, 250 * linhas + 110)   # 110 px para legenda, eixo e nota de rodape
}
