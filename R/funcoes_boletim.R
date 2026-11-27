# =============================================================================
# Funções compartilhadas pelo boletim (BOLETIM_SRAG.Rmd) e pelo painel (app.R)
#
# Tudo o que calcula número ou desenha gráfico mora aqui, para que o painel e o
# documento em Word mostrem exatamente os mesmos resultados.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(readr)
  library(stringr)
  library(stringi)
  library(lubridate)
  library(glue)
  library(scales)
  library(tibble)
  library(forcats)
  library(purrr)
  library(slider)
})

# =============================================================================
# 1. CONSTANTES
# =============================================================================

VIRUS_MAP <- c(
  PCR_FLUASU = "Influenza A",
  PCR_FLUBLI = "Influenza B",
  PCR_SARS2  = "SARS-CoV-2",
  PCR_VSR    = "Vírus Sincicial Respiratório",
  PCR_PARA1  = "Parainfluenza 1",
  PCR_PARA2  = "Parainfluenza 2",
  PCR_PARA3  = "Parainfluenza 3",
  PCR_PARA4  = "Parainfluenza 4",
  PCR_ADENO  = "Adenovírus",
  PCR_METAP  = "Metapneumovírus",
  PCR_BOCA   = "Bocavírus",
  PCR_RINO   = "Rinovírus",
  PCR_OUTRO  = "Outro vírus respiratório"
)

# Codificação da ficha de SRAG (SIVEP-Gripe)
MAPA_SEXO <- c("M" = "Masculino", "F" = "Feminino", "I" = "Ignorado")
NIVEIS_SEXO <- c("Masculino", "Feminino", "Ignorado")
CORES_SEXO <- c("Masculino" = "#2171B5", "Feminino" = "#E7298A", "Ignorado" = "#999999")

MAPA_RACA <- c("1" = "Branca", "2" = "Preta", "3" = "Amarela",
               "4" = "Parda", "5" = "Indígena", "9" = "Ignorada")
NIVEIS_RACA <- c("Branca", "Preta", "Amarela", "Parda", "Indígena", "Ignorada")
CORES_RACA <- c("Branca" = "#9ECAE1", "Preta" = "#08306B", "Amarela" = "#FDAE6B",
                "Parda" = "#6BAED6", "Indígena" = "#74C476", "Ignorada" = "#BDBDBD")

NIVEIS_IDADE <- c("Menor de 2", "2 a 10", "11 a 19", "20 a 29", "30 a 39",
                  "40 a 49", "50 a 59", "60 a 69", "70 a 79", "80 e mais")

CORES_FE1 <- c(
  "80 e mais"  = "#08306B", "70 a 79" = "#08519C", "60 a 69" = "#2171B5",
  "50 a 59"    = "#4292C6", "40 a 49" = "#6BAED6", "30 a 39" = "#9ECAE1",
  "20 a 29"    = "#C6DBEF", "11 a 19" = "#FDD0A2", "2 a 10"  = "#FDAE6B",
  "Menor de 2" = "#F16913"
)

CORES_ZONAS <- c(
  "Zona de controle"       = "#2ECC71",
  "Zona de segurança" = "#F1C40F",
  "Zona de alerta"         = "#E67E22",
  "Zona epidêmica"    = "#E74C3C"
)

NIVEL_PLANO <- c(
  "zona de controle"       = "verde",
  "zona de segurança" = "amarelo",
  "zona de alerta"         = "laranja",
  "zona epidêmica"    = "vermelho"
)

NOMES_UF <- c(
  AC = "Acre", AL = "Alagoas", AP = "Amapá", AM = "Amazonas", BA = "Bahia",
  CE = "Ceará", DF = "Distrito Federal", ES = "Espírito Santo", GO = "Goiás",
  MA = "Maranhão", MT = "Mato Grosso", MS = "Mato Grosso do Sul",
  MG = "Minas Gerais", PA = "Pará", PB = "Paraíba", PR = "Paraná",
  PE = "Pernambuco", PI = "Piauí", RJ = "Rio de Janeiro",
  RN = "Rio Grande do Norte", RS = "Rio Grande do Sul", RO = "Rondônia",
  RR = "Roraima", SC = "Santa Catarina", SP = "São Paulo",
  SE = "Sergipe", TO = "Tocantins"
)

# Parâmetros do Rt (método de Cori et al., 2013) — em DIAS
RT_MEAN_SI        <- 3.5
RT_STD_SI         <- 2.5
RT_MIN_DIAS       <- 21
RT_MIN_CASOS      <- 20
RT_MIN_CASOS_JAN  <- 5

# =============================================================================
# 2. UTILITÁRIOS
# =============================================================================

padronizar_nomes <- function(x) {
  x |>
    as.character() |>
    stringi::stri_trans_general("Latin-ASCII") |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z0-9 ]", " ") |>
    stringr::str_squish()
}

formatar_nome_territorio <- function(x) {
  minusculas <- c("de", "da", "do", "das", "dos", "e")
  vapply(x, function(nome) {
    partes <- strsplit(nome, " ")[[1]]
    if (length(partes) == 0) return(nome)
    partes <- vapply(seq_along(partes), function(i) {
      p <- partes[i]
      if (p %in% c("i", "ii", "iii", "iv", "v", "vi")) return(toupper(p))
      if (i > 1 && p %in% minusculas) return(p)
      paste0(toupper(substr(p, 1, 1)), substr(p, 2, nchar(p)))
    }, character(1))
    paste(partes, collapse = " ")
  }, character(1), USE.NAMES = FALSE)
}

listar_e <- function(x) {
  x <- as.character(x)
  if (length(x) == 0) return("")
  if (length(x) == 1) return(x)
  paste0(paste(x[-length(x)], collapse = ", "), " e ", x[length(x)])
}

# Formatadores vetorizados (servem em texto e em rótulo de gráfico)
formatar_n <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0) return(character(0))
  out <- rep("-", length(x))
  ok <- is.finite(x)
  if (any(ok)) {
    out[ok] <- formatC(round(x[ok]), format = "f", digits = 0,
                       big.mark = ".", decimal.mark = ",")
  }
  out
}

formatar_dec <- function(x, casas = 1) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0) return(character(0))
  out <- rep("-", length(x))
  ok <- is.finite(x)
  if (any(ok)) {
    out[ok] <- formatC(x[ok], format = "f", digits = casas,
                       big.mark = ".", decimal.mark = ",")
  }
  out
}

formatar_pct <- function(prop, casas = 1) paste0(formatar_dec(100 * prop, casas), "%")

vazio <- function(x) {
  is.null(x) || length(x) == 0 || (is.character(x) && all(!nzchar(trimws(x))))
}

# Média móvel que não quebra quando a série é mais curta que a janela
media_movel <- function(x, k = 4) {
  slider::slide_dbl(x, mean, na.rm = TRUE, .before = k - 1, .complete = FALSE)
}

# Colunas PCR -> 0/1/NA (evita comparar texto com número)
padroniza_pcr <- function(df, cols = dplyr::starts_with("PCR_")) {
  df %>%
    dplyr::mutate(
      dplyr::across({{ cols }}, ~ {
        x_chr <- stringr::str_to_upper(stringr::str_trim(as.character(.x)))
        pos <- x_chr %in% c("1", "TRUE", "T", "POSITIVO", "POS", "DETECTADO", "REAGENTE")
        as.integer(dplyr::if_else(is.na(x_chr), NA, pos))
      })
    )
}

# Domingo que inicia a semana epidemiológica `se` do ano `ano`
inicio_se <- function(ano, se) {
  d <- as.Date(paste0(ano, "-01-04"))
  domingo_se1 <- d - (lubridate::wday(d) - 1)
  domingo_se1 + (se - 1) * 7
}

classificar_zona <- function(inc, media, l1, l2) {
  dplyr::case_when(
    is.na(inc)   ~ NA_character_,
    inc <= media ~ "zona de controle",
    inc <= l1    ~ "zona de segurança",
    inc <= l2    ~ "zona de alerta",
    TRUE         ~ "zona epidêmica"
  )
}

# Gráfico-placeholder: mantém a figura no lugar e explica por que está vazia
grafico_vazio <- function(mensagem) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0,
                      label = stringr::str_wrap(mensagem, 70),
                      size = 4.5, colour = "grey25") +
    ggplot2::theme_void()
}

# Escala do eixo de semanas, compartilhada pelos gráficos de série temporal
escala_semanas <- function(semanas, max_rotulos = 30) {
  if (is.null(semanas) || nrow(semanas) == 0) return(ggplot2::scale_x_continuous())
  passo <- max(1, ceiling(nrow(semanas) / max_rotulos))
  idx <- seq(1, nrow(semanas), by = passo)
  ggplot2::scale_x_continuous(
    breaks = semanas$semana_idx[idx],
    labels = semanas$semana_rotulo[idx]
  )
}

tema_boletim <- function(base_size = 13) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(axis.title = ggplot2::element_text(face = "bold"))
}

# =============================================================================
# 3. PARÂMETROS
# =============================================================================

#' Normaliza a configuração vinda do params.yaml OU dos controles do painel.
#' Devolve uma lista com todos os campos resolvidos (período, rótulos, caminhos).
normalizar_params <- function(P) {

  pega <- function(nome, padrao = NULL, alternativo = NULL) {
    v <- P[[nome]]
    if (is.null(v) && !is.null(alternativo)) v <- P[[alternativo]]
    if (is.null(v)) return(padrao)
    if (is.character(v) && length(v) == 1 && !nzchar(trimws(v))) return(padrao)
    v
  }

  cfg <- list()

  # ----- período ------------------------------------------------------------
  cfg$modo_periodo <- tolower(as.character(pega("modo_periodo", "ano")))
  if (!cfg$modo_periodo %in% c("ano", "datas", "semanas")) {
    stop("modo_periodo deve ser \"ano\", \"datas\" ou \"semanas\" (recebido: ",
         cfg$modo_periodo, ").")
  }

  cfg$ano_ref <- as.integer(pega("ano_ref", NA))
  if (is.na(cfg$ano_ref)) stop("Parametro 'ano_ref' nao definido.")

  cfg$se_inicio <- as.integer(pega("se_inicio", 1))
  cfg$se_fim    <- as.integer(pega("se_fim", 53))

  if (cfg$modo_periodo == "ano") {
    cfg$periodo_ini <- as.Date(paste0(cfg$ano_ref, "-01-01"))
    cfg$periodo_fim <- as.Date(paste0(cfg$ano_ref, "-12-31"))
  } else if (cfg$modo_periodo == "datas") {
    cfg$periodo_ini <- suppressWarnings(as.Date(as.character(pega("data_inicio", NA))))
    cfg$periodo_fim <- suppressWarnings(as.Date(as.character(pega("data_fim", NA))))
    if (is.na(cfg$periodo_ini) || is.na(cfg$periodo_fim)) {
      stop("Com modo_periodo: \"datas\" e preciso preencher data_inicio e data_fim (AAAA-MM-DD).")
    }
  } else {
    if (is.na(cfg$se_inicio) || is.na(cfg$se_fim) || cfg$se_inicio < 1 || cfg$se_fim > 53) {
      stop("Com modo_periodo: \"semanas\" e preciso se_inicio e se_fim entre 1 e 53.")
    }
    cfg$periodo_ini <- inicio_se(cfg$ano_ref, cfg$se_inicio)
    cfg$periodo_fim <- inicio_se(cfg$ano_ref, cfg$se_fim) + 6
  }

  if (cfg$periodo_fim < cfg$periodo_ini) {
    stop("O fim do periodo (", cfg$periodo_fim, ") e anterior ao inicio (", cfg$periodo_ini, ").")
  }

  cfg$ano_ref_epi      <- lubridate::epiyear(cfg$periodo_fim)
  cfg$periodo_multiano <- lubridate::epiyear(cfg$periodo_ini) != cfg$ano_ref_epi

  cfg$rotulo_periodo <- switch(
    cfg$modo_periodo,
    "ano"     = as.character(cfg$ano_ref),
    "datas"   = paste0(format(cfg$periodo_ini, "%d/%m/%Y"), " a ",
                       format(cfg$periodo_fim, "%d/%m/%Y")),
    "semanas" = paste0("SE ", sprintf("%02d", cfg$se_inicio), " a SE ",
                       sprintf("%02d", cfg$se_fim), " de ", cfg$ano_ref)
  )
  cfg$rotulo_periodo_frase <- if (cfg$modo_periodo == "ano") {
    paste0("em ", cfg$rotulo_periodo)
  } else {
    paste0("no período de ", cfg$rotulo_periodo)
  }

  # ----- série histórica ----------------------------------------------------
  cfg$filtro_ano   <- as.integer(unlist(pega("filtro_ano", c(cfg$ano_ref - 5, cfg$ano_ref))))
  cfg$ano_hist_ini <- min(cfg$filtro_ano, na.rm = TRUE)
  cfg$ano_hist_fim <- max(cfg$filtro_ano, na.rm = TRUE)

  cfg$anos_historico  <- as.integer(pega("anos_historico", 5))
  cfg$ano_epidemico   <- as.integer(unlist(pega("ano_epidemico", integer(0))))
  cfg$limiares_dp     <- as.numeric(unlist(pega("limiares_dp", c(1, 2, 3))))
  if (length(cfg$limiares_dp) < 3) cfg$limiares_dp <- c(1, 2, 3)
  cfg$semanas_nowcast <- as.integer(pega("semanas_nowcast", 4))
  cfg$diag_max        <- as.integer(pega("diag_max", 12))
  cfg$diag_colunas    <- as.integer(pega("diag_colunas", 3))

  # ----- território ---------------------------------------------------------
  estado <- pega("estado", NULL, alternativo = "uf")
  cfg$estado <- if (vazio(estado)) NULL else toupper(trimws(as.character(estado)[1]))

  # Coluna de UF para bases que nao tenham os niveis de localidade montados.
  # Com eles, montar_contexto usa local_estado (residencia) e ignora esta.
  cfg$coluna_uf        <- as.character(pega("coluna_uf", "SG_UF_NOT"))
  cfg$coluna_municipio <- as.character(pega("coluna_municipio", "ID_MN_RESI_pad"))

  municipio <- pega("municipio", NULL, alternativo = "municipios")
  cfg$municipio <- if (vazio(municipio)) character(0) else as.character(unlist(municipio))
  cfg$usar_filtro_municipio <- isTRUE(pega("usar_filtro_municipio", TRUE)) &&
    length(cfg$municipio) > 0

  cfg$nome_territorio <- pega("nome_territorio", NULL)

  # ----- identificação do boletim (tudo opcional) ---------------------------
  texto_ou_vazio <- function(nome) {
    v <- pega(nome, NULL)
    if (vazio(v)) "" else trimws(paste(as.character(unlist(v)), collapse = "\n"))
  }
  cfg$titulo      <- texto_ou_vazio("titulo")
  cfg$secretaria  <- texto_ou_vazio("secretaria")
  cfg$setor       <- texto_ou_vazio("setor")
  cfg$equipe      <- texto_ou_vazio("equipe")
  cfg$edicao      <- texto_ou_vazio("edicao")
  cfg$se_boletim  <- texto_ou_vazio("se_boletim")
  cfg$ano_boletim <- texto_ou_vazio("ano_boletim")

  # ----- filtros de demografia e de selecao de casos -------------------------
  lista_ou_vazio <- function(nome) {
    v <- pega(nome, NULL)
    if (vazio(v)) character(0) else as.character(unlist(v))
  }
  cfg$sexo          <- lista_ou_vazio("sexo")
  cfg$raca          <- lista_ou_vazio("raca")
  cfg$faixa_etaria  <- lista_ou_vazio("faixa_etaria")
  cfg$virus         <- lista_ou_vazio("virus")

  # ----- agrupamento do grafico de Rt ---------------------------------------
  cfg$rt_nivel <- as.character(pega("rt_nivel", "local_bairro"))
  it <- pega("rt_itens", NULL)
  cfg$rt_itens <- if (vazio(it)) character(0) else as.character(unlist(it))
  vr <- pega("rt_virus", NULL)
  cfg$rt_virus <- if (vazio(vr)) character(0) else as.character(unlist(vr))

  # ----- fonte de dados -----------------------------------------------------
  cfg$fonte <- {
    f <- pega("fonte", "api")
    if (identical(as.character(f), "arquivo")) "arquivo" else "api"
  }
  ap <- pega("arquivo_proprio", NULL)
  cfg$arquivo_proprio <- if (vazio(ap)) "" else as.character(ap)

  # ----- arquivos -----------------------------------------------------------
  cfg$arquivo_srag      <- as.character(pega("arquivo_srag", "Bancos/SRAG.csv"))
  cfg$arquivo_populacao <- as.character(pega("arquivo_populacao", "Bancos/POP.csv"))
  apl <- pega("arquivo_populacao_local", NULL)
  cfg$arquivo_populacao_local <- if (vazio(apl)) NULL else as.character(apl)
  cfg$arquivo_saida <- pega("arquivo_saida", NULL)

  cfg
}

# ler_params() e carregar_bases() agora vivem em R/fontes_dados.R: o painel le
# da API do SUS / do arquivo do usuario e a populacao da API do IBGE.

# Como em montar_contexto, a UF considerada e a de residencia (local_estado),
# caindo para a coluna indicada so quando os niveis de localidade nao existem.
coluna_uf_efetiva <- function(srag, coluna_uf = "SG_UF_NOT") {
  if ("local_estado" %in% names(srag)) "local_estado" else coluna_uf
}

ufs_disponiveis <- function(bases, coluna_uf = "SG_UF_NOT") {
  col <- coluna_uf_efetiva(bases$srag, coluna_uf)
  if (!col %in% names(bases$srag)) return(character(0))
  u <- sort(unique(toupper(trimws(as.character(bases$srag[[col]])))))
  u[!is.na(u) & nzchar(u)]
}

territorios_disponiveis <- function(bases, estado = NULL, coluna_uf = "SG_UF_NOT") {
  d <- bases$srag
  col <- coluna_uf_efetiva(d, coluna_uf)
  if (!is.null(estado) && col %in% names(d)) {
    d <- d[!is.na(d[[col]]) & toupper(trimws(as.character(d[[col]]))) == estado, ]
  }
  names(sort(table(d$municipio), decreasing = TRUE))
}

# =============================================================================
# 5. CONTEXTO DO BOLETIM  (filtros + população + rótulos)
# =============================================================================

preparar_populacao <- function(bases, cfg, territorios_sel, anos_necessarios) {

  avisos <- character(0)
  pop <- NULL
  pop_uf_total <- NULL
  nome_uf_extenso <- NULL

  if (!is.null(bases$pop)) {
    pop <- bases$pop

    col_mun <- dplyr::case_when(
      "municipio"         %in% names(pop) ~ "municipio",
      "id_municipio_nome" %in% names(pop) ~ "id_municipio_nome",
      "nome_municipio"    %in% names(pop) ~ "nome_municipio",
      TRUE ~ NA_character_
    )
    if (is.na(col_mun)) stop("Coluna de municipio nao encontrada em ", cfg$arquivo_populacao)

    pop <- pop %>%
      dplyr::rename(municipio = dplyr::all_of(col_mun)) %>%
      dplyr::mutate(
        municipio = padronizar_nomes(municipio),
        ano       = as.integer(ano),
        populacao = as.numeric(populacao)
      )

    if (!is.null(cfg$estado) && "sigla_uf" %in% names(pop)) {
      pop <- pop %>% dplyr::filter(toupper(sigla_uf) == cfg$estado)
      if ("sigla_uf_nome" %in% names(pop) && nrow(pop) > 0) {
        nome_uf_extenso <- pop$sigla_uf_nome[1]
      }
    }

    # total vindo da fonte (Codeplan no DF, IBGE nos demais) tem prioridade
    pop_uf_total <- if (!is.null(bases$pop_uf) && nrow(bases$pop_uf) > 0) {
      bases$pop_uf
    } else {
      pop %>% dplyr::group_by(ano) %>%
        dplyr::summarise(populacao = sum(populacao, na.rm = TRUE), .groups = "drop")
    }

    pop <- pop %>% dplyr::select(ano, municipio, populacao)
  }

  # ----- população local (territórios fora do recorte municipal do IBGE) -----
  if (!is.null(bases$pop_local)) {
    pl <- bases$pop_local
    if (!all(c("municipio", "populacao") %in% names(pl))) {
      avisos <- c(avisos, paste0(
        "O arquivo ", cfg$arquivo_populacao_local,
        " foi ignorado: faltam as colunas 'municipio' e/ou 'populacao'."
      ))
    } else {
      pl <- pl %>%
        dplyr::mutate(
          municipio = padronizar_nomes(municipio),
          populacao = as.numeric(populacao)
        ) %>%
        dplyr::filter(!is.na(populacao), populacao > 0)

      if (!"ano" %in% names(pl)) {
        pl <- tidyr::expand_grid(ano = anos_necessarios, pl)
      } else {
        pl <- pl %>% dplyr::mutate(ano = as.integer(ano))
        completar <- pl %>%
          dplyr::group_by(municipio) %>%
          dplyr::slice_max(ano, n = 1, with_ties = FALSE) %>%
          dplyr::ungroup() %>%
          dplyr::select(municipio, populacao) %>%
          tidyr::expand_grid(ano = anos_necessarios) %>%
          dplyr::anti_join(pl, by = c("municipio", "ano"))
        pl <- dplyr::bind_rows(pl, completar)
      }

      pl <- pl %>% dplyr::select(ano, municipio, populacao)
      pop <- dplyr::bind_rows(
        pl,
        if (is.null(pop)) NULL else dplyr::anti_join(pop, pl, by = c("ano", "municipio"))
      )
      avisos <- c(avisos, paste0(
        "População local carregada de ", cfg$arquivo_populacao_local,
        " para ", dplyr::n_distinct(pl$municipio), " território(s)."
      ))
    }
  }

  # ----- denominador --------------------------------------------------------
  pop_municipio <- NULL
  pop_total <- NULL
  territorios_sem_pop <- character(0)
  tem_populacao <- FALSE
  motivo <- NULL

  if (is.null(pop)) {
    motivo <- paste0("a base de população não foi encontrada em ",
                     cfg$arquivo_populacao)
  } else if (!cfg$usar_filtro_municipio && !is.null(pop_uf_total) && nrow(pop_uf_total) > 0) {
    # Todos os territórios da UF: o denominador correto é a população total da UF
    pop_total <- pop_uf_total
    pop_municipio <- pop %>% dplyr::filter(municipio %in% territorios_sel)
    territorios_sem_pop <- setdiff(territorios_sel, unique(pop_municipio$municipio))
    tem_populacao <- TRUE
  } else {
    pop_municipio <- pop %>% dplyr::filter(municipio %in% territorios_sel)
    territorios_sem_pop <- setdiff(territorios_sel, unique(pop_municipio$municipio))

    if (length(territorios_sem_pop) == length(territorios_sel)) {
      motivo <- paste0(
        "nenhum dos territórios selecionados (",
        listar_e(formatar_nome_territorio(territorios_sel)),
        ") tem população na base ", cfg$arquivo_populacao
      )
    } else if (length(territorios_sem_pop) > 0) {
      motivo <- paste0("faltam dados de população para ",
                       listar_e(formatar_nome_territorio(territorios_sem_pop)))
    } else {
      pop_total <- pop_municipio %>%
        dplyr::group_by(ano) %>%
        dplyr::summarise(populacao = sum(populacao, na.rm = TRUE), .groups = "drop")
      tem_populacao <- TRUE
    }
  }

  pop_ano_ref <- NA_real_
  if (tem_populacao) {
    v <- pop_total$populacao[pop_total$ano == cfg$ano_ref_epi]
    if (length(v) == 1 && is.finite(v) && v > 0) {
      pop_ano_ref <- v
    } else {
      tem_populacao <- FALSE
      motivo <- paste0("a base de população não tem o ano ", cfg$ano_ref_epi)
    }
  }

  if (!tem_populacao) {
    avisos <- c(avisos, paste0(
      "As seções que dependem de incidência (incidência semanal e ",
      "diagramas de controle) não puderam ser calculadas porque ", motivo,
      ". Para resolver, preencha o arquivo indicado em 'arquivo_populacao_local' ",
      "(modelo em Bancos/POP_LOCAL_MODELO.csv)."
    ))
  }

  list(
    pop = pop, pop_municipio = pop_municipio, pop_total = pop_total,
    pop_ano_ref = pop_ano_ref, tem_populacao = tem_populacao,
    motivo_sem_populacao = motivo, territorios_sem_pop = territorios_sem_pop,
    nome_uf_extenso = nome_uf_extenso, avisos = avisos
  )
}

#' Filtros de demografia e de tipo de virus.
#'
#' Campos vazios nao filtram nada. O filtro de virus mantem os casos com
#' deteccao positiva de pelo menos um dos virus escolhidos.
aplicar_filtros_demograficos <- function(srag, cfg) {

  if (length(cfg$sexo) > 0 && "SEXO" %in% names(srag)) {
    srag <- dplyr::filter(srag, SEXO %in% cfg$sexo)
  }
  if (length(cfg$raca) > 0 && "RACA_COR" %in% names(srag)) {
    srag <- dplyr::filter(srag, RACA_COR %in% cfg$raca)
  }
  if (length(cfg$faixa_etaria) > 0 && "FE1" %in% names(srag)) {
    srag <- dplyr::filter(srag, FE1 %in% cfg$faixa_etaria)
  }

  if (length(cfg$virus) > 0) {
    cols <- names(VIRUS_MAP)[unname(VIRUS_MAP) %in% cfg$virus]
    cols <- intersect(cols, names(srag))
    if (length(cols) > 0) {
      marca <- Reduce(`|`, lapply(cols, function(cc) {
        v <- suppressWarnings(as.integer(as.character(srag[[cc]])))
        !is.na(v) & v == 1
      }))
      srag <- srag[marca, , drop = FALSE]
    }
  }
  srag
}

#' Virus com ao menos uma deteccao positiva por PCR na base carregada.
#'
#' Serve para popular o filtro geral de virus com o que existe de fato nos
#' dados, em ordem de frequencia. E calculado sobre a base inteira - antes do
#' recorte de periodo e do proprio filtro de virus - para que escolher um virus
#' nao esvazie a lista de opcoes.
virus_identificados <- function(srag) {
  cols <- intersect(names(VIRUS_MAP), names(srag))
  if (length(cols) == 0 || nrow(srag) == 0) return(character(0))

  n <- vapply(cols, function(cc) {
    x <- stringr::str_to_upper(stringr::str_trim(as.character(srag[[cc]])))
    sum(x %in% c("1", "TRUE", "T", "POSITIVO", "POS", "DETECTADO", "REAGENTE"))
  }, numeric(1))

  n <- n[is.finite(n) & n > 0]
  if (length(n) == 0) return(character(0))
  unname(VIRUS_MAP[names(sort(n, decreasing = TRUE))])
}

#' Aplica os filtros de UF, território e período e devolve o "contexto" usado
#' por todos os gráficos, textos e tabelas.
montar_contexto <- function(bases, cfg) {

  srag <- bases$srag
  n_total_base <- nrow(srag)

  # ----- UF -----------------------------------------------------------------
  # A analise e por LOCAL DE RESIDENCIA, nao de notificacao: e a residencia que
  # define a populacao do denominador da incidencia. local_estado e a UF de
  # residencia, caindo para a de notificacao so quando aquela nao vem
  # preenchida. A coluna de notificacao fica como alternativa para bases
  # proprias que nao tenham a de residencia.
  col_uf <- if ("local_estado" %in% names(srag)) "local_estado" else cfg$coluna_uf
  cfg$coluna_uf_usada <- col_uf
  n_fora_uf <- 0L

  if (!is.null(cfg$estado)) {
    if (!col_uf %in% names(srag)) {
      stop("Coluna de UF '", col_uf, "' nao existe na base. ",
           "Ajuste 'coluna_uf'. Colunas: ", paste(names(srag), collapse = ", "))
    }
    ufs_base <- sort(unique(toupper(trimws(as.character(srag[[col_uf]])))))
    if (!cfg$estado %in% ufs_base) {
      stop("A UF '", cfg$estado, "' nao aparece na coluna ", col_uf,
           " da base. UFs disponiveis: ", paste(ufs_base, collapse = ", "))
    }
    antes <- nrow(srag)
    srag <- srag %>%
      dplyr::filter(toupper(trimws(.data[[col_uf]])) == cfg$estado)
    n_fora_uf <- antes - nrow(srag)
  }
  n_apos_uf <- nrow(srag)

  # ----- território ---------------------------------------------------------
  territorios_base <- sort(unique(srag$municipio))
  territorios_frequentes <- names(sort(table(srag$municipio), decreasing = TRUE))

  if (cfg$usar_filtro_municipio) {
    sel <- unique(padronizar_nomes(cfg$municipio))
    nao_encontrados <- setdiff(sel, territorios_base)

    if (length(nao_encontrados) > 0) {
      sugestoes <- vapply(nao_encontrados, function(m) {
        cand <- agrep(m, territorios_base, max.distance = 0.25, value = TRUE)
        if (length(cand) == 0) "sem sugestao" else paste(utils::head(cand, 3), collapse = " / ")
      }, character(1))
      stop(
        "Os seguintes territorios nao existem na base",
        if (!is.null(cfg$estado)) paste0(" para a UF ", cfg$estado) else "", ":\n",
        paste0("  - \"", nao_encontrados, "\"  (parecidos: ", sugestoes, ")", collapse = "\n"),
        "\n\nTerritorios com mais casos (de ", length(territorios_base), " disponiveis): ",
        paste(utils::head(territorios_frequentes, 40), collapse = ", "),
        if (length(territorios_base) > 40) ", ..." else ""
      )
    }

    # %in% e nao ==: com == o R recicla o vetor linha a linha e descarta casos
    srag <- srag %>% dplyr::filter(municipio %in% sel)
    territorios_sel <- sel
  } else {
    territorios_sel <- territorios_base
  }

  if (nrow(srag) == 0) stop("Nenhum caso restou apos os filtros de UF/municipio.")

  # ----- filtros de demografia e de virus ------------------------------------
  # Aplicados antes do recorte de periodo, para que a serie historica e o
  # diagrama de controle tratem da mesma populacao mostrada nos graficos.
  srag <- aplicar_filtros_demograficos(srag, cfg)
  if (nrow(srag) == 0) {
    stop("Nenhum caso restou apos os filtros de sexo/raça/faixa etária/vírus. ",
         "Afrouxe os filtros na barra lateral.")
  }

  # ----- período ------------------------------------------------------------
  srag_ref <- srag %>%
    dplyr::filter(DT_SIN_PRI >= cfg$periodo_ini, DT_SIN_PRI <= cfg$periodo_fim)

  if (nrow(srag_ref) == 0) {
    stop("Nenhum caso no periodo de ", format(cfg$periodo_ini, "%d/%m/%Y"), " a ",
         format(cfg$periodo_fim, "%d/%m/%Y"), ". A base tem casos de ",
         format(min(srag$DT_SIN_PRI), "%d/%m/%Y"), " a ",
         format(max(srag$DT_SIN_PRI), "%d/%m/%Y"), ".")
  }

  semanas <- srag_ref %>%
    dplyr::distinct(ano, SE) %>%
    dplyr::arrange(ano, SE) %>%
    dplyr::mutate(
      semana_idx = dplyr::row_number(),
      semana_rotulo = if (cfg$periodo_multiano) paste0(ano, "/", sprintf("%02d", SE))
                      else sprintf("%02d", SE)
    )

  srag_ref <- srag_ref %>% dplyr::left_join(semanas, by = c("ano", "SE"))

  # marcador de óbito, usado em várias seções
  if ("OBITOSRAG" %in% names(srag_ref)) {
    srag_ref <- srag_ref %>% dplyr::mutate(.obito = padronizar_nomes(OBITOSRAG) == "sim")
  } else if ("EVOL1" %in% names(srag_ref)) {
    srag_ref <- srag_ref %>% dplyr::mutate(.obito = padronizar_nomes(EVOL1) == "obito")
  } else {
    srag_ref <- srag_ref %>% dplyr::mutate(.obito = NA)
  }

  # Territorios cuja residencia predominante e a UF escolhida. Casos notificados
  # no DF mas residentes em Goias, por exemplo, entram nos totais da UF (que e
  # como o SIVEP conta), mas nao devem virar facetas do diagrama por municipio.
  territorios_uf <- if (!is.null(cfg$estado) && "local_estado" %in% names(srag)) {
    srag %>%
      dplyr::count(municipio, local_estado) %>%
      dplyr::group_by(municipio) %>%
      dplyr::slice_max(n, n = 1, with_ties = FALSE) %>%
      dplyr::ungroup() %>%
      dplyr::filter(local_estado == cfg$estado) %>%
      dplyr::pull(municipio)
  } else territorios_sel

  # ----- população ----------------------------------------------------------
  anos_necessarios <- sort(unique(c(srag$ano, cfg$ano_ref_epi)))
  pp <- preparar_populacao(bases, cfg, territorios_sel, anos_necessarios)

  # ----- detecções virais ---------------------------------------------------
  cols_pcr <- intersect(names(VIRUS_MAP), names(srag))
  avisos <- pp$avisos
  if (length(cols_pcr) == 0) {
    avisos <- c(avisos, "Nenhuma coluna PCR_* reconhecida: as seções de vírus ficarão vazias.")
  }

  # A diferenca entre o total da base e o total analisado nao e erro: sao
  # notificacoes de quem mora em outra UF. Dito aqui para que ninguem compare o
  # numero do painel com o total bruto e ache que algo se perdeu.
  if (n_fora_uf > 0 && identical(col_uf, "local_estado")) {
    avisos <- c(avisos, paste0(
      formatar_n(n_fora_uf), " notificação(ões) da base são de residentes de outras UFs e ",
      "ficaram de fora: a análise é por local de residência. Casos de residentes ",
      "de ", cfg$estado, " notificados em outras UFs não constam desta base, porque ",
      "a API entrega os dados por UF de notificação."))
  }

  deteccoes <- if (length(cols_pcr) > 0) {
    srag_ref %>%
      dplyr::select(dplyr::any_of(c("SE", "ano", "semana_idx", "semana_rotulo", "municipio",
                                    "local_bairro", "local_municipio", "local_estado",
                                    "FE1", "TP_IDADE", "NU_IDADE_N", "DT_SIN_PRI")),
                    dplyr::all_of(cols_pcr)) %>%
      padroniza_pcr(cols = dplyr::all_of(cols_pcr)) %>%
      tidyr::pivot_longer(dplyr::all_of(cols_pcr), names_to = "virus_col", values_to = "valor") %>%
      dplyr::filter(valor == 1) %>%
      dplyr::mutate(virus_nome = unname(VIRUS_MAP[virus_col])) %>%
      dplyr::filter(!is.na(virus_nome))
  } else {
    srag_ref[0, ] %>%
      dplyr::mutate(virus_col = character(0), valor = integer(0), virus_nome = character(0))
  }

  # ----- rótulos ------------------------------------------------------------
  uf_extenso <- if (!is.null(cfg$estado)) {
    if (!is.na(NOMES_UF[cfg$estado])) unname(NOMES_UF[cfg$estado])
    else if (!is.null(pp$nome_uf_extenso)) pp$nome_uf_extenso
    else cfg$estado
  } else NULL

  nomes_sel <- formatar_nome_territorio(sort(territorios_sel))

  rotulo_territorio <- if (!vazio(cfg$nome_territorio)) {
    as.character(cfg$nome_territorio)
  } else if (!cfg$usar_filtro_municipio) {
    if (is.null(cfg$estado)) "Brasil" else uf_extenso
  } else if (length(nomes_sel) <= 4) {
    paste0(listar_e(nomes_sel),
           if (!is.null(cfg$estado)) paste0(" (", cfg$estado, ")") else "")
  } else {
    paste0(length(nomes_sel), " municípios/territórios",
           if (!is.null(cfg$estado)) paste0(" de ", cfg$estado) else "")
  }

  rotulo_territorio_frase <- paste0("em ", rotulo_territorio)
  if (!cfg$usar_filtro_municipio && identical(rotulo_territorio, "Distrito Federal")) {
    rotulo_territorio_frase <- "no Distrito Federal"
  }

  list(
    cfg = cfg,
    srag = srag, srag_ref = srag_ref, semanas = semanas, deteccoes = deteccoes,
    cols_pcr = cols_pcr,
    territorios_base = territorios_base, territorios_frequentes = territorios_frequentes,
    territorios_sel = territorios_sel, territorios_uf = territorios_uf,
    pop = pp$pop, pop_municipio = pp$pop_municipio, pop_total = pp$pop_total,
    pop_ano_ref = pp$pop_ano_ref, tem_populacao = pp$tem_populacao,
    motivo_sem_populacao = pp$motivo_sem_populacao,
    territorios_sem_pop = pp$territorios_sem_pop,
    rotulo_territorio = rotulo_territorio,
    rotulo_territorio_frase = rotulo_territorio_frase,
    rotulo_periodo = cfg$rotulo_periodo,
    rotulo_periodo_frase = cfg$rotulo_periodo_frase,
    fonte_populacao = bases$fonte_populacao %||% "API do IBGE (SIDRA)",
    total_notificacoes = nrow(srag_ref), total_deteccoes = nrow(deteccoes),
    n_total_base = n_total_base, n_apos_uf = n_apos_uf, n_fora_uf = n_fora_uf,
    coluna_uf_usada = col_uf,
    avisos = avisos
  )
}

# =============================================================================
# 6. INDICADORES
# =============================================================================

calcular_indicadores <- function(B) {

  srag_ref <- B$srag_ref
  det      <- B$deteccoes
  I <- list()

  # --- casos por ano / semana (série histórica) -----------------------------
  I$casos_ano <- B$srag %>%
    dplyr::filter(ano >= B$cfg$ano_hist_ini, ano <= B$cfg$ano_hist_fim) %>%
    dplyr::count(ano, SE, name = "casos") %>%
    tidyr::complete(ano = B$cfg$ano_hist_ini:B$cfg$ano_hist_fim, SE = 1:53,
                    fill = list(casos = 0)) %>%
    dplyr::arrange(ano, SE) %>%
    dplyr::mutate(semana_id = dplyr::row_number())

  # --- incidência semanal ---------------------------------------------------
  I$incidencia <- srag_ref %>%
    dplyr::count(semana_idx, name = "casos") %>%
    dplyr::left_join(B$semanas, by = "semana_idx") %>%
    dplyr::arrange(semana_idx)
  if (B$tem_populacao) {
    I$incidencia <- I$incidencia %>%
      dplyr::mutate(inc = 100000 * casos / B$pop_ano_ref)
  }

  # --- resumo ---------------------------------------------------------------
  casos_semanal <- srag_ref %>% dplyr::count(semana_idx, name = "casos")
  total_obitos  <- sum(srag_ref$.obito, na.rm = TRUE)
  dp <- stats::sd(casos_semanal$casos, na.rm = TRUE)

  I$resumo <- list(
    total       = nrow(srag_ref),
    media       = mean(casos_semanal$casos, na.rm = TRUE),
    dp          = if (is.na(dp)) 0 else dp,
    semanas     = nrow(B$semanas),
    obitos      = total_obitos,
    letalidade  = if (nrow(srag_ref) > 0) 100 * total_obitos / nrow(srag_ref) else NA_real_,
    incidencia  = if (B$tem_populacao) 100000 * nrow(srag_ref) / B$pop_ano_ref else NA_real_,
    populacao   = B$pop_ano_ref,
    deteccoes   = nrow(det)
  )

  # --- vírus ----------------------------------------------------------------
  I$virus_total <- det %>%
    dplyr::count(virus_nome, name = "casos") %>%
    dplyr::mutate(percentual = 100 * casos / sum(casos)) %>%
    dplyr::arrange(dplyr::desc(casos))

  I$virus_semana <- det %>% dplyr::count(semana_idx, virus_nome, name = "casos")

  I$virus_prop <- if (nrow(I$virus_semana) > 0) {
    I$virus_semana %>%
      dplyr::group_by(virus_nome) %>%
      dplyr::summarise(total = sum(casos), .groups = "drop") %>%
      dplyr::mutate(
        percentual    = 100 * total / sum(total),
        virus_legenda = paste0(virus_nome, " (", formatar_dec(percentual, 1), "%)")
      ) %>%
      dplyr::arrange(dplyr::desc(total))
  } else {
    tibble::tibble(virus_nome = character(0), total = numeric(0),
                   percentual = numeric(0), virus_legenda = character(0))
  }

  I$virus_fe <- if ("FE1" %in% names(det)) {
    det %>%
      dplyr::filter(!is.na(FE1), !is.na(semana_idx)) %>%
      dplyr::count(semana_idx, virus_nome, FE1, name = "casos") %>%
      dplyr::mutate(FE1 = factor(FE1, levels = rev(NIVEIS_IDADE)))
  } else det[0, ]

  # --- hospitais ------------------------------------------------------------
  I$hospitais <- if ("NM_UN_INTE" %in% names(srag_ref)) {
    srag_ref %>%
      dplyr::filter(!is.na(NM_UN_INTE)) %>%
      dplyr::count(NM_UN_INTE, name = "casos") %>%
      dplyr::arrange(dplyr::desc(casos)) %>%
      dplyr::mutate(percentual = 100 * casos / sum(casos))
  } else tibble::tibble(NM_UN_INTE = character(0), casos = integer(0), percentual = numeric(0))

  # --- faixa etária ---------------------------------------------------------
  I$faixa <- if ("FE1" %in% names(srag_ref)) {
    srag_ref %>%
      dplyr::filter(!is.na(FE1)) %>%
      dplyr::mutate(FE1 = factor(FE1, levels = NIVEIS_IDADE)) %>%
      dplyr::count(FE1, name = "casos", .drop = FALSE) %>%
      dplyr::mutate(percentual = 100 * casos / sum(casos))
  } else tibble::tibble(FE1 = factor(), casos = integer(0), percentual = numeric(0))

  # --- faixa etaria por sexo -------------------------------------------------
  I$faixa_sexo <- if (all(c("FE1", "SEXO") %in% names(srag_ref))) {
    srag_ref %>%
      dplyr::filter(!is.na(FE1)) %>%
      dplyr::mutate(
        FE1 = factor(FE1, levels = NIVEIS_IDADE),
        SEXO = factor(dplyr::coalesce(SEXO, "Ignorado"), levels = NIVEIS_SEXO)
      ) %>%
      dplyr::count(FE1, SEXO, name = "casos", .drop = FALSE) %>%
      dplyr::filter(!is.na(FE1)) %>%
      dplyr::mutate(percentual = 100 * casos / sum(casos))
  } else tibble::tibble()

  # --- raca/cor --------------------------------------------------------------
  I$raca <- if ("RACA_COR" %in% names(srag_ref)) {
    srag_ref %>%
      dplyr::filter(!is.na(RACA_COR)) %>%
      dplyr::mutate(RACA_COR = factor(RACA_COR, levels = NIVEIS_RACA)) %>%
      dplyr::count(RACA_COR, name = "casos", .drop = FALSE) %>%
      dplyr::filter(casos > 0) %>%
      dplyr::mutate(percentual = 100 * casos / sum(casos))
  } else tibble::tibble()

  # --- sexo ------------------------------------------------------------------
  I$sexo <- if ("SEXO" %in% names(srag_ref)) {
    srag_ref %>%
      dplyr::filter(!is.na(SEXO)) %>%
      dplyr::count(SEXO, name = "casos") %>%
      dplyr::mutate(percentual = 100 * casos / sum(casos)) %>%
      dplyr::arrange(dplyr::desc(casos))
  } else tibble::tibble()

  # --- menores de 2 anos ----------------------------------------------------
  I$bebes_base <- if (all(c("TP_IDADE", "NU_IDADE_N") %in% names(det))) {
    det %>%
      dplyr::filter(TP_IDADE %in% c(1, 2)) %>%
      dplyr::mutate(Tipo = dplyr::case_when(TP_IDADE == 1 ~ "Dias", TP_IDADE == 2 ~ "Meses"))
  } else det[0, ] %>% dplyr::mutate(Tipo = character(0))

  I$bebes <- I$bebes_base %>%
    dplyr::count(Tipo, NU_IDADE_N, name = "n") %>%
    dplyr::group_by(Tipo) %>% dplyr::mutate(prop = n / sum(n)) %>% dplyr::ungroup()

  I$bebes_virus <- I$bebes_base %>%
    dplyr::count(Tipo, NU_IDADE_N, virus_nome, name = "n") %>%
    dplyr::group_by(Tipo, NU_IDADE_N) %>% dplyr::mutate(prop = n / sum(n)) %>% dplyr::ungroup()

  # --- ventilação -----------------------------------------------------------
  I$ventilacao <- if ("SUPORT_VEN2" %in% names(srag_ref)) {
    srag_ref %>%
      dplyr::filter(!is.na(SUPORT_VEN2)) %>%
      dplyr::count(SUPORT_VEN2, name = "casos") %>%
      dplyr::mutate(percentual = 100 * casos / sum(casos)) %>%
      dplyr::arrange(dplyr::desc(casos))
  } else tibble::tibble(SUPORT_VEN2 = character(0), casos = integer(0), percentual = numeric(0))

  # --- óbitos ---------------------------------------------------------------
  I$obitos_semana <- srag_ref %>%
    dplyr::group_by(semana_idx) %>%
    dplyr::summarise(obitos = sum(.obito, na.rm = TRUE), .groups = "drop") %>%
    dplyr::right_join(B$semanas, by = "semana_idx") %>%
    dplyr::mutate(obitos = tidyr::replace_na(obitos, 0)) %>%
    dplyr::arrange(semana_idx)

  I$obitos_detalhe <- srag_ref %>%
    dplyr::filter(.obito, !is.na(FE1)) %>%
    dplyr::count(SE, municipio, FE1, name = "obitos") %>%
    dplyr::mutate(
      municipio = formatar_nome_territorio(municipio),
      FE1 = factor(FE1, levels = NIVEIS_IDADE)
    ) %>%
    dplyr::arrange(SE, municipio, FE1)

  # --- internação -----------------------------------------------------------
  I$internacao <- if (all(c("Tempo_Evolucao", "FE1") %in% names(srag_ref))) {
    srag_ref %>%
      dplyr::filter(!is.na(Tempo_Evolucao), !is.na(FE1)) %>%
      dplyr::mutate(FE1 = factor(FE1, levels = NIVEIS_IDADE))
  } else srag_ref[0, ]

  # --- evolução -------------------------------------------------------------
  I$evolucao <- if ("EVOL1" %in% names(srag_ref)) {
    srag_ref %>%
      dplyr::filter(!is.na(EVOL1)) %>%
      dplyr::count(EVOL1, name = "casos") %>%
      dplyr::mutate(percentual = 100 * casos / sum(casos)) %>%
      dplyr::arrange(dplyr::desc(casos))
  } else tibble::tibble(EVOL1 = character(0), casos = integer(0), percentual = numeric(0))

  I
}

# =============================================================================
# 7. GRÁFICOS
# =============================================================================

graf_casos_por_ano <- function(B, I) {
  df <- I$casos_ano
  if (nrow(df) == 0 || sum(df$casos) == 0) {
    return(grafico_vazio(paste0("Sem casos registrados entre ", B$cfg$ano_hist_ini,
                                " e ", B$cfg$ano_hist_fim, ".")))
  }
  limites <- df %>% dplyr::group_by(ano) %>%
    dplyr::summarise(fim = max(semana_id), meio = mean(semana_id), .groups = "drop")

  ggplot(df, aes(x = semana_id, y = casos)) +
    geom_col(fill = "#1f77b4", width = 0.9) +
    geom_vline(data = limites, aes(xintercept = fim), color = "gray50", linetype = "dotted") +
    scale_x_continuous(breaks = limites$meio, labels = limites$ano, expand = c(0.01, 0)) +
    labs(x = "Ano (barras = semanas epidemiológicas)", y = "Número de casos") +
    tema_boletim() +
    theme(axis.text.x = element_text(face = "bold", color = "black"))
}

#' Casos (barras) + incidência (linha vermelha, eixo à direita) por semana.
#' `nc` é o resultado de nowcast_atraso(): quando disponível, acrescenta a
#' linha tracejada preta com os casos corrigidos pelo atraso de notificação.
graf_incidencia <- function(B, I, nc = NULL) {
  if (!B$tem_populacao) {
    return(grafico_vazio(paste0(
      "Incidência não calculada: ", B$motivo_sem_populacao,
      ". Preencha o arquivo de população local."
    )))
  }
  df <- I$incidencia
  nw <- dados_nowcast_semana(B, I, nc)

  topo <- max(c(df$casos, nw$estimado), na.rm = TRUE)
  fator <- topo / max(df$inc, na.rm = TRUE)

  g <- ggplot(df, aes(x = semana_idx)) +
    geom_col(aes(y = casos), fill = "#1f77b4", width = 0.9) +
    geom_line(aes(y = inc * fator), color = "red", linewidth = 1.2)

  if (nrow(nw) > 0) {
    g <- g +
      geom_ribbon(data = nw, aes(x = semana_idx, ymin = ic_inf, ymax = ic_sup),
                  inherit.aes = FALSE, fill = "grey30", alpha = 0.18) +
      geom_line(data = nw, aes(x = semana_idx, y = estimado),
                inherit.aes = FALSE, colour = "black", linewidth = 1,
                linetype = "dashed")
  }

  g +
    scale_y_continuous(
      name = "Número de casos",
      sec.axis = sec_axis(~ . / fator, name = "Incidência por 100 mil hab.")
    ) +
    escala_semanas(B$semanas) +
    labs(x = "Semana epidemiológica",
         subtitle = if (is.null(nc)) NULL else stringr::str_wrap(nowcast_subtitulo(nc, nrow(nw)), 100)) +
    tema_boletim() +
    theme(
      axis.title.y.right = element_text(color = "red", face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", color = "black"),
      plot.subtitle = element_text(size = 9, colour = "grey30")
    )
}

#' Alinha o resultado do nowcasting ao eixo de semanas do período selecionado.
dados_nowcast_semana <- function(B, I, nc) {
  vazio <- tibble::tibble(semana_idx = numeric(0), estimado = numeric(0),
                          ic_inf = numeric(0), ic_sup = numeric(0))
  if (is.null(nc) || !isTRUE(nc$disponivel) || is.null(nc$tabela)) return(vazio)

  nc$tabela %>%
    dplyr::inner_join(B$semanas, by = c("ano", "SE")) %>%
    dplyr::select(semana_idx, observado, estimado, ic_inf, ic_sup, fator) %>%
    dplyr::arrange(semana_idx)
}

graf_virus_total <- function(B, I) {
  df <- I$virus_total
  if (nrow(df) == 0) return(grafico_vazio("Nenhuma detecção viral por PCR no período."))
  df <- df %>% dplyr::mutate(virus_nome = forcats::fct_reorder(virus_nome, casos))

  ggplot(df, aes(x = virus_nome, y = casos)) +
    geom_col(fill = "#4E79A7") +
    geom_text(aes(label = paste0(formatar_n(casos), " (", formatar_dec(percentual, 1), "%)")),
              vjust = -0.3, size = 4) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(x = "Tipo de vírus", y = "Número de detecções") +
    tema_boletim() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

graf_virus_semana <- function(B, I) {
  if (nrow(I$virus_semana) == 0) {
    return(grafico_vazio("Nenhuma detecção viral por PCR no período."))
  }
  df <- I$virus_semana %>%
    dplyr::left_join(I$virus_prop, by = "virus_nome") %>%
    dplyr::group_by(semana_idx) %>%
    dplyr::mutate(prop = casos / sum(casos)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(virus_legenda = factor(virus_legenda, levels = I$virus_prop$virus_legenda))

  ggplot(df, aes(x = semana_idx, y = prop, fill = virus_legenda)) +
    geom_col(width = 0.95) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    escala_semanas(B$semanas) +
    scale_fill_brewer(palette = "Set3") +
    labs(x = "Semana epidemiológica", y = "Proporção das detecções",
         fill = "Tipo de vírus\n(participação no período)") +
    tema_boletim() +
    theme(
      legend.title = element_text(face = "bold"),
      legend.text  = element_text(size = 9),
      axis.text.x  = element_text(angle = 45, hjust = 1)
    )
}

graf_virus_fe <- function(B, I) {
  df <- I$virus_fe
  if (nrow(df) == 0) {
    return(grafico_vazio("Sem detecções virais com faixa etária preenchida."))
  }
  ggplot(df, aes(x = semana_idx, y = casos, fill = FE1)) +
    geom_col(position = "stack", width = 0.9) +
    facet_wrap(~ virus_nome, ncol = 2, scales = "free_y") +
    scale_fill_manual(values = CORES_FE1, name = "Faixa etária", drop = FALSE) +
    escala_semanas(B$semanas, max_rotulos = 15) +
    labs(x = "Semana epidemiológica", y = "Número de casos") +
    tema_boletim(12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
}

graf_hospitais <- function(B, I, maximo = 25) {
  df <- I$hospitais
  if (nrow(df) == 0) {
    return(grafico_vazio("Sem informação de unidade de internação (NM_UN_INTE)."))
  }
  ggplot(utils::head(df, maximo), aes(x = casos, y = reorder(NM_UN_INTE, casos))) +
    geom_col(fill = "forestgreen", alpha = 0.8) +
    labs(x = "Número de casos", y = "Unidade de internação") +
    tema_boletim(11)
}

#' Casos por faixa etária, com o sexo empilhado dentro de cada barra.
#' A altura da barra continua sendo o total da faixa, e o rótulo em % segue
#' sendo a participação da faixa no total — o desenho original não muda.
graf_faixa_etaria <- function(B, I) {
  df <- I$faixa
  if (nrow(df) == 0 || sum(df$casos) == 0) {
    return(grafico_vazio("Sem informação de faixa etária (FE1)."))
  }

  ds <- I$faixa_sexo
  if (is.null(ds) || nrow(ds) == 0 || sum(ds$casos) == 0) {
    g <- ggplot(df, aes(x = FE1, y = casos)) +
      geom_col(fill = "#4E79A7")
  } else {
    g <- ggplot(ds, aes(x = FE1, y = casos, fill = SEXO)) +
      geom_col(width = 0.8) +
      scale_fill_manual(values = CORES_SEXO, name = "Sexo", drop = FALSE)
  }

  g +
    geom_text(data = df, aes(x = FE1, y = casos, label = paste0(formatar_dec(percentual, 1), "%")),
              inherit.aes = FALSE, vjust = -0.3, size = 4) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(x = "Faixa etária", y = "Número de casos") +
    tema_boletim() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "right")
}

#' Casos por raça/cor: barras verticais com número e proporção.
graf_raca_cor <- function(B, I) {
  df <- I$raca
  if (is.null(df) || nrow(df) == 0 || sum(df$casos) == 0) {
    return(grafico_vazio("Sem informação de raça/cor (campo CS_RACA) no período."))
  }
  ggplot(df, aes(x = RACA_COR, y = casos, fill = RACA_COR)) +
    geom_col(width = 0.75) +
    geom_text(aes(label = paste0(formatar_n(casos), "\n(", formatar_dec(percentual, 1), "%)")),
              vjust = -0.25, size = 3.8, lineheight = 0.95) +
    scale_fill_manual(values = CORES_RACA, guide = "none", drop = FALSE) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
    labs(x = "Raça/cor", y = "Número de casos") +
    tema_boletim()
}

graf_bebes <- function(B, I) {
  df <- I$bebes
  if (nrow(df) == 0) {
    return(grafico_vazio("Sem casos de menores de 2 anos com detecção viral no período."))
  }
  ggplot(df, aes(x = NU_IDADE_N, y = prop)) +
    geom_col(fill = "steelblue") +
    geom_text(aes(label = formatar_pct(prop, 1)), vjust = -0.3, size = 3.2) +
    facet_wrap(~ Tipo, scales = "free") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                       expand = expansion(mult = c(0, 0.12))) +
    labs(x = "Idade (dias ou meses de vida)", y = "Proporção dos casos") +
    tema_boletim(12)
}

graf_virus_bebes <- function(B, I) {
  df <- I$bebes_virus
  if (nrow(df) == 0) {
    return(grafico_vazio("Sem casos de menores de 2 anos com detecção viral no período."))
  }
  ggplot(df, aes(x = NU_IDADE_N, y = prop, fill = virus_nome)) +
    geom_col(position = "fill") +
    facet_wrap(~ Tipo, scales = "free_x") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_fill_brewer(palette = "Set3") +
    labs(x = "Idade (dias ou meses de vida)", y = "Proporção das detecções",
         fill = "Vírus detectado") +
    tema_boletim(12) +
    theme(legend.title = element_text(face = "bold"), legend.text = element_text(size = 9),
          axis.text.x = element_text(angle = 45, hjust = 1))
}

graf_ventilacao <- function(B, I) {
  df <- I$ventilacao
  if (nrow(df) == 0 || sum(df$casos) == 0) {
    return(grafico_vazio("Sem informação de suporte ventilatório (SUPORT_VEN2)."))
  }
  ggplot(df, aes(x = reorder(SUPORT_VEN2, -casos), y = casos)) +
    geom_col(fill = "#E15759") +
    geom_text(aes(label = paste0(formatar_dec(percentual, 1), "%")), vjust = -0.3, size = 4.5) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(x = "Tipo de suporte ventilatório", y = "Número de casos") +
    tema_boletim() +
    theme(axis.text.x = element_text(angle = 20, hjust = 1))
}

graf_obitos <- function(B, I) {
  df <- I$obitos_semana
  if (nrow(df) == 0 || sum(df$obitos) == 0) {
    return(grafico_vazio("Nenhum óbito por SRAG registrado no período."))
  }
  ggplot(df, aes(x = semana_idx, y = obitos)) +
    geom_col(fill = "#B22222", width = 0.9) +
    escala_semanas(B$semanas) +
    labs(x = "Semana epidemiológica", y = "Número de óbitos") +
    tema_boletim() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

graf_internacao <- function(B, I) {
  df <- I$internacao
  if (nrow(df) == 0) {
    return(grafico_vazio("Sem informação de tempo de internação (Tempo_Evolucao)."))
  }
  ggplot(df, aes(x = FE1, y = Tempo_Evolucao)) +
    geom_boxplot(fill = "#59A14F", outlier.alpha = 0.3) +
    labs(x = "Faixa etária", y = "Tempo de internação (dias)") +
    tema_boletim() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

graf_evolucao <- function(B, I) {
  df <- I$evolucao
  if (nrow(df) == 0 || sum(df$casos) == 0) {
    return(grafico_vazio("Sem informação de evolução do caso (EVOL1)."))
  }
  ggplot(df, aes(x = reorder(EVOL1, casos), y = casos)) +
    geom_col(fill = "#F28E2B") +
    geom_text(aes(label = paste0(formatar_dec(percentual, 1), "%")), hjust = -0.1, size = 4.5) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    coord_flip() +
    labs(x = "Desfecho", y = "Número de casos") +
    tema_boletim()
}

# =============================================================================
# 8. DIAGRAMA DE CONTROLE
# =============================================================================

calcular_diagrama <- function(B) {
  cfg <- B$cfg
  anos_hist <- setdiff(seq(cfg$ano_ref_epi - cfg$anos_historico, cfg$ano_ref_epi - 1),
                       cfg$ano_epidemico)
  anos_hist <- anos_hist[anos_hist >= min(B$srag$ano, na.rm = TRUE)]

  D <- list(anos_hist = anos_hist, ok = FALSE, limiares = NULL,
            atual = NULL, nowcast = NULL)

  D$legenda <- paste0(
    "Limiares calculados com os anos ", listar_e(as.character(anos_hist)),
    if (length(cfg$ano_epidemico) > 0) {
      paste0(" (excluído(s) o(s) ano(s) epidêmico(s) ",
             listar_e(as.character(cfg$ano_epidemico)), ")")
    } else "",
    ". Faixas: média, média + ", cfg$limiares_dp[1], "DP, + ",
    cfg$limiares_dp[2], "DP e + ", cfg$limiares_dp[3], "DP."
  )

  if (!B$tem_populacao || length(anos_hist) < 2) return(D)

  # semanas históricas sem caso entram como zero: descartá-las elevaria a média
  hist_se <- B$srag %>%
    dplyr::filter(ano %in% anos_hist) %>%
    dplyr::count(ano, SE, name = "casos") %>%
    tidyr::complete(ano = anos_hist, SE = 1:52, fill = list(casos = 0)) %>%
    dplyr::left_join(B$pop_total, by = "ano") %>%
    dplyr::filter(!is.na(populacao), populacao > 0) %>%
    dplyr::mutate(inc = 100000 * casos / populacao)

  if (dplyr::n_distinct(hist_se$ano) < 2) return(D)

  D$limiares <- hist_se %>%
    dplyr::group_by(SE) %>%
    dplyr::summarise(media = mean(inc, na.rm = TRUE),
                     dp = stats::sd(inc, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(
      dp    = tidyr::replace_na(dp, 0),
      lim_1 = media + cfg$limiares_dp[1] * dp,
      lim_2 = media + cfg$limiares_dp[2] * dp,
      lim_3 = media + cfg$limiares_dp[3] * dp
    )

  D$atual <- B$srag_ref %>%
    dplyr::count(SE, name = "casos") %>%
    dplyr::arrange(SE) %>%
    dplyr::mutate(inc = 100000 * casos / B$pop_ano_ref,
                  inc_ma = media_movel(inc, k = 4))

  ultima <- max(D$atual$SE, na.rm = TRUE)
  D$nowcast <- D$atual %>%
    dplyr::filter(SE > ultima - cfg$semanas_nowcast, SE <= ultima) %>%
    dplyr::mutate(inc_corrigida = inc_ma,
                  ic_low = inc_corrigida * 0.9,
                  ic_up  = inc_corrigida * 1.1) %>%
    tidyr::drop_na(inc_corrigida)

  D$ok <- TRUE
  D
}

graf_diagrama <- function(B, D) {
  if (!D$ok) {
    return(grafico_vazio(paste0(
      "Diagrama de controle não calculado: ",
      if (!B$tem_populacao) B$motivo_sem_populacao
      else paste0("são necessários ao menos 2 anos de histórico; disponíveis: ",
                  if (length(D$anos_hist) == 0) "nenhum" else listar_e(as.character(D$anos_hist))),
      "."
    )))
  }

  p <- ggplot() +
    geom_ribbon(data = D$limiares, aes(x = SE, ymin = 0, ymax = media,
                                       fill = "Zona de controle"), alpha = 0.35) +
    geom_ribbon(data = D$limiares, aes(x = SE, ymin = media, ymax = lim_1,
                                       fill = "Zona de segurança"), alpha = 0.35) +
    geom_ribbon(data = D$limiares, aes(x = SE, ymin = lim_1, ymax = lim_2,
                                       fill = "Zona de alerta"), alpha = 0.35) +
    geom_ribbon(data = D$limiares, aes(x = SE, ymin = lim_2, ymax = lim_3,
                                       fill = "Zona epidêmica"), alpha = 0.35) +
    geom_line(data = D$atual, aes(x = SE, y = inc, color = "Incidência observada"),
              linewidth = 1.2)

  if (!is.null(D$nowcast) && nrow(D$nowcast) > 0) {
    p <- p +
      geom_ribbon(data = D$nowcast, aes(x = SE, ymin = ic_low, ymax = ic_up),
                  fill = "gray40", alpha = 0.3) +
      geom_line(data = D$nowcast,
                aes(x = SE, y = inc_corrigida,
                    color = "Incidência corrigida (média móvel)"),
                linewidth = 1.1, linetype = "22")
  }

  p +
    scale_fill_manual(values = CORES_ZONAS, breaks = names(CORES_ZONAS)) +
    scale_color_manual(values = c("Incidência observada" = "#1F4E99",
                                  "Incidência corrigida (média móvel)" = "#8B0000")) +
    labs(x = "Semana epidemiológica", y = "Incidência por 100 mil hab.",
         fill = "Faixas de risco", color = "",
         caption = stringr::str_wrap(D$legenda, 110)) +
    tema_boletim() +
    theme(legend.position = "right", plot.caption = element_text(hjust = 0, size = 9))
}

#' Diagrama de controle por municipio.
#'
#' @param max_municipios quantas facetas no maximo, em ordem de numero de casos.
#'   Sem esse limite, uma UF inteira vira uma parede de graficos ilegivel.
calcular_diagrama_municipio <- function(B, max_municipios = 12) {
  cfg <- B$cfg
  anos_hist <- setdiff(seq(cfg$ano_ref_epi - cfg$anos_historico, cfg$ano_ref_epi - 1),
                       cfg$ano_epidemico)
  anos_hist <- anos_hist[anos_hist >= min(B$srag$ano, na.rm = TRUE)]

  DM <- list(ok = FALSE, limiares = NULL, atual = NULL, nowcast = NULL,
             anos_hist = anos_hist, n_disponiveis = 0L, n_mostrados = 0L)

  if (!B$tem_populacao || is.null(B$pop_municipio) || nrow(B$pop_municipio) == 0 ||
      length(anos_hist) < 2) return(DM)

  com_pop <- intersect(B$territorios_sel, unique(B$pop_municipio$municipio))

  # Com a UF inteira selecionada, so entram os territorios da propria UF: os
  # residentes de outros estados notificados aqui nao sao municipios daqui.
  if (!isTRUE(cfg$usar_filtro_municipio) && length(B$territorios_uf %||% character(0)) > 0) {
    com_pop <- intersect(com_pop, B$territorios_uf)
  }
  if (length(com_pop) == 0) return(DM)

  DM$n_disponiveis <- length(com_pop)

  # Mantem os territorios com mais casos no periodo
  if (length(com_pop) > max_municipios) {
    ordem <- B$srag_ref %>%
      dplyr::filter(municipio %in% com_pop) %>%
      dplyr::count(municipio, name = "casos") %>%
      dplyr::arrange(dplyr::desc(casos))
    com_pop <- utils::head(ordem$municipio, max_municipios)
  }
  DM$n_mostrados <- length(com_pop)

  hist_mun <- B$srag %>%
    dplyr::filter(ano %in% anos_hist, municipio %in% com_pop) %>%
    dplyr::count(ano, SE, municipio, name = "casos") %>%
    tidyr::complete(ano = anos_hist, SE = 1:52, municipio = com_pop, fill = list(casos = 0)) %>%
    dplyr::left_join(B$pop_municipio, by = c("ano", "municipio")) %>%
    dplyr::filter(!is.na(populacao), populacao > 0) %>%
    dplyr::mutate(inc = 100000 * casos / populacao)

  if (nrow(hist_mun) == 0 || dplyr::n_distinct(hist_mun$ano) < 2) return(DM)

  DM$limiares <- hist_mun %>%
    dplyr::group_by(municipio, SE) %>%
    dplyr::summarise(media = mean(inc), dp = stats::sd(inc), .groups = "drop") %>%
    dplyr::mutate(
      dp    = tidyr::replace_na(dp, 0),
      lim_1 = media + cfg$limiares_dp[1] * dp,
      lim_2 = media + cfg$limiares_dp[2] * dp,
      lim_3 = media + cfg$limiares_dp[3] * dp
    )

  pop_ref <- B$pop_municipio %>%
    dplyr::filter(ano == cfg$ano_ref_epi) %>%
    dplyr::select(municipio, populacao)

  DM$atual <- B$srag_ref %>%
    dplyr::filter(municipio %in% com_pop) %>%
    dplyr::count(municipio, SE, name = "casos") %>%
    dplyr::left_join(pop_ref, by = "municipio") %>%
    dplyr::filter(!is.na(populacao), populacao > 0) %>%
    dplyr::mutate(inc = 100000 * casos / populacao) %>%
    dplyr::arrange(municipio, SE) %>%
    dplyr::group_by(municipio) %>%
    dplyr::mutate(inc_ma = media_movel(inc, k = 4)) %>%
    dplyr::ungroup()

  if (nrow(DM$atual) == 0) return(DM)

  ultima <- max(DM$atual$SE, na.rm = TRUE)
  DM$nowcast <- DM$atual %>%
    dplyr::filter(SE > ultima - cfg$semanas_nowcast, SE <= ultima) %>%
    dplyr::mutate(inc_corrigida = inc_ma,
                  ic_low = inc_corrigida * 0.9,
                  ic_up  = inc_corrigida * 1.1) %>%
    tidyr::drop_na(inc_corrigida)

  DM$ok <- TRUE
  DM
}

graf_diagrama_municipio <- function(B, DM, legenda = "", colunas = 3) {
  if (!DM$ok) {
    return(grafico_vazio(paste0(
      "Diagrama por município não calculado: ",
      if (!B$tem_populacao) B$motivo_sem_populacao
      else "não há população por território e histórico suficiente",
      "."
    )))
  }

  p <- ggplot() +
    geom_ribbon(data = DM$limiares, aes(x = SE, ymin = 0, ymax = media,
                                        fill = "Zona de controle"), alpha = 0.35) +
    geom_ribbon(data = DM$limiares, aes(x = SE, ymin = media, ymax = lim_1,
                                        fill = "Zona de segurança"), alpha = 0.35) +
    geom_ribbon(data = DM$limiares, aes(x = SE, ymin = lim_1, ymax = lim_2,
                                        fill = "Zona de alerta"), alpha = 0.35) +
    geom_ribbon(data = DM$limiares, aes(x = SE, ymin = lim_2, ymax = lim_3,
                                        fill = "Zona epidêmica"), alpha = 0.35) +
    geom_line(data = DM$atual, aes(x = SE, y = inc, color = "Incidência observada"),
              linewidth = 1)

  if (nrow(DM$nowcast) > 0) {
    p <- p +
      geom_ribbon(data = DM$nowcast, aes(x = SE, ymin = ic_low, ymax = ic_up),
                  fill = "gray40", alpha = 0.3) +
      geom_line(data = DM$nowcast,
                aes(x = SE, y = inc_corrigida,
                    color = "Incidência corrigida (média móvel)"),
                linewidth = 1, linetype = "22")
  }

  p <- p +
    scale_fill_manual(values = CORES_ZONAS, breaks = names(CORES_ZONAS)) +
    scale_color_manual(values = c("Incidência observada" = "#1F4E99",
                                  "Incidência corrigida (média móvel)" = "#8B0000")) +
    labs(x = "Semana epidemiológica", y = "Incidência por 100 mil hab.",
         fill = "Faixas de risco", color = "",
         caption = stringr::str_wrap(legenda_diagrama_municipio(DM, legenda), 130)) +
    tema_boletim() +
    theme(legend.position = "right", strip.text = element_text(face = "bold", size = 11),
          plot.caption = element_text(hjust = 0, size = 9))

  if (dplyr::n_distinct(DM$atual$municipio) > 1) {
    p <- p + facet_wrap(~ municipio, scales = "free_y", ncol = colunas,
                        labeller = as_labeller(formatar_nome_territorio))
  }
  p
}

#' Acrescenta a nota sobre o corte de territorios, quando houver.
legenda_diagrama_municipio <- function(DM, legenda = "") {
  n_disp <- DM$n_disponiveis %||% 0L
  n_most <- DM$n_mostrados %||% 0L
  if (n_disp > n_most && n_most > 0) {
    legenda <- paste0(
      "Mostrando os ", n_most, " territórios com mais casos no período, de ", n_disp,
      " com população disponível. ", legenda)
  }
  legenda
}

#' Quantas linhas a grade de facetas vai ter — usado para dimensionar o bloco.
linhas_diagrama_municipio <- function(DM, colunas = 3) {
  n <- if (is.null(DM$atual)) 0L else dplyr::n_distinct(DM$atual$municipio)
  max(1L, ceiling(n / max(1, colunas)))
}

tabela_zonas_municipio <- function(B, DM) {
  if (!DM$ok || nrow(DM$nowcast) == 0) return(tibble::tibble())

  DM$nowcast %>%
    dplyr::left_join(DM$limiares, by = c("municipio", "SE")) %>%
    dplyr::mutate(zona = classificar_zona(inc_corrigida, media, lim_1, lim_2)) %>%
    tidyr::drop_na(zona) %>%
    dplyr::group_by(municipio) %>%
    dplyr::summarise(
      zona = names(sort(table(zona), decreasing = TRUE))[1],
      inc_ultima = dplyr::last(inc_corrigida),
      delta = if (dplyr::n() >= 2) dplyr::last(inc_corrigida) - dplyr::nth(inc_corrigida, -2) else 0,
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      tendencia = dplyr::case_when(delta < -0.5 ~ "declínio",
                                   delta >  0.5 ~ "aumento",
                                   TRUE ~ "estabilidade"),
      nivel = unname(NIVEL_PLANO[zona]),
      Territorio = formatar_nome_territorio(municipio),
      `Zona de risco` = stringr::str_to_sentence(zona),
      `Incidência corrigida` = formatar_dec(inc_ultima, 1),
      `Tendência` = tendencia,
      `Nível do plano` = nivel
    ) %>%
    dplyr::arrange(dplyr::desc(inc_ultima)) %>%
    dplyr::select(Territorio, `Zona de risco`, `Incidência corrigida`,
                  `Tendência`, `Nível do plano`)
}

# =============================================================================
# 9. Rt  (Cori et al., 2013 — sobre série DIÁRIA)
# =============================================================================

#' Rt por grupo, pelo metodo de Cori et al. (2013).
#'
#' @param agrupar "virus" (padrao) = uma serie por virus, somando os territorios
#'                selecionados; "local" = uma serie por localidade, somando os virus.
#' @param nivel   usado quando agrupar = "local": "local_bairro", "local_municipio"
#'                ou "local_estado".
#' @param itens   quais grupos calcular (NULL = todos).
#'
#' Em qualquer modo sai UMA linha por painel, que e o que mantem o grafico legivel.
#' Rotulo do nivel de localidade.
rotulo_nivel_rt <- function(nivel) {
  unname(NIVEIS_LOCALIDADE[nivel %||% "local_bairro"]) %||% "Localidade"
}

#' Rt por localidade e virus, pelo metodo de Cori et al. (2013).
#'
#' O agrupamento e SEMPRE por localidade; o virus e uma selecao, nao um
#' agrupamento. Cada combinacao localidade x virus vira uma serie, e e isso
#' que alimenta as facetas de dentro de cada localidade.
#'
#' @param nivel  "local_bairro", "local_municipio" ou "local_estado"
#' @param locais quais localidades calcular (NULL = todas)
#' @param virus  quais virus (NULL = todos os presentes)
#' @param por_semana guarda um ponto por semana epidemiologica
calcular_rt <- function(B, nivel = "local_bairro", locais = NULL, virus = NULL,
                        por_semana = TRUE) {

  det <- B$deteccoes
  if (is.null(det) || nrow(det) == 0 || !"DT_SIN_PRI" %in% names(det)) return(NULL)
  if (!requireNamespace("EpiEstim", quietly = TRUE)) return(NULL)

  nivel <- nivel %||% "local_bairro"
  if (!nivel %in% names(det)) return(NULL)

  det$local <- det[[nivel]]
  det <- det[!is.na(det$local) & nzchar(det$local), , drop = FALSE]
  if (!is.null(locais) && length(locais) > 0) det <- det[det$local %in% locais, , drop = FALSE]
  if (!is.null(virus)  && length(virus)  > 0) det <- det[det$virus_nome %in% virus, , drop = FALSE]
  if (nrow(det) == 0) return(NULL)

  serie <- det %>% dplyr::count(local, virus_nome, DT_SIN_PRI, name = "I")

  # Todas as series terminam na mesma data, para que o "Rt mais recente" de
  # cada combinacao seja comparavel.
  data_fim <- min(B$cfg$periodo_fim, max(B$srag_ref$DT_SIN_PRI, na.rm = TRUE))

  uma_serie <- function(df) {
    df <- df %>% dplyr::arrange(DT_SIN_PRI) %>% dplyr::filter(DT_SIN_PRI <= data_fim)
    if (nrow(df) == 0 || sum(df$I) < RT_MIN_CASOS) return(NULL)

    datas <- seq(min(df$DT_SIN_PRI), data_fim, by = "day")
    if (length(datas) < RT_MIN_DIAS) return(NULL)

    incid <- tibble::tibble(dates = datas) %>%
      dplyr::left_join(df %>% dplyr::select(dates = DT_SIN_PRI, I), by = "dates") %>%
      dplyr::mutate(I = tidyr::replace_na(I, 0)) %>%
      as.data.frame()

    # suppressWarnings (e nao handler de warning): o EpiEstim avisa sobre
    # janelas com poucos casos, e tratar o aviso como falha descartaria series validas
    res <- suppressWarnings(tryCatch(
      EpiEstim::estimate_R(
        incid, method = "parametric_si",
        config = EpiEstim::make_config(list(mean_si = RT_MEAN_SI, std_si = RT_STD_SI))
      ),
      error = function(e) NULL
    ))
    if (is.null(res) || is.null(res$R) || nrow(res$R) == 0) return(NULL)

    casos_janela <- mapply(function(a, b) sum(incid$I[a:b]), res$R$t_start, res$R$t_end)

    tibble::tibble(
      local        = df$local[1],
      virus        = df$virus_nome[1],
      data         = incid$dates[res$R$t_end],
      Rt           = res$R$`Mean(R)`,
      low          = res$R$`Quantile.0.025(R)`,
      high         = res$R$`Quantile.0.975(R)`,
      casos_janela = as.numeric(casos_janela)
    )
  }

  out <- serie %>% dplyr::group_split(local, virus_nome) %>% purrr::map_dfr(uma_serie)
  if (is.null(out) || nrow(out) == 0) return(NULL)

  out <- out %>%
    dplyr::filter(is.finite(Rt)) %>%
    # Janela sem nenhum caso devolve a media da distribuicao a priori (Rt = 5),
    # o que desenha uma linha reta alta sem nenhuma informacao por tras.
    dplyr::filter(casos_janela > 0) %>%
    dplyr::mutate(SE = lubridate::epiweek(data), ano_epi = lubridate::epiyear(data),
                  nivel = nivel)

  # A estimativa e diaria em janelas de 7 dias, o que gera uma linha muito
  # ruidosa ao longo de um ano. Guardar um ponto por semana epidemiologica
  # (a ultima janela de cada semana) deixa o grafico legivel sem mudar o metodo.
  if (isTRUE(por_semana)) {
    out <- out %>%
      dplyr::group_by(local, virus, ano_epi, SE) %>%
      dplyr::slice_max(data, n = 1, with_ties = FALSE) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(local, virus, data)
  }
  out
}

#' Quantos casos cada localidade tem, para o cabecalho da sanfona.
resumo_locais_rt <- function(B, nivel = "local_bairro", locais = NULL, virus = NULL) {
  det <- B$deteccoes
  if (is.null(det) || nrow(det) == 0 || !nivel %in% names(det)) return(tibble::tibble())
  d <- det
  d$local <- d[[nivel]]
  d <- d[!is.na(d$local) & nzchar(d$local), , drop = FALSE]
  if (!is.null(locais) && length(locais) > 0) d <- d[d$local %in% locais, , drop = FALSE]
  if (!is.null(virus)  && length(virus)  > 0) d <- d[d$virus_nome %in% virus, , drop = FALSE]
  if (nrow(d) == 0) return(tibble::tibble())
  d %>%
    dplyr::group_by(local) %>%
    dplyr::summarise(deteccoes = dplyr::n(),
                     n_virus = dplyr::n_distinct(virus_nome), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(deteccoes))
}

#' Grafico de uma localidade: uma faceta por virus.
graf_rt_local <- function(B, RT, local = NULL, colunas = 2) {
  if (is.null(RT) || nrow(RT) == 0) {
    return(grafico_vazio(paste0(
      "Sem série suficiente para estimar o Rt: são necessários ao menos ",
      RT_MIN_DIAS, " dias e ", RT_MIN_CASOS, " casos por vírus nesta localidade."
    )))
  }
  d <- if (is.null(local)) RT else dplyr::filter(RT, local == !!local)
  if (nrow(d) == 0) {
    return(grafico_vazio("Sem série suficiente para estimar o Rt nesta localidade."))
  }

  ultimo <- d %>%
    dplyr::filter(casos_janela >= RT_MIN_CASOS_JAN) %>%
    dplyr::group_by(virus) %>%
    dplyr::slice_max(data, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup()

  g <- ggplot(d, aes(x = data, y = Rt)) +
    geom_ribbon(aes(ymin = low, ymax = high), fill = "#aec7e8", alpha = 0.45) +
    geom_line(colour = "#1f77b4", linewidth = 0.9) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "red") +
    facet_wrap(~ virus, ncol = colunas, scales = "free_y") +
    scale_x_date(date_labels = "%d/%m") +
    labs(x = "Data (fim da janela de 7 dias)", y = "Rt estimado") +
    tema_boletim(12) +
    theme(strip.text = element_text(face = "bold"))

  if (nrow(ultimo) > 0) {
    ultimo <- ultimo %>%
      dplyr::left_join(d %>% dplyr::group_by(virus) %>%
                         dplyr::summarise(topo = max(high, na.rm = TRUE), .groups = "drop"),
                       by = "virus")
    g <- g + geom_label(data = ultimo,
                        aes(x = min(d$data), y = topo, label = sprintf("Rt %.2f", Rt)),
                        hjust = 0, vjust = 1, size = 3.2, colour = "black",
                        fill = "white", linewidth = 0.2, alpha = 0.85)
  }
  g
}

#' Versao para o boletim em Word: grade localidade x virus numa figura so.
graf_rt <- function(B, RT, nivel = NULL, max_locais = 8) {
  if (is.null(RT) || nrow(RT) == 0) {
    return(grafico_vazio(paste0(
      "Não foi possível estimar o Rt no período: são necessárias séries com ",
      "pelo menos ", RT_MIN_DIAS, " dias e ", RT_MIN_CASOS, " casos por localidade e vírus."
    )))
  }

  ordem <- RT %>% dplyr::count(local, wt = casos_janela, name = "peso") %>%
    dplyr::arrange(dplyr::desc(peso))
  manter <- utils::head(ordem$local, max_locais)
  d <- RT %>% dplyr::filter(local %in% manter) %>%
    dplyr::mutate(local_rot = formatar_nome_territorio(local),
                  local_rot = factor(local_rot,
                                     levels = formatar_nome_territorio(manter)))

  rodape <- if (nrow(ordem) > max_locais) {
    paste0("Mostrando as ", max_locais, " localidades com mais detecções, de ",
           nrow(ordem), " selecionadas.")
  } else NULL

  ggplot(d, aes(x = data, y = Rt)) +
    geom_ribbon(aes(ymin = low, ymax = high), fill = "#aec7e8", alpha = 0.45) +
    geom_line(colour = "#1f77b4", linewidth = 0.8) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "red") +
    facet_grid(local_rot ~ virus, scales = "free_y", switch = "y") +
    scale_x_date(date_labels = "%d/%m") +
    labs(x = "Data (fim da janela de 7 dias)", y = "Rt estimado", caption = rodape) +
    tema_boletim(10) +
    theme(strip.text.x = element_text(face = "bold", size = 9),
          strip.text.y.left = element_text(face = "bold", size = 9, angle = 0),
          plot.caption = element_text(hjust = 0, size = 8))
}

interpretar_rt <- function(rt) {
  dplyr::case_when(
    rt > 2     ~ "transmissão muito intensa, com rápida expansão dos casos",
    rt > 1.05  ~ "tendência de crescimento da transmissão",
    rt >= 0.95 ~ "estabilidade, com número de casos semelhante ao das semanas anteriores",
    rt >= 0.7  ~ "redução gradual da transmissão",
    TRUE       ~ "forte redução da transmissão"
  )
}

#' Rt mais recente por localidade e virus.
tabela_rt <- function(RT, nivel = NULL) {
  if (is.null(RT) || nrow(RT) == 0) return(tibble::tibble())
  # So janelas com casos suficientes: a ultima janela de uma serie encerrada
  # traz Rt instavel e nao descreve a situacao atual.
  d <- RT %>%
    dplyr::filter(casos_janela >= RT_MIN_CASOS_JAN) %>%
    dplyr::group_by(local, virus) %>%
    dplyr::slice_max(data, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup()
  if (nrow(d) == 0) return(tibble::tibble())

  d %>%
    dplyr::mutate(
      Localidade = formatar_nome_territorio(local),
      Semana = sprintf("SE %02d", SE),
      `Casos na janela` = formatar_n(casos_janela),
      `Rt (IC 95%)` = paste0(formatar_dec(Rt, 2), " (", formatar_dec(low, 2),
                             " - ", formatar_dec(high, 2), ")"),
      `Interpretação` = stringr::str_to_sentence(interpretar_rt(Rt))
    ) %>%
    dplyr::arrange(Localidade, dplyr::desc(Rt)) %>%
    dplyr::select(Localidade, `Vírus` = virus, Semana, `Casos na janela`,
                  `Rt (IC 95%)`, `Interpretação`)
}

# =============================================================================
# 10. TEXTOS INTERPRETATIVOS
# =============================================================================

txt_ondas <- function(B, I) {
  picos <- I$casos_ano %>%
    dplyr::group_by(ano) %>%
    dplyr::summarise(total = sum(casos), pico = max(casos, na.rm = TRUE),
                     semana_pico = SE[which.max(casos)], .groups = "drop") %>%
    dplyr::filter(total > 0) %>%
    dplyr::arrange(ano)

  if (nrow(picos) == 0) return("")
  if (nrow(picos) == 1) {
    return(as.character(glue::glue(
      "A série histórica configurada contém apenas o ano de {picos$ano[1]}, com ",
      "{formatar_n(picos$total[1])} casos e pico na semana epidemiológica ",
      "{picos$semana_pico[1]}. Amplie o intervalo em `filtro_ano` para comparar anos."
    )))
  }

  p1 <- picos[1, ]; p2 <- picos[nrow(picos), ]
  ano_maior <- picos$ano[which.max(picos$total)]
  as.character(glue::glue(
    "A distribuição semanal dos casos de SRAG {B$rotulo_territorio_frase} entre ",
    "{p1$ano} e {p2$ano} evidencia um padrão sazonal recorrente, com diferenças no ",
    "momento e na intensidade das ondas epidêmicas. Em {p1$ano}, o pico de notificações ",
    "ocorreu na semana epidemiológica {p1$semana_pico}, com {formatar_n(p1$pico)} casos; ",
    "em {p2$ano}, o pico foi registrado na semana {p2$semana_pico}, com ",
    "{formatar_n(p2$pico)} casos. O ano com maior volume total de notificações da série ",
    "foi {ano_maior} ({formatar_n(max(picos$total))} casos). Essas variações reforçam a ",
    "importância do monitoramento contínuo para a identificação antecipada de ",
    "períodos críticos."
  ))
}

txt_resumo <- function(B, I) {
  r <- I$resumo
  variabilidade <- dplyr::case_when(
    r$dp < 5  ~ "baixa variabilidade na distribuição semanal",
    r$dp < 15 ~ "moderada variabilidade na distribuição semanal",
    TRUE      ~ "alta variabilidade na distribuição semanal"
  )

  frase_obitos <- if (r$obitos == 0) {
    "Não foram registrados óbitos por SRAG no período analisado."
  } else if (r$obitos == 1) {
    glue::glue("Observou-se um único óbito atribuído à SRAG, o que corresponde a ",
               "uma letalidade de {formatar_dec(r$letalidade, 1)}%.")
  } else {
    glue::glue("Foram registrados {formatar_n(r$obitos)} óbitos por SRAG, com letalidade ",
               "de {formatar_dec(r$letalidade, 1)}%.")
  }

  frase_inc <- if (B$tem_populacao) {
    glue::glue(" Considerando a população de referência de {formatar_n(r$populacao)} ",
               "habitantes, a incidência acumulada no período foi de ",
               "{formatar_dec(r$incidencia, 1)} casos por 100 mil habitantes.")
  } else ""

  as.character(glue::glue(
    "{stringr::str_to_sentence(B$rotulo_periodo_frase)}, foram notificados ",
    "{formatar_n(r$total)} casos de Síndrome Respiratória Aguda Grave (SRAG) ",
    "{B$rotulo_territorio_frase}, com média de {formatar_dec(r$media, 1)} casos por semana ",
    "epidemiológica e desvio-padrão de {formatar_dec(r$dp, 1)}, indicando {variabilidade}. ",
    "{frase_obitos}{frase_inc} O padrão observado reforça a importância da manutenção ",
    "da vigilância epidemiológica ativa."
  ))
}

txt_virus <- function(B, I) {
  d <- I$virus_total
  if (nrow(d) == 0) return("")
  texto <- glue::glue(
    "{stringr::str_to_sentence(B$rotulo_periodo_frase)}, entre os casos de SRAG com detecção ",
    "positiva por PCR {B$rotulo_territorio_frase}, o agente mais frequente foi o ",
    "{d$virus_nome[1]}, responsável por {formatar_dec(d$percentual[1], 1)}% das ",
    "{formatar_n(sum(d$casos))} detecções."
  )
  if (nrow(d) >= 3) {
    texto <- glue::glue(texto, " Em seguida, destacaram-se o {d$virus_nome[2]} ",
                        "({formatar_dec(d$percentual[2], 1)}%) e o {d$virus_nome[3]} ",
                        "({formatar_dec(d$percentual[3], 1)}%).")
  } else if (nrow(d) == 2) {
    texto <- glue::glue(texto, " O segundo agente mais frequente foi o {d$virus_nome[2]} ",
                        "({formatar_dec(d$percentual[2], 1)}%).")
  }
  as.character(glue::glue(
    texto, " Os demais vírus apresentaram participação minoritária. Esses dados ",
    "indicam o papel do {d$virus_nome[1]} na circulação viral respiratória do período, ",
    "com evidência de cocirculação de múltiplos agentes etiológicos."
  ))
}

txt_virus_semana <- function(B, I) {
  if (nrow(I$virus_semana) == 0) return("")
  pico <- I$virus_semana %>%
    dplyr::group_by(semana_idx) %>% dplyr::summarise(total = sum(casos), .groups = "drop") %>%
    dplyr::slice_max(total, n = 1, with_ties = FALSE) %>%
    dplyr::left_join(B$semanas, by = "semana_idx")

  texto <- glue::glue(
    "A circulação viral ao longo das semanas epidemiológicas analisadas ",
    "({B$semanas$semana_rotulo[1]} a {B$semanas$semana_rotulo[nrow(B$semanas)]}) teve pico de ",
    "detecções na semana {pico$semana_rotulo}, com {formatar_n(pico$total)} registros. ",
    "O {I$virus_prop$virus_nome[1]} foi o agente predominante"
  )
  if (nrow(I$virus_prop) >= 2) {
    texto <- glue::glue(texto, ", seguido pelo {I$virus_prop$virus_nome[2]}")
  }
  as.character(glue::glue(
    texto, ". O acompanhamento semanal permite identificar a substituição de agentes ao ",
    "longo do tempo e antecipar períodos de maior demanda assistencial."
  ))
}

txt_virus_fe <- function(B, I) {
  d <- I$virus_fe
  if (nrow(d) == 0) return("")
  v <- d %>% dplyr::group_by(virus_nome) %>% dplyr::summarise(total = sum(casos), .groups = "drop") %>%
    dplyr::slice_max(total, n = 1, with_ties = FALSE)
  p <- d %>% dplyr::group_by(semana_idx) %>% dplyr::summarise(total = sum(casos), .groups = "drop") %>%
    dplyr::slice_max(total, n = 1, with_ties = FALSE) %>%
    dplyr::left_join(B$semanas, by = "semana_idx")
  f <- d %>% dplyr::group_by(FE1) %>% dplyr::summarise(total = sum(casos), .groups = "drop") %>%
    dplyr::slice_max(total, n = 1, with_ties = FALSE)

  as.character(glue::glue(
    "O gráfico mostra a distribuição dos casos com detecção viral por semana ",
    "epidemiológica, estratificados por tipo de vírus e faixa etária. O ",
    "{v$virus_nome} foi o vírus mais frequentemente identificado no período, totalizando ",
    "{formatar_n(v$total)} casos. O pico geral ocorreu na semana {p$semana_rotulo}, com ",
    "{formatar_n(p$total)} casos considerando todos os vírus combinados. Em relação ao ",
    "perfil etário, a faixa que mais contribuiu foi \"{as.character(f$FE1)}\". Cada vírus ",
    "apresenta padrões próprios de sazonalidade e atinge faixas etárias específicas, ",
    "o que reforça a importância da vigilância laboratorial."
  ))
}

txt_hospitais <- function(B, I) {
  d <- I$hospitais
  if (nrow(d) == 0) return("")
  top <- utils::head(d, 2)
  texto <- glue::glue(
    "{stringr::str_to_sentence(B$rotulo_periodo_frase)}, as internações por SRAG ",
    "{B$rotulo_territorio_frase} distribuíram-se por {formatar_n(nrow(d))} unidades de saúde. ",
    "A unidade com maior volume foi {top$NM_UN_INTE[1]}, responsável por ",
    "{formatar_dec(top$percentual[1], 1)}% das internações do período"
  )
  if (nrow(top) >= 2) {
    texto <- glue::glue(texto, ", seguida por {top$NM_UN_INTE[2]}, com ",
                        "{formatar_dec(top$percentual[2], 1)}%")
  }
  as.character(glue::glue(
    texto, ". A concentração em poucas unidades costuma refletir a capacidade instalada ",
    "e a definição regional de referência para SRAG."
  ))
}

txt_faixa <- function(B, I) {
  d <- I$faixa
  if (nrow(d) == 0 || sum(d$casos) == 0) return("")
  top2 <- d %>% dplyr::arrange(dplyr::desc(casos)) %>% utils::head(2)
  pct_idosos <- sum(d$percentual[as.character(d$FE1) %in% c("60 a 69", "70 a 79", "80 e mais")])
  pct_criancas <- sum(d$percentual[as.character(d$FE1) %in% c("Menor de 2", "2 a 10")])

  # participacao por sexo, quando o campo estiver preenchido
  frase_sexo <- ""
  if (!is.null(I$sexo) && nrow(I$sexo) > 0) {
    sx <- I$sexo %>% dplyr::filter(as.character(SEXO) != "Ignorado")
    if (nrow(sx) >= 2) {
      frase_sexo <- as.character(glue::glue(
        " Quanto ao sexo, {sx$SEXO[1]} responderam por {formatar_dec(sx$percentual[1], 1)}% ",
        "dos casos e {sx$SEXO[2]}, por {formatar_dec(sx$percentual[2], 1)}%."
      ))
    }
  }

  perfil <- if (pct_criancas > pct_idosos) "um perfil predominantemente pediátrico"
            else if (pct_idosos > pct_criancas) "um perfil concentrado em idosos"
            else "uma distribuição equilibrada entre crianças e idosos"

  segunda <- if (nrow(top2) >= 2) {
    glue::glue(" e a faixa \"{as.character(top2$FE1[2])}\", {formatar_dec(top2$percentual[2], 1)}%")
  } else ""

  as.character(glue::glue(
    "{stringr::str_to_sentence(B$rotulo_periodo_frase)}, a distribuição etária dos casos ",
    "de SRAG {B$rotulo_territorio_frase} evidencia {perfil}. A faixa ",
    "\"{as.character(top2$FE1[1])}\" concentrou {formatar_dec(top2$percentual[1], 1)}% dos ",
    "registros{segunda}. As crianças de até 10 anos responderam por ",
    "{formatar_dec(pct_criancas, 1)}% dos casos e as pessoas de 60 anos ou mais, por ",
    "{formatar_dec(pct_idosos, 1)}%.{frase_sexo} Esse padrão orienta a priorização das ",
    "ações de vigilância e de vacinação nos grupos mais atingidos."
  ))
}

txt_raca <- function(B, I) {
  d <- I$raca
  if (is.null(d) || nrow(d) == 0 || sum(d$casos) == 0) return("")
  d <- d %>% dplyr::arrange(dplyr::desc(casos)) %>%
    dplyr::mutate(RACA_COR = as.character(RACA_COR))

  ignorada <- d$percentual[d$RACA_COR == "Ignorada"]
  informados <- d %>% dplyr::filter(RACA_COR != "Ignorada")

  texto <- glue::glue(
    "Entre os {formatar_n(sum(d$casos))} casos de SRAG com raça/cor registrada ",
    "{B$rotulo_territorio_frase} {B$rotulo_periodo_frase}, a categoria mais frequente foi ",
    "**{d$RACA_COR[1]}** ({formatar_dec(d$percentual[1], 1)}%)"
  )
  if (nrow(d) >= 2) {
    texto <- glue::glue(texto, ", seguida por {d$RACA_COR[2]} ",
                        "({formatar_dec(d$percentual[2], 1)}%)")
  }
  if (nrow(d) >= 3) {
    texto <- glue::glue(texto, " e {d$RACA_COR[3]} ({formatar_dec(d$percentual[3], 1)}%)")
  }
  texto <- glue::glue(texto, ".")

  if (length(ignorada) == 1 && ignorada >= 5) {
    texto <- glue::glue(
      texto, " A proporção de registros com raça/cor ignorada ",
      "({formatar_dec(ignorada, 1)}%) limita a leitura do perfil racial e indica ",
      "oportunidade de melhoria no preenchimento da ficha de notificação."
    )
  }
  if (nrow(informados) > 0) {
    texto <- glue::glue(
      texto, " A comparação entre grupos deve considerar a composição ",
      "racial da população do território, que não está refletida ",
      "neste gráfico de números absolutos."
    )
  }
  as.character(texto)
}

txt_bebes <- function(B, I) {
  d <- I$bebes
  if (nrow(d) == 0) return("")
  # with_ties = FALSE: sem isso, empates geram várias linhas e o texto é repetido
  pd <- d %>% dplyr::filter(Tipo == "Dias")  %>% dplyr::slice_max(prop, n = 1, with_ties = FALSE)
  pm <- d %>% dplyr::filter(Tipo == "Meses") %>% dplyr::slice_max(prop, n = 1, with_ties = FALSE)

  texto <- glue::glue(
    "Entre os menores de 2 anos com detecção viral {B$rotulo_territorio_frase} ",
    "{B$rotulo_periodo_frase}, observa-se concentração nas idades mais precoces."
  )
  if (nrow(pd) == 1) {
    texto <- glue::glue(texto, " Entre os recém-nascidos com idade em dias, o maior número ",
                        "de casos ocorreu aos {pd$NU_IDADE_N} dias de vida ",
                        "({formatar_pct(pd$prop)} desse subgrupo).")
  }
  if (nrow(pm) == 1) {
    texto <- glue::glue(texto, " Entre os lactentes com idade em meses, a maior proporção ",
                        "ocorreu aos {pm$NU_IDADE_N} mês(es) ({formatar_pct(pm$prop)}).")
  }
  as.character(glue::glue(
    texto, " Esse perfil reforça a vulnerabilidade dos menores de 6 meses frente aos vírus ",
    "respiratórios e a importância de estratégias de proteção específicas, como a ",
    "imunização materna e a imunização passiva contra o VSR."
  ))
}

txt_virus_bebes <- function(B, I) {
  if (nrow(I$bebes_base) == 0) return("")
  top <- I$bebes_base %>% dplyr::count(virus_nome, name = "total") %>%
    dplyr::arrange(dplyr::desc(total)) %>%
    dplyr::mutate(percentual = 100 * total / sum(total))

  texto <- glue::glue(
    "Entre os menores de 2 anos com SRAG e detecção viral {B$rotulo_territorio_frase} ",
    "{B$rotulo_periodo_frase}, o {top$virus_nome[1]} foi o agente predominante ",
    "({formatar_dec(top$percentual[1], 1)}% das detecções nesse grupo)"
  )
  if (nrow(top) >= 2) {
    texto <- glue::glue(texto, ", seguido pelo {top$virus_nome[2]} ",
                        "({formatar_dec(top$percentual[2], 1)}%)")
  }
  if (nrow(top) >= 3) {
    texto <- glue::glue(texto, " e pelo {top$virus_nome[3]} ",
                        "({formatar_dec(top$percentual[3], 1)}%)")
  }
  as.character(glue::glue(
    texto, ". Esse padrão reforça a importância das ações preventivas dirigidas aos ",
    "lactentes, incluindo imunização materna e medidas de proteção precoce."
  ))
}

txt_ventilacao <- function(B, I) {
  d <- I$ventilacao
  if (nrow(d) == 0 || sum(d$casos) == 0) return("")

  # As categorias são identificadas pelo rótulo, não pela posição no ranking:
  # ordenar e assumir "o terceiro é quem não usou suporte" troca os números.
  chave <- padronizar_nomes(d$SUPORT_VEN2)
  soma <- function(idx) if (any(idx)) sum(d$percentual[idx]) else NA_real_

  i_nao_inv <- stringr::str_detect(chave, "nao invasivo")
  i_inv     <- stringr::str_detect(chave, "invasivo") & !i_nao_inv
  i_sem     <- chave == "nao"

  pct_nao_inv <- soma(i_nao_inv); pct_inv <- soma(i_inv); pct_sem <- soma(i_sem)

  partes <- c()
  if (!is.na(pct_nao_inv)) partes <- c(partes, glue::glue("{formatar_dec(pct_nao_inv, 1)}% receberam suporte ventilatório não invasivo"))
  if (!is.na(pct_inv))     partes <- c(partes, glue::glue("{formatar_dec(pct_inv, 1)}% necessitaram de ventilação mecânica invasiva"))
  if (!is.na(pct_sem))     partes <- c(partes, glue::glue("{formatar_dec(pct_sem, 1)}% não utilizaram suporte ventilatório"))
  if (length(partes) == 0) {
    partes <- glue::glue("a categoria mais frequente foi \"{d$SUPORT_VEN2[1]}\" ",
                         "({formatar_dec(d$percentual[1], 1)}%)")
  }

  pct_algum <- sum(c(pct_nao_inv, pct_inv), na.rm = TRUE)

  as.character(glue::glue(
    "{stringr::str_to_sentence(B$rotulo_periodo_frase)}, entre os {formatar_n(sum(d$casos))} ",
    "casos de SRAG com informação de suporte ventilatório {B$rotulo_territorio_frase}, ",
    "{listar_e(partes)}. No conjunto, {formatar_dec(pct_algum, 1)}% dos pacientes necessitaram ",
    "de algum tipo de suporte ventilatório, o que reforça a importância do preparo da rede ",
    "assistencial em períodos epidêmicos."
  ))
}

txt_obitos <- function(B, I) {
  d <- I$obitos_semana
  total <- sum(d$obitos)
  if (total == 0) {
    return(as.character(glue::glue(
      "{stringr::str_to_sentence(B$rotulo_periodo_frase)}, não foram registrados óbitos ",
      "por SRAG {B$rotulo_territorio_frase}. O cenário indica ausência de letalidade no ",
      "período analisado, mantendo-se a recomendação de vigilância contínua."
    )))
  }
  pico <- d %>% dplyr::slice_max(obitos, n = 1, with_ties = FALSE)
  as.character(glue::glue(
    "{stringr::str_to_sentence(B$rotulo_periodo_frase)}, foram registrados {formatar_n(total)} ",
    "óbitos por SRAG {B$rotulo_territorio_frase}, distribuídos em ",
    "{formatar_n(sum(d$obitos > 0))} semanas epidemiológicas, com letalidade de ",
    "{formatar_dec(100 * total / B$total_notificacoes, 1)}%. A maior concentração ocorreu na ",
    "semana {pico$semana_rotulo}, com {formatar_n(pico$obitos)} óbito(s). O acompanhamento ",
    "semanal da mortalidade permite identificar mudanças no perfil de gravidade dos casos."
  ))
}

txt_internacao <- function(B, I) {
  d <- I$internacao
  if (nrow(d) == 0) return("")
  resumo <- d %>%
    dplyr::group_by(FE1) %>%
    dplyr::summarise(mediana = stats::median(Tempo_Evolucao, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(!is.na(mediana)) %>%
    dplyr::arrange(dplyr::desc(mediana))
  if (nrow(resumo) == 0) return("")

  med_criancas <- stats::median(
    d$Tempo_Evolucao[as.character(d$FE1) %in% c("Menor de 2", "2 a 10")], na.rm = TRUE)

  texto <- glue::glue(
    "{stringr::str_to_sentence(B$rotulo_periodo_frase)}, a duração das internações por ",
    "SRAG {B$rotulo_territorio_frase} variou entre as faixas etárias. A faixa ",
    "\"{as.character(resumo$FE1[1])}\" apresentou a maior mediana de permanência, de ",
    "{formatar_dec(resumo$mediana[1], 1)} dias"
  )
  if (nrow(resumo) >= 2) {
    texto <- glue::glue(texto, ", seguida por \"{as.character(resumo$FE1[2])}\", com ",
                        "{formatar_dec(resumo$mediana[2], 1)} dias")
  }
  if (!is.na(med_criancas)) {
    texto <- glue::glue(texto, ". Entre as crianças de até 10 anos, a mediana foi de ",
                        "{formatar_dec(med_criancas, 1)} dias")
  }
  as.character(glue::glue(
    texto, ". Tempos de permanência mais longos têm impacto direto na carga assistencial ",
    "e na ocupação de leitos hospitalares."
  ))
}

txt_evolucao <- function(B, I) {
  d <- I$evolucao
  if (nrow(d) == 0 || sum(d$casos) == 0) return("")
  chave <- padronizar_nomes(d$EVOL1)
  pct <- function(regex) {
    i <- stringr::str_detect(chave, regex)
    if (!any(i)) return(NA_real_)
    sum(d$percentual[i])
  }
  cura <- pct("^cura"); ignorado <- pct("ignorado")
  obito <- pct("^obito$"); obito_outras <- pct("outras causas")

  partes <- c()
  if (!is.na(cura))         partes <- c(partes, glue::glue("{formatar_dec(cura, 1)}% evoluíram para cura"))
  if (!is.na(obito))        partes <- c(partes, glue::glue("{formatar_dec(obito, 1)}% para óbito por SRAG"))
  if (!is.na(obito_outras)) partes <- c(partes, glue::glue("{formatar_dec(obito_outras, 1)}% para óbito por outras causas"))
  if (!is.na(ignorado))     partes <- c(partes, glue::glue("{formatar_dec(ignorado, 1)}% permaneceram com evolução ignorada"))

  texto <- glue::glue(
    "Entre os {formatar_n(sum(d$casos))} casos de SRAG com desfecho informado ",
    "{B$rotulo_territorio_frase} {B$rotulo_periodo_frase}, {listar_e(partes)}."
  )
  if (!is.na(ignorado) && ignorado >= 5) {
    texto <- glue::glue(
      texto, " A proporção de registros com evolução ignorada ",
      "({formatar_dec(ignorado, 1)}%) indica oportunidade de melhoria no encerramento ",
      "oportuno das fichas de notificação."
    )
  }
  as.character(texto)
}

txt_diagrama <- function(B, D) {
  if (!D$ok) {
    return(paste0(
      "O diagrama de controle não pôde ser calculado. Verifique a população de ",
      "referência e o parâmetro de anos de histórico."
    ))
  }
  if (is.null(D$nowcast) || nrow(D$nowcast) == 0) {
    return(paste0(
      "Não há semanas suficientes no período selecionado para a correção por atraso ",
      "de notificação. Amplie o intervalo para obter a classificação de risco."
    ))
  }

  zonas <- D$nowcast %>%
    dplyr::left_join(D$limiares, by = "SE") %>%
    dplyr::mutate(zona = classificar_zona(inc_corrigida, media, lim_1, lim_2)) %>%
    tidyr::drop_na(zona)

  zona_predom <- zonas %>% dplyr::count(zona) %>%
    dplyr::slice_max(n, n = 1, with_ties = FALSE) %>% dplyr::pull(zona)
  cor_nome <- unname(NIVEL_PLANO[zona_predom])

  serie <- D$nowcast %>% dplyr::arrange(SE) %>% dplyr::pull(inc_corrigida)
  delta <- if (length(serie) >= 2) serie[length(serie)] - serie[length(serie) - 1] else 0
  tendencia <- dplyr::case_when(delta < -0.5 ~ "declínio",
                                delta >  0.5 ~ "aumento",
                                TRUE ~ "estabilidade")

  as.character(glue::glue(
    "Nas últimas {nrow(zonas)} semanas epidemiológicas analisadas, a incidência de SRAG ",
    "{B$rotulo_territorio_frase} manteve-se predominantemente na **{zona_predom}**.\n\n",
    "A curva corrigida por atraso de notificação indica um padrão de **{tendencia}**, ",
    "sugerindo que o cenário tende a se manter ou a evoluir, no curto prazo, nessa mesma faixa ",
    "de risco.\n\n",
    "Com base na situação recente, recomenda-se a ativação do **nível {cor_nome}** do ",
    "plano de contingência, com foco em:\n\n",
    "* reforço da vigilância e encerramento oportuno dos casos;\n",
    "* avaliação da capacidade de resposta hospitalar e laboratorial;\n",
    "* comunicação contínua com as equipes locais e com a população sob risco."
  ))
}

txt_rt <- function(B, RT, nivel = NULL) {
  if (is.null(RT) || nrow(RT) == 0) {
    return(as.character(glue::glue(
      "Não foi possível estimar o número de reprodução efetivo (Rt) ",
      "{B$rotulo_periodo_frase}: as séries por localidade e vírus são curtas demais ",
      "(mínimo de {RT_MIN_DIAS} dias e {RT_MIN_CASOS} casos). ",
      "Amplie o intervalo de tempo ou escolha um nível de localidade mais agregado."
    )))
  }
  as.character(glue::glue(
    "O Rt foi estimado por {tolower(rotulo_nivel_rt(nivel))} e por vírus, pelo método de ",
    "Cori et al. (2013), sobre a série **diária** de casos com detecção viral, ",
    "em janelas móveis de 7 dias, adotando intervalo serial com média ",
    "de {formatar_dec(RT_MEAN_SI, 1)} dias e desvio-padrão de {formatar_dec(RT_STD_SI, 1)} dias. ",
    "São reportadas apenas janelas com ao menos {RT_MIN_CASOS_JAN} casos. Valores acima de 1 ",
    "indicam transmissão em expansão; abaixo de 1, em retração. Séries curtas produzem ",
    "intervalos de confiança largos e devem ser interpretadas com cautela."
  ))
}

# Tabela de resumo (usada no boletim e nos cartões do painel)
tabela_resumo <- function(B, I) {
  r <- I$resumo
  tibble::tibble(
    Indicador = c(
      "Casos de SRAG no período", "Média semanal de casos", "Desvio-padrão semanal",
      "Semanas analisadas", "Óbitos por SRAG", "Letalidade (%)",
      "População de referência", "Incidência no período (por 100 mil hab.)"
    ),
    Valor = c(
      formatar_n(r$total), formatar_dec(r$media, 1), formatar_dec(r$dp, 1),
      formatar_n(r$semanas), formatar_n(r$obitos),
      if (is.na(r$letalidade)) "-" else formatar_dec(r$letalidade, 1),
      if (B$tem_populacao) formatar_n(r$populacao) else "não disponível",
      if (is.na(r$incidencia)) "não disponível" else formatar_dec(r$incidencia, 1)
    )
  )
}

# =============================================================================
# 11. IDENTIFICAÇÃO DO BOLETIM E AMBIENTE
# =============================================================================

#' Semana epidemiológica e ano sugeridos para a edição do boletim:
#' a última semana coberta pelo período selecionado.
se_ano_sugeridos <- function(cfg) {
  list(
    se  = as.integer(lubridate::epiweek(cfg$periodo_fim)),
    ano = as.integer(lubridate::epiyear(cfg$periodo_fim))
  )
}

#' Bloco de identificação que abre o boletim (título, secretaria, edição, equipe).
#' Campos em branco simplesmente não aparecem.
cabecalho_boletim <- function(B, cfg) {

  titulo <- if (nzchar(cfg$titulo)) cfg$titulo else {
    paste0("Boletim de SRAG - ", B$rotulo_territorio, ", ", B$rotulo_periodo)
  }

  linhas <- c(paste0("# ", titulo), "")

  inst <- c(cfg$secretaria, cfg$setor)
  inst <- inst[nzchar(inst)]
  if (length(inst) > 0) linhas <- c(linhas, paste0(inst, "  "), "")

  edicao <- c()
  if (nzchar(cfg$edicao)) {
    e <- cfg$edicao
    edicao <- c(edicao, if (grepl("^[0-9]+$", e)) paste0("Edi\u00e7\u00e3o n\u00ba ", e) else e)
  }
  if (nzchar(cfg$se_boletim)) {
    se <- cfg$se_boletim
    se_txt <- if (grepl("^[0-9]+$", se)) paste0("Semana epidemiol\u00f3gica ", sprintf("%02d", as.integer(se))) else se
    if (nzchar(cfg$ano_boletim)) se_txt <- paste0(se_txt, " de ", cfg$ano_boletim)
    edicao <- c(edicao, se_txt)
  } else if (nzchar(cfg$ano_boletim)) {
    edicao <- c(edicao, cfg$ano_boletim)
  }
  if (length(edicao) > 0) {
    linhas <- c(linhas, paste0("**", paste(edicao, collapse = " \u00b7 "), "**"), "")
  }

  linhas <- c(
    linhas,
    paste0("**Territ\u00f3rio:** ", B$rotulo_territorio, "  "),
    paste0("**Per\u00edodo analisado:** ",
           format(cfg$periodo_ini, "%d/%m/%Y"), " a ", format(cfg$periodo_fim, "%d/%m/%Y"),
           if (cfg$modo_periodo != "datas") paste0(" (", B$rotulo_periodo, ")") else "", "  "),
    paste0("**Casos de SRAG no per\u00edodo:** ", formatar_n(B$total_notificacoes), "  "),
    paste0("**Fonte dos dados:** ", rotulo_fonte(cfg), "  "),
    paste0("**Emitido em:** ", format(Sys.Date(), "%d/%m/%Y"))
  )

  if (nzchar(cfg$equipe)) {
    equipe <- unlist(strsplit(cfg$equipe, "[\n;]"))
    equipe <- trimws(equipe)
    equipe <- equipe[nzchar(equipe)]
    if (length(equipe) > 0) {
      linhas <- c(linhas, "", "**Equipe respons\u00e1vel**", "",
                  paste0("* ", equipe))
    }
  }

  paste(linhas, collapse = "\n")
}

#' Nome legivel da fonte de dados usada no boletim.
rotulo_fonte <- function(cfg) {
  if (identical(cfg$fonte, "arquivo") && nzchar(cfg$arquivo_proprio %||% "")) {
    paste0("arquivo enviado (", basename(cfg$arquivo_proprio), ")")
  } else if (identical(cfg$fonte, "arquivo")) {
    "arquivo enviado"
  } else {
    "API de Dados Abertos do SUS (SIVEP-Gripe)"
  }
}

#' Procura o Pandoc (necessário só para gerar o .docx).
#'
#' O RStudio pode estar instalado para todos os usuários (Program Files) ou só
#' para o usuário (AppData); o Quarto também traz o Pandoc. Procura em todos.
localizar_pandoc <- function() {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) return(FALSE)
  if (rmarkdown::pandoc_available()) return(TRUE)

  bases <- c(
    file.path(Sys.getenv("LOCALAPPDATA"), "Programs", "RStudio", "resources", "app"),
    file.path(Sys.getenv("PROGRAMFILES"), "RStudio", "resources", "app"),
    file.path(Sys.getenv("ProgramFiles(x86)"), "RStudio", "resources", "app"),
    "C:/Program Files/RStudio/resources/app",
    file.path(Sys.getenv("LOCALAPPDATA"), "Programs", "RStudio"),
    file.path(Sys.getenv("PROGRAMFILES"), "RStudio")
  )

  candidatos <- c(
    as.vector(outer(bases,
                    c("bin/quarto/bin/tools", "bin/quarto/bin/tools/x86_64", "bin/pandoc", "bin"),
                    function(b, s) file.path(b, s))),
    file.path(Sys.getenv("LOCALAPPDATA"), "Pandoc"),
    file.path(Sys.getenv("PROGRAMFILES"), "Pandoc"),
    file.path(Sys.getenv("LOCALAPPDATA"), "Programs", "Quarto", "bin", "tools"),
    file.path(Sys.getenv("PROGRAMFILES"), "Quarto", "bin", "tools"),
    file.path(Sys.getenv("PROGRAMFILES"), "Quarto", "bin", "tools", "x86_64")
  )

  pandoc_path <- Sys.which("pandoc")
  if (nzchar(pandoc_path)) candidatos <- c(dirname(pandoc_path), candidatos)

  for (p in candidatos) {
    if (!dir.exists(p)) next
    if (!any(file.exists(file.path(p, c("pandoc.exe", "pandoc"))))) next
    Sys.setenv(RSTUDIO_PANDOC = p)
    if (rmarkdown::pandoc_available()) return(TRUE)
  }
  FALSE
}

MSG_SEM_PANDOC <- paste(
  "Pandoc nao encontrado. Ele e necessario apenas para gerar o arquivo .docx.",
  "Instale o RStudio (que ja inclui o Pandoc) ou o Pandoc (https://pandoc.org/installing.html).",
  "Se o RStudio ja estiver instalado, abra-o e rode Sys.getenv(\"RSTUDIO_PANDOC\")",
  "para ver o caminho."
)
