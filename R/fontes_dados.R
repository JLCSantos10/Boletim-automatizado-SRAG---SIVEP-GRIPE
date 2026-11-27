# =============================================================================
# Fontes de dados do painel
#
#   1. SRAG     -> API de Dados Abertos do SUS (DEMAS/MS) ou arquivo do usuário
#   2. População -> API do IBGE (SIDRA) ou arquivo local
#   3. Cache    -> Bancos/cache/*.json, para o painel abrir rápido
#   4. Preferências -> Bancos/preferencias.json (última configuração usada)
#
# Tudo que vai para disco é JSON ou CSV legível, sem banco de dados.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(lubridate)
  library(stringr); library(tibble)
  # jsonlite NAO e anexado de proposito: jsonlite::validate() mascara
  # shiny::validate() e quebra as mensagens do painel. Use jsonlite:: sempre.
})

DIR_BANCOS <- "Bancos"
DIR_CACHE  <- file.path(DIR_BANCOS, "cache")
ARQ_PREFS  <- file.path(DIR_BANCOS, "preferencias.json")
UA         <- "boletim-srag/2.0"

garantir_pastas <- function() {
  dir.create(DIR_CACHE, recursive = TRUE, showWarnings = FALSE)
  invisible(TRUE)
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# =============================================================================
# 0. HTTP
# =============================================================================

http_json <- function(url, tentativas = 4, timeout = 180, pausa = 2) {
  for (i in seq_len(tentativas)) {
    h <- curl::new_handle(timeout = timeout, useragent = UA, accept_encoding = "gzip")
    curl::handle_setheaders(h, Accept = "application/json")
    res <- tryCatch(curl::curl_fetch_memory(url, handle = h), error = function(e) e)
    if (!inherits(res, "error") && isTRUE(res$status_code == 200)) {
      txt <- rawToChar(res$content); Encoding(txt) <- "UTF-8"
      return(jsonlite::fromJSON(txt, simplifyVector = TRUE))
    }
    if (i < tentativas) Sys.sleep(pausa * i)
  }
  motivo <- if (inherits(res, "error")) conditionMessage(res) else paste("HTTP", res$status_code)
  stop("Falha ao consultar ", url, " (", motivo, ")")
}

montar_url <- function(base, params) {
  if (length(params) == 0) return(base)
  q <- paste0(names(params), "=",
              vapply(params, function(v) utils::URLencode(as.character(v), reserved = TRUE),
                     character(1)), collapse = "&")
  paste0(base, "?", q)
}

# =============================================================================
# 1. CACHE EM JSON
#
# Cada item guarda {gravado_em, impressao, dados}. "impressao" é a assinatura da
# origem (ex.: último mês disponível na API, data de modificação do arquivo);
# quando ela muda, o cache é descartado e os dados são recalculados.
# =============================================================================

cache_arquivo <- function(chave) file.path(DIR_CACHE, paste0(chave, ".json"))

cache_gravar <- function(chave, dados, impressao = NULL) {
  garantir_pastas()
  jsonlite::write_json(
    list(gravado_em = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
         impressao = impressao %||% "", dados = dados),
    cache_arquivo(chave), auto_unbox = TRUE, dataframe = "columns", na = "null"
  )
  invisible(TRUE)
}

cache_ler <- function(chave, impressao = NULL, validade_horas = Inf) {
  arq <- cache_arquivo(chave)
  if (!file.exists(arq)) return(NULL)
  obj <- tryCatch(jsonlite::read_json(arq, simplifyVector = TRUE), error = function(e) NULL)
  if (is.null(obj) || is.null(obj$dados)) return(NULL)

  if (!is.null(impressao) && !identical(as.character(obj$impressao), as.character(impressao))) {
    return(NULL)   # a origem mudou
  }
  if (is.finite(validade_horas)) {
    idade <- as.numeric(difftime(Sys.time(), as.POSIXct(obj$gravado_em), units = "hours"))
    if (is.na(idade) || idade > validade_horas) return(NULL)
  }
  obj$dados
}

cache_info <- function(chave) {
  arq <- cache_arquivo(chave)
  if (!file.exists(arq)) return(NULL)
  obj <- tryCatch(jsonlite::read_json(arq, simplifyVector = TRUE), error = function(e) NULL)
  if (is.null(obj)) return(NULL)
  list(gravado_em = obj$gravado_em, impressao = obj$impressao,
       tamanho_mb = round(file.size(arq) / 1024^2, 2))
}

cache_limpar <- function(padrao = NULL) {
  garantir_pastas()
  arqs <- list.files(DIR_CACHE, pattern = "\\.(json|rds)$", full.names = TRUE)
  if (!is.null(padrao)) arqs <- arqs[grepl(padrao, basename(arqs))]
  unlink(arqs)
  invisible(length(arqs))
}

# =============================================================================
# 2. PREFERÊNCIAS (última configuração usada)
# =============================================================================

PREFS_PADRAO <- list(
  uf = "", escopo = "uf", municipios = "",
  fonte = "api", arquivo_proprio = "",
  modo_periodo = "ano", ano_ref = as.integer(format(Sys.Date(), "%Y")),
  data_inicio = "", data_fim = "", se_inicio = 1L, se_fim = 53L,
  filtro_ano_ini = as.integer(format(Sys.Date(), "%Y")) - 5L,
  filtro_ano_fim = as.integer(format(Sys.Date(), "%Y")),
  anos_historico = 5L, ano_epidemico = "2021",
  limiares_dp = c(1, 2, 3), semanas_nowcast = 4L,
  diag_max = 12L, diag_colunas = 3L,
  nome_territorio = "",
  f_sexo = "", f_raca = "", f_faixa = "", f_virus = "",
  rt_nivel = "local_bairro", rt_colunas = 2L,
  rt_itens = "", rt_virus = "",
  titulo = "", secretaria = "", setor = "",
  edicao = "", se_boletim = "", ano_boletim = "", equipe = ""
)

ler_preferencias <- function() {
  if (!file.exists(ARQ_PREFS)) return(PREFS_PADRAO)
  p <- tryCatch(jsonlite::read_json(ARQ_PREFS, simplifyVector = TRUE), error = function(e) NULL)
  if (is.null(p)) return(PREFS_PADRAO)
  utils::modifyList(PREFS_PADRAO, p)
}

salvar_preferencias <- function(novas) {
  garantir_pastas()
  atuais <- ler_preferencias()
  jsonlite::write_json(utils::modifyList(atuais, novas), ARQ_PREFS,
                       auto_unbox = TRUE, pretty = TRUE, na = "null")
  invisible(TRUE)
}

# =============================================================================
# 3. SRAG VIA API DE DADOS ABERTOS DO SUS
#
# Regras descobertas na prática (ver README):
#   - limit <= 1000 e offset = NÚMERO DA PÁGINA (0 = primeira)
#   - só "sg_uf_not + anomes_sin_pri" responde rápido; dt_sin_pri sozinho dá timeout
#   - o parâmetro "campos" é documentado mas responde 502 - não usar
#   - gzip reduz a resposta ~28x
# =============================================================================

API_SRAG      <- "https://apidadosabertos.saude.gov.br/vigilancia-e-meio-ambiente/srag-2019-2026"
API_LIM       <- 1000L

api_srag_total <- function(uf = NULL, anomes = NULL) {
  p <- list(limit = 1)
  if (!is.null(uf) && nzchar(uf)) p$sg_uf_not <- uf
  if (!is.null(anomes))           p$anomes_sin_pri <- anomes
  d <- http_json(montar_url(API_SRAG, p))
  as.integer(d$total_registros %||% NA_integer_)
}

seq_meses <- function(de, ate) {
  d <- as.Date(sprintf("%d-%02d-01", de %/% 100, de %% 100))
  a <- as.Date(sprintf("%d-%02d-01", ate %/% 100, ate %% 100))
  if (a < d) return(integer(0))
  as.integer(format(seq(d, a, by = "month"), "%Y%m"))
}

#' Primeiro e último mês com dados na API (resultado fica em cache por 12 h).
#' Usado no subtítulo do seletor de fonte de dados.
api_srag_intervalo <- function(forcar = FALSE) {
  if (!forcar) {
    c0 <- cache_ler("api_intervalo", validade_horas = 12)
    if (!is.null(c0)) return(as.list(c0))
  }
  hoje <- as.integer(format(Sys.Date(), "%Y%m"))
  ultimo <- NA_integer_
  for (am in rev(seq_meses(201901, hoje))) {
    n <- tryCatch(api_srag_total(anomes = am), error = function(e) NA_integer_)
    if (!is.na(n) && n > 0) { ultimo <- am; break }
  }
  primeiro <- 201901L
  out <- list(primeiro = primeiro, ultimo = ultimo,
              consultado_em = format(Sys.time(), "%Y-%m-%d %H:%M"))
  cache_gravar("api_intervalo", out)
  out
}

rotulo_intervalo_api <- function(intervalo = NULL) {
  iv <- intervalo %||% tryCatch(api_srag_intervalo(), error = function(e) NULL)
  if (is.null(iv) || is.na(iv$ultimo)) return("intervalo indisponível")
  fmt <- function(am) {
    d <- as.Date(sprintf("%d-%02d-01", am %/% 100, am %% 100))
    format(d, "%m/%Y")
  }
  paste0("dados de ", fmt(iv$primeiro), " a ", fmt(iv$ultimo))
}

#' Baixa um mês inteiro de uma UF (todas as páginas).
api_srag_mes <- function(uf, anomes) {
  total <- api_srag_total(uf, anomes)
  if (is.na(total) || total == 0) return(NULL)
  n_pag <- ceiling(total / API_LIM)
  partes <- vector("list", n_pag)
  for (p in seq_len(n_pag)) {
    d <- http_json(montar_url(API_SRAG, list(
      sg_uf_not = uf, anomes_sin_pri = anomes, limit = API_LIM, offset = p - 1L
    )))
    partes[[p]] <- d[["srag_2019_2026"]]
  }
  dplyr::bind_rows(lapply(partes, as.data.frame))
}

#' Baixa um intervalo, guardando um .rds por mês em Bancos/cache.
#' Os `meses_revalidar` últimos meses são sempre rebaixados, porque o SIVEP
#' recebe correções retroativas.
api_srag_baixar <- function(uf, de = 201901, ate = NULL, meses_revalidar = 3,
                            progresso = NULL) {
  garantir_pastas()
  if (is.null(ate)) {
    iv <- api_srag_intervalo()
    ate <- if (is.na(iv$ultimo)) as.integer(format(Sys.Date(), "%Y%m")) else iv$ultimo
  }
  meses <- seq_meses(de, ate)
  recentes <- utils::tail(meses, meses_revalidar)
  partes <- vector("list", length(meses))

  for (i in seq_along(meses)) {
    am <- meses[i]
    arq <- file.path(DIR_CACHE, sprintf("srag_%s_%d.rds", uf, am))
    if (file.exists(arq) && !(am %in% recentes)) {
      partes[[i]] <- readRDS(arq)
    } else {
      d <- tryCatch(api_srag_mes(uf, am), error = function(e) NULL)
      if (!is.null(d)) saveRDS(d, arq)
      partes[[i]] <- d
    }
    if (is.function(progresso)) progresso(i / length(meses), sprintf("%s %d", uf, am))
  }
  dplyr::bind_rows(partes)
}

# ----- conversão do formato bruto para o esquema do painel -------------------

MAPA_SUPORTE  <- c("1" = "Sim, invasivo", "2" = "Sim, não invasivo", "3" = "Não")
MAPA_EVOLUCAO <- c("1" = "Cura", "2" = "Óbito",
                   "3" = "Óbito por outras causas", "9" = "Ignorado")
COLS_PCR_API  <- c("pcr_fluasu", "pcr_flubli", "pcr_sars2", "pcr_vsr",
                   "pcr_para1", "pcr_para2", "pcr_para3", "pcr_para4",
                   "pcr_adeno", "pcr_metap", "pcr_boca", "pcr_rino", "pcr_outro")

idade_em_anos <- function(nu_idade_n, tp_idade) {
  n <- suppressWarnings(as.numeric(nu_idade_n)); t <- as.character(tp_idade)
  dplyr::case_when(t == "1" ~ n / 365.25, t == "2" ~ n / 12, t == "3" ~ n, TRUE ~ NA_real_)
}

faixa_etaria <- function(anos) {
  as.character(cut(anos, breaks = c(-Inf, 2, 11, 20, 30, 40, 50, 60, 70, 80, Inf),
                   right = FALSE, labels = NIVEIS_IDADE))
}

api_srag_converter <- function(bruto, classi_fin_excluir = character(0),
                               apenas_hospitalizados = FALSE) {
  if (is.null(bruto) || nrow(bruto) == 0) return(NULL)
  pega <- function(n) if (n %in% names(bruto)) as.character(bruto[[n]]) else rep(NA_character_, nrow(bruto))

  d <- tibble::tibble(
    ID_MN_RESI = pega("id_mn_resi"),
    SG_UF_NOT  = pega("sg_uf_not"),
    SG_UF_RES  = pega("sg_uf"),
    CO_MUN_RES = pega("co_mun_res"),
    DT_SIN_PRI = suppressWarnings(as.Date(pega("dt_sin_pri"))),
    DT_DIGITA  = suppressWarnings(as.Date(pega("dt_digita"))),
    DT_INTERNA = suppressWarnings(as.Date(pega("dt_interna"))),
    DT_EVOLUCA = suppressWarnings(as.Date(pega("dt_evoluca"))),
    TP_IDADE   = suppressWarnings(as.integer(pega("tp_idade"))),
    NU_IDADE_N = suppressWarnings(as.integer(pega("nu_idade_n"))),
    NM_UN_INTE = pega("nm_un_inte"),
    CS_SEXO    = pega("cs_sexo"),
    CS_RACA    = pega("cs_raca"),
    PCR_RESUL  = suppressWarnings(as.integer(pega("pcr_resul"))),
    CLASSI_FIN = pega("classi_fin"),
    .hospital  = pega("hospital"),
    .suport    = pega("suport_ven"),
    .evol      = pega("evolucao")
  )
  for (col in COLS_PCR_API) d[[toupper(col)]] <- suppressWarnings(as.integer(pega(col)))

  d <- d %>%
    dplyr::filter(!is.na(DT_SIN_PRI)) %>%
    dplyr::mutate(
      ID_MN_RESI_pad = padronizar_nomes(ID_MN_RESI),
      municipio = padronizar_nomes(ID_MN_RESI),
      SE  = as.integer(lubridate::epiweek(DT_SIN_PRI)),
      ano = as.integer(lubridate::epiyear(DT_SIN_PRI)),
      FE1 = faixa_etaria(idade_em_anos(NU_IDADE_N, TP_IDADE)),
      SEXO = unname(MAPA_SEXO[toupper(CS_SEXO)]),
      RACA_COR = unname(MAPA_RACA[as.character(CS_RACA)]),
      SUPORT_VEN2 = unname(MAPA_SUPORTE[.suport]),
      EVOL1       = unname(MAPA_EVOLUCAO[.evol]),
      OBITOSRAG = dplyr::case_when(.evol == "2" ~ "Sim",
                                   .evol %in% c("1", "3") ~ "Nao",
                                   TRUE ~ NA_character_),
      Tempo_Evolucao = dplyr::if_else(
        !is.na(DT_INTERNA) & !is.na(DT_EVOLUCA) & DT_EVOLUCA >= DT_INTERNA,
        as.numeric(DT_EVOLUCA - DT_INTERNA), NA_real_)
    )

  if (apenas_hospitalizados)      d <- dplyr::filter(d, .hospital == "1")
  if (length(classi_fin_excluir)) d <- dplyr::filter(d, !(CLASSI_FIN %in% classi_fin_excluir))
  dplyr::select(d, -dplyr::starts_with("."))
}

#' Ponto de entrada: dados de SRAG de uma UF, prontos para o painel.
srag_da_api <- function(uf, de = 201901, ate = NULL, progresso = NULL, ...) {
  bruto <- api_srag_baixar(uf, de = de, ate = ate, progresso = progresso)
  if (is.null(bruto) || nrow(bruto) == 0) {
    stop("A API nao devolveu registros para a UF ", uf, ".")
  }
  api_srag_converter(bruto, ...)
}

# =============================================================================
# 4. SRAG A PARTIR DE ARQUIVO DO USUÁRIO
#
# Aceita o formato já usado pelo projeto (Bancos/SRAG.csv) e o formato bruto da
# API salvo em CSV. Valida as colunas mínimas e deriva o que faltar.
# =============================================================================

COLS_MINIMAS <- c("DT_SIN_PRI")

srag_de_arquivo <- function(caminho, coluna_municipio = NULL) {
  if (!file.exists(caminho)) stop("Arquivo nao encontrado: ", caminho)

  d <- if (grepl("\\.rds$", caminho, ignore.case = TRUE)) {
    readRDS(caminho)
  } else {
    readr::read_csv(caminho, show_col_types = FALSE, progress = FALSE,
                    guess_max = 50000)
  }
  if (nrow(d) == 0) stop("O arquivo esta vazio.")

  # se vier no formato bruto da API (nomes minúsculos), converte
  if ("dt_sin_pri" %in% names(d) && !"DT_SIN_PRI" %in% names(d)) {
    return(api_srag_converter(d))
  }

  # coluna de município
  cand <- c(coluna_municipio, "ID_MN_RESI_pad", "ID_MN_RESI", "municipio", "MUNICIPIO")
  cand <- cand[!is.na(cand) & cand %in% names(d)]
  if (length(cand) == 0) {
    stop("Nenhuma coluna de municipio encontrada. Esperado uma destas: ",
         "ID_MN_RESI_pad, ID_MN_RESI, municipio.")
  }
  d$ID_MN_RESI_pad <- padronizar_nomes(d[[cand[1]]])
  d$municipio <- d$ID_MN_RESI_pad
  if (!"ID_MN_RESI" %in% names(d)) d$ID_MN_RESI <- d[[cand[1]]]

  faltam <- setdiff(COLS_MINIMAS, names(d))
  if (length(faltam)) stop("Faltam colunas obrigatorias: ", paste(faltam, collapse = ", "))

  d %>%
    dplyr::mutate(
      DT_SIN_PRI = lubridate::as_date(DT_SIN_PRI),
      SE  = as.integer(lubridate::epiweek(DT_SIN_PRI)),
      ano = as.integer(lubridate::epiyear(DT_SIN_PRI))
    ) %>%
    dplyr::filter(!is.na(DT_SIN_PRI))
}

# =============================================================================
# 5. POPULAÇÃO VIA API DO IBGE
#
#   - SIDRA tabela 6579 (Estimativas de População), variável 9324
#       n6 = município, n3 = UF
#   - Censo 2022 pela tabela 4714, variável 93 (a 6579 não tem 2022 nem 2023)
#   - 2023 é interpolado entre 2022 e 2024
#
# O código do IBGE tem 7 dígitos e o do SIVEP tem 6 (sem o dígito verificador),
# por isso guardamos também `codigo6`, que é a chave de junção com `co_mun_res`.
# =============================================================================

SIDRA <- "https://apisidra.ibge.gov.br/values"
IBGE_LOCALIDADES <- "https://servicodados.ibge.gov.br/api/v1/localidades"

sidra_valores <- function(caminho) {
  d <- http_json(paste0(SIDRA, caminho))
  if (length(d) <= 1) return(tibble::tibble())
  cab <- d[1, ]
  corpo <- d[-1, , drop = FALSE]
  tibble::as_tibble(corpo)
}

ibge_anos_disponiveis <- function() {
  d <- cache_ler("ibge_anos", validade_horas = 24 * 30)
  if (!is.null(d)) return(as.integer(d))
  p <- http_json("https://servicodados.ibge.gov.br/api/v3/agregados/6579/periodos")
  anos <- sort(as.integer(p$id))
  cache_gravar("ibge_anos", anos)
  anos
}

#' População por município (todos do Brasil ou de uma UF) nos anos pedidos.
#' Devolve: ano, codigo (7 dígitos), codigo6, municipio, uf, populacao
ibge_populacao_municipios <- function(anos, uf = NULL, progresso = NULL) {
  anos <- sort(unique(as.integer(anos)))
  disp <- ibge_anos_disponiveis()

  pedir <- intersect(anos, disp)
  partes <- list()

  for (i in seq_along(pedir)) {
    a <- pedir[i]
    r <- sidra_valores(sprintf("/t/6579/n6/all/v/9324/p/%d", a))
    if (nrow(r) == 0) next
    partes[[length(partes) + 1L]] <- tibble::tibble(
      ano = a,
      codigo = as.character(r$D1C),
      municipio_uf = as.character(r$D1N),
      populacao = suppressWarnings(as.numeric(r$V))
    )
    if (is.function(progresso)) progresso(i / max(1, length(pedir)), paste("IBGE", a))
  }

  # 2022: Censo (a tabela de estimativas não traz)
  if (2022 %in% anos && !(2022 %in% pedir)) {
    r <- tryCatch(sidra_valores("/t/4714/n6/all/v/93/p/2022"), error = function(e) tibble::tibble())
    if (nrow(r) > 0) {
      partes[[length(partes) + 1L]] <- tibble::tibble(
        ano = 2022L, codigo = as.character(r$D1C),
        municipio_uf = as.character(r$D1N),
        populacao = suppressWarnings(as.numeric(r$V))
      )
    }
  }

  if (length(partes) == 0) return(tibble::tibble())
  pop <- dplyr::bind_rows(partes)

  pop <- pop %>%
    dplyr::mutate(
      codigo6 = substr(codigo, 1, 6),
      # A SIDRA alterna entre "Brasília (DF)" e "Ariquemes - RO"; a sigla sai do
      # próprio código IBGE, que é sempre confiável.
      uf = unname(SIGLA_POR_CODIGO_UF[substr(codigo, 1, 2)]),
      municipio = stringr::str_remove(municipio_uf, "\\s*[-(]\\s*[A-Z]{2}\\)?\\s*$"),
      municipio_pad = padronizar_nomes(municipio)
    ) %>%
    dplyr::filter(!is.na(populacao))

  if (!is.null(uf) && nzchar(uf)) pop <- dplyr::filter(pop, uf == !!uf)

  # anos faltantes (ex.: 2023) são interpolados dentro de cada município
  faltantes <- setdiff(anos, unique(pop$ano))
  if (length(faltantes) > 0 && nrow(pop) > 0) {
    pop <- pop %>%
      dplyr::group_by(codigo) %>%
      dplyr::group_modify(~ {
        base <- .x
        novos <- lapply(faltantes, function(a) {
          antes  <- base %>% dplyr::filter(ano < a) %>% dplyr::slice_max(ano, n = 1, with_ties = FALSE)
          depois <- base %>% dplyr::filter(ano > a) %>% dplyr::slice_min(ano, n = 1, with_ties = FALSE)
          if (nrow(antes) == 0 && nrow(depois) == 0) return(NULL)
          valor <- if (nrow(antes) && nrow(depois)) {
            peso <- (a - antes$ano) / (depois$ano - antes$ano)
            antes$populacao + peso * (depois$populacao - antes$populacao)
          } else if (nrow(antes)) antes$populacao else depois$populacao
          ref <- if (nrow(antes)) antes else depois
          dplyr::mutate(ref, ano = a, populacao = valor)
        })
        dplyr::bind_rows(base, dplyr::bind_rows(novos))
      }) %>%
      dplyr::ungroup()
  }

  pop %>%
    dplyr::filter(ano %in% anos) %>%
    dplyr::select(ano, codigo, codigo6, municipio, municipio_pad, uf, populacao) %>%
    dplyr::arrange(ano, municipio)
}

#' População total por UF e ano (nível n3 da SIDRA).
ibge_populacao_uf <- function(anos, uf = NULL) {
  anos <- sort(unique(as.integer(anos)))
  disp <- ibge_anos_disponiveis()
  partes <- list()
  for (a in intersect(anos, disp)) {
    r <- sidra_valores(sprintf("/t/6579/n3/all/v/9324/p/%d", a))
    if (nrow(r) == 0) next
    partes[[length(partes) + 1L]] <- tibble::tibble(
      ano = a, uf_nome = as.character(r$D1N),
      codigo_uf = as.character(r$D1C),
      populacao = suppressWarnings(as.numeric(r$V))
    )
  }
  if (length(partes) == 0) return(tibble::tibble())
  out <- dplyr::bind_rows(partes) %>%
    dplyr::mutate(sigla = unname(SIGLA_POR_CODIGO_UF[codigo_uf]))
  if (!is.null(uf) && nzchar(uf)) out <- dplyr::filter(out, sigla == !!uf)
  out
}

SIGLA_POR_CODIGO_UF <- c(
  "11" = "RO", "12" = "AC", "13" = "AM", "14" = "RR", "15" = "PA", "16" = "AP",
  "17" = "TO", "21" = "MA", "22" = "PI", "23" = "CE", "24" = "RN", "25" = "PB",
  "26" = "PE", "27" = "AL", "28" = "SE", "29" = "BA", "31" = "MG", "32" = "ES",
  "33" = "RJ", "35" = "SP", "41" = "PR", "42" = "SC", "43" = "RS", "50" = "MS",
  "51" = "MT", "52" = "GO", "53" = "DF"
)

#' População pronta para o painel: filtrada pela UF e pelos territórios
#' selecionados, no formato (ano, municipio, populacao) usado pelos cálculos.
#'
#' A busca é feita nesta ordem, por território:
#'   1. arquivo local de população (Bancos/POP_LOCAL.csv), que tem prioridade
#'   2. código IBGE (6 primeiros dígitos) batendo com co_mun_res
#'   3. nome padronizado do município dentro da UF
populacao_para_painel <- function(uf, anos, territorios = NULL,
                                  de_para_codigo = NULL,
                                  arquivo_local = file.path(DIR_BANCOS, "POP_LOCAL.csv"),
                                  progresso = NULL) {

  anos <- sort(unique(as.integer(anos)))
  avisos <- character(0)

  mun <- tryCatch(ibge_populacao_municipios(anos, uf = uf, progresso = progresso),
                  error = function(e) { avisos <<- c(avisos, paste("IBGE:", conditionMessage(e)));
                                        tibble::tibble() })
  ufs <- tryCatch(ibge_populacao_uf(anos, uf = uf), error = function(e) tibble::tibble())

  pop <- tibble::tibble(ano = integer(0), municipio = character(0), populacao = numeric(0))

  if (nrow(mun) > 0) {
    por_nome <- mun %>%
      dplyr::transmute(ano, municipio = municipio_pad, populacao)
    pop <- dplyr::bind_rows(pop, por_nome)

    # junção por código, quando o painel souber o co_mun_res de cada território
    if (!is.null(de_para_codigo) && nrow(de_para_codigo) > 0) {
      por_codigo <- de_para_codigo %>%
        dplyr::mutate(codigo6 = as.character(codigo6)) %>%
        dplyr::inner_join(mun %>% dplyr::select(ano, codigo6, populacao),
                          by = "codigo6", relationship = "many-to-many") %>%
        dplyr::transmute(ano, municipio = territorio, populacao)
      pop <- dplyr::bind_rows(por_codigo, pop)
    }
  }

  # população local tem prioridade sobre o IBGE
  if (file.exists(arquivo_local)) {
    pl <- tryCatch(readr::read_csv(arquivo_local, show_col_types = FALSE, progress = FALSE),
                   error = function(e) NULL)
    if (!is.null(pl) && all(c("municipio", "populacao") %in% names(pl))) {
      pl <- pl %>%
        dplyr::mutate(municipio = padronizar_nomes(municipio),
                      populacao = as.numeric(populacao)) %>%
        dplyr::filter(!is.na(populacao), populacao > 0)
      pl <- if ("ano" %in% names(pl)) {
        dplyr::mutate(pl, ano = as.integer(ano))
      } else {
        tidyr::expand_grid(ano = anos, pl)
      }
      # completa anos ausentes com o valor mais recente informado
      compl <- pl %>% dplyr::group_by(municipio) %>%
        dplyr::slice_max(ano, n = 1, with_ties = FALSE) %>% dplyr::ungroup() %>%
        dplyr::select(municipio, populacao) %>%
        tidyr::expand_grid(ano = anos) %>%
        dplyr::anti_join(pl, by = c("municipio", "ano"))
      pl <- dplyr::bind_rows(pl, compl) %>% dplyr::select(ano, municipio, populacao)
      pop <- dplyr::bind_rows(pl, pop)
      avisos <- c(avisos, paste0("População local de ", basename(arquivo_local),
                                 " aplicada a ", dplyr::n_distinct(pl$municipio), " território(s)."))
    }
  }

  pop <- pop %>% dplyr::distinct(ano, municipio, .keep_all = TRUE)

  total_uf <- if (nrow(ufs) > 0) {
    ufs %>% dplyr::select(ano, populacao)
  } else {
    pop %>% dplyr::group_by(ano) %>%
      dplyr::summarise(populacao = sum(populacao, na.rm = TRUE), .groups = "drop")
  }

  list(municipios = pop, uf = total_uf, avisos = avisos)
}

# =============================================================================
# 6. ASSINATURA DAS FONTES (para saber quando recalcular o cache)
# =============================================================================

impressao_fonte <- function(fonte, uf = "", arquivo = "") {
  if (identical(fonte, "api")) {
    iv <- tryCatch(api_srag_intervalo(), error = function(e) list(ultimo = NA))
    paste0("api|", uf, "|", iv$ultimo %||% NA)
  } else {
    inf <- if (nzchar(arquivo) && file.exists(arquivo)) file.info(arquivo) else NULL
    paste0("arquivo|", basename(arquivo), "|",
           if (is.null(inf)) "" else paste0(inf$size, "|", as.integer(inf$mtime)))
  }
}

# =============================================================================
# 7. MONTAGEM DAS BASES PARA O PAINEL
#
# Substitui o antigo carregar_bases() que lia Bancos/SRAG.csv + Bancos/POP.csv.
# =============================================================================

#' População no formato que preparar_populacao() espera
#' (ano, municipio, sigla_uf, populacao), já reconciliada com os nomes de
#' território que aparecem na base de casos.
montar_populacao <- function(cfg, srag, anos, progresso = NULL) {

  uf <- cfg$estado %||% ""
  avisos <- character(0)

  # Sem filtro de UF na consulta: territorios de fora (o entorno goiano, por
  # exemplo) tambem precisam de populacao. O recorte e feito depois.
  ib <- tryCatch(ibge_populacao_municipios(anos, uf = NULL, progresso = progresso),
                 error = function(e) { avisos <<- c(avisos, paste("IBGE:", conditionMessage(e)))
                                       tibble::tibble() })

  if (nrow(ib) == 0) {
    # sem internet ou IBGE fora do ar: usa o CSV local, se existir
    antigo <- file.path(DIR_BANCOS, "POP.csv")
    if (file.exists(antigo)) {
      avisos <- c(avisos, "API do IBGE indispon\u00edvel; usando Bancos/POP.csv.")
      pop <- readr::read_csv(antigo, show_col_types = FALSE, progress = FALSE)
      col <- intersect(c("municipio", "id_municipio_nome", "nome_municipio"), names(pop))[1]
      pop <- pop %>%
        dplyr::rename(municipio = dplyr::all_of(col)) %>%
        dplyr::mutate(municipio = padronizar_nomes(municipio),
                      ano = as.integer(ano), populacao = as.numeric(populacao))
      if (nzchar(uf) && "sigla_uf" %in% names(pop)) pop <- dplyr::filter(pop, sigla_uf == uf)
      return(list(pop = dplyr::select(pop, ano, municipio,
                                      sigla_uf = dplyr::any_of("sigla_uf"), populacao),
                  avisos = avisos))
    }
    return(list(pop = NULL, avisos = c(avisos, "N\u00e3o foi poss\u00edvel obter a popula\u00e7\u00e3o.")))
  }

  # Reconcilia o nome do IBGE com o nome usado na base de casos, pelo código
  # IBGE de 6 dígitos (o SIVEP usa o código sem o dígito verificador).
  # `uf_sel` em vez de `uf`: dentro do dplyr, `uf` seria a COLUNA do data.frame
  uf_sel <- uf
  fonte_pop <- "API do IBGE (SIDRA)"

  # total da UF antes de qualquer recorte por território
  pop_uf <- if (nzchar(uf_sel)) {
    ib %>% dplyr::filter(.data$uf == uf_sel)
  } else ib
  pop_uf <- pop_uf %>%
    dplyr::group_by(ano) %>%
    dplyr::summarise(populacao = sum(populacao, na.rm = TRUE), .groups = "drop")

  # Mantém os municípios da UF e também os de fora que aparecem na base de
  # casos (o entorno goiano, por exemplo), todos marcados com a UF escolhida
  # para que o recorte por território não os descarte.
  nomes_base <- unique(srag$municipio)
  pop <- ib %>%
    dplyr::filter(.data$uf == uf_sel | municipio_pad %in% nomes_base) %>%
    dplyr::transmute(ano, codigo6, municipio = municipio_pad,
                     sigla_uf = uf_sel, populacao)

  if ("CO_MUN_RES" %in% names(srag)) {
    mapa <- srag %>%
      dplyr::transmute(codigo6 = substr(as.character(CO_MUN_RES), 1, 6),
                       nome_srag = ID_MN_RESI_pad) %>%
      dplyr::filter(!is.na(codigo6), nzchar(codigo6), !is.na(nome_srag)) %>%
      dplyr::distinct(codigo6, .keep_all = TRUE)
    pop <- pop %>%
      dplyr::left_join(mapa, by = "codigo6") %>%
      dplyr::mutate(municipio = dplyr::coalesce(nome_srag, municipio)) %>%
      dplyr::select(-nome_srag)
  }

  pop <- pop %>%
    dplyr::select(ano, municipio, sigla_uf, populacao) %>%
    dplyr::filter(!is.na(populacao), populacao > 0) %>%
    dplyr::distinct(ano, municipio, .keep_all = TRUE)

  # No DF a populacao por Regiao Administrativa vem da Codeplan e tem
  # prioridade; o entorno (GO, MG...) continua vindo do IBGE.
  if (identical(uf, "DF")) {
    cp <- populacao_codeplan(anos)
    if (!is.null(cp) && nrow(cp) > 0) {
      pop <- dplyr::bind_rows(
        dplyr::select(cp, ano, municipio, sigla_uf, populacao),
        dplyr::anti_join(pop, cp, by = c("ano", "municipio"))
      ) %>% dplyr::distinct(ano, municipio, .keep_all = TRUE)
      fonte_pop <- "Codeplan (RAs do DF) + API do IBGE (entorno)"
      # o total do DF tambem passa a ser o da Codeplan
      pop_uf <- cp %>% dplyr::group_by(ano) %>%
        dplyr::summarise(populacao = sum(populacao, na.rm = TRUE), .groups = "drop")
      avisos <- c(avisos, paste0(
        "População das Regiões Administrativas do DF vinda da Codeplan (",
        basename(ARQ_CODEPLAN), "), ", dplyr::n_distinct(cp$municipio),
        " RAs. Territórios de fora do DF usam o IBGE."
      ))
    }
  }

  list(pop = pop, pop_uf = pop_uf, fonte_populacao = fonte_pop, avisos = avisos)
}

#' Carrega base de casos + população conforme a fonte escolhida.
#' `cfg$fonte` é "api" (padrão) ou "arquivo".
carregar_bases <- function(cfg, progresso = NULL) {

  avisos <- character(0)

  # ----- casos -------------------------------------------------------------
  srag <- if (identical(cfg$fonte, "arquivo")) {
    if (is.null(cfg$arquivo_proprio) || !nzchar(cfg$arquivo_proprio)) {
      stop("Nenhum arquivo de dados foi enviado.")
    }
    srag_de_arquivo(cfg$arquivo_proprio, cfg$coluna_municipio)
  } else {
    if (is.null(cfg$estado) || !nzchar(cfg$estado)) {
      stop("Selecione uma UF para buscar os dados na API.")
    }
    srag_da_api(cfg$estado, progresso = progresso)
  }

  if (is.null(srag) || nrow(srag) == 0) stop("A fonte de dados nao devolveu registros.")

  # No DF, consolida os nomes da base de casos na lista oficial de RAs
  # (Asa Norte + Asa Sul + Brasilia = Plano Piloto, etc.), para casar com a
  # populacao da Codeplan e com o nivel "bairro/RA" dos graficos.
  if (identical(cfg$estado, "DF") && file.exists(ARQ_CODEPLAN)) {
    srag <- srag %>%
      dplyr::mutate(
        ID_MN_RESI_pad = normalizar_ra_df(ID_MN_RESI_pad),
        municipio      = normalizar_ra_df(municipio)
      )
    avisos <- c(avisos, paste0(
      "Territórios do DF consolidados nas Regiões Administrativas oficiais ",
      "(Asa Norte, Asa Sul e Brasília viraram Plano Piloto)."
    ))
  }

  srag <- adicionar_niveis_localidade(srag, cfg$estado)

  # ----- população ---------------------------------------------------------
  anos <- sort(unique(c(srag$ano, cfg$ano_ref_epi)))
  mp <- montar_populacao(cfg, srag, anos, progresso = progresso)
  avisos <- c(avisos, mp$avisos)

  # população local (territórios fora do recorte municipal do IBGE)
  arq_local <- file.path(DIR_BANCOS, "POP_LOCAL.csv")
  pop_local <- if (file.exists(arq_local)) {
    tryCatch(readr::read_csv(arq_local, show_col_types = FALSE, progress = FALSE),
             error = function(e) NULL)
  } else NULL

  list(srag = srag, pop = mp$pop, pop_uf = mp$pop_uf,
       fonte_populacao = mp$fonte_populacao %||% "API do IBGE (SIDRA)",
       pop_local = pop_local, avisos = avisos)
}

#' Intervalo de datas efetivamente presente numa base de casos.
intervalo_da_base <- function(srag) {
  if (is.null(srag) || nrow(srag) == 0) return(NULL)
  list(inicio = min(srag$DT_SIN_PRI, na.rm = TRUE),
       fim    = max(srag$DT_SIN_PRI, na.rm = TRUE))
}

rotulo_intervalo_base <- function(srag) {
  iv <- intervalo_da_base(srag)
  if (is.null(iv)) return("")
  paste0("dados de ", format(iv$inicio, "%d/%m/%Y"), " a ", format(iv$fim, "%d/%m/%Y"))
}

# =============================================================================
# 8. CACHE DA BASE MONTADA
#
# Dois arquivos por fonte, em Bancos/cache:
#
#   base_<chave>.json  manifesto legível + os dados já consolidados
#                      (casos por semana/território/vírus, população usada).
#                      É o arquivo lido primeiro quando o painel abre: se a
#                      assinatura da fonte não mudou, nada é recalculado.
#   base_<chave>.rds   as linhas individuais, para a tabela e o boletim.
#                      Binário de propósito: 100 mil linhas em JSON levam
#                      dezenas de segundos para abrir em máquina lenta.
# =============================================================================

cache_base_arquivos <- function(chave) {
  list(json = file.path(DIR_CACHE, paste0(chave, ".json")),
       rds  = file.path(DIR_CACHE, paste0(chave, ".rds")))
}

resumo_para_json <- function(bases) {
  s <- bases$srag
  por_semana <- s %>%
    dplyr::count(ano, SE, name = "casos") %>%
    dplyr::arrange(ano, SE)
  por_territorio <- s %>%
    dplyr::count(ID_MN_RESI_pad, name = "casos") %>%
    dplyr::arrange(dplyr::desc(casos))
  cols_pcr <- intersect(names(VIRUS_MAP), names(s))
  por_virus <- if (length(cols_pcr)) {
    tibble::tibble(
      virus = unname(VIRUS_MAP[cols_pcr]),
      deteccoes = vapply(cols_pcr, function(c) sum(s[[c]] == 1, na.rm = TRUE), numeric(1))
    ) %>% dplyr::arrange(dplyr::desc(deteccoes))
  } else tibble::tibble()

  list(casos_por_semana = por_semana, casos_por_territorio = por_territorio,
       deteccoes_por_virus = por_virus,
       populacao = bases$pop %||% tibble::tibble())
}

cache_base_gravar <- function(chave, bases, impressao) {
  garantir_pastas()
  arq <- cache_base_arquivos(chave)
  iv <- intervalo_da_base(bases$srag)

  manifesto <- list(
    gravado_em = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    impressao = impressao,
    registros = nrow(bases$srag),
    colunas = names(bases$srag),
    periodo = list(inicio = as.character(iv$inicio), fim = as.character(iv$fim)),
    avisos = bases$avisos %||% character(0),
    dados = resumo_para_json(bases)
  )
  jsonlite::write_json(manifesto, arq$json, auto_unbox = TRUE, pretty = TRUE,
                       dataframe = "columns", na = "null")
  saveRDS(bases, arq$rds, compress = "xz")
  invisible(TRUE)
}

cache_base_ler <- function(chave, impressao) {
  arq <- cache_base_arquivos(chave)
  if (!file.exists(arq$json) || !file.exists(arq$rds)) return(NULL)
  man <- tryCatch(jsonlite::read_json(arq$json, simplifyVector = TRUE), error = function(e) NULL)
  if (is.null(man)) return(NULL)
  if (!identical(as.character(man$impressao), as.character(impressao))) return(NULL)
  tryCatch(readRDS(arq$rds), error = function(e) NULL)
}

cache_info <- function(chave) {
  arq <- file.path(DIR_CACHE, paste0(chave, ".json"))
  if (!file.exists(arq)) return(NULL)
  man <- tryCatch(jsonlite::read_json(arq, simplifyVector = TRUE), error = function(e) NULL)
  if (is.null(man)) return(NULL)
  rds <- file.path(DIR_CACHE, paste0(chave, ".rds"))
  tam <- sum(file.size(c(arq, rds)), na.rm = TRUE)
  list(gravado_em = man$gravado_em, impressao = man$impressao,
       registros = man$registros %||% NA, tamanho_mb = round(tam / 1024^2, 1))
}

# =============================================================================
# 9. GERAÇÃO DO BOLETIM EM WORD
#
# Só existe por aqui: o painel chama esta função com as bases já carregadas,
# então o .Rmd não refaz download nem reconverte nada.
# =============================================================================

gerar_boletim_word <- function(bases, cfg, destino, semanas_nowcast = NULL) {

  if (!localizar_pandoc()) stop(MSG_SEM_PANDOC)
  if (!is.null(semanas_nowcast)) cfg$semanas_nowcast <- semanas_nowcast

  rmd <- c("R/boletim.Rmd", file.path("..", "R", "boletim.Rmd"))
  rmd <- rmd[file.exists(rmd)][1]
  if (is.na(rmd)) stop("Modelo 'R/boletim.Rmd' nao encontrado.")

  dir_tmp <- file.path(tempdir(), paste0("boletim_", as.integer(Sys.time())))
  dir.create(dir_tmp, recursive = TRUE, showWarnings = FALSE)

  raiz <- normalizePath(".", winslash = "/")
  saveRDS(list(bases = bases, cfg = cfg, raiz = raiz), file.path(dir_tmp, "entrada.rds"))
  file.copy(rmd, file.path(dir_tmp, "boletim.Rmd"), overwrite = TRUE)

  out <- rmarkdown::render(
    input = file.path(dir_tmp, "boletim.Rmd"),
    output_format = "word_document", output_file = "boletim.docx",
    knit_root_dir = dir_tmp, envir = new.env(), quiet = TRUE,
    params = NULL
  )
  file.copy(out, destino, overwrite = TRUE)
  invisible(destino)
}

# =============================================================================
# 10. POPULAÇÃO DO DISTRITO FEDERAL (Codeplan)
#
# O IBGE trata o DF como um único município ("Brasília"), então não há
# população por Região Administrativa. Quando a UF escolhida é o DF e existe
# Bancos/codeplan_populacao_long.csv, a população das RAs vem dessa planilha.
# Territórios de fora do DF (entorno goiano, por exemplo) continuam no IBGE.
# =============================================================================

ARQ_CODEPLAN <- file.path(DIR_BANCOS, "codeplan_populacao_long.csv")

# A base de casos usa nomes que não são os da lista oficial de RAs.
# Plano Piloto aparece dividido em Asa Norte/Asa Sul/Brasília.
DEPARA_RA_DF <- c(
  "brasilia asa norte"                 = "plano piloto",
  "brasilia asa sul"                   = "plano piloto",
  "brasilia"                           = "plano piloto",
  "scia estrutural"                    = "estrutural scia",
  "setor de industria e abastecimento" = "sia",
  "sudoeste octogonal"                 = "sudoeste octogonal"
)

#' Converte os nomes da base de casos para a RA oficial da Codeplan.
normalizar_ra_df <- function(x) {
  v <- unname(DEPARA_RA_DF[x])
  dplyr::coalesce(v, x)
}

#' População por Região Administrativa do DF, somando as faixas etárias.
#' Devolve ano, municipio (RA padronizada), populacao.
populacao_codeplan <- function(anos = NULL, caminho = ARQ_CODEPLAN) {
  if (!file.exists(caminho)) return(NULL)

  d <- tryCatch(
    readr::read_delim(caminho, delim = ";", show_col_types = FALSE,
                      progress = FALSE, locale = readr::locale(encoding = "UTF-8")),
    error = function(e) NULL
  )
  if (is.null(d)) return(NULL)

  col_ra  <- intersect(c("regiao_adm", "ra", "municipio", "territorio"), names(d))[1]
  col_pop <- intersect(c("pop", "populacao", "total"), names(d))[1]
  if (is.na(col_ra) || is.na(col_pop)) return(NULL)

  out <- d %>%
    dplyr::mutate(
      ano = as.integer(ano),
      municipio = padronizar_nomes(.data[[col_ra]]),
      populacao = suppressWarnings(as.numeric(.data[[col_pop]]))
    ) %>%
    dplyr::filter(!is.na(ano), !is.na(populacao)) %>%
    dplyr::group_by(ano, municipio) %>%
    dplyr::summarise(populacao = sum(populacao, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(populacao > 0)

  if (!is.null(anos)) {
    # anos fora da planilha herdam o ano mais próximo disponível
    faltam <- setdiff(anos, unique(out$ano))
    if (length(faltam) > 0 && nrow(out) > 0) {
      extra <- lapply(faltam, function(a) {
        disp <- unique(out$ano)
        ref <- disp[which.min(abs(disp - a))]
        out %>% dplyr::filter(ano == ref) %>% dplyr::mutate(ano = a)
      })
      out <- dplyr::bind_rows(out, dplyr::bind_rows(extra))
    }
    out <- dplyr::filter(out, ano %in% anos)
  }
  dplyr::mutate(out, sigla_uf = "DF")
}

#' Lista de municípios do IBGE (código de 6 dígitos -> nome), em cache.
ibge_municipios <- function() {
  d <- cache_ler("ibge_municipios", validade_horas = 24 * 90)
  if (!is.null(d)) return(tibble::as_tibble(d))
  m <- http_json(paste0(IBGE_LOCALIDADES, "/municipios"))
  # a UF sai dos 2 primeiros dígitos do código: o caminho aninhado
  # microrregiao$mesorregiao$UF$sigla nem sempre vem preenchido
  out <- tibble::tibble(
    codigo = as.character(m$id),
    nome = padronizar_nomes(m$nome)
  ) %>%
    dplyr::mutate(codigo6 = substr(codigo, 1, 6),
                  uf = unname(SIGLA_POR_CODIGO_UF[substr(codigo, 1, 2)])) %>%
    dplyr::select(codigo6, nome, uf)
  cache_gravar("ibge_municipios", out)
  out
}

# =============================================================================
# 11. NÍVEIS DE LOCALIDADE
#
# Três recortes geográficos, usados nos agrupamentos dos gráficos:
#   local_bairro    território como aparece na notificação (RA, no DF)
#   local_municipio município do IBGE (no DF, todas as RAs viram Brasília)
#   local_estado    UF de residência
# =============================================================================

NIVEIS_LOCALIDADE <- c("local_bairro" = "Bairro / Regi\u00e3o Administrativa",
                       "local_municipio" = "Munic\u00edpio",
                       "local_estado" = "Estado (UF)")

adicionar_niveis_localidade <- function(srag, uf = NULL) {
  if (is.null(srag) || nrow(srag) == 0) return(srag)

  srag$local_bairro <- srag$municipio

  # estado: UF de residência, caindo para a de notificação quando faltar
  uf_res <- if ("SG_UF_RES" %in% names(srag)) srag$SG_UF_RES else NA_character_
  uf_not <- if ("SG_UF_NOT" %in% names(srag)) srag$SG_UF_NOT else NA_character_
  srag$local_estado <- toupper(dplyr::coalesce(uf_res, uf_not, NA_character_))

  # município: pelo código IBGE de 6 dígitos presente em co_mun_res
  srag$local_municipio <- srag$municipio
  if ("CO_MUN_RES" %in% names(srag)) {
    mapa <- tryCatch(ibge_municipios(), error = function(e) NULL)
    if (!is.null(mapa) && nrow(mapa) > 0) {
      chave <- substr(as.character(srag$CO_MUN_RES), 1, 6)
      nome <- unname(stats::setNames(mapa$nome, mapa$codigo6)[chave])

      # territórios sem código IBGE (as RAs do DF) herdam o único município da
      # sua UF, quando ela tiver apenas um - que é exatamente o caso do DF
      sem <- is.na(nome)
      if (any(sem)) {
        por_uf <- mapa %>% dplyr::count(uf, name = "n") %>% dplyr::filter(n == 1)
        if (nrow(por_uf) > 0) {
          unico <- mapa %>% dplyr::filter(uf %in% por_uf$uf) %>%
            dplyr::select(uf, nome)
          nome[sem] <- unname(stats::setNames(unico$nome, unico$uf)[srag$local_estado[sem]])
        }
      }
      srag$local_municipio <- dplyr::coalesce(nome, srag$municipio)
    }
  }
  srag
}

#' Opções disponíveis em cada nível, ordenadas por número de casos.
opcoes_localidade <- function(srag, nivel) {
  if (!nivel %in% names(srag)) return(character(0))
  v <- srag[[nivel]]
  v <- v[!is.na(v) & nzchar(v)]
  names(sort(table(v), decreasing = TRUE))
}
