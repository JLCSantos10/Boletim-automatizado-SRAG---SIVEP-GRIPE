# =============================================================================
# Mapas coropléticos
#
# As malhas vêm da pasta Mapas/ (shapefiles). A leitura é cara — o arquivo de
# municípios tem 309 MB — então cada malha é lida uma vez, recortada no driver,
# projetada para WGS84, simplificada e guardada em Bancos/cache como .rds.
#
# Camadas e chave de junção com a base de casos:
#   bairro     Regiões Administrativas do DF   nome padronizado
#   municipio  municípios do IBGE              código IBGE de 6 dígitos
#   estado     Unidades da Federação do IBGE   sigla da UF
#
# No DF o campo co_mun_res não distingue as RAs, por isso a camada de município
# é ligada pelo nome do município do IBGE (local_municipio), e não pelo código
# da notificação.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tibble)
})

DIR_MAPAS <- "Mapas"

CAMADAS_MAPA <- c(
  bairro    = "Bairro / Região Administrativa",
  municipio = "Município",
  estado    = "Estado (UF)"
)

# dir      pasta dentro de Mapas/
# nome     coluna com o nome do território no shapefile
# uf       coluna de UF, quando existir, usada para recortar na leitura
# tol      tolerância da simplificação, em graus (0,001° ~ 100 m)
# ufs      UFs a que a malha se aplica; NULL = todas
# min_ufs  quantas UFs o recorte precisa ter para a camada fazer sentido
MALHAS <- list(
  bairro = list(dir = "regioes_administrativas", nome = "ra_nome", uf = NULL,
                tol = 0.0005, ufs = "DF", min_ufs = 1),
  municipio = list(dir = "BR_Municipios_2025", nome = "NM_MUN", uf = "SIGLA_UF",
                   tol = 0.003, ufs = NULL, min_ufs = 1),
  # com uma única UF em tela esta camada seria um polígono só: não é um mapa
  estado = list(dir = "BR_UF_2025", nome = "NM_UF", uf = "SIGLA_UF",
                tol = 0.02, ufs = NULL, min_ufs = 2)
)

# Preenchimento e contorno dos territórios sem valor a mostrar. A linha é
# discreta de propósito: serve para dar forma à área vazia, não para competir
# com o dado.
CINZA_MAPA <- "#F5F5F5"
LINHA_VAZIA_MAPA <- "rgba(0,0,0,0.35)"

# O shapefile das RAs chama "SCIA" o que a base de casos registra como
# "estrutural scia".
DEPARA_MAPA <- c("estrutural scia" = "scia")

arquivo_malha <- function(camada) {
  m <- MALHAS[[camada]]
  if (is.null(m)) return(NA_character_)
  arqs <- list.files(file.path(DIR_MAPAS, m$dir), pattern = "[.]shp$", full.names = TRUE)
  if (length(arqs) == 0) NA_character_ else arqs[1]
}

malha_disponivel <- function(camada) {
  !is.na(arquivo_malha(camada)) && requireNamespace("sf", quietly = TRUE)
}

#' Camadas que podem ser oferecidas no seletor.
#'
#' Só entram as que têm shapefile em Mapas/ e que fazem sentido para as UFs em
#' tela — a malha de RAs, por exemplo, só serve ao Distrito Federal.
camadas_disponiveis <- function(ufs = NULL) {
  ok <- vapply(names(CAMADAS_MAPA), function(cm) {
    if (!malha_disponivel(cm)) return(FALSE)
    m <- MALHAS[[cm]]
    if (length(ufs) > 0 && length(ufs) < (m$min_ufs %||% 1)) return(FALSE)
    is.null(m$ufs) || length(ufs) == 0 || any(ufs %in% m$ufs)
  }, logical(1))
  CAMADAS_MAPA[ok]
}

#' Camadas que valem a pena oferecer para o contexto em tela.
#'
#' Além de exigir a malha em Mapas/, descarta a camada que renderia um
#' território só: no DF, por exemplo, "Município" seria um polígono único
#' (Brasília), o que não é um mapa.
camadas_para_contexto <- function(B, minimo = 2L) {
  op <- camadas_disponiveis(ufs_do_contexto(B))
  if (length(op) == 0 || is.null(B$srag_ref)) return(op)

  ok <- vapply(names(op), function(cm) {
    col <- switch(cm, bairro = "local_bairro", municipio = "local_municipio",
                  estado = "local_estado", NULL)
    if (is.null(col) || !col %in% names(B$srag_ref)) return(FALSE)
    loc <- unique(B$srag_ref[[col]])
    loc <- loc[!is.na(loc) & nzchar(loc)]
    sum(territorio_na_uf(B, loc, cm)) >= minimo
  }, logical(1))

  op[ok]
}

#' UFs que o mapa deve cobrir.
#'
#' Com uma UF escolhida na barra lateral, o mapa fica nela. A base vem filtrada
#' pela UF de *notificação*, então um caso notificado no DF pode ser de um
#' residente de Goiás: ele conta nos indicadores, mas não deve espalhar
#' polígonos soltos pelo país.
ufs_do_contexto <- function(B) {
  uf <- B$cfg$estado %||% character(0)
  uf <- uf[!is.na(uf) & nzchar(uf)]
  if (length(uf) > 0) return(uf)

  u <- if (!is.null(B$srag_ref) && "local_estado" %in% names(B$srag_ref)) {
    unique(B$srag_ref$local_estado)
  } else character(0)
  u <- u[!is.na(u) & nzchar(u)]
  sort(u)
}

#' Lê a malha do shapefile, projeta para WGS84 e simplifica.
#' Sem cache: quem chama é carregar_malha().
malha_ler <- function(camada, ufs = NULL) {
  m <- MALHAS[[camada]]
  arq <- arquivo_malha(camada)
  suppressMessages(sf::sf_use_s2(FALSE))

  # o recorte por UF vai no driver, para não trazer o país inteiro à memória
  nome_sql <- tools::file_path_sans_ext(basename(arq))
  x <- if (!is.null(m$uf) && length(ufs) > 0) {
    lista <- paste0("'", ufs, "'", collapse = ",")
    sf::st_read(arq, quiet = TRUE,
                query = paste0("SELECT * FROM \"", nome_sql, "\" WHERE ",
                               m$uf, " IN (", lista, ")"))
  } else {
    sf::st_read(arq, quiet = TRUE)
  }
  if (nrow(x) == 0) return(NULL)

  x <- sf::st_transform(x, 4326)
  x <- suppressWarnings(sf::st_simplify(x, dTolerance = m$tol, preserveTopology = TRUE))
  x <- x[!sf::st_is_empty(sf::st_geometry(x)), , drop = FALSE]
  if (nrow(x) == 0) return(NULL)

  nome_mapa <- as.character(x[[m$nome]])
  chave <- switch(
    camada,
    bairro    = padronizar_nomes(nome_mapa),
    municipio = substr(as.character(x[["CD_MUN"]]), 1, 6),
    estado    = as.character(x[["SIGLA_UF"]])
  )
  uf <- if (is.null(m$uf)) (m$ufs %||% NA_character_)[1] else as.character(x[[m$uf]])

  out <- sf::st_sf(nome_mapa = nome_mapa, chave = chave, uf = uf,
                   geometry = sf::st_geometry(x))
  out[!is.na(out$chave) & nzchar(out$chave), , drop = FALSE]
}

# Malhas já montadas nesta sessão. Ler 23 arquivos .rds e colá-los custa ~1,5 s,
# e isso aconteceria a cada mudança de filtro.
.MALHAS_MEM <- new.env(parent = emptyenv())

#' Malha de uma camada, recortada, simplificada e em WGS84.
#'
#' O cache em disco guarda um arquivo por UF, e não um por seleção: trocar de
#' UF reaproveita o que já foi lido, e só o que falta vai ao shapefile.
carregar_malha <- function(camada, ufs = NULL, forcar = FALSE) {
  if (!malha_disponivel(camada)) return(NULL)
  chave_mem <- paste0(camada, "|", paste(sort(unique(ufs)), collapse = ","))
  if (!forcar && !is.null(.MALHAS_MEM[[chave_mem]])) return(.MALHAS_MEM[[chave_mem]])

  out <- malha_montar(camada, ufs, forcar)
  if (!is.null(out)) .MALHAS_MEM[[chave_mem]] <- out
  out
}

#' Monta a malha a partir do cache em disco, lendo do shapefile o que faltar.
malha_montar <- function(camada, ufs = NULL, forcar = FALSE) {
  garantir_pastas()
  m <- MALHAS[[camada]]

  # malha sem coluna de UF (as RAs do DF): arquivo único
  if (is.null(m$uf)) {
    destino <- file.path(DIR_CACHE, paste0("malha_", camada, ".rds"))
    if (!forcar && file.exists(destino)) {
      x <- tryCatch(readRDS(destino), error = function(e) NULL)
      if (!is.null(x)) return(x)
    }
    out <- malha_ler(camada, NULL)
    if (!is.null(out)) saveRDS(out, destino, compress = "xz")
    return(out)
  }

  ufs <- sort(unique(ufs[!is.na(ufs) & nzchar(ufs)]))
  if (length(ufs) == 0) return(NULL)

  arqs <- file.path(DIR_CACHE, paste0("malha_", camada, "_", ufs, ".rds"))
  faltam <- if (forcar) rep(TRUE, length(ufs)) else !file.exists(arqs)

  if (any(faltam)) {
    nova <- malha_ler(camada, ufs[faltam])      # uma só consulta ao driver
    if (!is.null(nova)) {
      for (i in which(faltam)) {
        saveRDS(nova[nova$uf == ufs[i], , drop = FALSE], arqs[i], compress = "xz")
      }
    }
  }

  partes <- lapply(arqs[file.exists(arqs)],
                   function(a) tryCatch(readRDS(a), error = function(e) NULL))
  partes <- partes[!vapply(partes, is.null, logical(1))]
  if (length(partes) == 0) return(NULL)
  out <- if (length(partes) == 1) partes[[1]] else do.call(rbind, partes)
  if (nrow(out) == 0) NULL else out
}

#' Converte a malha para o GeoJSON que o plotly espera (lista aninhada).
malha_geojson <- function(malha) {
  tmp <- tempfile(fileext = ".geojson")
  on.exit(unlink(tmp), add = TRUE)
  suppressWarnings(sf::st_write(malha, tmp, driver = "GeoJSON", quiet = TRUE,
                                delete_dsn = TRUE))
  jsonlite::fromJSON(tmp, simplifyVector = FALSE)
}

# =============================================================================
# Agregação dos casos na camada escolhida
#
# Entra o contexto já filtrado (UF, território, período, sexo, raça-cor, faixa
# etária e vírus), de modo que o mapa responde aos mesmos filtros do resto do
# painel.
# =============================================================================

#' Casos, população e incidência por território da camada.
#'
#' Devolve tibble(local, casos, chave, populacao, incidencia, nome).
dados_mapa <- function(B, camada = "municipio") {

  col <- switch(camada, bairro = "local_bairro", municipio = "local_municipio",
                estado = "local_estado", NULL)
  vazio <- tibble::tibble(local = character(0), casos = integer(0),
                          chave = character(0), populacao = numeric(0),
                          incidencia = numeric(0), nome = character(0),
                          na_uf = logical(0))
  if (is.null(col) || is.null(B$srag_ref) || !col %in% names(B$srag_ref)) return(vazio)

  d <- B$srag_ref %>% dplyr::filter(!is.na(.data[[col]]), nzchar(.data[[col]]))
  if (nrow(d) == 0) return(vazio)

  casos <- d %>% dplyr::count(local = .data[[col]], name = "casos")

  # Cada território pertence a uma UF; os de fora do recorte saem do desenho,
  # mas continuam contados para que o texto diga quantos casos ficaram de fora.
  casos$na_uf <- territorio_na_uf(B, casos$local, camada)

  if (identical(camada, "estado")) {
    casos$chave <- casos$local
    pop <- populacao_por_uf_mapa(B, casos$local)
  } else if (identical(camada, "municipio")) {
    casos$chave <- codigo_ibge_municipio(B, casos$local)
    pop <- populacao_por_municipio_mapa(B, casos$local)
  } else {
    casos$chave <- dplyr::coalesce(unname(DEPARA_MAPA[casos$local]), casos$local)
    pop <- populacao_por_bairro_mapa(B, casos$local)
  }

  casos %>%
    dplyr::left_join(pop, by = "local") %>%
    dplyr::mutate(
      populacao  = suppressWarnings(as.numeric(populacao)),
      incidencia = dplyr::if_else(!is.na(populacao) & populacao > 0,
                                  100000 * casos / populacao, NA_real_),
      nome       = if (identical(camada, "estado")) {
        dplyr::coalesce(unname(NOMES_UF[local]), local)
      } else formatar_nome_territorio(local)
    ) %>%
    dplyr::filter(!is.na(chave), nzchar(chave)) %>%
    dplyr::arrange(dplyr::desc(casos))
}

#' Reúne dados e malha de uma camada e separa o que fica de fora do desenho.
#'
#' Os dois mapas e o texto partem deste mesmo resultado. A malha devolvida é a
#' do recorte **inteiro**, e não só a dos territórios com casos: é ela que dá
#' contorno às áreas vazias.
preparar_mapa <- function(B, camada) {
  d <- dados_mapa(B, camada)
  malha <- tryCatch(carregar_malha(camada, ufs_do_contexto(B)), error = function(e) NULL)
  fonte <- fonte_populacao_mapa(B, camada)

  fora_uf <- d[!d$na_uf, , drop = FALSE]        # residentes de outra UF
  d <- d[d$na_uf, , drop = FALSE]

  if (is.null(malha) || nrow(malha) == 0) {
    return(list(camada = camada, dados = d, malha = NULL,
                fora = d[0, , drop = FALSE], fora_uf = fora_uf,
                fonte_pop = fonte))
  }

  dentro <- d$chave %in% malha$chave
  list(
    camada    = camada,
    dados     = d[dentro, , drop = FALSE],
    malha     = malha,
    fora      = d[!dentro, , drop = FALSE],     # sem polígono nesta camada
    fora_uf   = fora_uf,
    fonte_pop = fonte
  )
}

#' De onde veio a população usada na incidência daquela camada.
fonte_populacao_mapa <- function(B, camada) {
  if (identical(camada, "estado")) "estimativas do IBGE por UF"
  else B$fonte_populacao %||% "API do IBGE (SIDRA)"
}

#' Cada território pertence à UF do recorte?
#'
#' A UF de um território é a que aparece na maioria das suas notificações —
#' o mesmo critério já usado no diagrama de controle por município.
territorio_na_uf <- function(B, locais, camada) {
  ufs <- ufs_do_contexto(B)
  if (length(ufs) == 0 || !"local_estado" %in% names(B$srag_ref)) {
    return(rep(TRUE, length(locais)))
  }
  if (identical(camada, "estado")) return(locais %in% ufs)

  col <- if (identical(camada, "bairro")) "local_bairro" else "local_municipio"
  de_para <- B$srag_ref %>%
    dplyr::filter(!is.na(.data[[col]]), !is.na(local_estado)) %>%
    dplyr::count(local = .data[[col]], local_estado) %>%
    dplyr::group_by(local) %>%
    dplyr::slice_max(n, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup()

  uf <- unname(stats::setNames(de_para$local_estado, de_para$local)[locais])
  !is.na(uf) & uf %in% ufs
}

#' Código IBGE de 6 dígitos de cada município, pelo nome e pela UF.
codigo_ibge_municipio <- function(B, municipios) {
  mapa <- tryCatch(ibge_municipios(), error = function(e) NULL)
  if (is.null(mapa) || nrow(mapa) == 0) return(rep(NA_character_, length(municipios)))

  uf_de <- B$srag_ref %>%
    dplyr::filter(!is.na(local_municipio)) %>%
    dplyr::count(local_municipio, local_estado) %>%
    dplyr::group_by(local_municipio) %>%
    dplyr::slice_max(n, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup()
  uf <- unname(stats::setNames(uf_de$local_estado, uf_de$local_municipio)[municipios])

  cod <- unname(stats::setNames(mapa$codigo6, paste(mapa$nome, mapa$uf))[
    paste(municipios, uf)])

  # sem UF conhecida, aceita o nome quando ele é único no país
  falta <- is.na(cod)
  if (any(falta)) {
    unicos <- mapa %>% dplyr::count(nome) %>% dplyr::filter(n == 1) %>% dplyr::pull(nome)
    so_nome <- stats::setNames(mapa$codigo6, mapa$nome)
    alvo <- ifelse(municipios[falta] %in% unicos, municipios[falta], NA_character_)
    cod[falta] <- unname(so_nome[alvo])
  }
  cod
}

#' População de cada UF (IBGE, nível n3).
populacao_por_uf_mapa <- function(B, ufs) {
  vazio <- tibble::tibble(local = character(0), populacao = numeric(0))
  out <- tryCatch(ibge_populacao_uf(B$cfg$ano_ref_epi), error = function(e) NULL)
  if (is.null(out) || nrow(out) == 0) return(vazio)
  out %>%
    dplyr::filter(!is.na(sigla), sigla %in% ufs) %>%
    dplyr::group_by(local = sigla) %>%
    dplyr::summarise(populacao = sum(populacao, na.rm = TRUE), .groups = "drop")
}

#' População por município: soma os territórios da base que caem no mesmo
#' município — no DF, todas as RAs somam no total de Brasília.
populacao_por_municipio_mapa <- function(B, municipios) {
  vazio <- tibble::tibble(local = character(0), populacao = numeric(0))
  if (is.null(B$pop_municipio) || nrow(B$pop_municipio) == 0) return(vazio)

  de_para <- B$srag_ref %>%
    dplyr::filter(!is.na(local_municipio)) %>%
    dplyr::distinct(municipio, local_municipio)

  B$pop_municipio %>%
    dplyr::filter(ano == B$cfg$ano_ref_epi) %>%
    dplyr::inner_join(de_para, by = "municipio") %>%
    dplyr::group_by(local = local_municipio) %>%
    dplyr::summarise(populacao = sum(populacao, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(local %in% municipios)
}

#' População de cada território da notificação (RA, no DF).
populacao_por_bairro_mapa <- function(B, bairros) {
  vazio <- tibble::tibble(local = character(0), populacao = numeric(0))
  if (is.null(B$pop_municipio) || nrow(B$pop_municipio) == 0) return(vazio)
  B$pop_municipio %>%
    dplyr::filter(ano == B$cfg$ano_ref_epi, municipio %in% bairros) %>%
    dplyr::group_by(local = municipio) %>%
    dplyr::summarise(populacao = sum(populacao, na.rm = TRUE), .groups = "drop")
}

# =============================================================================
# Desenho
# =============================================================================

ESCALAS_MAPA <- list(
  casos = list(cor = "Blues", titulo = "Número de casos", legenda = "Casos"),
  incidencia = list(cor = "YlOrRd", titulo = "Incidência",
                    legenda = "/100 mil hab.")
)

#' Mapa coroplético de uma medida: "casos" ou "incidencia".
#' Recebe o resultado de preparar_mapa().
mapa_plotly <- function(m, medida = "casos", altura = 420) {

  cfg_cor <- ESCALAS_MAPA[[medida]] %||% ESCALAS_MAPA$casos
  malha <- if (is.null(m)) NULL else m$malha
  dados <- if (is.null(m)) NULL else m$dados

  if (is.null(malha) || nrow(malha) == 0) {
    return(pl_aviso(paste("Malha indisponível para esta camada.",
                          "Verifique a pasta Mapas/."), altura))
  }
  if (is.null(dados) || nrow(dados) == 0) {
    return(pl_aviso("Sem casos para mapear com os filtros atuais.", altura))
  }

  d <- dados
  d$valor <- if (identical(medida, "incidencia")) d$incidencia else as.numeric(d$casos)
  d <- dplyr::filter(d, !is.na(valor))
  if (nrow(d) == 0) {
    return(pl_aviso(if (identical(medida, "incidencia"))
      "Sem população de referência para calcular a incidência destes territórios."
      else "Sem casos para mapear com os filtros atuais.", altura))
  }

  gj <- tryCatch(malha_geojson(malha), error = function(e) NULL)
  if (is.null(gj)) return(pl_aviso("Não foi possível preparar a malha.", altura))

  rotulo <- paste0(
    "<b>", d$nome, "</b><br>", formatar_n(d$casos), " casos<br>",
    dplyr::if_else(is.na(d$incidencia), "incidência indisponível",
                   paste0(formatar_dec(d$incidencia, 1), " / 100 mil hab.")),
    "<extra></extra>")

  p <- plotly::plot_ly()

  # Fundo: os territórios da camada que não entram nesta medida. Sem ele a área
  # fica branca e sem forma, e não dá para saber o que é "zero" e o que é
  # "fora do mapa".
  vazios <- malha$nome_mapa[!(malha$chave %in% d$chave)]
  chaves_vazias <- malha$chave[!(malha$chave %in% d$chave)]
  if (length(chaves_vazias) > 0) {
    sem <- if (identical(medida, "incidencia")) "sem incidência calculada"
           else "sem casos no período"
    p <- p %>% plotly::add_trace(
      type = "choropleth", geojson = gj, featureidkey = "properties.chave",
      locations = chaves_vazias, z = rep(0, length(chaves_vazias)),
      zmin = 0, zmax = 1, showscale = FALSE,
      colorscale = list(list(0, CINZA_MAPA), list(1, CINZA_MAPA)),
      hovertemplate = paste0("<b>", formatar_nome_territorio(vazios), "</b><br>",
                             sem, "<extra></extra>"),
      marker = list(line = list(width = 0.6, color = LINHA_VAZIA_MAPA))
    )
  }

  p <- p %>%
    plotly::add_trace(
      type = "choropleth", geojson = gj, featureidkey = "properties.chave",
      locations = d$chave, z = d$valor, hovertemplate = rotulo,
      colorscale = cfg_cor$cor, reversescale = FALSE,
      marker = list(line = list(width = 0.4, color = "white")),
      colorbar = list(title = list(text = cfg_cor$legenda, side = "right"),
                      thickness = 12, len = 0.85, x = 1)
    ) %>%
    plotly::layout(
      geo = list(fitbounds = "locations", visible = FALSE,
                 projection = list(type = "mercator")),
      margin = list(l = 4, r = 4, t = 36, b = 4),
      annotations = list(list(
        x = 0, y = 1.07, xref = "paper", yref = "paper", xanchor = "left",
        text = paste0("<b>", cfg_cor$titulo, "</b>"), showarrow = FALSE,
        font = list(size = 13, color = "#333333")))
    )

  p$x$layout$height <- altura
  cfg_plotly(p)
}

# Como o nível entra no meio da frase.
NIVEL_FRASE_MAPA <- c(
  bairro    = "bairro ou região administrativa",
  municipio = "município",
  estado    = "unidade da federação"
)

#' Texto que acompanha os dois mapas. Recebe o resultado de preparar_mapa().
txt_mapa <- function(B, m) {
  if (is.null(m) || is.null(m$dados) || nrow(m$dados) == 0) {
    return("Sem casos para mapear com os filtros atuais.")
  }
  d <- dplyr::arrange(m$dados, dplyr::desc(casos))
  nivel <- unname(NIVEL_FRASE_MAPA[m$camada]) %||% "território"

  texto <- glue::glue(
    "Os mapas mostram os casos de SRAG {B$rotulo_territorio_frase} ",
    "{B$rotulo_periodo_frase}, agregados por {nivel} de residência. ",
    "O maior número absoluto foi em **{d$nome[1]}** ({formatar_n(d$casos[1])} casos)")
  if (nrow(d) >= 2) {
    texto <- glue::glue(texto, ", seguido por {d$nome[2]} ({formatar_n(d$casos[2])})")
  }
  texto <- glue::glue(texto, ".")

  di <- d %>% dplyr::filter(!is.na(incidencia)) %>% dplyr::arrange(dplyr::desc(incidencia))
  if (nrow(di) > 0 && !identical(di$local[1], d$local[1])) {
    texto <- glue::glue(
      texto, " Pela incidência, porém, o maior risco por habitante foi em ",
      "**{di$nome[1]}** ({formatar_dec(di$incidencia[1], 1)} por 100 mil), o que separa ",
      "volume de casos de intensidade da transmissão: territórios populosos ",
      "concentram casos sem necessariamente ter o maior risco individual.")
  } else if (nrow(di) > 0) {
    texto <- glue::glue(
      texto, " {di$nome[1]} lidera também em incidência ",
      "({formatar_dec(di$incidencia[1], 1)} por 100 mil).")
  }

  texto <- glue::glue(
    texto, " A incidência usa a população de {B$cfg$ano_ref_epi} ",
    "({m$fonte_pop}).")

  sem_pop <- sum(is.na(d$incidencia))
  if (sem_pop > 0) {
    texto <- glue::glue(
      texto, " {sem_pop} território(s) aparecem apenas no mapa de casos, ",
      "por falta de população de referência.")
  }

  # casos de residentes de outra UF: contam nos indicadores, mas o mapa fica
  # no recorte escolhido
  if (!is.null(m$fora_uf) && nrow(m$fora_uf) > 0) {
    fu <- dplyr::arrange(m$fora_uf, dplyr::desc(casos))
    texto <- glue::glue(
      texto, " O mapa cobre apenas o recorte selecionado: outros ",
      "{formatar_n(sum(fu$casos))} casos são de residentes de fora dele ",
      "(o maior volume vem de {fu$nome[1]}, com {formatar_n(fu$casos[1])}) e ",
      "entram nos indicadores, mas não no desenho.")
  }

  # territórios do recorte que a malha desta camada não cobre
  if (!is.null(m$fora) && nrow(m$fora) > 0) {
    fora <- dplyr::arrange(m$fora, dplyr::desc(casos))
    texto <- glue::glue(
      texto, " Outros {formatar_n(nrow(fora))} território(s), com ",
      "{formatar_n(sum(fora$casos))} casos, não têm polígono nesta camada ",
      "(o maior é {fora$nome[1]}); troque a camada para vê-los.")
  }
  as.character(texto)
}
