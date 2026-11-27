# =============================================================================
# Painel SRAG — ponto de entrada
#
# Windows : duplo clique em abrir_painel.bat
# Linux   : ./abrir_painel_linux.sh
# RStudio : abra este arquivo e clique em "Run App"
#
# Dados    -> API de Dados Abertos do SUS ou arquivo do usuário
# População -> API do IBGE (SIDRA)
# Cache    -> Bancos/cache (.json e .rds), para abrir rápido
# Boletim  -> só pelo botão do painel (não existe mais geração isolada)
# =============================================================================

options(encoding = "UTF-8")
if (is.null(getOption("repos")) || is.na(getOption("repos")["CRAN"]) ||
    getOption("repos")["CRAN"] %in% c("", "@CRAN@")) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
}

pacotes <- c("shiny", "bslib", "DT", "plotly", "jsonlite", "curl",
             "dplyr", "tidyr", "ggplot2", "readr", "stringr", "stringi",
             "lubridate", "glue", "scales", "tibble", "forcats", "purrr",
             "slider", "EpiEstim", "rmarkdown", "knitr")
faltando <- pacotes[!(pacotes %in% rownames(utils::installed.packages()))]
if (length(faltando) > 0) utils::install.packages(faltando)

library(shiny); library(bslib); library(DT); library(plotly)

for (f in c("R/funcoes_boletim.R", "R/fontes_dados.R", "R/nowcasting.R", "R/graficos.R")) {
  source(f, encoding = "UTF-8")
}

options(shiny.maxRequestSize = 500 * 1024^2)
garantir_pastas()

PREFS <- ler_preferencias()

UFS <- stats::setNames(names(NOMES_UF), paste0(names(NOMES_UF), " — ", unname(NOMES_UF)))

DT_PT <- list(
  sEmptyTable = "Nenhum registro encontrado",
  sInfo = "Mostrando _START_ a _END_ de _TOTAL_ registros",
  sInfoEmpty = "Mostrando 0 registros", sInfoFiltered = "(filtrado de _MAX_)",
  sLengthMenu = "Mostrar _MENU_ registros", sLoadingRecords = "Carregando...",
  sProcessing = "Processando...", sSearch = "Buscar:",
  sZeroRecords = "Nenhum registro encontrado",
  oPaginate = list(sFirst = "Primeiro", sLast = "Último",
                   sNext = "Próximo", sPrevious = "Anterior")
)
opcoes_dt <- function(...) utils::modifyList(
  list(pageLength = 15, scrollX = TRUE, language = DT_PT), list(...))

cartao <- function(titulo, id_plot, id_texto = NULL, altura = 420) {
  bslib::card(
    bslib::card_header(titulo),
    plotly::plotlyOutput(id_plot, height = paste0(altura, "px")),
    if (!is.null(id_texto)) bslib::card_footer(uiOutput(id_texto))
  )
}

# =============================================================================
# UI
# =============================================================================

ui <- bslib::page_navbar(
  title = "Painel SRAG",
  theme = bslib::bs_theme(version = 5, preset = "flatly"),
  fillable = FALSE,

  sidebar = bslib::sidebar(
    width = 350,
    title = "Filtros",

    # ---------------------------------------------------------- fonte de dados
    radioButtons("fonte", "Fonte dos dados",
                 choiceNames = list(
                   tagList(tags$b("Dados via API"),
                           tags$div(class = "text-muted small",
                                    textOutput("sub_api", inline = TRUE))),
                   tagList(tags$b("Meus próprios dados"),
                           tags$div(class = "text-muted small",
                                    "envie um .csv ou .rds com os casos"))
                 ),
                 choiceValues = c("api", "arquivo"),
                 selected = PREFS$fonte),

    conditionalPanel(
      "input.fonte == 'arquivo'",
      fileInput("arquivo", NULL, accept = c(".csv", ".rds", ".CSV", ".RDS"),
                buttonLabel = "Escolher...", placeholder = "nenhum arquivo"),
      div(class = "text-muted small mb-2", textOutput("sub_arquivo"))
    ),

    hr(),

    selectInput("uf", "UF",
                choices = c("Selecione a UF..." = "", UFS),
                selected = PREFS$uf),

    radioButtons("escopo", "Abrangência",
                 choices = c("UF inteira" = "uf", "Selecionar territórios" = "mun"),
                 selected = PREFS$escopo),
    conditionalPanel(
      "input.escopo == 'mun'",
      selectizeInput("municipios", NULL, choices = NULL, multiple = TRUE,
                     options = list(placeholder = "digite para buscar...",
                                    plugins = list("remove_button")))
    ),

    hr(),

    radioButtons("modo", "Intervalo de tempo",
                 choices = c("Ano inteiro" = "ano", "Intervalo de datas" = "datas",
                             "Semanas epidemiológicas" = "semanas"),
                 selected = PREFS$modo_periodo),
    conditionalPanel("input.modo != 'datas'",
                     numericInput("ano_ref", "Ano de referência", PREFS$ano_ref,
                                  2000, 2100, 1)),
    conditionalPanel("input.modo == 'datas'",
                     dateRangeInput("datas", "Período", language = "pt-BR",
                                    format = "dd/mm/yyyy", separator = " até ")),
    conditionalPanel("input.modo == 'semanas'",
                     sliderInput("semanas", "Semanas epidemiológicas", 1, 53,
                                 c(PREFS$se_inicio, PREFS$se_fim), 1)),

    hr(),

    # Os demais filtros ficam recolhidos em dois grupos, para a barra lateral
    # não virar uma lista interminável.
    bslib::accordion(
      open = FALSE, class = "mb-2",

      bslib::accordion_panel(
        "Demografia", value = "demo", icon = icon("users"),
        selectizeInput("f_sexo", "Sexo", choices = NIVEIS_SEXO, multiple = TRUE,
                       options = list(placeholder = "todos",
                                      plugins = list("remove_button"))),
        selectizeInput("f_raca", "Raça/cor", choices = NIVEIS_RACA, multiple = TRUE,
                       options = list(placeholder = "todas",
                                      plugins = list("remove_button"))),
        selectizeInput("f_faixa", "Faixa etária", choices = NIVEIS_IDADE, multiple = TRUE,
                       options = list(placeholder = "todas",
                                      plugins = list("remove_button"))),
        div(class = "text-muted small", textOutput("sub_demografia"))
      ),

      bslib::accordion_panel(
        "Estatística", value = "estat", icon = icon("chart-column"),
        selectizeInput("f_virus", "Tipo de vírus", choices = unname(VIRUS_MAP),
                       multiple = TRUE,
                       options = list(placeholder = "todos",
                                      plugins = list("remove_button"))),
        div(class = "text-muted small mb-3",
            "Mantém apenas os casos com detecção positiva do(s) vírus escolhido(s)."),
        sliderInput("f_anos", "Intervalo de anos (série histórica)",
                    min = 2019, max = as.integer(format(Sys.Date(), "%Y")),
                    value = c(PREFS$filtro_ano_ini, PREFS$filtro_ano_fim),
                    step = 1, sep = ""),
        numericInput("anos_hist", "Anos de histórico (limiares)",
                     PREFS$anos_historico, 2, 20, 1),
        textInput("ano_epid", "Ano(s) epidêmico(s) a excluir", PREFS$ano_epidemico),
        numericInput("nowcast", "Nowcasting: semanas a corrigir",
                     PREFS$semanas_nowcast, 0, 12, 1),
        div(class = "text-muted small mb-3", textOutput("sub_nowcast")),
        fluidRow(
          column(6, numericInput("diag_max", "Municípios no diagrama",
                                 PREFS$diag_max, 1, 40, 1)),
          column(6, numericInput("diag_colunas", "Colunas do diagrama",
                                 PREFS$diag_colunas, 1, 5, 1))
        ),
        div(class = "text-muted small", textOutput("sub_diagrama"))
      )
    ),

    actionButton("limpar_filtros", "Limpar filtros de demografia e vírus",
                 class = "btn-outline-secondary btn-sm w-100 mb-2"),

    hr(),

    actionButton("abrir_modal_docx", "Gerar boletim em Word",
                 class = "btn-primary w-100", icon = icon("file-word")),
    br(), br(),
    downloadButton("baixar_csv", "Baixar dados filtrados (CSV)",
                   class = "btn-outline-secondary w-100"),
    br(), br(),
    actionButton("atualizar", "Atualizar dados da fonte",
                 class = "btn-outline-secondary w-100", icon = icon("rotate")),
    div(class = "text-muted small mt-2", textOutput("sub_cache"))
  ),

  bslib::nav_panel(
    "Visão geral", icon = icon("chart-line"),
    uiOutput("cabecalho"),
    bslib::layout_columns(
      fill = FALSE,
      bslib::value_box("Casos no período", textOutput("vb_casos"),
                       showcase = icon("hospital-user"), theme = "primary"),
      bslib::value_box("Óbitos", textOutput("vb_obitos"),
                       showcase = icon("heart-crack"), theme = "danger"),
      bslib::value_box("Letalidade", textOutput("vb_letal"),
                       showcase = icon("percent"), theme = "warning"),
      bslib::value_box("Incidência /100 mil", textOutput("vb_inc"),
                       showcase = icon("users"), theme = "info")
    ),
    cartao("Casos por semana epidemiológica e ano", "g_ano", "t_ondas"),
    cartao("Casos, incidência e nowcasting", "g_inc", "t_resumo", 460),
    bslib::card(bslib::card_header("Resumo da situação epidemiológica"),
                tableOutput("tab_resumo"))
  ),

  bslib::nav_panel(
    "Vírus", icon = icon("virus"),
    cartao("Vírus detectados por PCR", "g_virus", "t_virus"),
    cartao("Composição viral por semana epidemiológica", "g_virus_sem", "t_virus_sem"),
    cartao("Vírus por semana e faixa etária", "g_virus_fe", "t_virus_fe", 640)
  ),

  bslib::nav_panel(
    "Perfil dos casos", icon = icon("user-group"),
    bslib::layout_columns(
      col_widths = c(6, 6),
      cartao("Faixa etária", "g_faixa", "t_faixa", 380),
      cartao("Suporte ventilatório", "g_vent", "t_vent", 380)
    ),
    bslib::layout_columns(
      col_widths = c(6, 6),
      cartao("Tempo de internação", "g_intern", "t_intern", 380),
      cartao("Evolução do caso", "g_evol", "t_evol", 380)
    ),
    cartao("Raça/cor", "g_raca", "t_raca", 420),
    cartao("Hospitalizações por unidade", "g_hosp", "t_hosp", 520)
  ),

  bslib::nav_panel(
    "Menores de 2 anos", icon = icon("baby"),
    cartao("Distribuição por idade", "g_bebes", "t_bebes", 400),
    cartao("Vírus detectados por idade", "g_bebes_virus", "t_bebes_virus", 400)
  ),

  bslib::nav_panel(
    "Óbitos", icon = icon("heart-crack"),
    cartao("Óbitos por semana epidemiológica", "g_obitos", "t_obitos", 400),
    bslib::card(bslib::card_header("Detalhamento dos óbitos"), DT::DTOutput("tab_obitos"))
  ),

  bslib::nav_panel(
    "Diagrama de controle", icon = icon("traffic-light"),
    cartao("Diagrama de controle — território selecionado", "g_diag", "t_diag", 460),
    bslib::card(bslib::card_header("Diagrama de controle por município"),
                uiOutput("bloco_diag_mun")),
    bslib::card(bslib::card_header("Classificação de risco por território"),
                DT::DTOutput("tab_zonas"))
  ),

  bslib::nav_panel(
    "Transmissibilidade (Rt)", icon = icon("arrow-trend-up"),
    bslib::card(
      bslib::card_header("Rt por localidade"),
      div(
        class = "px-3 py-2",
        bslib::layout_columns(
          col_widths = c(6, 6), fill = FALSE,
          selectInput("rt_nivel", "Nível da localidade",
                      choices = c("Bairro / Região Administrativa" = "local_bairro",
                                  "Município" = "local_municipio",
                                  "Estado (UF)" = "local_estado"),
                      selected = PREFS$rt_nivel),
          numericInput("rt_colunas", "Facetas por linha", PREFS$rt_colunas, 1, 4, 1)
        ),

        selectizeInput("rt_virus", "Vírus a exibir nas facetas", choices = NULL,
                       multiple = TRUE, width = "100%",
                       options = list(placeholder = "todos os vírus do período",
                                      plugins = list("remove_button"))),
        div(class = "d-flex gap-2 flex-wrap mb-3",
            actionButton("rt_virus_todos", "Selecionar todos os vírus",
                         class = "btn-outline-secondary btn-sm", icon = icon("check-double")),
            actionButton("rt_virus_limpar", "Limpar vírus",
                         class = "btn-outline-secondary btn-sm")),

        selectizeInput("rt_itens", "Localidades", choices = NULL, multiple = TRUE,
                       width = "100%",
                       options = list(placeholder = "escolha as localidades...",
                                      plugins = list("remove_button"))),
        div(class = "d-flex gap-2 flex-wrap",
            actionButton("rt_todos", "Selecionar todas as localidades",
                         class = "btn-outline-secondary btn-sm", icon = icon("check-double")),
            actionButton("rt_limpar", "Limpar localidades",
                         class = "btn-outline-secondary btn-sm"),
            actionButton("calc_rt", "Calcular todas (para a tabela)",
                         class = "btn-outline-primary btn-sm", icon = icon("table"))),

        tags$p(class = "text-muted small mt-2 mb-0",
               paste("Cada localidade vira uma seção abaixo. Expanda para ver o Rt",
                     "facetado por vírus — o cálculo roda só ao expandir, porque é",
                     "a parte mais pesada do painel."))
      )
    ),

    uiOutput("rt_sanfona"),

    bslib::card(bslib::card_header("Rt mais recente por localidade e vírus"),
                DT::DTOutput("tab_rt"), bslib::card_footer(uiOutput("t_rt")))
  ),

  bslib::nav_panel(
    "Dados", icon = icon("table"),
    bslib::card(bslib::card_header("Casos do período selecionado"), DT::DTOutput("tab_dados"))
  )
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {

  # parâmetros que não ficam na barra lateral (vêm das preferências e do modal)
  av <- reactiveValues(
    filtro_ano_ini  = PREFS$filtro_ano_ini,
    filtro_ano_fim  = PREFS$filtro_ano_fim,
    anos_historico  = PREFS$anos_historico,
    ano_epidemico   = PREFS$ano_epidemico,
    limiares_dp     = PREFS$limiares_dp,
    nome_territorio = PREFS$nome_territorio
  )
  arquivo_enviado <- reactiveVal(PREFS$arquivo_proprio %||% "")
  recarregar <- reactiveVal(0L)

  # ---------------------------------------------------------------- subtítulos
  intervalo_api <- reactive({
    invalidateLater(1000 * 60 * 60)
    tryCatch(api_srag_intervalo(), error = function(e) NULL)
  })
  output$sub_api <- renderText(rotulo_intervalo_api(intervalo_api()))

  output$sub_arquivo <- renderText({
    a <- arquivo_enviado()
    if (!nzchar(a) || !file.exists(a)) "nenhum arquivo carregado"
    else paste0("em uso: ", basename(a))
  })

  # --------------------------------------------------------- preferências vivas
  # Uma unica gravacao por flush: dois observadores gravando o mesmo arquivo
  # se sobrescreviam e a UF se perdia.
  observe({
    req(input$fonte, input$modo)
    salvar_preferencias(c(
      if (nzchar(input$uf %||% "")) list(uf = input$uf) else NULL,
      list(
      fonte = input$fonte, escopo = input$escopo,
      municipios = paste(input$municipios %||% character(0), collapse = ";"),
      modo_periodo = input$modo, ano_ref = input$ano_ref,
      se_inicio = input$semanas[1], se_fim = input$semanas[2],
      data_inicio = as.character(input$datas[1]), data_fim = as.character(input$datas[2]),
      semanas_nowcast = input$nowcast,
      diag_max = input$diag_max %||% 12,
      diag_colunas = input$diag_colunas %||% 3,
      filtro_ano_ini = (input$f_anos %||% c(NA, NA))[1],
      filtro_ano_fim = (input$f_anos %||% c(NA, NA))[2],
      anos_historico = input$anos_hist,
      ano_epidemico = input$ano_epid %||% "",
      f_sexo  = paste(input$f_sexo  %||% character(0), collapse = ";"),
      f_raca  = paste(input$f_raca  %||% character(0), collapse = ";"),
      f_faixa = paste(input$f_faixa %||% character(0), collapse = ";"),
      f_virus = paste(input$f_virus %||% character(0), collapse = ";"),
      rt_nivel = input$rt_nivel %||% "local_bairro",
      rt_colunas = input$rt_colunas %||% 2,
      rt_itens = paste(input$rt_itens %||% character(0), collapse = ";"),
      rt_virus = paste(input$rt_virus %||% character(0), collapse = ";")
    )))
  })

  # restaura os filtros guardados na abertura
  observeEvent(TRUE, once = TRUE, {
    partes <- function(x) {
      v <- strsplit(x %||% "", ";")[[1]]
      v[nzchar(v)]
    }
    updateSelectizeInput(session, "f_sexo",  selected = partes(PREFS$f_sexo))
    updateSelectizeInput(session, "f_raca",  selected = partes(PREFS$f_raca))
    updateSelectizeInput(session, "f_faixa", selected = partes(PREFS$f_faixa))
    updateSelectizeInput(session, "f_virus", selected = partes(PREFS$f_virus))
  })

  observeEvent(input$limpar_filtros, {
    for (id in c("f_sexo", "f_raca", "f_faixa", "f_virus")) {
      updateSelectizeInput(session, id, selected = character(0))
    }
    showNotification("Filtros de demografia e vírus limpos.", type = "message")
  })

  output$sub_diagrama <- renderText({
    dm <- tryCatch(DM(), error = function(e) NULL)
    if (is.null(dm) || !isTRUE(dm$ok)) return("diagrama por município indisponível")
    disp <- dm$n_disponiveis %||% 0L; most <- dm$n_mostrados %||% 0L
    if (disp > most) paste0("mostrando ", most, " de ", disp, " territórios com população")
    else paste0(most, " território(s) no diagrama")
  })

  output$sub_demografia <- renderText({
    n <- length(input$f_sexo %||% character(0)) + length(input$f_raca %||% character(0)) +
         length(input$f_faixa %||% character(0)) + length(input$f_virus %||% character(0))
    if (n == 0) "nenhum filtro aplicado" else paste0(n, " filtro(s) ativo(s)")
  })

  observeEvent(input$arquivo, {
    req(input$arquivo)
    destino <- file.path(DIR_CACHE, paste0("enviado_", input$arquivo$name))
    file.copy(input$arquivo$datapath, destino, overwrite = TRUE)
    arquivo_enviado(destino)
    salvar_preferencias(list(arquivo_proprio = destino, fonte = "arquivo"))
    recarregar(recarregar() + 1L)
  })

  observeEvent(input$atualizar, {
    cache_limpar("^(base_|api_intervalo)")
    recarregar(recarregar() + 1L)
    showNotification("Cache limpo. Recarregando os dados da fonte...", type = "message")
  })

  # ------------------------------------------------------------------- dados
  # Lê do .json em Bancos/cache quando a assinatura da fonte não mudou.
  bases <- reactive({
    recarregar()
    fonte <- input$fonte %||% "api"
    uf    <- input$uf %||% ""
    arq   <- arquivo_enviado()

    if (identical(fonte, "api") && !nzchar(uf)) {
      shiny::validate(need(FALSE, paste(
        "Selecione uma UF na barra lateral para carregar os dados.",
        "A escolha fica guardada para as próximas vezes."
      )))
    }
    if (identical(fonte, "arquivo") && (!nzchar(arq) || !file.exists(arq))) {
      shiny::validate(need(FALSE, "Envie um arquivo .csv ou .rds com os casos."))
    }

    chave <- paste0("base_", fonte, "_", if (nzchar(uf)) uf else "arquivo")
    imp   <- tryCatch(impressao_fonte(fonte, uf, arq), error = function(e) "")

    cacheado <- cache_base_ler(chave, imp)
    if (!is.null(cacheado)) return(cacheado)

    cfg_min <- list(fonte = fonte, estado = uf, arquivo_proprio = arq,
                    coluna_municipio = NULL,
                    ano_ref_epi = as.integer(format(Sys.Date(), "%Y")))

    out <- withProgress(message = "Carregando dados (primeira vez demora)...", value = 0.05, {
      tryCatch(
        carregar_bases(cfg_min, progresso = function(p, txt) setProgress(p * 0.9, detail = txt)),
        error = function(e) e
      )
    })
    if (inherits(out, "error")) shiny::validate(need(FALSE, conditionMessage(out)))

    cache_base_gravar(chave, out, imp)
    out
  })

  output$sub_cache <- renderText({
    fonte <- input$fonte %||% "api"; uf <- input$uf %||% ""
    inf <- cache_info(paste0("base_", fonte, "_", if (nzchar(uf)) uf else "arquivo"))
    if (is.null(inf)) "sem cache local ainda"
    else paste0("cache de ", inf$gravado_em, " (", inf$tamanho_mb, " MB)")
  })

  # ------------------------------------------------------------- seletor de UF
  observe({
    b <- tryCatch(bases(), error = function(e) NULL)
    req(!is.null(b), nrow(b$srag) > 0)
    terr <- sort(unique(b$srag$ID_MN_RESI_pad))
    nomes <- stats::setNames(terr, formatar_nome_territorio(terr))
    sel <- intersect(padronizar_nomes(strsplit(PREFS$municipios %||% "", ";")[[1]]), terr)
    updateSelectizeInput(session, "municipios", choices = nomes, selected = sel, server = TRUE)
  })

  # ------------------------------------------------------------- configuração
  cfg_atual <- shiny::debounce(reactive({
    req(input$modo)
    anos_ep <- suppressWarnings(as.integer(trimws(strsplit(input$ano_epid %||% "", "[,;]")[[1]])))
    anos_ep <- anos_ep[!is.na(anos_ep)]

    out <- tryCatch(normalizar_params(list(
      modo_periodo = input$modo, ano_ref = input$ano_ref,
      data_inicio = as.character(input$datas[1]), data_fim = as.character(input$datas[2]),
      se_inicio = input$semanas[1], se_fim = input$semanas[2],
      filtro_ano = input$f_anos %||% c(av$filtro_ano_ini, av$filtro_ano_fim),
      anos_historico = input$anos_hist, ano_epidemico = anos_ep,
      limiares_dp = av$limiares_dp, semanas_nowcast = input$nowcast,
      diag_max = input$diag_max, diag_colunas = input$diag_colunas,
      sexo = input$f_sexo, raca = input$f_raca,
      faixa_etaria = input$f_faixa, virus = input$f_virus,
      estado = input$uf, coluna_uf = "SG_UF_NOT", coluna_municipio = "ID_MN_RESI_pad",
      municipio = input$municipios, usar_filtro_municipio = identical(input$escopo, "mun"),
      nome_territorio = av$nome_territorio,
      rt_nivel = input$rt_nivel %||% "local_bairro",
      rt_itens = input$rt_itens, rt_virus = input$rt_virus,
      fonte = input$fonte, arquivo_proprio = arquivo_enviado(),
      titulo = PREFS$titulo, secretaria = PREFS$secretaria, setor = PREFS$setor,
      edicao = PREFS$edicao, se_boletim = PREFS$se_boletim,
      ano_boletim = PREFS$ano_boletim, equipe = PREFS$equipe
    )), error = function(e) e)
    if (inherits(out, "error")) shiny::validate(need(FALSE, conditionMessage(out)))
    out
  }), 700)

  B <- reactive({
    cfg <- cfg_atual()
    if (cfg$usar_filtro_municipio && length(cfg$municipio) == 0) {
      shiny::validate(need(FALSE, "Selecione pelo menos um território ou mude para \"UF inteira\"."))
    }
    out <- tryCatch(montar_contexto(bases(), cfg), error = function(e) e)
    if (inherits(out, "error")) shiny::validate(need(FALSE, conditionMessage(out)))
    out
  })

  I  <- reactive(calcular_indicadores(B()))
  D  <- reactive(calcular_diagrama(B()))
  DM <- reactive(calcular_diagrama_municipio(B(), max_municipios = input$diag_max %||% 12))

  NC <- reactive({
    n <- input$nowcast %||% 0
    if (is.na(n) || n <= 0) return(NULL)
    nowcast_atraso(B()$srag_ref, semanas_corrigir = n)
  })

  output$sub_nowcast <- renderText({
    nc <- tryCatch(NC(), error = function(e) NULL)
    if (is.null(nc)) return("sem correção de atraso")
    if (!isTRUE(nc$disponivel)) return(paste0("indisponível: ", nc$motivo %||% "-"))
    paste0("erro estimado ±", formatar_dec(nc$erro_relativo, 1),
           "% na última semana")
  })

  # ===================================================== Rt por localidade ====
  # O agrupamento e sempre por localidade; o virus e selecao, nao agrupamento.
  # Cada localidade vira uma secao da sanfona e so e calculada ao ser expandida.

  # identificador estavel e seguro para o nome do output
  digest_local <- function(x) gsub("[^A-Za-z0-9]", "_", iconv(x, to = "ASCII//TRANSLIT"))

  opcoes_locais <- reactive({
    b <- tryCatch(B(), error = function(e) NULL)
    req(!is.null(b))
    nivel <- input$rt_nivel %||% "local_bairro"
    req(nivel %in% names(b$srag_ref))
    v <- opcoes_localidade(b$srag_ref, nivel)
    stats::setNames(v, formatar_nome_territorio(v))
  })

  opcoes_virus_rt <- reactive({
    b <- tryCatch(B(), error = function(e) NULL)
    req(!is.null(b))
    v <- b$deteccoes$virus_nome
    names(sort(table(v[!is.na(v)]), decreasing = TRUE))
  })

  # So repovoa quando o conjunto de opcoes muda; o que ja foi escolhido e
  # continua valido e preservado.
  observeEvent(opcoes_locais(), {
    op <- opcoes_locais()
    sel <- intersect(isolate(input$rt_itens) %||% character(0), unname(op))
    if (length(sel) == 0) {
      sel <- intersect(strsplit(PREFS$rt_itens %||% "", ";")[[1]], unname(op))
    }
    if (length(sel) == 0) sel <- utils::head(unname(op), 5)   # as 5 maiores
    updateSelectizeInput(session, "rt_itens", choices = op, selected = sel)
  })

  observeEvent(opcoes_virus_rt(), {
    op <- opcoes_virus_rt()
    sel <- intersect(isolate(input$rt_virus) %||% character(0), op)
    if (length(sel) == 0) sel <- intersect(strsplit(PREFS$rt_virus %||% "", ";")[[1]], op)
    if (length(sel) == 0) sel <- utils::head(op, 4)           # os 4 mais frequentes
    updateSelectizeInput(session, "rt_virus", choices = op, selected = sel)
  })

  observeEvent(input$rt_todos, {
    op <- opcoes_locais()
    updateSelectizeInput(session, "rt_itens", choices = op, selected = unname(op))
  })
  observeEvent(input$rt_limpar, {
    updateSelectizeInput(session, "rt_itens", choices = opcoes_locais(),
                         selected = character(0))
  })
  observeEvent(input$rt_virus_todos, {
    op <- opcoes_virus_rt()
    updateSelectizeInput(session, "rt_virus", choices = op, selected = op)
  })
  observeEvent(input$rt_virus_limpar, {
    updateSelectizeInput(session, "rt_virus", choices = opcoes_virus_rt(),
                         selected = character(0))
  })

  # ----- cache do Rt por localidade -----------------------------------------
  # reactiveValues (e nao um environment comum) para que a tabela abaixo se
  # atualize sozinha assim que uma seção nova for calculada. A leitura
  # dentro de rt_do_local() é isolada, para não criar laço.
  cache_rt <- reactiveValues()

  observeEvent(list(cfg_atual(), input$rt_nivel, input$rt_virus), {
    for (k in names(cache_rt)) cache_rt[[k]] <- NULL
  }, ignoreInit = TRUE)

  rt_do_local <- function(loc) {
    chave <- paste0("L", loc)
    pronto <- isolate(cache_rt[[chave]])
    if (!is.null(pronto)) return(pronto)
    r <- calcular_rt(B(), nivel = input$rt_nivel %||% "local_bairro",
                     locais = loc, virus = input$rt_virus)
    cache_rt[[chave]] <- if (is.null(r)) tibble::tibble() else r
    isolate(cache_rt[[chave]])
  }

  # ----- sanfona: uma secao por localidade -----------------------------------
  output$rt_sanfona <- renderUI({
    locais <- input$rt_itens
    if (length(locais) == 0) {
      return(bslib::card(div(class = "p-3 text-muted",
                             "Escolha ao menos uma localidade na lista acima.")))
    }
    if (length(input$rt_virus %||% character(0)) == 0) {
      return(bslib::card(div(class = "p-3 text-muted",
                             "Escolha ao menos um vírus para montar as facetas.")))
    }

    resumo <- resumo_locais_rt(B(), input$rt_nivel %||% "local_bairro",
                               locais, input$rt_virus)
    ordem <- if (nrow(resumo) > 0) resumo$local else locais
    ordem <- c(ordem, setdiff(locais, ordem))

    paineis <- lapply(ordem, function(loc) {
      linha <- resumo[resumo$local == loc, , drop = FALSE]
      detalhe <- if (nrow(linha) == 1) {
        sprintf("%s detecções · %d vírus", formatar_n(linha$deteccoes), linha$n_virus)
      } else "sem detecções no período"

      bslib::accordion_panel(
        title = div(
          class = "d-flex justify-content-between align-items-center w-100 pe-2",
          tags$span(class = "fw-semibold", formatar_nome_territorio(loc)),
          tags$span(class = "text-muted small", detalhe)
        ),
        value = loc,
        plotly::plotlyOutput(paste0("rt_g_", digest_local(loc)), height = "auto")
      )
    })

    do.call(bslib::accordion,
            c(list(id = "rt_acc", open = FALSE, multiple = TRUE), paineis))
  })

  # cria um output por localidade; so calcula quando a secao esta aberta
  observe({
    locais <- input$rt_itens
    req(length(locais) > 0)
    for (loc in locais) {
      local({
        l <- loc
        id <- paste0("rt_g_", digest_local(l))
        output[[id]] <- plotly::renderPlotly({
          req(l %in% (input$rt_acc %||% character(0)))
          r <- withProgress(
            message = paste("Estimando Rt de", formatar_nome_territorio(l), "..."),
            value = 0.4, rt_do_local(l))
          pl_rt_local(B(), r, l, colunas = input$rt_colunas %||% 2)
        })
      })
    }
  })

  # ----- tabela: o que ja foi calculado, ou tudo pelo botao ------------------
  RT <- reactive({
    locais <- input$rt_itens
    req(length(locais) > 0)
    partes <- lapply(locais, function(l) {
      v <- cache_rt[[paste0("L", l)]]
      if (is.null(v) || nrow(v) == 0) NULL else v
    })
    dplyr::bind_rows(partes)
  })

  observeEvent(input$calc_rt, {
    locais <- input$rt_itens
    shiny::validate(need(length(locais) > 0, "Escolha ao menos uma localidade."))
    withProgress(message = "Estimando Rt de todas as localidades...", value = 0, {
      for (i in seq_along(locais)) {
        rt_do_local(locais[i])
        setProgress(i / length(locais),
                    detail = formatar_nome_territorio(locais[i]))
      }
    })
  })

  md <- function(x) if (length(x) == 1 && nzchar(x)) shiny::markdown(x) else NULL

  # --------------------------------------------------------------- cabeçalho
  output$cabecalho <- renderUI({
    b <- B()
    div(class = "mb-3",
        h4(paste0(b$rotulo_territorio, " — ", b$rotulo_periodo)),
        tags$p(class = "text-muted mb-0",
               sprintf("%s · %s casos · fonte: %s",
                       rotulo_intervalo_base(b$srag), formatar_n(b$total_notificacoes),
                       if (identical(input$fonte, "api")) "API de Dados Abertos do SUS"
                       else basename(arquivo_enviado()))),
        if (length(b$avisos) > 0)
          div(class = "alert alert-warning mt-2 small mb-0",
              tags$ul(class = "mb-0", lapply(b$avisos, tags$li))))
  })

  output$vb_casos  <- renderText(formatar_n(I()$resumo$total))
  output$vb_obitos <- renderText(formatar_n(I()$resumo$obitos))
  output$vb_letal  <- renderText({
    v <- I()$resumo$letalidade; if (is.na(v)) "-" else paste0(formatar_dec(v, 1), "%")
  })
  output$vb_inc <- renderText({
    v <- I()$resumo$incidencia
    if (is.na(v)) "sem população" else formatar_dec(v, 1)
  })

  # ----------------------------------------------------------------- gráficos
  output$g_ano       <- plotly::renderPlotly(pl_casos_por_ano(B(), I()))
  output$g_inc       <- plotly::renderPlotly(pl_incidencia(B(), I(), NC()))
  output$g_virus     <- plotly::renderPlotly(pl_virus_total(B(), I()))
  output$g_virus_sem <- plotly::renderPlotly(pl_virus_semana(B(), I()))
  output$g_virus_fe  <- plotly::renderPlotly(pl_virus_fe(B(), I()))
  output$g_faixa     <- plotly::renderPlotly(pl_faixa_etaria(B(), I()))
  output$g_vent      <- plotly::renderPlotly(pl_ventilacao(B(), I()))
  output$g_intern    <- plotly::renderPlotly(pl_internacao(B(), I()))
  output$g_evol      <- plotly::renderPlotly(pl_evolucao(B(), I()))
  output$g_hosp      <- plotly::renderPlotly(pl_hospitais(B(), I()))
  output$g_raca      <- plotly::renderPlotly(pl_raca_cor(B(), I()))
  output$g_bebes       <- plotly::renderPlotly(pl_bebes(B(), I()))
  output$g_bebes_virus <- plotly::renderPlotly(pl_virus_bebes(B(), I()))
  output$g_obitos    <- plotly::renderPlotly(pl_obitos(B(), I()))
  output$g_diag      <- plotly::renderPlotly(pl_diagrama(B(), D()))
  # O bloco cresce conforme o numero de facetas, em vez de espremer tudo em 620 px.
  output$bloco_diag_mun <- renderUI({
    dm <- tryCatch(DM(), error = function(e) NULL)
    altura <- if (is.null(dm)) 420 else altura_diagrama_municipio(dm, input$diag_colunas %||% 3)
    plotly::plotlyOutput("g_diag_mun", height = paste0(altura, "px"))
  })

  output$g_diag_mun <- plotly::renderPlotly(
    pl_diagrama_mun(B(), DM(), D()$legenda, colunas = input$diag_colunas %||% 3))


  # ------------------------------------------------------------------- textos
  output$t_ondas     <- renderUI(md(txt_ondas(B(), I())))
  output$t_resumo    <- renderUI(md(txt_resumo(B(), I())))
  output$t_virus     <- renderUI(md(txt_virus(B(), I())))
  output$t_virus_sem <- renderUI(md(txt_virus_semana(B(), I())))
  output$t_virus_fe  <- renderUI(md(txt_virus_fe(B(), I())))
  output$t_faixa     <- renderUI(md(txt_faixa(B(), I())))
  output$t_vent      <- renderUI(md(txt_ventilacao(B(), I())))
  output$t_intern    <- renderUI(md(txt_internacao(B(), I())))
  output$t_evol      <- renderUI(md(txt_evolucao(B(), I())))
  output$t_hosp      <- renderUI(md(txt_hospitais(B(), I())))
  output$t_raca      <- renderUI(md(txt_raca(B(), I())))
  output$t_bebes       <- renderUI(md(txt_bebes(B(), I())))
  output$t_bebes_virus <- renderUI(md(txt_virus_bebes(B(), I())))
  output$t_obitos    <- renderUI(md(txt_obitos(B(), I())))
  output$t_diag      <- renderUI(md(txt_diagrama(B(), D())))
  output$t_rt <- renderUI(md(txt_rt(B(), RT(), input$rt_nivel)))

  output$tab_resumo <- renderTable(tabela_resumo(B(), I()), striped = TRUE, width = "100%")

  output$tab_obitos <- DT::renderDT({
    d <- I()$obitos_detalhe
    shiny::validate(need(nrow(d) > 0, "Nenhum óbito com faixa etária preenchida."))
    DT::datatable(d, rownames = FALSE,
                  colnames = c("SE", "Território", "Faixa etária", "Óbitos"),
                  options = opcoes_dt())
  })

  output$tab_zonas <- DT::renderDT({
    z <- tabela_zonas_municipio(B(), DM())
    shiny::validate(need(nrow(z) > 0, paste(
      "Sem classificação por território: e preciso população por território",
      "e pelo menos dois anos de histórico.")))
    DT::datatable(z, rownames = FALSE, options = list(dom = "t", language = DT_PT)) |>
      DT::formatStyle("Zona de risco", target = "row",
        backgroundColor = DT::styleEqual(
          c("Zona de controle", "Zona de segurança", "Zona de alerta", "Zona epidêmica"),
          c("#eafaf1", "#fef9e7", "#fdf2e9", "#fdedec")))
  })

  output$tab_rt <- DT::renderDT({
    t <- tabela_rt(RT(), input$rt_nivel)
    shiny::validate(need(nrow(t) > 0, paste(
      "A tabela mostra as localidades já calculadas. Expanda uma seção acima",
      "ou use \"Calcular todas\" para preenchê-la.")))
    DT::datatable(t, rownames = FALSE, filter = "top", options = opcoes_dt(pageLength = 20))
  })

  dados_filtrados <- reactive({
    B()$srag_ref |>
      dplyr::select(-dplyr::any_of(c(".obito", "semana_idx"))) |>
      dplyr::mutate(ID_MN_RESI_pad = formatar_nome_territorio(ID_MN_RESI_pad))
  })
  output$tab_dados <- DT::renderDT(
    DT::datatable(dados_filtrados(), rownames = FALSE, filter = "top",
                  options = opcoes_dt(pageLength = 25)))

  nome_arquivo <- function(prefixo, ext) {
    b <- B()
    limpar <- function(x) gsub("^-+|-+$", "",
                               gsub("[^A-Za-z0-9]+", "-", iconv(x, to = "ASCII//TRANSLIT")))
    paste0(prefixo, "_", limpar(b$rotulo_territorio), "_", limpar(b$rotulo_periodo), ext)
  }

  output$baixar_csv <- downloadHandler(
    filename = function() nome_arquivo("SRAG", ".csv"),
    content  = function(file) readr::write_excel_csv2(dados_filtrados(), file))

  # ====================================================== modal do boletim ====
  valor_salvo <- function(id, padrao) {
    v <- isolate(input[[id]]); if (is.null(v)) padrao else v
  }

  observeEvent(input$abrir_modal_docx, {
    cfg <- cfg_atual(); b <- B(); sug <- se_ano_sugeridos(cfg)
    tem_pandoc <- localizar_pandoc()

    showModal(modalDialog(
      title = "Gerar boletim em Word", size = "l", easyClose = FALSE,

      if (!tem_pandoc)
        div(class = "alert alert-danger",
            tags$b("Pandoc não encontrado."), " Ele é necessário apenas para o .docx. ",
            "Instale o RStudio (que já o inclui) ou o Pandoc ",
            tags$a(href = "https://pandoc.org/installing.html", target = "_blank", "(pandoc.org)"), "."),

      div(class = "alert alert-light border small",
          tags$b("Tudo aqui é opcional e fica guardado como padrão"), " para a próxima vez. ",
          "Território e período vêm da barra lateral: ",
          tags$b(b$rotulo_territorio), " · ", tags$b(b$rotulo_periodo), "."),

      bslib::accordion(
        open = "ident",
        bslib::accordion_panel(
          "Identificação do boletim", value = "ident", icon = icon("id-card"),
          textInput("bol_titulo", "Título", valor_salvo("bol_titulo", PREFS$titulo),
                    width = "100%",
                    placeholder = paste0("Boletim de SRAG - ", b$rotulo_territorio,
                                         ", ", b$rotulo_periodo)),
          textInput("bol_secretaria", "Secretaria / instituição",
                    valor_salvo("bol_secretaria", PREFS$secretaria), width = "100%"),
          textInput("bol_setor", "Área / diretoria responsável",
                    valor_salvo("bol_setor", PREFS$setor), width = "100%"),
          fluidRow(
            column(4, textInput("bol_edicao", "Nº da edição",
                                valor_salvo("bol_edicao", PREFS$edicao))),
            column(4, numericInput("bol_se", "Semana epidemiológica",
                                   valor_salvo("bol_se", sug$se), 1, 53, 1)),
            column(4, numericInput("bol_ano", "Ano",
                                   valor_salvo("bol_ano", sug$ano), 2000, 2100, 1))),
          textAreaInput("bol_equipe", "Equipe responsável",
                        valor_salvo("bol_equipe", PREFS$equipe), width = "100%", rows = 3,
                        placeholder = "Um nome por linha")
        ),

        bslib::accordion_panel(
          "Território", value = "terr", icon = icon("location-dot"),
          textInput("bol_nome_terr", "Nome do território nos títulos",
                    valor_salvo("bol_nome_terr", av$nome_territorio), width = "100%",
                    placeholder = paste0("automático: ", b$rotulo_territorio))
        ),

        bslib::accordion_panel(
          "Faixas do diagrama de controle", value = "hist", icon = icon("chart-area"),
          div(class = "text-muted small mb-2",
              "Intervalo de anos, anos de histórico e anos epidêmicos ficam no grupo ",
              tags$b("Estatística"), " da barra lateral."),
          fluidRow(
            column(4, numericInput("bol_dp1", "DP da zona de segurança",
                                   valor_salvo("bol_dp1", av$limiares_dp[1]), 0.5, 5, 0.5)),
            column(4, numericInput("bol_dp2", "DP da zona de alerta",
                                   valor_salvo("bol_dp2", av$limiares_dp[2]), 0.5, 6, 0.5)),
            column(4, numericInput("bol_dp3", "DP do topo do gráfico",
                                   valor_salvo("bol_dp3", av$limiares_dp[3]), 0.5, 8, 0.5)))
        )
      ),

      footer = tagList(
        modalButton("Fechar"),
        actionButton("aplicar_av", "Aplicar ao painel", class = "btn-outline-secondary"),
        downloadButton("baixar_docx", "Gerar e baixar", class = "btn-primary")
      )
    ))
  })

  # aplica (e guarda) os parâmetros avançados
  salvar_do_modal <- function() {
    av$limiares_dp     <- c(input$bol_dp1, input$bol_dp2, input$bol_dp3)
    av$nome_territorio <- input$bol_nome_terr
    salvar_preferencias(list(
      limiares_dp = c(input$bol_dp1, input$bol_dp2, input$bol_dp3),
      nome_territorio = input$bol_nome_terr,
      titulo = input$bol_titulo, secretaria = input$bol_secretaria,
      setor = input$bol_setor, edicao = input$bol_edicao,
      se_boletim = as.character(input$bol_se), ano_boletim = as.character(input$bol_ano),
      equipe = input$bol_equipe
    ))
  }

  observeEvent(input$aplicar_av, {
    salvar_do_modal()
    showNotification("Parâmetros aplicados e salvos como padrão.", type = "message")
  })

  output$baixar_docx <- downloadHandler(
    filename = function() nome_arquivo("BOLETIM_SRAG", ".docx"),
    content = function(file) {
      if (!localizar_pandoc()) { showNotification(MSG_SEM_PANDOC, type = "error", duration = NULL)
                                 stop(MSG_SEM_PANDOC) }
      salvar_do_modal()
      cfg <- isolate(cfg_atual())
      txt <- function(id) {
        v <- input[[id]]
        if (is.null(v) || length(v) == 0 || is.na(v)) "" else trimws(as.character(v))
      }
      cfg$titulo <- txt("bol_titulo"); cfg$secretaria <- txt("bol_secretaria")
      cfg$setor <- txt("bol_setor");   cfg$edicao <- txt("bol_edicao")
      cfg$se_boletim <- txt("bol_se"); cfg$ano_boletim <- txt("bol_ano")
      cfg$equipe <- txt("bol_equipe")
      if (nzchar(txt("bol_nome_terr"))) cfg$nome_territorio <- txt("bol_nome_terr")

      withProgress(message = "Gerando o boletim em Word...", value = 0.3, {
        gerar_boletim_word(bases(), cfg, file, semanas_nowcast = input$nowcast)
      })
    })
}

shinyApp(ui, server)
