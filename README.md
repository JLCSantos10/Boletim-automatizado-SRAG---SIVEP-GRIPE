# Painel SRAG — vigilância de Síndrome Respiratória Aguda Grave

Painel interativo para explorar os dados de SRAG do **SIVEP-Gripe** `[9]` `[14]` e gerar o
boletim epidemiológico em Word. Os dados vêm direto das APIs públicas — **não é preciso baixar
nenhum arquivo à mão**.

| Fonte | Origem |
|---|---|
| Casos | [API de Dados Abertos do SUS](https://apidadosabertos.saude.gov.br/v1) (DEMAS/MS) `[10]` |
| População | [API do IBGE — SIDRA](https://apisidra.ibge.gov.br) `[11]` `[12]` |
| População do DF por RA | planilha da Codeplan incluída no projeto `[13]` |
| Alternativa | os seus próprios dados, enviados pelo painel |

---

## Índice

1. [Passo a passo: instalar e abrir](#1-passo-a-passo-instalar-e-abrir)
2. [Passo a passo: usar o painel](#2-passo-a-passo-usar-o-painel)
3. [O que cada aba mostra](#3-o-que-cada-aba-mostra)
4. [Passo a passo: gerar o boletim em Word](#4-passo-a-passo-gerar-o-boletim-em-word)
5. [Como cada indicador é calculado](#5-como-cada-indicador-é-calculado)
6. [De onde vêm os dados](#6-de-onde-vêm-os-dados)
7. [Casos especiais de território](#7-casos-especiais-de-território)
8. [Cache: por que o painel abre rápido](#8-cache-por-que-o-painel-abre-rápido)
9. [Arquitetura dos arquivos](#9-arquitetura-dos-arquivos)
10. [Solução de problemas](#10-solução-de-problemas)
11. [Referências](#11-referências)

---

## 1. Passo a passo: instalar e abrir

### 1.1 Instale o R

| Sistema | Como |
|---|---|
| Windows | [cran.r-project.org](https://cran.r-project.org/bin/windows/base/) → baixe e instale |
| Debian/Ubuntu | `sudo apt install r-base` |
| Fedora | `sudo dnf install R` |
| macOS | `brew install r` |

### 1.2 Instale o Pandoc (só para gerar o Word)

O Pandoc converte o boletim para `.docx`. **O painel funciona sem ele**; apenas o botão de
gerar o documento fica indisponível.

O jeito mais simples é instalar o [RStudio](https://posit.co/download/rstudio-desktop/), que já
traz o Pandoc embutido. Alternativa: [pandoc.org/installing](https://pandoc.org/installing.html).

O projeto procura o Pandoc sozinho (`localizar_pandoc()` em `R/fontes_dados.R`) no `PATH`, no
RStudio instalado em `Program Files` ou em `AppData`, no Quarto e no Pandoc avulso.

### 1.3 Abra o painel

| Sistema | Como |
|---|---|
| Windows | duplo clique em **`abrir_painel.bat`** |
| Linux / macOS | `chmod +x abrir_painel_linux.sh` (só na 1ª vez) e depois `./abrir_painel_linux.sh` |
| RStudio | abra `app.R` e clique em **Run App** |
| Terminal | `Rscript -e "shiny::runApp(getwd(), port = 4040, launch.browser = TRUE)"` |

O painel abre em `http://127.0.0.1:4040`. Ele é uma aplicação `shiny` `[18]` rodando em R `[19]`
na sua própria máquina — nada é enviado para fora.

> **Primeira execução:** os pacotes R são instalados automaticamente (pode levar alguns
> minutos). Isso acontece uma única vez.

---

## 2. Passo a passo: usar o painel

### Passo 1 — Escolha a fonte dos dados

No alto da barra lateral há duas opções:

**a) Dados via API** *(padrão)* — o subtítulo mostra o intervalo disponível, por exemplo
*"dados de 01/2019 a 01/2026"*. É consultado automaticamente e fica em cache por 12 horas.

**b) Meus próprios dados** — clique em *Escolher...* e envie um `.csv` ou `.rds`. São aceitos:

- o formato já usado pelo projeto (`ID_MN_RESI_pad`, `DT_SIN_PRI`, `PCR_*`, `FE1`…);
- o formato bruto da API (colunas minúsculas: `dt_sin_pri`, `id_mn_resi`, `pcr_*`…), que o
  painel converte sozinho.

O único obrigatório é a **data dos primeiros sintomas** e **uma coluna de município**. Se
faltar algo, o painel diz exatamente o quê. O arquivo enviado passa a ser o padrão nas
próximas aberturas.

### Passo 2 — Escolha a UF

Na primeira abertura a UF vem **vazia de propósito** e nada é carregado. Assim que você
escolher, duas coisas acontecem:

1. o painel baixa todo o histórico daquela UF (veja o [passo 7](#passo-7--atualize-os-dados));
2. a UF vira o padrão — nas próximas vezes o painel já abre nela.

### Passo 3 — Escolha a abrangência

- **UF inteira** — todos os territórios do estado;
- **Selecionar territórios** — escolha um ou vários na lista com busca.

O título, as legendas e os textos se ajustam sozinhos ao que você escolher
(*"Planaltina e Sobradinho (DF)"*, *"Distrito Federal"*, *"12 municípios de SP"*).

### Passo 4 — Escolha o intervalo de tempo

| Modo | O que usa | Como aparece |
|---|---|---|
| **Ano inteiro** | ano de referência | `2025` |
| **Intervalo de datas** | duas datas | `01/11/2024 a 31/03/2025` |
| **Semanas epidemiológicas** | SE inicial e final | `SE 20 a SE 40 de 2025` |

O modo *datas* aceita períodos que atravessam a virada do ano; nesse caso os gráficos semanais
passam a rotular as semanas como `2024/44`, `2024/45`, …, `2025/14`.

### Passo 5 — Aplique os filtros (opcional)

Os demais filtros ficam recolhidos em dois grupos:

**Demografia**

| Filtro | Opções |
|---|---|
| Sexo | Masculino, Feminino, Ignorado |
| Raça/cor | Branca, Preta, Amarela, Parda, Indígena, Ignorada |
| Faixa etária | as dez faixas do boletim |

**Estatística**

| Filtro | Para que serve |
|---|---|
| Tipo de vírus | mantém só os casos com detecção positiva do(s) vírus escolhido(s) |
| Intervalo de anos | a série histórica da figura 1 |
| Anos de histórico | quantos anos anteriores entram nos limiares do diagrama de controle |
| Ano(s) epidêmico(s) | anos atípicos excluídos do cálculo dos limiares |
| Nowcasting | quantas semanas finais corrigir |
| Municípios no diagrama | quantas facetas no diagrama de controle por município |
| Colunas do diagrama | largura da grade de facetas |

Os filtros são combináveis e valem para **tudo**: gráficos, textos, diagrama de controle, Rt e
o boletim em Word. São aplicados **antes** do recorte de período, para que a série histórica
trate da mesma população mostrada nos gráficos.

O botão **Limpar filtros** zera o grupo Demografia e o tipo de vírus de uma vez. Se a
combinação não deixar nenhum caso, o painel avisa em vez de mostrar gráficos vazios.

### Passo 6 — Leia os gráficos

Todos são **plotly**: passe o mouse para ver os valores, dê zoom, isole séries clicando na
legenda e exporte em PNG pelo ícone da câmera. Abaixo de cada gráfico há um **texto
interpretativo** gerado a partir dos próprios dados, pronto para copiar.

As escalas, cores e eixos são os mesmos do boletim em Word: os gráficos do documento são
feitos em `ggplot2` `[16]` e a versão interativa é uma conversão do mesmo objeto `[17]`,
justamente para nada se deslocar.

### Passo 7 — Atualize os dados

| Situação | O que fazer |
|---|---|
| UF nova, nunca usada | é automático: o painel baixa o histórico (≈15 min para o DF, 113.713 registros) |
| Já usou essa UF antes | nada: o painel lê do cache em menos de 1 segundo |
| A API publicou um mês novo | é automático: a assinatura da fonte muda e os dados são rebaixados |
| Quer forçar | clique em **Atualizar dados da fonte** |

---

## 3. O que cada aba mostra

### Visão geral

1. **Cartões** — casos no período, óbitos, letalidade e incidência por 100 mil habitantes.
2. **Casos por semana epidemiológica e ano** — a série histórica do intervalo de anos
   escolhido, com uma barra por semana e divisórias entre os anos.
3. **Casos, incidência e nowcasting** — barras azuis com os casos, linha vermelha com a
   incidência (eixo à direita) e **linha tracejada preta** com os casos corrigidos pelo atraso
   de notificação (veja o [item 5.5](#55-nowcasting--correção-do-atraso-de-notificação)).
4. **Resumo da situação epidemiológica** — a tabela que abre o boletim.

### Vírus

Vírus detectados por PCR (total e participação), composição viral por semana epidemiológica e a
distribuição por semana × faixa etária × vírus.

### Perfil dos casos

- **Faixa etária** com o **sexo empilhado** dentro de cada barra. A altura continua sendo o
  total da faixa e o rótulo em % continua sendo a participação da faixa no total, então a
  leitura original não muda.
- **Suporte ventilatório**, **tempo de internação** (boxplot) e **evolução do caso**.
- **Raça/cor** — barras verticais com o número e a proporção de cada categoria.
- **Hospitalizações por unidade** — as 25 unidades com mais internações.

### Menores de 2 anos

Distribuição por idade em dias e meses, e os vírus detectados por idade.

### Óbitos

Óbitos por semana epidemiológica e o detalhamento por SE, território e faixa etária.

### Diagrama de controle

O diagrama do território selecionado e, logo abaixo, **um por município**, com a tabela de
classificação de risco. Veja o [item 5.4](#54-diagrama-de-controle).

O bloco do diagrama por município **cresce conforme o número de facetas** — não há altura
fixa espremendo tudo. Dois controles no grupo *Estatística* ajustam a grade: quantos
municípios mostrar (padrão 12, em ordem de número de casos) e quantas colunas usar (padrão 3).
Quando há mais territórios do que o limite, o rodapé da figura informa quantos ficaram de
fora.

### Transmissibilidade (Rt)

Veja o [item 5.6](#56-rt--número-de-reprodução-efetivo).

### Dados

A tabela de casos do período, com filtro por coluna e download em CSV (separador `;`, pronto
para o Excel em português).

---

## 4. Passo a passo: gerar o boletim em Word

1. Clique em **Gerar boletim em Word** na barra lateral.
2. A janela que abre tem três blocos, **todos opcionais**:

| Bloco | Campos |
|---|---|
| **Identificação** | título, secretaria, área responsável, nº da edição, semana epidemiológica, ano, equipe |
| **Território** | nome do território nos títulos (vazio = automático) |
| **Faixas do diagrama de controle** | multiplicadores de desvio-padrão das zonas |

3. Preencha o que quiser. O que ficar em branco simplesmente não aparece no documento.
4. Clique em **Gerar e baixar**.

Território, período e filtros vêm da barra lateral — o documento sai exatamente com o recorte
que está na tela. Tudo o que você digitar **fica guardado como padrão** para a próxima vez. O
botão *Aplicar ao painel* atualiza os gráficos sem gerar o documento.

Exemplo do cabeçalho produzido:

```
# Boletim Epidemiológico de SRAG

Secretaria de Estado de Saúde do Distrito Federal
Diretoria de Vigilância Epidemiológica

**Edição nº 12 · Semana epidemiológica 47 de 2025**

**Território:** Planaltina e Sobradinho (DF)
**Período analisado:** 01/01/2025 a 31/12/2025 (2025)
**Casos de SRAG no período:** 940
**Fonte dos dados:** API de Dados Abertos do SUS (SIVEP-Gripe)
**Emitido em:** 26/11/2026

**Equipe responsável**
- Fulano de Tal — Epidemiologista
```

> **Não existe mais geração isolada.** Os antigos `params.yaml`, `render_boletim.R` e
> `atualizar_boletim.bat` foram removidos: toda a configuração está na tela e fica guardada em
> `Bancos/preferencias.json`.

---

## 5. Como cada indicador é calculado

### 5.1 Semana epidemiológica

A semana (`SE`) e o ano (`ano`) saem da **data dos primeiros sintomas** (`DT_SIN_PRI`) pelo
calendário epidemiológico, que vai de domingo a sábado `[1]` `[5]`.

**Por que isso importa:** usar o ano civil junto da semana epidemiológica erra a virada de ano.
30/12/2019, por exemplo, pertence à SE 1 de **2020**, não de 2019. O painel usa `epiweek()` e
`epiyear()` justamente para não cometer esse erro.

### 5.2 Incidência

```
incidência(SE) = 100.000 × casos(SE) / população do território selecionado
```

O denominador é a **soma da população de todos os territórios selecionados** — ou a população
total da UF, quando não há filtro de município `[6]`.

**Passo a passo do cálculo:**

1. conta os casos de cada semana epidemiológica do período;
2. busca a população do ano de referência (veja o [item 6.2](#62-população-ibge));
3. divide e multiplica por 100.000.

### 5.3 Letalidade

```
letalidade(%) = 100 × óbitos por SRAG / casos no período
```

O óbito é identificado pelo campo `EVOLUCAO = 2` da ficha do SIVEP-Gripe `[9]`.

### 5.4 Diagrama de controle

O diagrama de controle compara a incidência observada com o que seria esperado para **aquela
mesma semana epidemiológica** em anos anteriores `[6]` `[7]` `[5]`.

**Passo a passo:**

1. **Escolhe os anos de histórico** — os `anos_historico` anos anteriores ao ano de referência,
   excluindo os anos marcados como epidêmicos (anos atípicos distorcem a média).
2. **Completa com zeros** — semanas históricas sem nenhum caso entram como zero. Descartá-las
   elevaria artificialmente a média histórica.
3. **Calcula média e desvio-padrão** da incidência de cada semana epidemiológica.
4. **Monta as faixas:**

| Zona | Intervalo | Nível do plano |
|---|---|---|
| Controle | 0 até a média | verde |
| Segurança | média até média + 1 DP | amarelo |
| Alerta | média + 1 DP até média + 2 DP | laranja |
| Epidêmica | acima de média + 2 DP | vermelho |

5. **Classifica as últimas semanas** usando a curva corrigida por atraso de notificação, e
   indica o nível do plano de contingência correspondente.

Os multiplicadores (1, 2 e 3 DP) são configuráveis na janela de geração do boletim.

#### Quais territórios entram no diagrama por município

Com a **UF inteira** selecionada, entram apenas os territórios cuja **residência predominante
é a própria UF**. Casos notificados no DF mas residentes em Goiás, por exemplo, continuam nos
totais da UF — é assim que o SIVEP conta — mas não viram facetas do diagrama: eles não são
municípios do DF.

> Sem essa regra, escolher "Distrito Federal" trazia 154 facetas, das quais 124 eram de outros
> estados (Anápolis, Natal, Manaus…). Com ela, sobram as 30 Regiões Administrativas que têm
> população disponível.

Quando você seleciona territórios explicitamente, são exatamente os escolhidos — sem filtro
por UF e sem corte.

### 5.5 Nowcasting — correção do atraso de notificação

As semanas mais recentes sempre parecem ter menos casos do que realmente têm, porque parte das
notificações ainda não foi digitada. O nowcasting estima esse déficit `[3]` `[4]`.

**Método: triângulo de notificação (*reporting triangle*)** `[3]`

1. Para cada caso, calcula o atraso em semanas entre a **data dos primeiros sintomas**
   (`DT_SIN_PRI`) e a **data de digitação** no SIVEP (`DT_DIGITA`).
2. Nas semanas já fechadas (mais antigas que o horizonte de atraso), estima a proporção
   acumulada de casos digitados com até *d* semanas de atraso — o `p_d`.
3. Nas semanas recentes, o observado é apenas a fração `p_d` do total:

   ```
   estimado = observado / p_d
   ```

4. O intervalo de confiança vem da variância binomial de uma proporção conhecida:

   ```
   Var(N) = n (1 − p) / p²
   ```

**Valores medidos no DF:** `p_d` foi de **18% na própria semana**, **63% com 1 semana** e
**92% com 4 semanas** de atraso.

No gráfico, a correção aparece como **linha tracejada preta** com faixa de incerteza, e o
subtítulo informa o erro estimado e o fator de correção aplicado.

> **Se a base não tiver `DT_DIGITA`** (é o caso do `Bancos/SRAG.csv` antigo), o painel informa
> isso no subtítulo e **não desenha a linha**, em vez de inventar uma correção.

#### Sobre o pacote `nowcasting` da FGV

O pacote [`nmecsys/nowcasting`](https://github.com/nmecsys/nowcasting) é *"Predicting Economic
Variables using Dynamic Factor Models"*: serve para nowcast de PIB e séries macroeconômicas com
dados de frequência mista. Ele **não modela atraso de notificação** e foi removido do CRAN em
25/05/2022 por depender do pacote arquivado `matlab`. Por isso o método epidemiológico acima
`[3]` `[4]` foi implementado em `R/nowcasting.R`, que é o que o gráfico precisa.

### 5.6 Rt — número de reprodução efetivo

O Rt estima quantas pessoas, em média, cada caso infecta no momento. Acima de 1, a transmissão
está em expansão; abaixo de 1, em retração.

**Método: Cori et al. (2013)** `[2]`, pelo pacote `EpiEstim` `[15]` `[8]`.

**Passo a passo:**

1. monta a série **diária** de casos com detecção viral, por localidade e por vírus;
2. estima o Rt em **janelas móveis de 7 dias**, adotando intervalo serial com média de
   **3,5 dias** e desvio-padrão de **2,5 dias**;
3. descarta séries com menos de **21 dias** ou **20 casos**;
4. descarta janelas **sem nenhum caso** — nelas o `EpiEstim` devolve a média da distribuição
   *a priori* (Rt = 5), o que desenhava uma linha reta alta sem informação por trás;
5. guarda **um ponto por semana epidemiológica** (a última janela de cada semana): a linha
   diária, com ~360 pontos por ano, fica ilegível sem acrescentar informação;
6. na tabela, reporta apenas janelas com pelo menos **5 casos**.

#### Como o gráfico de Rt é organizado

O agrupamento é **sempre por localidade**. O vírus não agrupa nada: ele é apenas uma
**seleção** que define quais facetas aparecem dentro de cada localidade.

Cada localidade vira uma **seção recolhível**. Ao expandir, aparecem os gráficos de Rt
**facetados por vírus** — uma linha por faceta, que é o que mantém tudo legível.

```
▼ CEILÂNDIA                       1.227 detecções · 4 vírus
  ┌──────────────────┬──────────────────┐
  │ Rinovírus        │ VSR              │
  │ Rt 0,84          │ Rt 1,17          │
  ├──────────────────┼──────────────────┤
  │ Adenovírus       │ SARS-CoV-2       │
  │ Rt 1,17          │ Rt 1,14          │
  └──────────────────┴──────────────────┘
► TAGUATINGA                        787 detecções · 4 vírus
► SAMAMBAIA                         849 detecções · 4 vírus
```

**Níveis de localidade:**

| Nível | O que é |
|---|---|
| Bairro / Região Administrativa | o território como veio na notificação (no DF, a RA) |
| Município | o município do IBGE (no DF, todas as RAs viram Brasília) |
| Estado (UF) | a UF de residência |

**Controles:**

| Controle | O que faz |
|---|---|
| Nível da localidade | define o que é uma "seção" |
| Vírus a exibir | quais facetas aparecem dentro de cada seção (+ *Selecionar todos* / *Limpar*) |
| Localidades | quais seções existem (+ *Selecionar todas* / *Limpar*) |
| Facetas por linha | 1 a 4 colunas dentro da seção |
| Calcular todas | calcula tudo de uma vez, para preencher a tabela |

**O cálculo roda só ao expandir uma seção** — cerca de 1,5 s por localidade — e fica em cache
enquanto os filtros não mudarem. É o que evita esperar pelo Rt de dezenas de localidades que
você talvez nem fosse olhar. A tabela abaixo mostra o Rt mais recente das localidades já
calculadas; o botão *Calcular todas* preenche o restante.

No boletim em Word a mesma informação vira **uma figura em grade** (localidade × vírus),
limitada às 8 localidades com mais detecções.

---

## 6. De onde vêm os dados

### 6.1 Casos: API de Dados Abertos do SUS

O portal antigo (`opendatasus.saude.gov.br`) foi descontinuado e **não é mais CKAN**. A API
atual `[10]` é aberta, sem chave, com dois endpoints de SRAG:

- `/vigilancia-e-meio-ambiente/srag-2019-2026` — ficha atual, com filtros;
- `/vigilancia-e-meio-ambiente/srag-2013-2018` — ficha antiga, **sem filtros**.

**Como o painel consulta, passo a passo:**

1. descobre o último mês com dados (resultado em cache por 12 h);
2. percorre **mês a mês** (`anomes_sin_pri`) dentro da UF escolhida;
3. pagina com `limit=1000`, repetindo em caso de erro 502;
4. salva um `.rds` por mês em `Bancos/cache`;
5. converte para o esquema do boletim (faixa etária, suporte ventilatório, evolução, óbito,
   tempo de internação), conforme o dicionário da ficha do SIVEP-Gripe `[9]`.

**Particularidades que o código já contorna:**

| Item | Comportamento real |
|---|---|
| `limit` | máximo de 1000 |
| `offset` | é **número de página** (0 = primeira), não deslocamento de linhas |
| `campos` | documentado, mas sempre responde 502 — não dá para reduzir o payload |
| `dt_sin_pri` sozinho | estoura o timeout |
| `sg_uf_not` sozinho | estoura o timeout; combinado com `anomes_sin_pri` responde em 2–3 s |
| gzip | reduz a resposta ~28× (755 KB → 27 KB) |

**Volume e atualidade medidos:**

| | |
|---|---|
| Brasil, 2019–2026 | 4.445.192 registros |
| DF, 2019–2026 | 113.713 registros |
| Último mês disponível | janeiro de 2026 |

**2018 não está incluído.** O endpoint "2013-2018" expõe só 2018 (47.756 registros no Brasil) e
usa a ficha antiga: datas em `DD/MM/AAAA`, UF e município como código, sem `tp_idade`, sem nome
de hospital, sem data de desfecho e sem os campos `pcr_*` por vírus (não havia rinovírus,
metapneumovírus, bocavírus nem SARS-CoV-2). Daria para trazer só a contagem de casos por
semana, deixando vazias as seções de vírus, hospitalização e internação — por isso a série
começa em 2019.

### 6.2 População: IBGE

A população **segue o filtro do painel**: escolher uma UF ou municípios filtra também a consulta
ao IBGE.

| Dado | Origem |
|---|---|
| Estimativas municipais | SIDRA tabela **6579**, variável 9324 `[11]` |
| Censo 2022 | SIDRA tabela **4714** (a 6579 não traz 2022 nem 2023) `[12]` |
| 2023 | interpolado entre 2022 e 2024 |
| UF inteira | nível `n3` da tabela 6579 |

A junção com a base de casos é feita pelo **código IBGE de 6 dígitos** — o SIVEP usa o código
sem o dígito verificador — e, como alternativa, pelo nome padronizado.

---

## 7. Casos especiais de território

### 7.1 Distrito Federal: população da Codeplan

As **Regiões Administrativas do DF** não existem no IBGE — para o instituto o DF é um único
município ("Brasília"). Por isso, quando a UF escolhida é o **DF**, a população das RAs vem de
`Bancos/codeplan_populacao_long.csv` `[13]` (colunas `ano;regiao_adm;faixa_etaria;pop;…`),
somando as faixas etárias. Territórios **de fora do DF** — o entorno goiano, por exemplo —
continuam usando o IBGE.

O total do DF também passa a ser o da Codeplan (3.239.675 em 2025, contra 2.996.899 do IBGE),
para que o denominador das RAs e o do DF inteiro venham da mesma fonte.

A base de casos usa nomes que não são os da lista oficial de RAs, então o painel consolida:

| Base de casos | Região Administrativa |
|---|---|
| `brasilia asa norte`, `brasilia asa sul`, `brasilia` | **Plano Piloto** |
| `scia estrutural` | **Estrutural (SCIA)** |
| `setor de industria e abastecimento` | **SIA** |

O painel avisa que fez essa consolidação. Com ela, incidência e diagrama de controle passam a
funcionar por RA — antes ficavam indisponíveis por falta de denominador.

### 7.2 Outros territórios fora do recorte do IBGE

Para distritos, regiões de saúde ou qualquer outro recorte sem população oficial:

1. abra `Bancos/POP_LOCAL_MODELO.csv` — ele já lista os territórios da sua base sem população,
   ordenados por número de casos;
2. preencha a coluna `populacao`;
3. salve como `Bancos/POP_LOCAL.csv`.

Formatos aceitos:

```csv
municipio,populacao
ceilandia,398374
taguatinga,205670
```

ou, variando por ano:

```csv
ano,municipio,populacao
2024,ceilandia,396000
2025,ceilandia,398374
```

Sem a coluna `ano`, o valor vale para todos os anos. A população local **tem prioridade** sobre
a do IBGE. Enquanto faltar, o painel continua funcionando e diz exatamente o que falta.

> Com a Codeplan e o IBGE combinados, restam **4 territórios sem população** entre os 482 da
> base do DF — e eles somam 9 casos em 113 mil, ou seja, **100% dos casos ficam cobertos**.

---

## 8. Cache: por que o painel abre rápido

Toda vez que os dados são atualizados, o painel grava em `Bancos/cache`:

| Arquivo | Conteúdo |
|---|---|
| `base_api_<UF>.json` | manifesto legível + dados consolidados (casos por semana, por território e por vírus, população usada) e a **assinatura da fonte** |
| `base_api_<UF>.rds` | as linhas individuais, para a tabela e o boletim |
| `srag_<UF>_<AAAAMM>.rds` | um arquivo por mês baixado da API |
| `api_intervalo.json`, `ibge_anos.json`, `ibge_municipios.json` | metadados das APIs |

**Passo a passo do que acontece ao abrir:**

1. o painel lê **primeiro o `.json`** e compara a assinatura da fonte;
2. se nada mudou, carrega o `.rds` — a base de 113.713 registros do DF abre em **0,3 s**, em vez
   de ~15 minutos;
3. se a API publicou um mês novo (ou o arquivo enviado mudou de tamanho/data), a assinatura
   muda sozinha, os dados são rebaixados e um `.json` novo é emitido.

As linhas ficam em `.rds` de propósito: 113 mil registros em JSON puro levariam dezenas de
segundos para abrir, o oposto do objetivo em computador lento. O `.json` guarda o manifesto e os
dados consolidados, que é o que o painel precisa consultar para decidir se recalcula.

O botão **Atualizar dados da fonte** apaga o cache e força o redownload.

---

## 9. Arquitetura dos arquivos

```
app.R                     painel (ponto de entrada)
abrir_painel.bat          atalho no Windows
abrir_painel_linux.sh     atalho no Linux/macOS
README.md
R/
  funcoes_boletim.R       cálculos, indicadores, gráficos ggplot e textos
  graficos.R              versão interativa (plotly) dos gráficos
  fontes_dados.R          APIs, upload, cache, preferências e geração do Word
  nowcasting.R            correção do atraso de notificação
  boletim.Rmd             modelo do boletim em Word
Bancos/
  cache/                  .json e .rds gerados pelo painel
  codeplan_populacao_long.csv   população por RA do DF
  POP_LOCAL.csv           (opcional) população de territórios não-IBGE
  POP_LOCAL_MODELO.csv    modelo pronto para preencher
  SRAG.csv, POP.csv       bases antigas, mantidas como alternativa offline
```

**Regra de ouro:** tudo que calcula número ou desenha gráfico vive em `R/funcoes_boletim.R`. O
painel e o boletim em Word chamam **as mesmas funções**, para que nunca divirjam.

---

## 10. Solução de problemas

| Sintoma | Causa e solução |
|---|---|
| O painel não carrega nada | Selecione uma UF. Na primeira abertura ela vem vazia de propósito. |
| Primeira carga muito demorada | É o download do histórico completo da UF. Acontece uma vez por UF; depois o cache resolve. |
| Seções de incidência vazias | Falta população para os territórios selecionados. Veja o [item 7.2](#72-outros-territórios-fora-do-recorte-do-ibge). |
| `Pandoc nao encontrado` | Instale o RStudio (que já o inclui) ou o Pandoc. Só afeta o `.docx`. |
| Rt não estimado | Período curto ou poucos casos por localidade/vírus. Amplie o intervalo ou use um nível mais agregado. |
| Nowcasting indisponível | A base não tem `DT_DIGITA`. Use a fonte via API. |
| "Nenhum caso restou após os filtros" | A combinação de filtros é restritiva demais. Use *Limpar filtros*. |
| Dados desatualizados | Clique em *Atualizar dados da fonte*. Lembrando que a própria API publica com defasagem. |

---

## 11. Referências

### Métodos epidemiológicos

1. BRASIL. Ministério da Saúde. **Guia de Vigilância Epidemiológica**. 10. ed. Brasília: Ministério da Saúde, 2023.
2. CORI, A.; FERGUSON, N. M.; FRASER, C.; CAUCHEMEZ, S. A New Framework and Software to Estimate Time-Varying Reproduction Numbers During Epidemics. ***American Journal of Epidemiology***, v. 178, n. 9, p. 1505–1512, 2013. — *método do Rt usado no painel.*
3. BASTOS, L. S. et al. A modelling approach for correcting reporting delays in disease surveillance data. ***Statistics in Medicine***, v. 38, n. 22, p. 4363–4377, 2019. — *base do nowcasting por triângulo de notificação.*
4. HÖHLE, M.; AN DER HEIDEN, M. Bayesian nowcasting during the STEC O104:H4 outbreak in Germany, 2011. ***Biometrics***, v. 70, n. 4, p. 993–1002, 2014.
5. CDC — Centers for Disease Control and Prevention. **Principles of Epidemiology in Public Health Practice**. 3. ed. Atlanta: CDC, 2012.
6. MEDRONHO, R. A. et al. **Epidemiologia**. 2. ed. São Paulo: Atheneu, 2009.
7. UNIVERSIDADE FEDERAL DE SANTA CATARINA. **Construção de diagramas de controle na vigilância em saúde**. Florianópolis: UFSC, 2024.
8. THOMPSON, R. N. et al. Improved inference of time-varying reproduction numbers during infectious disease outbreaks. ***Epidemics***, v. 29, 100356, 2019.
9. BRASIL. Ministério da Saúde. **SIVEP-Gripe — ficha de registro individual de Síndrome Respiratória Aguda Grave e dicionário de dados**. Brasília: Ministério da Saúde.

### Fontes de dados

10. BRASIL. Ministério da Saúde. **API de Dados Abertos do SUS (DEMAS)**. Disponível em: https://apidadosabertos.saude.gov.br/v1
11. IBGE. **Estimativas da população residente para os municípios e Unidades da Federação brasileiros**. SIDRA, tabela 6579.
12. IBGE. **Censo Demográfico 2022 — população residente**. SIDRA, tabela 4714.
13. CODEPLAN/IPEDF. **Projeções populacionais das Regiões Administrativas do Distrito Federal**. Planilha `codeplan_populacao_long.csv`, incluída no projeto.
14. SOUSA, M. L. et al. Vigilância da Síndrome Respiratória Aguda Grave: análise dos dados de notificação no Brasil. ***Epidemiologia e Serviços de Saúde***, v. 34, n. 1, 2025.

### Software

15. CORI, A. et al. **EpiEstim: Estimate Time Varying Reproduction Numbers from Epidemic Curves**. Pacote R, versão 2.2.5.
16. WICKHAM, H. **ggplot2: Elegant Graphics for Data Analysis**. 2. ed. Springer, 2016. Pacote R, versão 4.0.3.
17. SIEVERT, C. **Interactive Web-Based Data Visualization with R, plotly, and shiny**. Chapman and Hall/CRC, 2020. Pacote R `plotly`, versão 4.12.0.
18. CHANG, W. et al. **shiny: Web Application Framework for R**. Pacote R, versão 1.11.1.
19. R CORE TEAM. **R: A Language and Environment for Statistical Computing**. R Foundation for Statistical Computing, Viena. Versão 4.5.2.

### Leitura complementar

20. FERREIRA, M. R.; COSTA, D. L.; PEREIRA, A. C. Estimativas do número reprodutivo efetivo (Rt) na vigilância viral respiratória. ***Revista Brasileira de Epidemiologia***, v. 26, e230001, 2023.

---

## Contato

José Lucas — santos.joselucas.37@gmail.com — [LinkedIn](https://www.linkedin.com/in/jose-lucas-santos)
