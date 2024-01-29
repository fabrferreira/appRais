#' municipios UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_municipios_ui <- function(id){
  ns <- NS(id)
  tagList(
    bslib::page_sidebar(
      fillable = FALSE,
      sidebar = bslib::sidebar(
        uiOutput(ns("filtro_municipio")),
        shinyWidgets::actionBttn(
          inputId = ns("press_button"),
          label = "Gerar análises",
          icon = icon("chart-pie", class = "fa-solid"),
          class = "botao_acao"
        )
      ),
      bslib::layout_columns(
        col_widths = c(4, 4, 4),
        uiOutput(ns("numero_vinculos")) |>
          shinycssloaders::withSpinner(),
        uiOutput(ns("rendimento_medio")) |>
          shinycssloaders::withSpinner(),
        uiOutput(ns("idade_media")) |>
          shinycssloaders::withSpinner()
      ),
      bslib::card(
        bslib::card_header(
          "Atividades econômicas com maior remuneracao"
        ),
        bslib::card_body(
          reactable::reactableOutput(
            ns("remun_atividade")
          ) |>
            shinycssloaders::withSpinner()
        ),
        bslib::card_footer(
          "Fonte: RAIS, MTE."
        )
      ),
      bslib::layout_columns(
        bslib::card(
          bslib::card_header(
            "Empregados por nível de escolaridade"
          ),
          bslib::card_body(
            echarts4r::echarts4rOutput(
              ns("escolaridade_forca")
            ) |>
              shinycssloaders::withSpinner()
          ),
          bslib::card_footer(
            "Fonte: RAIS, MTE."
          ),
          full_screen = TRUE
        ),
        bslib::card(
          bslib::card_header(
            "Remuneração por nível de escolaridade"
          ),
          bslib::card_body(
            echarts4r::echarts4rOutput(
              ns("remuneracao_escolaridade")
            ) |>
              shinycssloaders::withSpinner()
          ),
          bslib::card_footer(
            "Fonte: RAIS, MTE."
          ),
          full_screen = TRUE
        )
      ),
      bslib::layout_columns(
        bslib::card(
          bslib::card_header(
            "Composição da força de trabalho por sexo"
          ),
          bslib::card_body(
            echarts4r::echarts4rOutput(
              ns("composicao_sexo")
            ) |>
              shinycssloaders::withSpinner()
          ),
          bslib::card_footer(
            "Fonte: RAIS, MTE."
          )
        ),
        bslib::card(
          bslib::card_header(
            "Composição da força de trabalho por raça"
          ),
          bslib::card_body(
            echarts4r::echarts4rOutput(
              ns("composicao_raca")
            ) |>
              shinycssloaders::withSpinner()
          ),
          bslib::card_footer(
            "Fonte: RAIS, MTE."
          )
        )
      )
    )
  )
}

#' municipios Server Functions
#'
#' @noRd
mod_municipios_server <- function(id, con){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    tab_rais <- dplyr::tbl(con, "tab_rais")

    output$filtro_municipio <- renderUI({

      opcoes_muni <- tab_rais |>
        dplyr::pull(nome_municipio) |>
        unique() |>
        sort()

      shinyWidgets::pickerInput(
        inputId = ns("filtro_municipio"),
        label = h6("Escolha um ou mais municípios:"),
        choices = opcoes_muni,
        multiple = TRUE,
        selected = NULL,
        options = shinyWidgets::pickerOptions(
          actionsBox = TRUE,
          selectAllText = "Selecionar tudo",
          deselectAllText = "Retirar seleção",
          noneSelectedText = "Nenhuma seleção realizada",
          liveSearch = TRUE
        )
      )
    })

    rais_filtrada <- eventReactive(input$press_button, {

      tab_rais |>
        dplyr::filter(
          nome_municipio %in% !!input$filtro_municipio
        )
    })

    output$numero_vinculos <- renderUI({

      rais_filtrada() |>
        dplyr::count() |>
        dplyr::pull(n) |>
        formatar_numero() |>
        bslib::value_box(
          title = "Número de vínculos",
          showcase = bsicons::bs_icon("briefcase"),
          theme = bslib::value_box_theme(
            bg = "#071e41",
            fg = "#fff"
          )
        )
    })

    output$rendimento_medio <- renderUI({

      rais_filtrada() |>
        dplyr::summarise(
          rendimento_medio = mean(valor_remuneracao_media, na.rm = TRUE)
        ) |>
        dplyr::pull(rendimento_medio) |>
        formatar_moeda() |>
        bslib::value_box(
          title = "Rendimento médio",
          showcase = bsicons::bs_icon("cash"),
          theme = bslib::value_box_theme(
            bg = "#071e41",
            fg = "#fff"
          )
        )
    })

    output$idade_media <- renderUI({

      rais_filtrada() |>
        dplyr::summarise(
          idade_media = mean(idade, na.rm = TRUE)
        ) |>
        dplyr::pull(idade_media) |>
        formatar_numero() |>
        bslib::value_box(
          title = "Idade média",
          showcase = bsicons::bs_icon("person-raised-hand"),
          theme = bslib::value_box_theme(
            bg = "#071e41",
            fg = "#fff"
          )
        )
    })

    output$remun_atividade <- reactable::renderReactable({

      rais_filtrada() |>
        dplyr::summarise(
          .by = "atividade_economica",
          remuneracao_media = mean(valor_remuneracao_media, na.rm = TRUE)
        ) |>
        dplyr::slice_max(
          order_by = remuneracao_media,
          n = 10
        ) |>
        dplyr::collect() |>
        dplyr::mutate(
          remuneracao_media = scales::dollar(
            x = remuneracao_media,
            big.mark = ".",
            decimal.mark = ",",
            prefix = "R$"
          )
        ) |>
        tabela_reactable(
          columns = list(
            atividade_economica = reactable::colDef(
              name = "Atividade econômica",
              minWidth = 50
            ),
            remuneracao_media = reactable::colDef(
              name = "Remuneração média",
              minWidth = 50,
              align = "right"
            )
          )
        )
    })

    output$escolaridade_forca <- echarts4r::renderEcharts4r({

      tab_escolaridade <- rais_filtrada() |>
        dplyr::count(escolaridade, sort = TRUE) |>
        dplyr::collect() |>
        dplyr::select(
          name = escolaridade,
          value = n
        ) |>
        dplyr::arrange(value) |>
        purrr::transpose()

      echarts4r::e_chart() |>
        echarts4r::e_list(
          list(
            series = list(
              type = "bar",
              data = tab_escolaridade
            ),
            data = list(
              groupId = "name"
            ),
            xAxis = list(
              type = "value",
              data = pull_value_from_vector(tab_escolaridade),
              name = "Empregados",
              nameLocation = "center",
              nameGap = 40,
              axisTicks = list(
                alignWithLabel = TRUE
              ),
              axisLabel = list(
                show = TRUE,
                hideOverlap = FALSE,
                formatter = htmlwidgets::JS(
                  "function(value){
                            return value.toLocaleString('pt-BR');
                        }"
                )
              )
            ),
            yAxis = list(
              type = "category",
              data = pull_name_from_vector(tab_escolaridade),
              nameLocation = "center",
              nameGap = 55,
              axisTicks = list(
                alignWithLabel = TRUE

              )
            ),
            color = "#2C3778FF",
            grid = list(
              containLabel = TRUE
            ),
            legend = list(
              show = TRUE
            ),
            tooltip = list(
              trigger = "item",
              formatter = htmlwidgets::JS(
                "function(params){
                        var text = '<b>Escolaridade</b>: ' + params.name;
                        text += '<br>';
                        text += '<b>Nº de pessoas</b>: ' + params.value.toLocaleString('pt-BR');
                        return text;
                    }"
              )
            ),
            toolbox = list(
              show = TRUE,
              feature = list(
                dataZoom = list(
                  yAxisIndex = "none"
                ),
                restore = list(
                  title = "Restaurar"
                ),
                saveAsImage = list(
                  title = "Salvar como imagem"
                )
              )
            )
          )
        )
    })

    output$remuneracao_escolaridade <- echarts4r::renderEcharts4r({

      tab_escolaridade_renda <- rais_filtrada() |>
        dplyr::summarise(
          valor_remun = mean(valor_remuneracao_media, na.rm = TRUE),
          .by = "escolaridade"
        ) |>
        dplyr::collect() |>
        dplyr::select(
          name = escolaridade,
          value = valor_remun
        ) |>
        dplyr::arrange(value) |>
        dplyr::mutate(
          value = round(value, 2)
        ) |>
        purrr::transpose()

      echarts4r::e_chart() |>
        echarts4r::e_list(
          list(
            series = list(
              type = "bar",
              data = tab_escolaridade_renda
            ),
            data = list(
              groupId = "name"
            ),
            xAxis = list(
              type = "value",
              data = pull_value_from_vector(tab_escolaridade_renda),
              name = "Valor em reais",
              nameLocation = "center",
              nameGap = 40,
              axisTicks = list(
                alignWithLabel = TRUE
              ),
              axisLabel = list(
                show = TRUE,
                hideOverlap = FALSE,
                formatter = htmlwidgets::JS(
                  "function(value){
                            return value.toLocaleString('pt-BR');
                        }"
                )
              )
            ),
            yAxis = list(
              type = "category",
              data = pull_name_from_vector(tab_escolaridade_renda),
              nameLocation = "center",
              nameGap = 55,
              axisTicks = list(
                alignWithLabel = TRUE

              )
            ),
            color = "#2C3778FF",
            grid = list(
              containLabel = TRUE
            ),
            legend = list(
              show = TRUE
            ),
            tooltip = list(
              trigger = "item",
              formatter = htmlwidgets::JS(
                "function(params){
                        var text = '<b>Escolaridade</b>: ' + params.name;
                        text += '<br>';
                        text += '<b>Remuneração</b>: ' + 'R$' + params.value.toLocaleString('pt-BR');
                        return text;
                    }"
              )
            ),
            toolbox = list(
              show = TRUE,
              feature = list(
                dataZoom = list(
                  yAxisIndex = "none"
                ),
                restore = list(
                  title = "Restaurar"
                ),
                saveAsImage = list(
                  title = "Salvar como imagem"
                )
              )
            )
          )
        )
    })

    output$composicao_sexo <- echarts4r::renderEcharts4r({

      sexo_composicao <- rais_filtrada() |>
        dplyr::count(sexo, sort = TRUE) |>
        dplyr::collect() |>
        dplyr::select(
          name = sexo,
          value = n
        ) |>
        purrr::transpose()

      echarts4r::e_chart() |>
        echarts4r::e_list(
          list(
            tooltip = list(
              trigger = "item",
              formatter = htmlwidgets::JS(
                "function(params){
                var text = '<b>Sexo: </b>' + params.name;
                text += '<br>';
                text += '<b>Nº empregados</b> ' + params.value.toLocaleString('pt-BR');
                return text;
                }"
              )
            ),
            legend = list(
              top = "5%",
              left = "center"
            ),
            series = list(
              list(
                name = "Masculino",
                type = "pie",
                radius = c("40%", "70%"),
                avoidLabelOverlap = FALSE,
                itemStyle = list(
                  borderRadius = 10,
                  borderColor = "#fff",
                  borderWidth = 2
                ),
                label = list(
                  show = FALSE,
                  position = "center"
                ),
                emphasis = list(
                  label = list(
                    show  = TRUE,
                    fontSize = 40,
                    fontWeight = "bold"
                  )
                ),
                labelLine = list(
                  show = FALSE
                ),
                data = sexo_composicao,
                color = c("#2C3778FF", "#CC00CC")
              )
            )
          )
        )
    })

    output$composicao_raca <- echarts4r::renderEcharts4r({

      raca_composicao <- rais_filtrada() |>
        dplyr::count(raca_cor, sort = TRUE) |>
        dplyr::collect() |>
        dplyr::select(
          name = raca_cor,
          value = n
        ) |>
        dplyr::filter(name != "Não identificado") |>
        purrr::transpose()

      echarts4r::e_chart() |>
        echarts4r::e_list(
          list(
            tooltip = list(
              trigger = "item",
              formatter = htmlwidgets::JS(
                "function(params){
                var text = '<b>Raça: </b>' + params.name;
                text += '<br>';
                text += '<b>Nº empregados</b> ' + params.value.toLocaleString('pt-BR');
                return text;
                }"
              )
            ),
            legend = list(
              top = "5%",
              left = "center"
            ),
            series = list(
              list(
                name = "Raça",
                type = "pie",
                radius = c("40%", "70%"),
                avoidLabelOverlap = FALSE,
                itemStyle = list(
                  borderRadius = 10,
                  borderColor = "#fff",
                  borderWidth = 2
                ),
                label = list(
                  show = FALSE,
                  position = "center"
                ),
                emphasis = list(
                  label = list(
                    show  = TRUE,
                    fontSize = 40,
                    fontWeight = "bold"
                  )
                ),
                labelLine = list(
                  show = FALSE
                ),
                data = raca_composicao,
                color = c("#FFD353FF", "#DE4F33FF", "#9F2D55FF", "#341648FF", "#0072CEFF")
              )
            )
          )
        )
    })
  })
}

## To be copied in the UI
# mod_municipios_ui("municipios_1")

## To be copied in the server
# mod_municipios_server("municipios_1")
