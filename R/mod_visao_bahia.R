#' visao_bahia UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_visao_bahia_ui <- function(id){
  ns <- NS(id)
  tagList(
    bslib::page_fluid(
      bslib::card(
        bslib::card_header(
          "Top 10 atividades econômicas com maior remuneração média"
        ),
        bslib::card_body(
          echarts4r::echarts4rOutput(
            ns("top_10_atv_economica")
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
            "Top 10 municípios com maior número de vínculos"
          ),
          bslib::card_body(
            echarts4r::echarts4rOutput(
              ns("top_10_muni_vinculos")
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
            "Decomposição dos vínculos ativos por sexo"
          ),
          bslib::card_body(
            echarts4r::echarts4rOutput(
              ns("top_10_muni_vinculos_sexo")
            ) |>
              shinycssloaders::withSpinner()
          ),
          bslib::card_footer(
            "Fonte: RAIS, MTE."
          ),
          full_screen = TRUE
        )
      )
    )
  )
}

#' visao_bahia Server Functions
#'
#' @noRd
mod_visao_bahia_server <- function(id, con){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    tab_rais <- dplyr::tbl(con, "tab_rais")

    output$top_10_atv_economica <- echarts4r::renderEcharts4r({

      top_10_rem <- tab_rais |>
        dplyr::summarise(
          remuneracao_media = mean(valor_remuneracao_media, na.rm = TRUE),
          .by = "atividade_economica"
        ) |>
        dplyr::slice_max(
          n = 10, order_by = remuneracao_media
        ) |>
        dplyr::collect() |>
        dplyr::arrange(remuneracao_media) |>
        dplyr::select(
          value = remuneracao_media,
          name = atividade_economica
        ) |>
        purrr::transpose()

      echarts4r::e_chart() |>
        echarts4r::e_list(
          list(
            series = list(
              type = "bar",
              data = top_10_rem
            ),
            data = list(
              groupId = "name"
            ),
            xAxis = list(
              type = "value",
              data = pull_value_from_vector(top_10_rem),
              name = "Valor em Reais",
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
              data = pull_name_from_vector(top_10_rem),
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
                        var text = '<b>Atividade econômica</b>: ' + params.name;
                        text += '<br>';
                        text += '<b>Valor da remuneração</b>: ' + params.value.toLocaleString('pt-BR');
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

    output$top_10_muni_vinculos <- echarts4r::renderEcharts4r({

      vinculos_muni <- tab_rais |>
        dplyr::count(nome_municipio, sort = TRUE) |>
        dplyr::select(
          name = nome_municipio,
          value = n
        ) |>
        dplyr::slice_max(
          order_by = value,
          n = 10
        ) |>
        dplyr::collect() |>
        dplyr::arrange(value) |>
        purrr::transpose()

      echarts4r::e_chart() |>
        echarts4r::e_list(
          list(
            series = list(
              type = "bar",
              data = vinculos_muni
            ),
            data = list(
              groupId = "name"
            ),
            xAxis = list(
              type = "value",
              data = pull_value_from_vector(vinculos_muni),
              name = "Número de vínculos",
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
              data = pull_name_from_vector(vinculos_muni),
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
                        var text = '<b>Nome do município</b>: ' + params.name;
                        text += '<br>';
                        text += '<b>Vínculos ativos</b>: ' + params.value.toLocaleString('pt-BR');
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

    output$top_10_muni_vinculos_sexo <- echarts4r::renderEcharts4r({

      vinculos_muni_sexo <- tab_rais |>
        dplyr::count(
          sexo, nome_municipio, sort = TRUE
        ) |>
        dplyr::collect() |>
        dplyr::select(
          name = nome_municipio,
          sexo,
          value = n
        )

      vinculos_sexo_desagregado <- vinculos_muni_sexo |>
        tidyr::pivot_wider(id_cols = name,
                           names_from = "sexo",
                           values_from = "value") |>
        dplyr::mutate(total = Masculino + Feminino) |>
        dplyr::slice_max(order_by = total,
                         n = 10) |>
        dplyr::select(
          name,
          masculino = Masculino,
          feminino = Feminino,
          total
        ) |>
        dplyr::arrange(
          total
        ) |>
        purrr::transpose()

      echarts4r::e_chart() |>
        echarts4r::e_list(
          list(
            series = list(
              list(
                name = "Homens",
                type = "bar",
                data = pull_col_from_vector(vinculos_sexo_desagregado, "masculino")
              ),
              list(
                name = "Mulheres",
                type = "bar",
                data = pull_col_from_vector(vinculos_sexo_desagregado, "feminino")
              )
            ),
            xAxis = list(
              type = "value",
              name = "Número de vínculos",
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
              data = pull_name_from_vector(vinculos_sexo_desagregado),
              nameLocation = "center",
              nameGap = 55,
              axisTicks = list(
                alignWithLabel = TRUE

              )
            ),
            color = c("#2C3778FF", "#CC00CC"),
            grid = list(
              containLabel = TRUE
            ),
            legend = list(
              list(
                show = TRUE,
                type = "scroll",
                icon = "circle"
              )
            ),
            tooltip = list(
              trigger = "item",
              formatter = htmlwidgets::JS(
                "function(params){
                        var text = '<b>Nome do município</b>: ' + params.name;
                        text += '<br>';
                        text += '<b>Vínculos ativos</b>: ' + params.value.toLocaleString('pt-BR');
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
  })
}

## To be copied in the UI
# mod_visao_bahia_ui("visao_bahia_1")

## To be copied in the server
# mod_visao_bahia_server("visao_bahia_1")
