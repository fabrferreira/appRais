#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  con <- RSQLite::dbConnect(RSQLite::SQLite(), "data-raw/bd_rais.sqlite")

  mod_visao_bahia_server("visao_bahia_1", con)

  mod_municipios_server("municipios_1", con)
}
