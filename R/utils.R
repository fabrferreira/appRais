pull_name_from_vector <- function(lista) {

  data <- purrr::map(.x = lista, .f = ~purrr::pluck(.x, "name"))

  return(data)
}

pull_value_from_vector <- function(lista) {

  data <- purrr::map(.x = lista, .f = ~purrr::pluck(.x, "value"))

  return(data)
}

pull_col_from_vector <- function(lista, col_name) {

  purrr::map(.x = lista, .f = ~purrr::pluck(.x, col_name))
}

tabela_reactable <- function(tab, ...) {
  tab |>
    reactable::reactable(
      ...,
      defaultColDef = reactable::colDef(
        style = list(
          fontSize = "14px"
        )
      )
    )
}

formatar_numero <- function(var, ...) {

  scales::number(x = var,
                 big.mark = ".",
                 decimal.mark = ",",
                 ...
  )
}

formatar_moeda <- function(var, ...) {

  scales::dollar(x = var,
                 big.mark = ".",
                 decimal.mark = ",",
                 prefix = "R$",
                 ...
  )
}

globalVariables(
  c(
    "Feminino", "Masculino", "atividade_economica", "escolaridade", "idade",
    "idade_media", "n", "name", "nome_municipio", "raca_cor", "remuneracao_media",
    "rendimento_medio", "sexo", "total", "valor_remun", "valor_remuneracao_media",
    "value"
  )
)
