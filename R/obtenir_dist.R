#' Obtenir la distribució de la població per una variable específica a partir del fitxer de les dades poblacionals
#'
#'
#' @param variable Variable la qual volem calcular 
#' @param dades_raking Dades en les quals tenim les variables que usarem en el calcul de la ponderació 
#' @param dades_population Dades poblacionals

obtenir_dist <- function(variable, data_imputed, data_population) {
  result <- as.data.frame(
    data_population |>
      filter(identificador == variable) |>
      select(codi_resposta, valor)  |>
      filter(!is.na(codi_resposta)) |>
      rename(!!sym(variable) := codi_resposta) |>
      mutate(Freq = (valor / 100) * nrow(data_imputed)) |> 
      select(!!sym(variable), Freq)
  )
  
  return(result)
}

