
#' Fit four parameter models with multiple start parameters
#'
#' This function allows you to fit four parameter models using multiple start parameters
#' @param data Dark adaptation data with a column 'Time' and 'Threshold'
#' @param start_val_grid Gird with the start values for a four parameter model (ct, CRbreak, S2, tf)
#'
#' @return
#' @export
#'
#' @examples
#' multistart_fit_p4_model(data, start_val_grid)

multistart_fit_p4_model <- function(data, start_val_grid) {

  result <- tibble::as_tibble(start_val_grid) %>%
    dplyr::mutate(combination = paste(ct, CRbreak, S2, tf)) %>%
    tidyr::nest(start_val=c(ct, CRbreak, S2, tf)) %>%
    dplyr::mutate(fit = purrr::map(start_val, ~fit_p4_model(data, .x ) ) )


  return(result)

}


