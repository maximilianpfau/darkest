#' Select best model based on AIC
#'
#' This function allows you to select the best model based on AIC
#' @param candidate_fits Multiple model fits obtained by the multistart_fit_p4_model() function
#'
#' @return
#' @export
#'
#' @examples
#' select_best_DA_fit(candidate_fits)

select_best_DA_fit <- function(candidate_fits) {

  cleaned <- candidate_fits %>%
    dplyr::filter(!is.na(fit))  %>%
    dplyr::mutate(glanced = purrr::map(fit, ~broom::glance(.x)) )

  best_fit <- cleaned %>%
    tidyr::unnest(glanced) %>%
    dplyr::slice(which.min(AIC)) %>%
    dplyr::select(fit) %>%
    purrr::flatten()

  return(best_fit$fit)

}

