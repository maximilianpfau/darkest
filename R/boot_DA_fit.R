#' Boot_DA_fit
#'
#' @param dat Input data
#' @param start_val_grid Gird with the start values for a four parameter model (ct, CRbreak, S2, tf)
#' @param n Number of bootstrap samples
#'
#' @return
#' @export
#'
#' @examples
#' boot_DA_fit(dat, start_val_grid, n)

boot_DA_fit <- function(dat, start_val_grid, n) {
  
  boots <- dat %>%
    dplyr::select(Time, Threshold) %>%
    rsample::bootstraps(times = n,  apparent = TRUE)
  
  
  boot_models <- boots %>%
    dplyr::mutate(fit = purrr::map(splits, ~ select_best_DA_fit(multistart_fit_p4_model(rsample::analysis(.x), start_val_grid))),
                  tidied = purrr::map(fit, broom::tidy))
  
  nested_boot_models <- boot_models %>%
    dplyr::select(fit) %>%
    tidyr::nest(boot_fits=c(fit))
  
  return(nested_boot_models)
  
}