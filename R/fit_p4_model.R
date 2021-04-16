#' Fit a four parameter model
#'
#' @param x Dark adaptation with columns Time and Threshold
#' @param start_vals Start values for the parameters
#' ct Cone threshold at the cone plateau [LogUnits],
#' tf Final rod threshold [LogUnits],
#' S2 Rate of rod decay [decades/min],
#' Time of cone-rod break ('Kohlrausch kink') [min]
#' @return
#' @export
#'
#' @examples
#' fit_p4_model(x, start_vals)

fit_p4_model <- function(x, start_vals) {

  fit <- NA

  try(
    fit <- minpack.lm::nlsLM(Threshold ~ ct + log10( 10^(S2 * (Time - CRbreak)) + 10^(tf-ct) ) * (Time > CRbreak),
                             data = x,
                             start = start_vals,
                             control = list(maxiter = 100)),
    silent = TRUE
  )

  return(fit)
}
