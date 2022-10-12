#' Examine the design
#'
#' @param effect assumed effect size. Numeric value.
#' @param se assume standard error. Positive, numeric value.
#' @param df degrees of freedom.
#' @param n Number of simulated trials.
#'
#' @return statistics
#' @export
#'
#' @examples
#' examine(1, 1, 1, 1)
examine <- function(effect = 1, se = 1, df = 1, n = 100){
  return (effect + se + df * n)
}
