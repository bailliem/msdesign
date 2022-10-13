#' examine: Examines power, Type S, and Type M error for a given design
#'
#' Calculates Power, Type S, and Type M error and returns them in a list or
#' df, depending on whether a single true effect size or range is provided.
#' retro_design() is faster as it uses the closed form solution from Lu et al.
#' (2018), but this function can be used for t distributions, whereas
#' retro_design() cannot. Function originally provided in Gelman and
#' Carlin (2014), reused with permission.
#'
#'
#' @param effect_size a numeric or list containing an estimate(s) of the "true"
#' effect size.
#' @param standard_error a numeric, standard error of the estimate
#' @param alpha a numeric, the statistical significance threshold
#' @param df a numeric, the degrees of freedom. df=Inf is equivalent
#' to a normal distribution.
#' @param number_simulations a numeric, how many times to simulate when calculating Type M
#' error.
#' @return either a list of length 3 containing the power, type s, and type M
#' error, or if A is a list, a df that is 4 by length(A), with an effect size
#' and it's corresponding power, type s, and type m errors in each row.
#' @examples
#' examine(1,3.28)
#' examine(list(.2,2,20),8.1)
#' examine(.5,1,df=10)
#' @export
#' @import stats
examine <- function (effect_size, standard_error, alpha=.05, df=Inf, number_simulations=10) {
  if (standard_error < 0){
    stop("The standard error std_err is negative, must be a positive value.")
  }

  UseMethod("examine", effect_size)
}




#' Numeric examine
#'
#' examine.numeric is the S3 method of the generic examine() function,
#' used when a single numeric is passed for the effect_size.
#'
#' @param effect_size a numeric, an estimate of the "true" effect size
#' @param standard_error a numeric, standard error of the estimate
#' @param alpha a numeric, the statistical significance threshold
#' @param df a numeric, the degrees of freedom. df=Inf is equivalent
#' to a normal distribution.
#' @param number_simulations a numeric, how many times to simulate when calculating Type M
#' error
#' @return A list of length 3 containing the power, type s, and type M
#' error.
#' @examples
#' examine(1,3.28)
#' examine(2,8.1)
#' examine(.5,1,df=10)
#' @export
examine.numeric <- function(effect_size, standard_error, alpha=.05, df=Inf, number_simulations=10){
  z <- qt(1-alpha/2, df)
  p.hi <- 1 - pt(z-effect_size/standard_error, df)
  p.lo <- pt(-z-effect_size/standard_error, df)
  power <- p.hi + p.lo
  typeS <- ifelse(effect_size >= 0, p.lo/power, 1- (p.lo/power))

  # Error suppressed below is intentional recycling when a vector is passed
  estimate <- suppressWarnings(effect_size + standard_error*rt(number_simulations,df))
  significant <- abs(estimate) > standard_error*z
  exaggeration <- abs(mean(abs(estimate)[significant])/effect_size)
  return(list(power=power, typeS=typeS, exaggeration=exaggeration))
}


