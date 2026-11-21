
#' Generate Biased Random Digits
#'
#' Generates a sequence of pseudo-random digits between 0 and 9
#' using the built-in linear congruential generator.
#'
#' This generator is intentionally biased and can be used to
#' demonstrate the ability of statistical tests to detect
#' deviations from a uniform distribution.
#'
#' @param n An integer specifying the number of digits to generate.
#'
#' @return An integer vector containing the generated digits.
#'
#' @examples
#' digits <- nexo_aleatoire(1000)
#' head(digits)
#'
#' @seealso [nexo_analyse()], [nexo_hz()], [nexo_serie()]
#'
#' @export

nexo_aleatoire <- function(n){

  a <- 1664525
  c <- 1013904223
  m <- 2^32

  seed <- floor(as.numeric(Sys.time()) * 1000) %% m

  values <- numeric(n)

  for(i in 1:n){

    seed <- (a * seed + c) %% m

    # Introduction volontaire d'un biais
    x <- seed / m

    if(x < 0.80){
      values[i] <- floor(x * 3 / 0.80)
    } else {
      values[i] <- sample(3:9, 1)
    }
  }

  return(values)
}
