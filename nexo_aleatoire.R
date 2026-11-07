#' Générateur pseudo-aléatoire interne de NEXO
#'
#' Ce générateur produit des chiffres entiers compris entre 0 et 9.
#' Il présente volontairement un biais statistique afin de permettre
#' de tester la capacité d'OSLO à détecter les générateurs biaisés.
#'
#' @param n Nombre de valeurs à générer.
#' @return Un vecteur numérique contenant n chiffres.
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
