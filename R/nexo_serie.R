
#' Chi-Square Test for Successive Digit Pair Uniformity
#'
#' Evaluates the distribution of successive digit pairs generated
#' by a random number generator.
#'
#' Unlike [nexo_hz()], which examines individual digit frequencies,
#' this function examines pairs of consecutive digits.
#'
#' A frequency matrix is constructed to record the observed
#' occurrences of all possible pairs of digits from 0 to 9.
#' The observed pair frequencies are then used to assess
#' uniformity using the chi-square goodness-of-fit test.
#'
#' @param data A dataset containing the generated digits.
#'
#' @return A list containing the observed pair-frequency matrix
#' and the statistical and graphical analysis results.
#'
#' @seealso [nexo_hz()], [nexo_analyse()]
#'
#' @export

nexo_serie <- function(data){
  library(ggplot2)
  #mes parametres
  n<- length(data[[5]])
  donnee <- data[[5]]
  dim(donnee) <- c(1,n)
  x_1=donnee[-n]
  x_2=donnee[-1]
  #'c'est ici que se trouve matrice de paire
  paires<- rbind(x_1,x_2)
  #'c'st dans effectif que je grade les frequences de mes couples
  effectifs <- table(
    factor(paires[1, ], levels = 0:9),
    factor(paires[2, ], levels = 0:9)
  )
  n <- sum(effectifs)
  E <- n / 100
  chi2 <- sum((effectifs - E)^2 / E)
  ddl <- 99# degre de liberte

  p_value <- pchisq(chi2, df = ddl, lower.tail = FALSE)
  frequence <- as.data.frame(effectifs)

  colnames(frequence) <- c("Premier", "Second", "Effectif")

  frequence$Paire <- paste0(
    frequence$Premier,
    frequence$Second
  )
  if (p_value<0.05){
    info <- "rejet de l'hypothese d'uniformité des paires"
  }
  else{
    info <- "pas de preuve suffisante contre l'uniformité des paires"
  }

  graphe <- ggplot(frequence, aes(x = Paire, y = Effectif)) +
    geom_col(fill = "steelblue") +
    geom_hline(
      yintercept = E,
      color = "red",
      linetype = "dashed"
    ) +
    labs(
      title = "Fréquence des paires successives",
      x = "Paires",
      y = "Effectifs observés"
    )+labs(subtitle =info) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1)
    )
  return(list(graphe=graphe,valeur=p_value))
}
#nexo_aleatoire(10)

