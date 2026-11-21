#' Run a Complete Random Number Generator Analysis
#'
#' Performs a complete statistical analysis of a random number
#' generator by combining individual digit uniformity and
#' successive digit pair uniformity tests.
#'
#' The function orchestrates the analysis by:
#' \enumerate{
#'   \item Generating datasets using the supplied generator.
#'   \item Evaluating individual digit uniformity with [nexo_hz()].
#'   \item Evaluating successive pair uniformity with [nexo_serie()].
#' }
#'
#' @param generator A function used to generate random digits.
#'
#' @return A list containing the graphical results of both tests:
#' \describe{
#'   \item{hz}{Results of the individual digit uniformity test.}
#'   \item{serie}{Results of the successive pair uniformity test.}
#' }
#'
#' @examples
#' result <- nexo_analyse(nexo_aleatoire)
#' print(result$hz)
#' print(result$serie)
#'
#' @seealso [nexo_data()], [nexo_hz()], [nexo_serie()]
#'
#' @export

nexo_analyse <- function(generator){

  if (!is.function(generator)){
    stop("L'argument generator doit être une fonction. Exemple : nexo_analyse(runif)")
  }

  # Génération des données nécessaires à l'ensemble des analyses
  data <- nexo_data(generator)

  # Premier test : équiprobabilité des éléments
  print("1 - Vérifions l'équiprobabilité des valeurs avec le test du chi-deux")

  result_hz <- nexo_hz(data)

  if (result_hz$couleur == "red"){
    print("Test 1 échoué : votre générateur présente un écart significatif.")
  } else {
    print("Test 1 réussi : aucune anomalie détectée.")
  }

  # Deuxième test : équiprobabilité des paires successives
  print("2 - Vérifions l'équiprobabilité des paires successives")

  result_serie <- nexo_serie(data)

  # Bilan des analyses
  if (result_hz$couleur == "red" || result_serie$valeur < 0.05){
    print("Conclusion : des indices de biais ont été détectés.")
  } else {
    print("Conclusion : aucune preuve suffisante de biais n'a été détectée.")
  }

  # Retour des graphiques
  return(list(
    hz = result_hz$graphe,
    serie = result_serie$graphe
  ))
}


