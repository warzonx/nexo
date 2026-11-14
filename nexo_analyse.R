#' Analyse statistique d'un générateur pseudo-aléatoire
#'
#' Cette fonction vérifie que l'argument fourni est bien une fonction,
#' génère les données nécessaires et lance plusieurs tests statistiques
#' afin d'étudier le comportement du générateur.
#'
#' @param generator Fonction génératrice acceptant un seul argument :
#' le nombre de valeurs à produire.
#' @return Une liste contenant les graphiques des analyses
#' d'équiprobabilité individuelle et sérielle.
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
