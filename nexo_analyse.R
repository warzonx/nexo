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
  stop("l'argument generator n'est pas une fonction,essayer ceci:nexo_analyse(runif)")
}
#c'est ici que je genere les données de toute mon analyse
  data <- nexo_data(generator)
  #premier analyse l'equiprobabilité des élèments
  print("1-voyons si vous passer le test de chi-deux")
  result_hz <- nexo_hz(data)
  #second analyse l'equiprobabilté des paires sucessifs
  print("2-voyons ensembles si les paire succesif de tes donnée sont equiprobables")
  result_serie <- nexo_serie(data)

  if (result_hz$couleur == "red" || result_serie$valeur < 0.05){
    print("votre générateur est biaisé")
  } else {
    print("aucune preuve que votre générateur soit biaisé")
  }
  return(list(hz = result_hz$graphe, serie = result_serie$graphe))
}

