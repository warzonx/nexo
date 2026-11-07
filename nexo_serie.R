#'contrairement a nexo_hz , ici nous ne recherchons pas d'equiprobabilité,
#'individuelle nous observons les paires des chiffres produits et verifions
#leurs equiprobabilité
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


