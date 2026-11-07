#cet fonction permet de realiser l'analyse frenquencielle d'un generateur
#dans l'idée de verifier l'uniformité de celle-ci et de poser le premier diagnostique
nexo_hz <- function(data){
  library(ggplot2)
  donnee <- data.frame(data[[1]])
  valeur <- data[[1]]
  couleur="green"
  hz <- table(factor(valeur,levels=0:9))
  p <- chisq.test(hz,p=rep(.1,10))$p.value
  if (p<=0.05){
    #uniformite rejeter
    couleur="red"
     info= "echecs test chi-deux, equiprobabilité impossible"
  }
  else{
    info="test chi-deux reussi, poursuivons !"
  }
  graphe<-ggplot(data=donnee,aes(x=valeur))+geom_bar(fill=couleur)+geom_hline(yintercept = (length(valeur)/10),linetype="dashed",color="red")+labs(title = "vérification de l'equiprobabilité",subtitle = info)
  print(graphe)
  return(list(couleur=couleur,graphe=graphe))
}
#data <- list(
  #matrix(sample(0:9, 100, replace = TRUE), ncol = 1),
  #matrix(sample(0:9, 100, replace = TRUE), ncol = 1)
#)
#nexo_hz(data)
