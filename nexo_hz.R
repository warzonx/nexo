
#' Chi-Square Test for Individual Digit Uniformity
#'
#' Evaluates whether the digits from 0 to 9 follow a uniform
#' distribution using the chi-square goodness-of-fit test.
#'
#' The observed frequencies of individual digits are compared
#' with their expected frequencies under the uniformity hypothesis.
#'
#' A significance level of 0.05 is used to assess the statistical
#' evidence against the uniform distribution.
#'
#' @param data A dataset containing the generated digits.
#'
#' @return A list containing the statistical analysis results
#' and the graphical representation of the test.
#'
#' @seealso [nexo_serie()], [nexo_analyse()]
#'
#' @export

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
