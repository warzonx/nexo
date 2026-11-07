#'cet fonction permet de generer les données necessaire a l'analyse des generateurs
nexo_data <- function(generator){
  n<- c(100,1000,10000,100000,1000000)
  data <- list()#elle stocker les données dans une liste contenant des matrices
  for (i in 1:length(n)){
    values=generator(n[i])
    data[[i]] <- matrix(values,ncol=1)
  }
  return(data)
}
# je test nexo_data(runif)

