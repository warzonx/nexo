
#' Generate Datasets for Statistical Analysis
#'
#' Generates datasets using a user-provided random number generator.
#' The function prepares samples of different sizes for subsequent
#' statistical analysis.
#'
#' The generator is supplied as a function, allowing users to test
#' different random number generators with the nexo package.
#'
#' @param generator A function used to generate random digits.
#' The function must accept a sample size as its argument.
#'
#' @return A list containing the generated datasets for the
#' different sample sizes.
#'
#' @seealso [nexo_aleatoire()], [nexo_analyse()]
#'
#' @export

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

