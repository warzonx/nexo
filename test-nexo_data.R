test_that("nexo_data produit cinq matrices", {
  data <- nexo_data(runif)

  expect_equal(length(data), 5)
})

test_that("les tailles des matrices sont correctes", {
  data <- nexo_data(runif)

  expect_equal(nrow(data[[1]]), 100)
  expect_equal(nrow(data[[5]]), 1000000)
})

test_that("les données sont numériques", {
  data <- nexo_data(runif)

  expect_true(is.numeric(data[[1]]))
})
