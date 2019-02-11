#require(testthat)
context("mutate_cond")

require(dplyr)
require(rlang)

test_that("column name as symbol",{
  ans <- iris %>%
    mutate_cond(
      Species == "setosa"
      , Petal.Length = 1.0
    )
  expect_true(all(filter(ans, Species == "setosa")$Petal.Length == 1))
  expect_true(all(filter(ans, Species != "setosa")$Petal.Length == 
                    filter(iris, Species != "setosa")$Petal.Length))
})

test_that("use of .data argument",{
  ans <- iris %>%
    mutate_cond(
      .data$Species == "setosa"
      , Petal.Length = 1.0
    )
  expect_true(all(filter(ans, Species == "setosa")$Petal.Length == 1))
  expect_true(all(filter(ans, Species != "setosa")$Petal.Length == 
                    filter(iris, Species != "setosa")$Petal.Length))
})

test_that("NA in condition",{
  irisNA <- iris
  irisNA$Species[3] <- NA
  expect_error(
    ans <- irisNA %>%
      mutate_cond(
        .data$Species == "setosa"
        , Petal.Length = 10.0
      )
    ,info = "missing values"
  )
  ans <- irisNA %>%
    mutate_cond(
      .data$Species == "setosa"
      , Petal.Length = 10.0
      , na.rmCond = TRUE
      
    )
  expect_true(all(filter(ans, Species == "setosa")$Petal.Length == 10.0))
  expect_true(all(filter(ans, Species != "setosa")$Petal.Length == 
                    filter(iris, Species != "setosa")$Petal.Length))
})

test_that("grouped data",{
  # condition depends on the subset of each group
  # Sepal.Length occurs both as symbol and as .data$Sepal.Length
  ans <- iris %>% group_by(!!sym("Species")) %>% 
    mutate_cond(
      Sepal.Length >= mean(.data$Sepal.Length)
      , Petal.Length = 10.0 # larger than maximum
    )
  tmp <- ans %>% group_by(!!sym("Species")) %>% mapGroups(function(dss){
    expect_true(all(filter(dss, Sepal.Length >= mean(Sepal.Length))$Petal.Length == 10))
    expect_true(all(filter(dss, Sepal.Length < mean(Sepal.Length))$Petal.Length != 10))
    data.frame()
  })
})


