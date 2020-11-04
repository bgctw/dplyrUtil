context("expandAllInconsistentFactorLevels")

.tmp.f <- function(){
  require(dplyr)
  require(testthat)
}

test_that("two data.frames",{
  df1 <- data.frame(f = factor(c("D","D","C")))
  df2 <- data.frame(f = factor(c("C","C","A"))
                    , desc = c("forC1","forC2","forA1")
                    ,stringsAsFactors = FALSE)
  res <- expandAllInconsistentFactorLevels(df1,df2 , .noWarningCols = "f")
  expect_true(is.list(res))
  expect_true(length(res) == 2)
  expect_equal(levels(res[[1]]$f), levels(res[[1]]$f))
  resb <- res[[1]] %>% left_join(res[[2]], by = "f")
  expect_true(is.factor(resb$f))
  expect_equal(resb$desc, c(NA,NA,"forC1","forC2"))
  #
  resL <- expandAllInconsistentFactorLevels(list(df1,df2) , .noWarningCols = "f")
  expect_equal(resL, res)
})

test_that("warning on relevelling",{
  df1 <- data.frame(f = factor(c("D","D","C")))
  df2 <- data.frame(f = factor(c("C","C","A"))
                    , desc = c("forC1","forC2","forA1"))
  expect_warning(res <- expandAllInconsistentFactorLevels(df1,df2))
  expect_true(is.list(res))
  expect_true(length(res) == 2)
  expect_equal(levels(res[[1]]$f), levels(res[[2]]$f))
  expect_equal(res[[2]]$desc, df2$desc)
})

test_that("three data.frames",{
  df1 <- data.frame(f = factor(c("D","D","C")))
  df2 <- data.frame(f = factor(c("C","C","A"))
                    , desc = c("forC1","forC2","forA1"))
  df3 <- data.frame(f = factor(c("C","E")))
  res <- expandAllInconsistentFactorLevels(df1,df2,df3 , .noWarningCols = "f")
  expect_true(is.list(res))
  expect_true(length(res) == 3)
  expect_equal(levels(res[[1]]$f), levels(res[[2]]$f))
  expect_equal(levels(res[[1]]$f), levels(res[[3]]$f))
  #
  resL <- expandAllInconsistentFactorLevels(list(df1,df2,df3) , .noWarningCols = "f")
  expect_equal(resL, res)
})

test_that("one data.frames",{
  df1 <- data.frame(f = factor(c("D","D","C")))
  res <- expandAllInconsistentFactorLevels(df1, .noWarningCols = "f")
  expect_true(is.list(res))
  expect_true(length(res) == 1)
  expect_equal(res[[1]], df1)
  #
  resL <- expandAllInconsistentFactorLevels(list(df1) , .noWarningCols = "f")
  expect_equal(resL, res)
})

test_that("no arguments",{
  res <- expandAllInconsistentFactorLevels()
  expect_equal(res, list())
})

test_that("empty list",{
  res <- expandAllInconsistentFactorLevels(list())
  expect_equal(res, list())
})


