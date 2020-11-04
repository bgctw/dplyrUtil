context("mutate_cond")

.tmp.f <- function(){
  library(testthat)
}

library(dplyr)
library(purrr)

ds <- tibble(
  food = c("banana","strawberry","pear","bread","corn flakes")
  , kind = factor(c(rep("fruit",3), rep("cereal",2)))
) %>%
  group_by(kind)

FUN = ~mutate(., countInKind = paste(first(kind),1:n()))
ans0 <- ds %>% split(group_indices(.)) %>% map_dfr(FUN)

test_that("mapGroups default",{
  ans <- ds %>% mapGroups(FUN, .omitWarning = TRUE)
  expect_equal(ans, ans0)
})

test_that("mapGroups warning",{
  expect_warning(
    ans <- ds %>% mapGroups(FUN)
  )
  expect_equal(ans, ans0)
})

test_that("mapGroups with no groupings",{
  ans <- ds %>% ungroup() %>% mapGroups(FUN, .omitWarning = TRUE)
  expect_equal(names(ans), names(ans0))
  expect_equal(select(ans, 1:2), ungroup(ds))
  expect_equal(ans$countInKind, paste(ans$kind[1], seq_len(nrow(ans))))
  #ansExp <- ungroup(ds) %>% as_mapper(FUN)()
  #expect_equal(ans, ansExp)
})

test_that("mapGroups with several groupings",{
  ans <- ds %>% group_by(kind, food) %>% mapGroups(FUN, .omitWarning = TRUE)
  expect_equal(names(ans), names(ans0))
  expect_equal(ans$countInKind, paste(ans$kind, "1"))
})

