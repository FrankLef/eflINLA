library(dplyr)


# declarations ------------------------------------------------------------

# the inla objects used
i04M07ctr <- readRDS(test_path("testdata", "fits", "i04M07ctr.rds"))


# tests -------------------------------------------------------------------

test_that("verify the inla model object", {
  expect_s3_class(i04M07ctr, "inla")
})

test_that("i04M07ctr: transform_hyper_marginal", {
  margs <- transform_hyper_marginal(i04M07ctr)
  # cat("\n", str(margs), "\n")

  # skip("manual")
  expect_length(margs, 2)
})

test_that("i04M07ctr: write_hyper_summary", {
  margs <- transform_hyper_marginal(i04M07ctr)
  summ <- write_hyper_summary(margs)
  # cat("\n", str(summ), "\n")

  # skip("manual")
  expect_length(summ, 6)
})

test_that("i04M07ctr: posterior_summary", {
  summ <- posterior_summary(i04M07ctr)
  # cat("\n")
  # print(summ)
  # cat("\n")

  # skip("manual")
  expect_s3_class(summ, "data.frame")
  expect_identical(dim(summ), c(3L, 6L))
})
