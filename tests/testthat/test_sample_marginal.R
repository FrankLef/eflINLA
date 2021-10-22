# declarations ------------------------------------------------------------
library(posterior, quietly = TRUE)


# the inla objects used
i04M07ctr <- readRDS(test_path("testdata", "fits", "i04M07ctr.rds"))


# tests -------------------------------------------------------------------

test_that("verify inla model object", {
  expect_s3_class(i04M07ctr, "inla")
})

test_that("extract_marginals_hyper", {

  marg <- extract_marginals_hyper(i04M07ctr)
  # cat("\n")
  # str(marg)
  # cat("\n")


  expect_type(marg, "list")
  expect_length(marg, 2)
  nm <- c("Beta for weight_c", "SD for the Gaussian observations")
  expect_identical(names(marg), nm)
})

test_that("extract_marginals", {

  marg <- extract_marginals(i04M07ctr)
  # cat("\n", str(marg), "\n")

  expect_type(marg, "list")
  expect_length(marg, 3)
  nm <- c("b_Intercept","b_weight_c", "sigma")
  expect_identical(names(marg), nm)
})

test_that("tidy_marg_draws_inla", {

  samples <- tidy_marg_draws_inla(i04M07ctr)
  # cat("\n")
  # print(samples)
  # print(nvariables(samples))
  # cat("\n")
  # cat("\n", print(nrow(i04M07ctr$marginals.fixed[[1]])), "\n")


  expect_s3_class(samples, "draws_rvars")
  nm <- c("b_Intercept","b_weight_c", "sigma")
  expect_identical(variables(samples), nm)
  expect_equal(niterations(samples), nrow(i04M07ctr$marginals.fixed[[1]]))
})
