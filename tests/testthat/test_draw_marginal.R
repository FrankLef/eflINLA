# declarations ------------------------------------------------------------
library(posterior, quietly = TRUE)



# the inla objects used
i04M07ctr <- readRDS(test_path("testdata", "fits", "i04M07ctr.rds"))


# tests -------------------------------------------------------------------

test_that("verify inla model object", {
  expect_s3_class(i04M07ctr, "inla")
})

test_that("transform_marginal_hyper", {

  marg <- transform_marginal_hyper(i04M07ctr)


  expect_type(marg, "list")
  expect_length(marg, 2)
  nm <- c("SD for the Gaussian observations", "Beta for weight_c")
  expect_identical(names(marg), nm)
})

test_that("extract_marginal", {

  marg <- extract_marginal(i04M07ctr)
  # cat("\n", str(marg), "\n")

  expect_type(marg, "list")
  expect_length(marg, 3)
  nm <- c("Intercept", "Sigma","b_weight_c")
  expect_identical(names(marg), nm)
})

test_that("draw_marginal", {

  samples <- draw_marginal(i04M07ctr)
  cat("\n")
  # print(samples)
  # print(nvariables(samples))
  # cat("\n")
  # cat("\n", print(nrow(i04M07ctr$marginals.fixed[[1]])), "\n")


  expect_s3_class(samples, "draws_rvars")
  nm <- c("Intercept", "Sigma","b_weight_c")
  expect_identical(variables(samples), nm)
  expect_equal(niterations(samples), nrow(i04M07ctr$marginals.fixed[[1]]))
})
