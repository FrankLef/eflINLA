# declarations ------------------------------------------------------------
library(posterior, quietly = TRUE)


# the inla objects used
i04M07ctr <- readRDS(test_path("testdata", "fits", "i04M07ctr.rds"))


# tests -------------------------------------------------------------------

test_that("verify inla model object", {
  expect_s3_class(i04M07ctr, "inla")
})

test_that("posterior_samples_sel", {

  sel <- posterior_samples_sel(i04M07ctr)
  # cat("\n")
  # print(sel)
  # cat("\n")

  expect_identical(sel, list("(Intercept)" = 0L))
})


test_that("tidy_draws_inla", {
  nsamples <- 2L

  # get the samples in list format
  samples <- tidy_draws_inla(i04M07ctr, n = nsamples)
  # cat("\n")
  # str(samples)
  # cat("\n")
  # skip("manual")
  expect_s3_class(samples, "draws_rvars")

  # skip("manual")
  # nm <- c("(Intercept)", "SD for the Gaussian observations",
  #         "Beta for weight_c")
  nm <- c("Intercept", "Sigma", "b_weight_c")
  expect_identical(variables(samples), nm)
  expect_equal(niterations(samples), nsamples)
})
