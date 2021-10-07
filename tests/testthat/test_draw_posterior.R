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

test_that("posterior_samples_list", {
  nsamples <- 2L

  sel <- sel <- posterior_samples_sel(i04M07ctr)

  samples <- INLA::inla.posterior.sample(n = nsamples,
                                         result = i04M07ctr,
                                         selection = sel)

  # get the samples in list format
  post <- extract_posterior_samples(samples, type = "post", sel)
  # cat("\n")
  # str(post)
  # cat("\n")

  # skip("manual")
  expect_identical(dim(post), c(2L, 3L))
})

test_that("draw_posterior: type = post", {
  nsamples <- 2L

  # get the samples in list format
  samples <- draw_posterior(i04M07ctr, n = nsamples, type = "post")
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


test_that("draw_posterior: type = fit", {

  nsamples <- 2L

  # get the samples in list format
  samples <- draw_posterior(i04M07ctr, n = nsamples, type = "fit")
  # cat("\n")
  # print(samples)
  # cat("\n")

  # skip("manual")
  expect_s3_class(samples, "draws_rvars")
  # skip("manual")
  expect_equal(nvariables(samples), nrow(i04M07ctr$.args$data))
  expect_equal(niterations(samples), nsamples)
})

test_that("draw_posterior: type = pred", {

  nsamples <- 2L

  # get the samples in list format
  samples <- draw_posterior(i04M07ctr, n = nsamples, type = "pred")
  # cat("\n")
  # print(samples)
  # cat("\n")

  # skip("manual")
  expect_s3_class(samples, "draws_rvars")
  # skip("manual")
  expect_equal(nvariables(samples), nrow(i04M07ctr$.args$data) + 1)
  expect_equal(niterations(samples), nsamples)
})
