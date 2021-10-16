# declarations ------------------------------------------------------------
library(posterior, quietly = TRUE)


# the inla objects used
i04M07ctr <- readRDS(test_path("testdata", "fits", "i04M07ctr.rds"))


# tests -------------------------------------------------------------------

test_that("verify inla model object", {
  expect_s3_class(i04M07ctr, "inla")
})


test_that("predict_inla", {

  nsamples <- 10L

  # get the samples in list format
  # samples <- draw_posterior(i04M07ctr, n = nsamples, type = "pred")
  samples <- predict_inla(i04M07ctr, n = nsamples)
  # cat("\n")
  # print(samples)
  # cat("\n")

  # skip("manual")
  expect_s3_class(samples, "draws_rvars")
  # skip("manual")
  expect_equal(nvariables(samples), nrow(i04M07ctr$.args$data))
  expect_equal(niterations(samples), nsamples)
})
