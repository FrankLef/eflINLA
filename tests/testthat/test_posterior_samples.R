# declarations ------------------------------------------------------------

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
  lst <- posterior_samples_list(samples, type = "post", sel)
  # cat("\n")
  # str(lst)
  # cat("\n")

  # skip("manual")
  expect_length(lst, nsamples)
})

test_that("posterior_samples: type = post", {
  nsamples <- 2L

  # get the samples in list format
  samples <- posterior_samples(i04M07ctr, n = nsamples, type = "post")
  # cat("\n")
  # str(samples)
  # cat("\n")
  expect_s3_class(samples, "data.frame")
  expect_identical(dim(samples), c(2L, 3L))
  nm <- c("(Intercept)", "SD for the Gaussian observations",
          "Beta for weight_c")
  # skip("manual")
  expect_identical(names(samples), nm)
})


test_that("posterior_samples: type = fit", {

  nsamples <- 2L

  # get the samples in list format
  samples <- posterior_samples(i04M07ctr, n = nsamples, type = "fit")
  # cat("\n")
  # print(samples)
  # cat("\n")

  # skip("manual")
  expect_s3_class(samples, "data.frame")
  # skip("manual")
  expect_identical(dim(samples), c(nsamples, nrow(i04M07ctr$.args$data)))
})

test_that("posterior_samples: type = pred", {

  nsamples <- 2L

  # get the samples in list format
  samples <- posterior_samples(i04M07ctr, n = nsamples, type = "pred")
  # cat("\n")
  # print(samples)
  # cat("\n")

  # skip("manual")
  expect_s3_class(samples, "data.frame")
  # skip("manual")
  expect_identical(dim(samples), c(nsamples, nrow(i04M07ctr$.args$data) + 1L))
})
