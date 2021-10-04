# declarations ------------------------------------------------------------

# the inla objects used
i04M07ctr <- readRDS(test_path("testdata", "fits", "i04M07ctr.rds"))


# tests -------------------------------------------------------------------

test_that("verify inla model object", {
  expect_s3_class(i04M07ctr, "inla")
})

test_that("transform_marginal_hyper", {

  marg <- eflINLA::transform_marginal_hyper(i04M07ctr, sd_name = "sd")


  expect_type(marg, "list")
  expect_length(marg, 2)
  nm <- c("Beta for weight_c", "sd for the Gaussian observations")
  expect_identical(names(marg), nm)
})

test_that("posterior_marginals: is.df = FALSE", {

  marg <- eflINLA::posterior_marginals(i04M07ctr, is.df = FALSE)
  # cat("\n", str(marg), "\n")

  expect_type(marg, "list")
  expect_length(marg, 3)
  nm <- c("(Intercept)","Beta for weight_c", "sd for the Gaussian observations")
  expect_identical(names(marg), nm)
})

test_that("posterior_marginals: is.df = TRUE", {

  marg <- eflINLA::posterior_marginals(i04M07ctr, is.df = TRUE)
  # cat("\n")
  # str(marg)
  # cat("\n")

  expect_s3_class(marg, "data.frame")
  expect_identical(names(marg), c("var", "x", "y"))
  expect_identical(dim(marg), c(225L, 3L))
})
