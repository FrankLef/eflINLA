# declarations ------------------------------------------------------------

library(posterior, quietly = TRUE)

# the inla objects used
i04M07ctr <- readRDS(test_path("testdata", "fits", "i04M07ctr.rds"))


# tests -------------------------------------------------------------------


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
