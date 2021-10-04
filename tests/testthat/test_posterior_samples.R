# declarations ------------------------------------------------------------

# the inla objects used
i04M07ctr <- readRDS(test_path("testdata", "fits", "i04M07ctr.rds"))


# tests -------------------------------------------------------------------

test_that("verify inla model object", {
  expect_s3_class(i04M07ctr, "inla")
})

test_that("posterior_samples_tags", {

  tags <- posterior_samples_tags(i04M07ctr)
  # cat("\n")
  # print(tags)
  # cat("\n")

  expect_identical(names(tags), c("Predictor", "weight_c", "(Intercept)"))
  expect_equivalent(tags, c(FALSE, TRUE, FALSE))
})
