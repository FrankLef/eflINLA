# declarations ------------------------------------------------------------
library(posterior, quietly = TRUE)


# the inla objects used
i04M07ctr <- readRDS(test_path("testdata", "fits", "i04M07ctr.rds"))

the_newdata <- data.frame(
  weight_c = modelr::seq_range(i04M07ctr$.args$data$weight_c, n = 10)
)

i04M07ctr_aug <- eflINLA::augment_inla(i04M07ctr, newdata = the_newdata)

# tests -------------------------------------------------------------------

test_that("verify inla model object", {
  expect_s3_class(i04M07ctr, "inla")
  expect_type(i04M07ctr_aug, "list")
  expect_identical(names(i04M07ctr_aug), c("inla", "new_pos"))
  expect_s3_class(i04M07ctr_aug$inla, "inla")
  expect_type(i04M07ctr_aug$new_pos, "integer")
  # cat("\n")
  # print(class(i04M07ctr_aug))
  # cat("\n")
})

test_that("linpred_marg_draws_inla", {
  nsamples <- 20L

  # i04M07ctr_aug$new_pos
  out <- linpred_marg_draws_inla(.result = i04M07ctr_aug$inla,
                                 new_pos = i04M07ctr_aug$new_pos,
                                 n = nsamples)
  # cat("\n")
  # print(names(out)[1])
  # cat("\n")

  expect_s3_class(out, "draws_rvars")
  nm <- paste("Predictor", i04M07ctr_aug$new_pos, sep = ".")
  expect_identical(variables(out), nm)
  expect_equal(niterations(out), nsamples)

})


test_that("epred_marg_draws_inla", {
  nsamples <- 25L

  # i04M07ctr_aug$new_pos
  out <- epred_marg_draws_inla(.result = i04M07ctr_aug$inla,
                                 new_pos = i04M07ctr_aug$new_pos,
                                 n = nsamples)
  # cat("\n")
  # print(names(out)[1])
  # cat("\n")

  expect_s3_class(out, "draws_rvars")
  nm <- paste("fitted.Predictor", i04M07ctr_aug$new_pos, sep = ".")
  expect_identical(variables(out), nm)
  expect_equal(niterations(out), nsamples)
})
