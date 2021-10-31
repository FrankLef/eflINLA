# declarations ------------------------------------------------------------
library(dplyr)
library(posterior, quietly = TRUE)
library(INLA)

a_file <- test_path("testdata", "summaries_sim3.rds")
sim3 <- readRDS(a_file)

the_inla <- sim3$inla

the_newdata <- data.frame(
  x_normal = modelr::seq_range(sim3$data$x_normal, n = 20),
  x_explog = modelr::seq_range(sim3$data$x_explog, n = 20)
)

the_inla_aug <- augment_inla(the_inla, newdata = the_newdata)


# tests -------------------------------------------------------------------

test_that("verify inla model object", {
  expect_s3_class(the_inla, "inla")
  expect_s3_class(the_newdata, "data.frame")
  expect_type(the_inla_aug, "list")
  expect_identical(names(the_inla_aug), c("inla", "new_pos"))
  expect_s3_class(the_inla_aug$inla, "inla")
  expect_type(the_inla_aug$new_pos, "integer")

})

test_that("linpred_marg_draws_inla", {
  nsamples <- 20L

  out <- linpred_marg_draws_inla(.result = the_inla_aug$inla,
                                 new_pos = the_inla_aug$new_pos,
                                 n = nsamples)
  # cat("\n")
  # print(names(out)[1])
  # cat("\n")

  expect_s3_class(out, "draws_rvars")
  nm <- paste("Predictor", the_inla_aug$new_pos, sep = ".")
  expect_identical(variables(out), nm)
  expect_equal(niterations(out), nsamples)

})


test_that("epred_marg_draws_inla", {
  nsamples <- 25L

  # the_inla_aug$new_pos
  out <- epred_marg_draws_inla(.result = the_inla_aug$inla,
                               new_pos = the_inla_aug$new_pos,
                               n = nsamples)
  # cat("\n")
  # print(names(out)[1])
  # cat("\n")

  expect_s3_class(out, "draws_rvars")
  nm <- paste("fitted.Predictor", the_inla_aug$new_pos, sep = ".")
  expect_identical(variables(out), nm)
  expect_equal(niterations(out), nsamples)
})
