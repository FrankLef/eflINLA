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
the_inla_aug$inla$.args$control.compute

# tests -------------------------------------------------------------------

test_that("verify inla model object", {
  expect_s3_class(the_inla, "inla")
  expect_s3_class(the_newdata, "data.frame")
  expect_type(the_inla_aug, "list")
  expect_identical(names(the_inla_aug), c("inla", "new_pos"))
  expect_s3_class(the_inla_aug$inla, "inla")
  expect_type(the_inla_aug$new_pos, "integer")
})


test_that("predicted_draws_inla", {

  nsamples <- 10L

  # get the samples in list format
  samples <- predicted_draws_inla(the_inla_aug$inla, n = nsamples)
  # cat("\n")
  # print(samples)
  # cat("\n")

  # skip("manual")
  expect_s3_class(samples, "draws_rvars")
  # skip("manual")
  expect_equal(nvariables(samples), nrow(the_inla_aug$inla$.args$data))
  expect_equal(niterations(samples), nsamples)
})
