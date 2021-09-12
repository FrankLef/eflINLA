library(dplyr)
library(modelr)


# declarations ------------------------------------------------------------

# data file used
load(file = test_path("testdata", "rethinking", "Howell1.rda"))
the_data <- Howell1 %>%
  dplyr::filter(age  >= 18) %>%
  dplyr::mutate(weight_c = as.vector(scale(weight, center = TRUE, scale = FALSE)))

# arguments to inla model object
the_args <-
  list(formula = height ~ f(weight_c, model = "clinear", range = c(0, Inf),
                            prior = "logtnormal", param = c(0, 1)),
       family = "gaussian",
       control.family = list(
         hyper = list(prec = list(prior = "loggamma", param = c(1, 0.00005)))),
       control.compute = list(config = TRUE, dic = TRUE, waic = TRUE),
       quantiles = c(0.025, 0.5, 0.975),
       data = the_data
       )

# the newdata to predict
the_newdata <- the_data %>%
  select(height, weight_c) %>%
  bind_rows(data.frame(height = NA_real_,
                       weight_c = seq_range(x = the_data$weight_c, n = 20L)))
# positions of dependent variable to predict
the_pos <- which(is.na(the_newdata$height))

the_args_new <- the_args
the_args_new$data <- the_newdata
the_args_new$control.predictor <- list(compute = TRUE)

the_inla_new <- do.call(INLA::inla, the_args_new)

# tests -------------------------------------------------------------------

test_that("verify new inla model object", {
  expect_s3_class(the_inla_new, "inla")
})

test_that("predict_inla: input check", {

  rgx <- "Must inherit from class"
  expect_error(predict_inla(object = 0L), regexp = rgx)
  rgx <- "Must be of type \'integerish\'"
  expect_error(predict_inla(object = the_inla_new, pos = letters[1:3]),
               regexp = rgx)
  rgx <- "Must be >= 1"
  expect_error(predict_inla(object = the_inla_new, pos = the_pos, n = 0),
               regexp = rgx)
})

test_that("predict_inla_extract", {
  nsamples <- 5L

  # get the samples
  the_samples <- INLA::inla.posterior.sample(n = nsamples, result = the_inla_new)
  expect_length(the_samples, nsamples)

  # test the extraction
  out <- predict_inla_extract(samples = the_samples, pos = the_pos)
  expect_length(out, 2L)
  expect_identical(names(out), c("sigmas", "predictors"))
})


test_that("predict_inla_sim", {
  nsamples <- 5L

  # get the samples
  the_samples <- INLA::inla.posterior.sample(n = nsamples, result = the_inla_new)
  expect_length(the_samples, nsamples)

  # get the extraction
  the_extract <- predict_inla_extract(samples = the_samples, pos = the_pos)
  expect_length(the_extract, 2L)
  expect_identical(names(the_extract), c("sigmas", "predictors"))

  # test the simulation
  out <- predict_inla_sim(sim_data = the_extract)
  expect_identical(dim(out), c(nsamples, length(the_extract$predictors[[1]])))
})


test_that("predict_inla", {
  nsamples <- 5L

  out <- predict_inla(object = the_inla_new, pos = the_pos, n = nsamples)
  expect_identical(dim(out), c(nsamples, length(the_pos)))
})
