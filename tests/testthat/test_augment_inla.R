# declarations ------------------------------------------------------------
library(dplyr)
library(INLA)

a_file <- test_path("testdata", "summaries_sim3.rds")
sim3 <- readRDS(a_file)

the_inla <- sim3$inla

the_newdata <- data.frame(
  x_normal = modelr::seq_range(sim3$data$x_normal, n = 20),
  x_explog = modelr::seq_range(sim3$data$x_explog, n = 20)
)


# tests -------------------------------------------------------------------

test_that("verify test objects", {
  expect_s3_class(the_inla, "inla")
  expect_s3_class(the_newdata, "data.frame")
})

test_that("create_newdata_inla: Input errors.", {
  # dataframe without the rows
  df <- runif(n = 10)
  rgx <- "Assertion on 'newdata' failed"
  expect_error(create_newdata_inla(the_inla, newdata = df), regexp = rgx)

  # the names are not a subset of the original names
  df <- the_newdata
  names(df) <- c("WRONG1", "WRONG2")
  rgx <- "Assertion on 'names[(]newdata[)]' failed"
  expect_error(create_newdata_inla(the_inla, newdata = df), regexp = rgx)
})


test_that("create_newdata_inla: Empty newdata.", {
  # dataframe without the rows
  df <- the_inla$.args$data[FALSE, ]
  out <- create_newdata_inla(the_inla, newdata = df)
  expect_identical(out, list("data" = the_inla$.args$data,
                             "new_pos" = integer()))
})



test_that("create_newdata_inla", {

  out <- create_newdata_inla(the_inla, newdata = the_newdata)

  expect_type(out, type = "list")
  expect_identical(names(out), c("data", "new_pos"))

  # check the vector of new position
  the_pos <- (nrow(the_inla$.args$data) + 1):(nrow(the_inla$.args$data) +
                                                 nrow(the_newdata))
  expect_identical(out$new_pos, the_pos)


  # check the inla data
  expect_s3_class(out$data, class = "data.frame")
  # the names must be the sae as the original data
  expect_identical(names(out$data), names(the_inla$.args$data))
  the_dim <- c(nrow(the_inla$.args$data) + nrow(the_newdata),
               length(the_inla$.args$data))
  # nb of rows must be increased by new rows
  expect_identical(dim(out$data), the_dim)
})


test_that("augment_inla: Input errors.", {
  # dataframe without the rows
  df <- runif(n = 10)
  rgx <- "Assertion on 'newdata' failed"
  expect_error(augment_inla(the_inla, newdata = df), regexp = rgx)

  # the names are not a subset of the original names
  df <- the_newdata
  names(df) <- c("WRONG1", "WRONG2")
  rgx <- "Assertion on 'names[(]newdata[)]' failed"
  expect_error(augment_inla(the_inla, newdata = df), regexp = rgx)
})

test_that("augment_inla: Empty newdata.", {
  # dataframe without the rows
  df <- the_inla$.args$data[FALSE, ]
  out <- augment_inla(the_inla, newdata = df)
  expect_identical(out, list("inla" = the_inla, "new_pos" = integer()))
})


test_that("augment_inla", {

  out <- augment_inla(the_inla, newdata = the_newdata)

  expect_type(out, type = "list")
  expect_identical(names(out), c("inla", "new_pos"))

  # check the vector of new position
  the_pos <- (nrow(the_inla$.args$data) + 1):(nrow(the_inla$.args$data) +
                                                 nrow(the_newdata))
  expect_identical(out$new_pos, the_pos)


  # check the inla class
  result <- out$inla
  expect_s3_class(result, class = "inla")
  # the names must be the same as the original data
  expect_identical(names(result$.args$data), names(the_inla$.args$data))
  the_dim <- c(nrow(the_inla$.args$data) + nrow(the_newdata),
               length(the_inla$.args$data))
  # nb of rows must be increased by new rows
  expect_identical(dim(result$.args$data), the_dim)

  # verify that the inla args are good
  the_args <- out$inla$.args
  # cat("\n")
  # print(the_args$offset)
  # cat("\n")
  expect_true(the_args$control.predictor$compute)
  expect_true(all(the_args$offset == 0))
  expect_true(the_args$control.compute$config)
})
