# declarations ------------------------------------------------------------

# the inla objects used
i04M07ctr <- readRDS(test_path("testdata", "fits", "i04M07ctr.rds"))

the_newdata <- data.frame(
  height = NA_real_,
  weight_c = modelr::seq_range(i04M07ctr$.args$data$weight_c, n = 10)
  )

# tests -------------------------------------------------------------------

test_that("verify inla model object", {
  expect_s3_class(i04M07ctr, "inla")
})

test_that("get_newdata_inla: Input errors.", {
  # dataframe without the rows
  df <- runif(n = 10)
  rgx <- "Assertion on 'newdata' failed"
  expect_error(get_newdata_inla(i04M07ctr, newdata = df), regexp = rgx)

  # the names are not a subset of the original names
  df <- the_newdata
  names(df) <- c("WRONG1", "WRONG2")
  rgx <- "Assertion on 'names[(]newdata[)]' failed"
  expect_error(get_newdata_inla(i04M07ctr, newdata = df), regexp = rgx)
})


test_that("create_newdata_inla: Empty newdata.", {
  # dataframe without the rows
  df <- i04M07ctr$.args$data[FALSE, ]
  out <- create_newdata_inla(i04M07ctr, newdata = df)
  expect_identical(out, list("data" = i04M07ctr$.args$data,
                             "newdata_pos" = integer()))
})


test_that("create_newdata_inla", {

  out <- create_newdata_inla(i04M07ctr, newdata = the_newdata)

  expect_type(out, type = "list")
  expect_identical(names(out), c("data", "newdata_pos"))

  # check the vector of new position
  the_pos <- (nrow(i04M07ctr$.args$data) + 1):(nrow(i04M07ctr$.args$data) +
                                                 nrow(the_newdata))
  expect_identical(out$newdata_pos, the_pos)


  # check the inla data
  expect_s3_class(out$data, class = "data.frame")
  # the names must be the sae as the original data
  expect_identical(names(out$data), names(i04M07ctr$.args$data))
  the_dim <- c(nrow(i04M07ctr$.args$data) + nrow(the_newdata),
               length(i04M07ctr$.args$data))
  # nb of rows must be increased by new rows
  expect_identical(dim(out$data), the_dim)
})



test_that("get_newdata_inla", {

  out <- get_newdata_inla(i04M07ctr, newdata = the_newdata)

  expect_type(out, type = "list")
  expect_identical(names(out), c("inla", "newdata_pos"))

  # check the vector of new position
  the_pos <- (nrow(i04M07ctr$.args$data) + 1):(nrow(i04M07ctr$.args$data) +
                                                 nrow(the_newdata))
  expect_identical(out$newdata_pos, the_pos)


  # check the inla
  result <- out$inla
  expect_s3_class(result, class = "inla")
  # the names must be the sae as the original data
  expect_identical(names(result$.args$data), names(i04M07ctr$.args$data))
  the_dim <- c(nrow(i04M07ctr$.args$data) + nrow(the_newdata),
               length(i04M07ctr$.args$data))
  # nb of rows must be increased by new rows
  expect_identical(dim(result$.args$data), the_dim)
})
