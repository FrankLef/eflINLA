# declarations ------------------------------------------------------------
library(posterior, quietly = TRUE)



# the inla objects used
i04M07ctr <- readRDS(test_path("testdata", "fits", "i04M07ctr.rds"))


# tests -------------------------------------------------------------------

test_that("verify inla model object", {
  expect_s3_class(i04M07ctr, "inla")
})
