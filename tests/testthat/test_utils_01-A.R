# declarations ------------------------------------------------------------

# the inla objects used
i04M07ctr <- readRDS(test_path("testdata", "fits", "i04M07ctr.rds"))


# tests -------------------------------------------------------------------

test_that("verify inla model object", {
  expect_s3_class(i04M07ctr, "inla")
})


test_that("prec2sd: log = FALSE", {
  nm <- "Precision for the Gaussian observations"

  marg <- i04M07ctr$marginals.hyperpar[[nm]]
  # cat("\n")
  # print(marg)
  # cat("\n")

  marg_sd <- prec2sd(marg, is_log=FALSE)
  # cat("\n")
  # print(marg_sd)
  # cat("\n")

  expect_identical(dim(marg_sd), c(75L, 2L))
  expect_equal(marg_sd[1, ], c(x = 4.626325, y = 0.1779419), tolerance = 1e-3)
  expect_equal(marg_sd[75, ], c(x = 5.477974, y = 0.1776431), tolerance = 1e-3)


  nm <- "Log precision for the Gaussian observations"
  intern_marg <- i04M07ctr$internal.marginals.hyperpar[[nm]]
  # cat("\n")
  # print(intern_marg)
  # cat("\n")

  intern_marg_sd <- prec2sd(intern_marg, is_log=TRUE)

  # NOTE: Both internal and non-log marginal should give the same
  #       result on the natural scale
  expect_identical(dim(intern_marg_sd), dim(marg_sd))
  expect_equal(marg_sd[1, ], intern_marg_sd[1, ], tolerance = 1e-3)
  expect_equal(marg_sd[75, ], intern_marg_sd[75, ], tolerance = 1e-3)
})

test_that("rename_inla2brms", {

  x <- c("Precision for the Gaussian observations", "Beta for A",
         "unknown", "Beta for B")

  y <- rename_inla2brms(x)

  target <- c("Sigma", "b_A", "unknown", "b_B")
  expect_identical(y, target)
})
