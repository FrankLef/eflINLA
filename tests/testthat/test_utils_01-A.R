# declarations ------------------------------------------------------------

# the inla objects used
i04M07ctr <- readRDS(test_path("testdata", "fits", "i04M07ctr.rds"))


# tests -------------------------------------------------------------------

test_that("verify inla model object", {
  expect_s3_class(i04M07ctr, "inla")
})


test_that("prec2sd", {
  # test with is_log = TRUE
  x <- c(2, -1, -0.1, 0, 0.1, 1, 2)
  y <- prec2sd(x, is_log = TRUE)
  z <- 1 / sqrt(exp(x))
  expect_identical(y, z)

  # test with is_log = FALSE
  x <- c(0.1, 1, 2)
  y <- prec2sd(x, is_log = FALSE)
  z <- 1 / sqrt(x)
  expect_identical(y, z)
})


test_that("prec2sd_marg: log = FALSE", {
  nm <- "Precision for the Gaussian observations"

  marg <- i04M07ctr$marginals.hyperpar[[nm]]
  # cat("\n")
  # print(marg)
  # cat("\n")

  marg_sd <- prec2sd_marg(marg, is_log=FALSE)
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

  intern_marg_sd <- prec2sd_marg(intern_marg, is_log=TRUE)

  # NOTE: Both internal and non-log marginal should give the same
  #       result on the natural scale
  expect_identical(dim(intern_marg_sd), dim(marg_sd))
  expect_equal(marg_sd[1, ], intern_marg_sd[1, ], tolerance = 1e-3)
  expect_equal(marg_sd[75, ], intern_marg_sd[75, ], tolerance = 1e-3)
})

test_that("rename_inla2brms", {

  df <- data.frame(
    x = c(NA_character_,
          "Precision for the Gaussian observations",
          "SD for the Gaussian observations",
          "sd for the Gaussian observations",
          "unknown", "(Intercept)",
          "Beta for A", "Beta for B", "Beta for B"),
    y = c(NA_character_,
          "Precision", "sigma", "sigma",
          "unknown", "b_Intercept",
          "b_A","b_B", "b_B")
    )

  z <- rename_inla2brms(df$x)

  expect_identical(z, df$y)
})


test_that("rename_inla", {

  x <- "test"
  y <- rename_inla(x)
  expect_identical(y, x)

  x <- "Precision for the Gaussian observations"
  y <- rename_inla(x, choice = "Precision")

  expect_identical(y, "SD for the Gaussian observations")
})
