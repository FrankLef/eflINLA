# declarations ------------------------------------------------------------
library(dplyr)
library(INLA)

a_file <- test_path("testdata", "summaries_sim1.rds")
sim1 <- readRDS(a_file)
a_file <- test_path("testdata", "summaries_sim2.rds")
sim2 <- readRDS(a_file)
a_file <- test_path("testdata", "summaries_sim3.rds")
sim3 <- readRDS(a_file)



# tests -------------------------------------------------------------------

test_that("verify inla model object", {
  expect_s3_class(sim1$inla, "inla")
})

test_that("extract_marginals(sim1$inla)", {
  margs <- extract_marginals(sim1$inla)
  expect_identical(names(margs), c("x_normal", "sigma"))
})

test_that("calc_moments_inla: x_normal",{

  # get the mean and sd from the marginal
  margs <- extract_marginals(sim1$inla)
  pos <- grep(pattern = "x_normal", x = names(margs))
  marg <- margs[[pos]]
  out <- calc_moments_inla(marg)
  # cli::cat_line("\n", "inla", col = "orange")
  # print(out)

  # get the stats from lm
  linest <- c("estimate" = coef(summary(sim1$lm))["x_normal","Estimate"],
                    "sd" = coef(summary(sim1$lm))["x_normal","Std. Error"])
  # cli::cat_line("\n", "linest", col = "orange")
  # print(linest)

  expect_type(out, "double")
  expect_identical(names(out), c("mean", "sd"))
  expect_equivalent(out[1], linest[1], tolerance = 1e-2)
  expect_equivalent(out[2], linest[2], tolerance = 1e-3)
})

test_that("posterior_summary_inla_one: x_normal",{

  # get the mean and sd from the marginal
  probs <- c(0.025, 0.975)
  probs_nms <- paste0("Q", probs)
  margs <- extract_marginals(sim1$inla)
  pos <- grep(pattern = "x_normal", x = names(margs))
  marg <- margs[[pos]]
  out <- posterior_summary_inla_one(marg, probs)
  # cli::cat_line("\n", "inla", col = "orange")
  # print(out)

  expect_type(out, "double")
  expect_identical(names(out), c("mean", "sd", probs_nms, "mode"))
})


test_that("extract_marginals(sim2$inla)", {
  margs <- extract_marginals(sim2$inla)
  expect_identical(names(margs), c("x_explog", "sigma"))
})

test_that("calc_moments_inla: x_explog",{

  # get the mean and sd from the marginal
  margs <- extract_marginals(sim2$inla)
  pos <- grep(pattern = "x_explog", x = names(margs))
  marg <- margs[[pos]]
  out <- list()
  out <- calc_moments_inla(marg)
  # cli::cat_line("\n", "inla", col = "orange")
  # print(out)

  # get the stats from lm
  linest <- c("estimate" = coef(summary(sim2$lm))["x_explog","Estimate"],
                    "sd" = coef(summary(sim2$lm))["x_explog","Std. Error"])
  # cli::cat_line("\n", "linest", col = "orange")
  # print(linest)

  expect_type(out, "double")
  expect_identical(names(out), c("mean", "sd"))
  expect_equivalent(out[1], linest[1], tolerance = 1e-3)
  expect_equivalent(out[2], linest[2], tolerance = 1e-3)
})


test_that("posterior_summary_inla_one: x_explog",{

  # get the mean and sd from the marginal
  the_probs <- c(0.025, 0.975)
  names(the_probs) <- paste0("Q", the_probs)
  margs <- extract_marginals(sim2$inla)
  pos <- grep(pattern = "x_explog", x = names(margs))
  marg <- margs[[pos]]
  out <- posterior_summary_inla_one(marg, probs = the_probs)
  # cli::cat_line("\n", "inla", col = "orange")
  # print(out)

  expect_type(out, "double")
  expect_identical(names(out), c("mean", "sd", names(the_probs), "mode"))
})



test_that("posterior_marg_hyper_summary_inla: sim3", {
  the_inla <- sim3$inla

  # get the quantiles and build their names
  the_probs <- sim3$inla$.args$quantiles
  names(the_probs) <- paste0("Q", the_probs)


  out <- posterior_summary_inla(the_inla)
  # cli::cat_line("\n", "inla summary", col = "orange")
  # print(out)


  expect_type(out, "list")
  the_cols <- c("var", "mean", "sd", names(the_probs), "mode")
  expect_identical(names(out), the_cols)
  expect_identical(nrow(out), 3L)
})




test_that("extract_marginals(sim3$inla)", {
  margs <- extract_marginals(sim3$inla)
  cat("\n")
  # print(names(margs))
  cat("\n")
  expect_identical(names(margs), c("x_normal", "b_x_explog", "sigma"))
})
