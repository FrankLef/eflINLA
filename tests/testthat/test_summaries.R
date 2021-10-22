# declarations ------------------------------------------------------------
library(dplyr)
library(INLA)

# simulation description
# x1, x2, x3 Are correlated
# x3 is lognormal distributed
# cat1 is a category with binomial distribution

# # the simulation used
# sim <- list(n = 500L,
#             intercept = c("mu" = 5, "sigma" = 1),
#             sigma_rate = 1,
#             cat1 = c("size" = 1, "p" = 0.5),
#             mus = c("x1" = 1, "x2" = 2, "x3" = 0),
#             sigmas = c("x1" = 0.75, "x2" = 1, "x3" = 1),
#             rhos = c("x1-x2" = 0.25, "x1-x3" = 0.5, "x2-x3" = 0.75),
#             beta = c("x1" = 1, "x2" = 2, "x3" = 0.5, "x4" = 10))
# sim <- within(sim, {
#   # Rho is the correlation matrix
#   Rho <- diag(nrow = length(rhos))
#   Rho[lower.tri(Rho)] <- rhos
#   Rho[upper.tri(Rho)] <- t(Rho[lower.tri(Rho)])
#   # Sigma is the covariance matrix
#   Sigma <- diag(sigmas) %*% Rho %*% diag(sigmas)
#   # create the data
#   mat <- MASS::mvrnorm(n = n, mu = mus, Sigma = Sigma)
# })
#
# sim <- within(sim, {
#   data <- as.data.frame(mat)
#   data <- data %>%
#     mutate(x0 = rnorm(n = n, mean = intercept["mu"], sd = intercept["sigma"]),
#            x1 = beta["x1"] * x1,
#            x2 = beta["x2"] * x2,
#            x3 = beta["x3"] * exp(x3),
#            x4 = beta["x4"] * rbinom(n = n, size = cat1["size"], p = cat1["p"]),
#            sigma = rexp(n, rate = sigma_rate),
#            y = x0 + x1 + x2 + x3 + x4 + rnorm(n = n, mean = 0L, sd = sigma)
#            )
#            })
# sapply(sim$data, mean)
#
# the_inla <- INLA::inla(
#   data = sim$data,
#   formula = y ~ x1 + x2 + x4 + f(x3, model = "clinear", range = c(0, Inf),
#                             prior = "logtnormal", param = c(0, 1)),
#   # control.compute = list(config = TRUE, dic = TRUE, waic = TRUE),
#   control.predictor = list(compute = TRUE))

a_file <- test_path("testdata", "summaries_sim1.rds")
sim <- readRDS(a_file)



# tests -------------------------------------------------------------------

test_that("verify inla model object", {
  expect_s3_class(sim$inla, "inla")
})


