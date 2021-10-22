library(simstudy)

sim <- list()
sim$def <- defData(varname = "x_normal", dist = "normal",
                   formula = 0, variance = 2)
# sim$def <- defData(sim$def, varname = "x_lognormal", dist = "normal",
#                    formula = 0, variance = 2)
sim$def <- defData(sim$def, varname = "sigma", dist = "exponential",
                   formula = 1)
sim$def <- defData(sim$def, varname = "y", dist = "normal",
                   formula = "x_normal", variance = "sigma^2")
sim$data <- genData(n = 100L, dtDefs = sim$def)

# create the inla
sim$args <- list(
  data = sim$data,
  formula = y ~ -1 + x_normal,
  family = "gaussian")
sim$inla <- do.call(INLA::inla, sim$args)


# create lmer
sim$lm <- stats::lm(formula = y ~ 0 + x_normal, data = sim$data)


# save the file
a_file <- testthat::test_path("testdata", "summaries_sim1.rds")
saveRDS(sim, file = a_file)

# create the inla
# sim$args <- list(
#   data = sim$data,
#   formula = y ~ -1 + x_normal + f(x_lognormal, model = "clinear", range = c(0, Inf),
#                                   prior = "logtnormal", param = c(0, 1)),
#   family = "gaussian",
#   control.fixed = list(mean.intercept = 0,
#                        prec.intercept = 0.001,
#                        mean = list(x_normal = 0, x2 = 0,
#                                    default = 0),
#                        prec = list(x1 = 1 / (2^2), x2 = 1 / (2^2),
#                                    default = 0.001)),
#   control.family = list(hyper = list(
#     prec = list(prior = "loggamma", param = c(1, 10e-5)))
#   ),
#   control.compute <- list(dic = TRUE, waic = TRUE),
#   quantiles <- c(0.025, 0.5, 0.975))
# sim$inla <- do.call(INLA::inla, sim$args)
#
# # save the file
# a_file <- testthat::test_path("testdata", "summaries_sim1.rds")
# saveRDS(sim, file = a_file)
