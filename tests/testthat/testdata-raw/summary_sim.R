library(simstudy)

sim <- list()
sim$defs <- defData(varname = "x_normal", dist = "normal",
                   formula = 0, variance = 2)
sim$defs <- defData(sim$defs, varname = "x_lognormal", dist = "normal",
                   formula = 0, variance = 2)
sim$defs <- defData(sim$defs, varname = "x_explog", dist = "nonrandom",
                    formula = "exp(x_lognormal)")
sim$defs <- defData(sim$defs, varname = "mu", dist = "nonrandom",
                   formula = "x_normal + x_explog")
sim$defs <- defData(sim$defs, varname = "sigma", dist = "exponential",
                   formula = 1)
sim$defs <- defData(sim$defs, varname = "y", dist = "normal",
                   formula = "mu", variance = "sigma^2")
# sim$defs

# sim1 normal -------------------------------------------------------------
# NOTE: For some reason updateDef gives error message
#       "Data definition des not exist" when using a list!
defs <- sim$defs
sim$defs <- updateDef(defs, changevar = "mu", newformula = "x_normal")
# sim$defs
sim$data <- genData(n = 100L, dtDefs = sim$defs)

sim$args <- list(
  data = sim$data,
  formula = y ~ -1 + x_normal,
  family = "gaussian",
  control.fixed = list(mean.intercept = 0,
                       prec.intercept = 0.001,
                       mean = list(x_normal = 0,
                                   default = 0),
                       prec = list(x_normal = 1 / (2^2),
                                   default = 0.001)),
  control.family = list(hyper = list(
    prec = list(prior = "loggamma", param = c(1, 1e-5)))
  ),
  quantiles <- c(0.025, 0.5, 0.975))
sim$inla <- do.call(INLA::inla, sim$args)

# create lmer
sim$lm <- stats::lm(formula = y ~ 0 + x_normal,
                    data = sim$data)

# save the file
a_file <- testthat::test_path("testdata", "summaries_sim1.rds")
saveRDS(sim, file = a_file)


# sim 2 lognormal ---------------------------------------------------------
# NOTE: For some reason updateDef gives error message
#       "Data definition des not exist" when using a list!
defs <- sim$defs
sim$defs <- updateDef(defs, changevar = "mu", newformula = "x_explog")
# sim$defs
sim$data <- genData(n = 100L, dtDefs = sim$defs)


sim$args <- list(
  data = sim$data,
  formula = y ~ -1 + x_explog,
  family = "gaussian",
  control.fixed = list(mean.intercept = 0,
                       prec.intercept = 0.001,
                       mean = list(x_lognormal = 0,
                                   default = 0),
                       prec = list(x_lognormal = 1 / (2^2),
                                   default = 0.001)),
  control.family = list(hyper = list(
    prec = list(prior = "loggamma", param = c(1, 1e-5)))
  ),
  quantiles <- c(0.025, 0.5, 0.975))
sim$inla <- do.call(INLA::inla, sim$args)

# create lmer
sim$lm <- stats::lm(formula = y ~ 0 + x_explog,
                    data = sim$data)

# save the file
a_file <- testthat::test_path("testdata", "summaries_sim2.rds")
saveRDS(sim, file = a_file)


# sim 3 normal + lognormal ------------------------------------------------
# NOTE: For some reason updateDef gives error message
#       "Data definition des not exist" when using a list!
defs <- sim$defs
sim$defs <- updateDef(defs, changevar = "mu",
                      newformula = "x_normal + x_explog")
# sim$defs
sim$data <- genData(n = 100L, dtDefs = sim$defs)
# str(sim$data)

sim$args <- list(
  data = sim$data,
  formula = y ~ -1 + x_normal +
    f(x_explog, model = "clinear", range = c(0, Inf),
      prior = "logtnormal", param = c(0, 1)),
  family = "gaussian",
  control.fixed = list(mean.intercept = 0,
                       prec.intercept = 0.001,
                       mean = list(x_normal = 0,
                                   default = 0),
                       prec = list(x_normal = 1 / (2^2),
                                   default = 0.001)),
  control.family = list(hyper = list(
    prec = list(prior = "loggamma", param = c(1, 1e-5)))
  ),
  quantiles <- c(0.025, 0.5, 0.975))
sim$inla <- do.call(INLA::inla, sim$args)

# to use coefficient with boundary we have to use nls
# source: https://stackoverflow.com/questions/21212782/positive-coefficients-for-lm-or-gls
sim$lm <- stats::lm(formula = y ~ 0 + x_normal + x_explog,
                    data = sim$data)

# save the file
a_file <- testthat::test_path("testdata", "summaries_sim3.rds")
saveRDS(sim, file = a_file)
