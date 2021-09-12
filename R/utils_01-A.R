prec2sd <- function(marg, log=FALSE) {
  if (log) {
    INLA::inla.tmarginal(fun = function(x) 1 / sqrt(exp(x)), marginal = marg)
  } else {
    INLA::inla.tmarginal(fun = function(x) 1 / sqrt(x), marginal = marg)
  }
}
