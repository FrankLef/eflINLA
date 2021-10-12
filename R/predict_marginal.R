linpred_marg_draws_inla <- function(.result, newdata = NA) {
  checkmate::assert_class(.result, classes = "inla", ordered = TRUE)





  marg <- .result$marginals.linear.predictor


}


linpred_marg_draws_inla <- function(.result) {
  checkmate::assert_class(.result, classes = "inla", ordered = TRUE)

  marg <- .result$marginals.fitted.values

}
