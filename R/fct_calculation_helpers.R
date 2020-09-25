#' Convert logit to probability
#' 
#' This is used for conversion of the results of the
#' logistic regression to the probability scale.
logit2prob <- function(logit) {
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

#' Function calculating the highest density interval using sampling
#' 
#' We use \code{\link[HDInterval]{hdi}} function in our
#' analysis. The present helper function is needed for
#' the Bayesian parameter estimation robustness test.
mode_HDI <- function(scale, density, crit_width = 0.95, n_samples = 1e5){
  samp <- sample(x = scale, size = n_samples, replace = TRUE, prob = density)
  hdi_result = HDInterval::hdi(samp, credMass = crit_width)
  result = c(scale[which(density == max(density))], # mode
             hdi_result[1], # lower bound
             hdi_result[2]) # upper bound
  
  # Only needed for the names of the result
  Crit_lb = (1 - crit_width) / 2
  Crit_ub = crit_width + (1 - crit_width) / 2
  
  names(result) = c("mode", paste0(Crit_lb * 100, "%"), paste0(Crit_ub * 100, "%"))
  return(result)
}

#' Function to fit smoothing spline
#' 
#' For the confirmatory analysis plot the cumulative
#' Bayes factors calculated with three priors needs
#' to be smoothed to ease understanding.
smoothie <- function(x, y) {
  fit <- smooth.spline(x, y, df = 80)
  
  predict(fit)$y
}
