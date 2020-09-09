### Functions for Bayes factor caclulation using beta prior
# These functions are required to run the Bayes factor analysis
# The custom code is necessary because we use beta priors, and 
# the BayesFactor package by default does not have built in beta priors
# We thank Richard Morey for his help in developing these functions!
# TODO: Rewrite documentation, snake-case names, make the functions more concise
#' Full alt beta
#' @export
fullAlt_beta <- Vectorize(function(p, y, N, alpha, beta) {
  exp(dbinom(y, N, p, log = TRUE) + stats::dbeta(p, alpha, beta, log = TRUE)) 
  }, "p")

#' Normalize beta
#' @export
normalize_beta <- function(alpha, beta, interval) {
  diff(stats::pbeta(interval, alpha, beta))
  }

#' Restricted alt beta
#' @export
restrictedAlt_beta <- function(p, y, N, y_prior, N_prior, interval) {
  alpha = y_prior + 1
  beta = N_prior - y_prior + 1
  fullAlt_beta(p, y, N, alpha, beta) / normalize_beta(alpha, beta, interval) * (p>interval[1] & p<interval[2])
  }

#' Marginal likelihood beta
#' @export
margLike_beta <- function(y, N, y_prior, N_prior, interval) {
  integrate(restrictedAlt_beta, interval[1], interval[2], 
            y = y, N = N, y_prior = y_prior, N_prior = N_prior, interval = interval)[[1]]
  }

#' Bayes factor beta
#' @export
BF01_beta <- Vectorize(function(y, N, y_prior, N_prior, interval, null_prob){
  stats::dbinom(y, N, null_prob) / margLike_beta(y = y, N = N, y_prior = y_prior, N_prior = N_prior, interval = interval)
  }, "y")
