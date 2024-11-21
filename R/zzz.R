#' @useDynLib ptmc
#' @importFrom Rcpp sourceCpp
#' @import coda
#' @import parallel
#' @import tidyr
#' @import dplyr
#' @import foreach
#' @import doParallel
#' @importFrom magrittr %>% %<>%
NULL


#' @title Model Specification for Parallel Tempering MCMC
#' @description This list defines the components of a model used in the parallel tempering MCMC algorithm. 
#' It includes parameter bounds, prior distributions, likelihood functions, and methods for handling discrete latent variables.
#'
#' @section Components:
#' 
#' @itemize
#'   @item \strong{lowerParSupport_fitted}: A numeric vector specifying the lower bounds for the parameters. 
#'         For the parameters \code{w_r}, \code{w_c}, and \code{w_s}, all values are constrained to be greater than or equal to 0.
#'         
#'   @item \strong{upperParSupport_fitted}: A numeric vector specifying the upper bounds for the parameters. 
#'         The parameters \code{w_r}, \code{w_c}, and \code{w_s} are restricted to values less than or equal to 100.
#'         
#'   @item \strong{namesOfParameters}: A character vector containing the names of the three parameters:
#'         \code{"w_r"}, \code{"w_c"}, and \code{"w_s"}. These represent rates for discrete categories 1, 2, and 3, respectively.
#'         
#'   @item \strong{discrete_length}: An integer specifying the number of discrete latent variables. Defaults to \code{30}, representing the number of observations in the data.
#'         
#'   @item \strong{samplePriorDistributions}: A function to generate prior samples for the parameters. Each parameter is sampled independently from a uniform distribution over the interval [0, 100].
#'         @param datalist An optional data list (not used in this function).
#'         @return A numeric vector of sampled parameter values.
#'         
#'   @item \strong{evaluateLogPrior}: A function to evaluate the log-prior probability of a parameter set.
#'         @param params A numeric vector of parameters \code{w_r}, \code{w_c}, and \code{w_s}.
#'         @param discrete The discrete latent variables (not used in this function).
#'         @param datalist A data list (not used in this function).
#'         @return The log-prior probability, calculated as the sum of uniform prior densities. Returns \code{-Inf} if \code{w_r}, \code{w_c}, and \code{w_s} are not in increasing order.
#'         
#'   @item \strong{initialiseDiscrete}: A function to initialize the discrete latent variables. It randomly assigns each observation to one of the three categories (1, 2, or 3).
#'         @param datalist A list containing \code{N}, the number of discrete variables.
#'         @return A numeric vector of discrete latent variables, sampled randomly from {1, 2, 3}.
#'         
#'   @item \strong{discreteSampling}: A function to update the discrete latent variables using a random walk.
#'         @param discrete The current state of the discrete latent variables.
#'         @param datalist A list containing \code{N}, the number of observations.
#'         @return An updated numeric vector of discrete latent variables after applying random operations.
#'         
#'   @item \strong{evaluateLogLikelihood}: A function to evaluate the log-likelihood of the data given the model parameters and discrete latent variables.
#'         @param params A numeric vector of parameters \code{w_r}, \code{w_c}, and \code{w_s}.
#'         @param discrete The discrete latent variables.
#'         @param covariance A covariance matrix (not used in this function).
#'         @param datalist A list containing \code{y} (observed data) and \code{N} (number of observations).
#'         @return The total log-likelihood for the model.
#'
#' @examples
#' model <- list(
#'   lowerParSupport_fitted = c(0, 0, 0),
#'   upperParSupport_fitted = c(100, 100, 100),
#'   namesOfParameters = c("w_r", "w_c", "w_s"),
#'   discrete_length = 30,
#'   samplePriorDistributions = function(datalist) { runif(3, 0, 100) },
#'   evaluateLogPrior = function(params, discrete, datalist) {
#'     lpr <- sum(dunif(params, 0, 100, log = TRUE))
#'     if (!(params[1] < params[2] & params[2] < params[3])) lpr <- -Inf
#'     return(lpr)
#'   },
#'   initialiseDiscrete = function(datalist) {
#'     N <- datalist$N
#'     sample(1:3, N, replace = TRUE)
#'   },
#'   discreteSampling = function(discrete, datalist) {
#'     u <- runif(1)
#'     N <- datalist$N
#'     if (u < 0.33) {
#'       j <- sample(1:N, 1)
#'       discrete[j] <- sample(1:3, 1)
#'     } else if (u < 0.66) {
#'       j_idx <- sample(1:N, 2)
#'       discrete[j_idx] <- rev(discrete[j_idx])
#'     }
#'     return(discrete)
#'   },
#'   evaluateLogLikelihood = function(params, discrete, covariance, datalist) {
#'     w_r <- params[1]
#'     w_c <- params[2]
#'     w_s <- params[3]
#'     ll <- 0
#'     for (i in seq_len(datalist$N)) {
#'       if (discrete[i] == 1) ll <- ll + dpois(datalist$y[i], w_r, log = TRUE)
#'       else if (discrete[i] == 2) ll <- ll + dpois(datalist$y[i], w_c, log = TRUE)
#'       else ll <- ll + dpois(datalist$y[i], w_s, log = TRUE)
#'     }
#'     return(ll)
#'   }
#' )