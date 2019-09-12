#' Simulation-Based Power Analysis for Two-Way Interaction Using Mean Patterns
#' 
#' @description Estimate power for the interaction term in a 2 x 2 design. This
#'   function uses mean patterns to do so. It simulates data from normal
#'   distributions where the standard deviations are 1. This means that mean
#'   differences between cells can be interpreted in the units of Cohen's d.
#'   Thus, one can hypothesize a Cohens'd d in one condition, a Cohen's d in
#'   another condition, and then run simulations to estimate power at different
#'   sample sizes. See examples.
#' 
#' @param a1b1 Mean in cell A1, B1
#' @param a1b2 Mean in cell A1, B2
#' @param a2b1 Mean in cell A2, B1
#' @param a2b2 Mean in cell A2, B2
#' @param start Smallest sample size N to test
#' @param end Largest sample size N to test
#' @param by The incrementing size from start to end
#' @param alpha The alpha level for determining significance
#' @param reps The number of times to repeat the simulations
#' @param verbose Logical. Print out current status in the simulation?
#' 
#' @return A data.frame containing columns indicating sample size and power
#'   at that sample size.
#' 
#' @examples
#' # imagine d = .5 in condition A and d = .0 in condition B
#' # try see what power is at 100, 150, and 200 N
#' set.seed(1839)
#' twipc_means(0, .5, 0, 0, 100, 200, 50)
#' 
#' @export
twipc_means <- function(a1b1, a1b2, a2b1, a2b2, start = 100, end = 500, by = 25, 
                        alpha = .05, reps = 1000, verbose = FALSE) {
  out <- lapply(seq(start, end, by), function(n) {
    if (verbose) cat("Starting simulation at N =", n, "\n")
    get_power(n, a1b1, a1b2, a2b1, a2b2, alpha, reps)
  })
  out <- as.data.frame(do.call(rbind, out))
  names(out) <- "Interaction Term Power"
  out$`Sample Size` <- seq(start, end, by)
  
  return(out[, c(2, 1)])
}
