#' Get Power for a Specific N, Mean Pattern, and Repititions
#' 
#' @param n Sample size
#' @param a1b1 Mean in cell A1, B1
#' @param a1b2 Mean in cell A1, B2
#' @param a2b1 Mean in cell A2, B1
#' @param a2b2 Mean in cell A2, B2
#' @param alpha Alpha level
#' @param reps Number of simulations to run
#' @return Numeric indicating the power
get_power <- function(n, a1b1, a1b2, a2b1, a2b2, alpha, reps) {
  mean(sapply(1:reps, function(zz) sim_model(n, a1b1, a1b2, a2b1, a2b2, alpha)))
}
