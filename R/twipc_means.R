#' Simulation-Based Power Analysis for Two-Way Interaction Using Mean Patterns
#' 
#' @description 
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
#' 
#' @return A data.frame containing columns indicating sample size and power
#'   at that sample size.
#' 
#' @examples
#' 
#' @export
twipc_means <- function(a1b1, a1b2, a2b1, a2b2, 
                        start, end, by, alpha, reps) {
  out <- lapply(
    seq(start, end, by),
    get_power, 
    a1b1, a1b2, a2b1, a2b2, alpha, reps
  )
  out <- as.data.frame(do.call(rbind, out))
  names(out) <- "Interaction Term Power"
  out$`Sample Size` <- seq(start, end, by)
  
  return(out[, c(2, 1)])
}
