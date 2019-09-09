#' Simulate a Model and Extract if p < alpha
#' 
#' @param n Sample size
#' @param a1b1 Mean in cell A1, B1
#' @param a1b2 Mean in cell A1, B2
#' @param a2b1 Mean in cell A2, B1
#' @param a2b2 Mean in cell A2, B2
#' @param alpha Alpha level
#' @return Logical indicating TRUE for less than alpha, FALSE otherwise
sim_model <- function(n, a1b1, a1b2, a2b1, a2b2, alpha) {
  a <- factor(sample(c("1", "2"), n, TRUE))
  b <- factor(sample(c("1", "2"), n, TRUE))
  y <- rnorm(
    n,
    ifelse(
      a == "1" & b == "1",
      a1b1,
      ifelse(
        a == "1" & b == "2",
        a1b2,
        ifelse(
          a == "2" & b == "1",
          a2b1,
          a2b2
        )
      )
    )
  )
  return(summary(lm(y ~ a * b))$coef["a2:b2", "Pr(>|t|)"] < alpha)
}
