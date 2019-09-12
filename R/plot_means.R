#' Plot the Two-Way Interaction Mean Patterns to be Simulated
#' 
#' @description Plot to see what pattern of means look like before one puts them
#'   into the simulation. This allows a user to make sure what they input to
#'   the simulation function matches the pattern of results they expect to see.
#' 
#' @param a1b1 Mean in cell A1, B1
#' @param a1b2 Mean in cell A1, B2
#' @param a2b1 Mean in cell A2, B1
#' @param a2b2 Mean in cell A2, B2
#' @param a String. Name of the factor (variable) representing A (x-axis)
#' @param a1 String. Name of the first level of Factor A
#' @param a2 String. Name of the second level of Factor A
#' @param b String. Name of the factor (variable) representing B 
#'   (different series/colors)
#' @param b1 String. Name of the first level of Factor B
#' @param b2 String. Name of the second level of Factor B
#' 
#' @return A ggplot2 object depicting the mean patterns
#' 
#' @examples
#' # imagine d = .5 in condition A and d = .0 in condition B
#' plot_means(0, .5, 0, 0)
#' 
#' @import ggplot2
#' @export
plot_means <- function(a1b1, a1b2, a2b1, a2b2, a = NULL, a1 = NULL, a2 = NULL, 
                       b = NULL, b1 = NULL, b2 = NULL) {
  dat <- data.frame(
    FactorA = c("Level 1", "Level 1", "Level 2", "Level 2"),
    FactorB = c("Level 1", "Level 2", "Level 1", "Level 2"),
    Outcome = c(a1b1, a1b2, a2b1, a2b2)
  )
  
  out <- ggplot(dat, aes(x = FactorA, y = Outcome, color = FactorB)) +
    geom_point(size = 4) +
    geom_line(mapping = aes(group = FactorB), size = 1) +
    theme_light() +
    scale_x_discrete(
      name = ifelse(is.null(a), "Factor A", a),
      labels = c(
        ifelse(is.null(a1), "Level 1", a1),
        ifelse(is.null(a2), "Level 2", a2)
      )
    ) +
    scale_color_discrete(
      name = ifelse(is.null(b), "Factor B", b),
      labels = c(
        ifelse(is.null(b1), "Level 1", b1),
        ifelse(is.null(b2), "Level 2", b2)
      )
    ) +
    theme(text = element_text(size = 18))
  
  return(out)
}
