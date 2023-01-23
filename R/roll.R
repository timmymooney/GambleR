#' Roll a pair of dice
#'
#' @description A function that simulates the rolling of a pair of dice (or die).
#'
#' @return A random score between 2 - 12, calculated by adding the score of two virtual dice together.
#'
#' @export
#'
#' @examples
#' score <- roll()
#' roll()

roll <- function() {
  die <- 1:6
  names(die) <- c("one", "two", "three", "four", "five", "six")
  dice <- sample(die, size = 2, replace = TRUE)
  sum(dice)
}
