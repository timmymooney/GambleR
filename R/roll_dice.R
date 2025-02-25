#' Roll the dice and test your luck!
#'
#' @description Simulates rolling one or more dice. Will you roll a lucky seven, or just a handful of disappointment?
#'
#' @param number_of_die A positive integer, stating the number of dice to roll. Default value is 1.
#'
#' @return A numeric value representing the total sum of all dice rolled.
#'
#' @export
#'
#' @examples
#' # Roll a single die
#' GambleR::roll_dice()
#'
#' # Roll two dice (classic style)
#' GambleR::roll_dice(number_of_die = 2)
roll_dice <- function(number_of_die = 1) {

  # Ensure input is a whole number and positive
  if (!is.numeric(number_of_die) || number_of_die < 1 || floor(number_of_die) != number_of_die) {
    stop("number_of_die must be a positive integer.")
  }

  die <- 1:6
  rolls <- sample(die, size = as.integer(number_of_die), replace = TRUE)
  sum(rolls)
}


