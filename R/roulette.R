#' Spin the wheel at gambleR's roulette table
#'
#' @description Spin the wheel and try your luck at guessing what colour the virtual ball will land on. This function resembles a very basic roulette table, a very fun way to gamble away any spare funds.
#'
#' @param prediction A string stating which colour the gambler wishes to place their stake on. The colours are "Red", "Black" or "Green". The odds that the ball will land on "Red" or "Black" are 2/1, and the odds for "Green" are 35/1.
#'
#' @param bet Any positive number, stating how much the gambler would like to bet on their prediction - the default is GBP(£) but no further information is required, other than the positive number.
#'
#' @return The function will return the result of the spin, stating which colour the ball has landed on as well as any prize won, if any.
#'
#' @export
#'
#' @examples
#' result <- roulette(prediction, "Red", bet = 10)
#'
#' roulette(prediction = "Black", bet = 10)

roulette <- function(prediction = prediction, bet = bet) {

  bet <- bet

  wheel <- data.frame(
    number = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 , 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36),
    colour = c("Green", "Red", "Black", "Red", "Black", "Red", "Black", "Red", "Black", "Red", "Black", "Red", "Black", "Red", "Black", "Red", "Black", "Red", "Black", "Red", "Black", "Red", "Black", "Red", "Black", "Red", "Black", "Red", "Black", "Red", "Black", "Red", "Black", "Red", "Black", "Red", "Black")
  )

  outcome <- sample(wheel$colour, size = 1, replace = TRUE,
                    prob = c(0.027, 0.027, 0.027, 0.027, 0.027, 0.027, 0.027, 0.027, 0.027, 0.027, 0.027, 0.027, 0.027, 0.027, 0.027, 0.027, 0.027, 0.027, 0.027, 0.027, 0.027, 0.027, 0.027, 0.027, 0.027, 0.027, 0.027, 0.027, 0.027, 0.027, 0.027, 0.027, 0.027, 0.027, 0.027, 0.027, 0.027))
  print(outcome)

  if(outcome == "Green") {
    prize <- bet * 35
  } else if(outcome == "Black") {
    prize <- bet * 2
  } else {
   prize <- bet * 2
  }

  prediction <- prediction

  if(prediction == outcome) {
    decider <- attr(prize, "decider")
    decider <- paste(decider, collapse = " ")
    string <- paste(decider, prize, sep = "Congratulations, you've won \n£")
    cat(string)
  } else {
    print("Unlucky, double down and try again?")
  }
}
