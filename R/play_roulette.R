#' Spin the wheel at GambleR's roulette table
#'
#' @description Step right up and test your luck at the GambleR roulette table! Will you walk away a legend, or will the house (which always wins) claim yet another victim? This function lets you place a simple bet on Red, Black, or Green—without the risk of re-mortgaging your house, putting it all on Black, and explaining your life choices to your now ex-spouse.
#'
#' @param prediction A string stating which colour you wish to bet on. Choose from "Red", "Black", or "Green". Red and Black offer even-money odds (2:1), while Green pays out big at 35:1 (European) or 38:2 (American) because, well, it’s called *gambling* for a reason.
#'
#' @param bet A positive number representing how much you’re willing to risk in imaginary casino currency. The default currency is GBP(£) when playing on a European table, but if you're playing on an `American` table, it'll be in USD($).
#'
#' @param roulette_type The type of roulette table you want to play on. The default is `European`, where there's only one Green zero. Feeling less-bold? Play the `American` table for more favourable odds on Green!
#'
#' @return The function returns the result of the spin, announcing where the ball landed and whether you won big, won small, or should probably rethink your life choices.
#'
#' @export
#'
#' @examples
#' # A cautious gambler playing it safe:
#' GambleR::roulette(prediction = "Black",
#'                   bet = 10,
#'                   roulette_type = "European")
#'
#' # Going for the big Green payout:
#' GambleR::roulette(prediction = "Green",
#'                   bet = 100,
#'                   roulette_type = "American")
play_roulette <- function(prediction = "Black",
                          bet = 10,
                          roulette_type = "European") {

  # first define the wheel layout, whether European or American
  if (roulette_type == "European") {
    currency <- "£"
    wheel <- data.frame(
      number = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,
                 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36),
      colour = c("Green", "Red", "Black", "Red", "Black", "Red", "Black", "Red", "Black",
                 "Red", "Black", "Black", "Red", "Black", "Red", "Black", "Red", "Black", "Red",
                 "Red", "Black", "Red", "Black", "Red", "Black", "Red", "Black", "Red", "Black",
                 "Black", "Red", "Black", "Red", "Black", "Red", "Black", "Red")
    )

  } else if (roulette_type == "American") {
    currency <- "$"
    wheel <- data.frame(
      number = c("00", 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,
                 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36),
      colour = c("Green", "Green", "Red", "Black", "Red", "Black", "Red", "Black", "Red", "Black",
                 "Red", "Black", "Black", "Red", "Black", "Red", "Black", "Red", "Black", "Red",
                 "Red", "Black", "Red", "Black", "Red", "Black", "Red", "Black", "Red", "Black",
                 "Black", "Red", "Black", "Red", "Black", "Red", "Black", "Red")
    )
  } else {
    stop("✋ Invalid roulette type. Choose 'European' or 'American'.")
  }

  # function to simulate spinning wheel animation
  print_spinner <- function(duration = 3) {
    frames <- c("◐", "◓", "◑", "◒")  # unicode characters for rotation effect

    cat("🤵 No more bets, spinning the wheel!\n")
    start_time <- Sys.time()

    while (difftime(Sys.time(), start_time, units = "secs") < duration) {
      for (frame in frames) {
        cat("\r", frame, sep = "")
        Sys.sleep(0.2)  # control speed of rotation
      }
    }

  }

  # simulate spinning the wheel
  print_spinner()

  # spin the wheel (randomly select a number)
  outcome <- sample(wheel$number, size = 1)

  # find the colour of the chosen number
  outcome_colour <- wheel$colour[wheel$number == outcome]

  # assign color emoji
  color_emoji <- ifelse(outcome_colour == "Red", "🟥",
                        ifelse(outcome_colour == "Black", "⬛️", "🟩"))

  cat("\r The wheel landed on:", color_emoji, outcome, "\n")

  # calculate and determine winnings based on correct odds
  prize <- 0

  # green bet (0 or 00) payout is different
  if (prediction == "Green" && outcome_colour == "Green") {
    prize <- ifelse(roulette_type == "American", bet * 19, bet * 35)  # 38:2 for American, 35:1 for European
    cat("🎉 Congratulations! You won", currency, prize, "💸\n")

    # colour bet (red or black)
  } else if (prediction == outcome_colour) {
    prize <- bet * 2  # 1:1 payout for correct colour bet
    cat("🎉 Congratulations! You won", currency, prize, "🤑\n")

  } else {
    cat("😢 Oof.. unlucky, double down and try again? 👀\n")
  }
}

