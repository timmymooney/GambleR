#' Try your luck on GambleR's slot machine! 🎰
#'
#' @description
#' Spin the reels and see if fortune favours you! This function simulates a classic slot machine.
#' The wheel spins, producing three random symbols, and you win based on the outcome.
#'
#' **Winning Rules:**
#' - **Three Diamonds (💎💎💎)**  ==  **JACKPOT! £100**
#' - **Two Cherries (🍒🍒)**  ==  **£5**
#' - **One Cherry (🍒)**  == **£2**
#' - **Any other combination**  ==  **No win**
#'
#' @return
#' Prints the slot machine's result along with any winnings.
#'
#' @export
#'
#' @examples
#' GambleR::play_slot_machine()
play_slot_machine <- function() {
  # define slot symbols
  wheel <- c("💎", "7️⃣", "🎰", "🍒", "🔔", "🍋")

  # start spinning animation
  cat("Spinning...\n")

  for (i in 1:10) {
    # short delay for flicker effect
    Sys.sleep(0.1)
    cat("\r", paste(sample(wheel, 3, replace = TRUE), collapse = " "), "    ")
    # ensures immediate update
    flush.console()
  }

  # final outcome
  symbols <- sample(wheel, size = 3, replace = TRUE)

  # display result
  cat("\r", paste(symbols, collapse = " "), "\n")

  # check for wins
  cherries <- sum(symbols == "🍒")
  diamonds <- sum(symbols == "💎")

  # determine winnings
  if (diamonds == 3) {
    cat("Three Diamonds JACKPOT!! You win £100\n")
  } else if (cherries == 2) {
    cat("Two cherries, you won £5!\n")
  } else if (cherries == 1) {
    cat("One cherry, you won £2!\n")
  } else {
    cat("😢 No win this time. Try again!\n")
  }
}

