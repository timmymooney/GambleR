#' Test your luck on the gambleR slot machine
#'
#' @description A function which requires no arguments, simulating the playing of a slot machine. The wheel is spun and three symbols are produced, along with any prize won. Prizes are won based on the result of the random spin. Three DD's is worth £100, three 7's are worth £80 and so on. For more on scoring and prizes. Refer to https://rstudio-education.github.io/hopr/programs.html
#'
#' @return The slot machine's wheel will be spun, producing a combination of three symbols as well as the amount won in £ (with any luck). The result of the spin will be printed in the console.
#'
#' @export
#'
#' @examples
#' result <- slot_machine()
#' slot_machine()
play_slot_machine <- function() {

  wheel <- c("DD", "7", "BBB", "BB", "B", "C", "0")
  symbols <- sample(wheel, size = 3, replace = TRUE,
                    prob = c(0.03, 0.03, 0.06, 0.1, 0.25, 0.01, 0.52))

  print(symbols)

  same <- symbols[1] == symbols[2] && symbols[2] == symbols[3]
  bars <- symbols %in% c("B", "BB", "BBB")
  diamonds <- sum(symbols == "DD")

  if(same) {
    payouts <- c("DD" = 100, "7" = 80, "BBB" = 40, "BB" = 25,
                 "B" = 10, "C" = 10, "0" = 0)
    prize <- unname(payouts[symbols[1]])
    prize <- prize * 2 ^ diamonds
  } else if(all(bars)) {
    prize <- 5
    prize <- prize * 2 ^ diamonds
  } else {
    cherries <- sum(symbols == "C")
    prize <- c(0, 2, 5)[cherries + 1]
    prize <- prize * 2 ^ diamonds
  }
  symbols <- attr(prize, "symbols")
  symbols <- paste(symbols, collapse = " ")
  string <- paste(symbols, prize, sep = "\n£")
  cat(string)
}
