#' Re-shuffle a deck of cards randomly and fairly
#' @param cards A two dimensional object with 52 rows and three columns, including all data within a deck of cards;faces, suits and values of each of the 52 cards in a typical deck.
#'
#' @return The deck of cards will be randomly re-shuffled.
#' @export
#'
#' @examples
#' cards_df <- shuffle(cards = cards_df)
#' shuffle(cards = cards_df)

shuffle <- function(cards = cards_df) {
  random <- sample(1:52, size = 52)
  cards[random, ]
}
