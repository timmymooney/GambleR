#' Deal the top card from a deck
#' @param cards A two dimensional object with 52 rows and three columns, including all data within a deck of cards;faces, suits and values of each of the 52 cards in a typical deck.
#'
#' @return The top card in the deck will be dealt, providing the face, suite and value of the card.
#' @export
#'
#' @examples
#' top_card <- deal(cards = cards_df)
#' deal(cards = cards_df)
deal <- function(cards) {
  cards[1, ]
}
