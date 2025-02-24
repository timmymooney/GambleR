#' Deal the top card from a deck
#'
#' @description A function that will deal the top card from the deck or cards dataframe.
#'
#' @param cards A two dimensional object with 52 rows and three columns, including all data within a deck of cards;faces, suits and values of each of the 52 cards in a typical deck.
#'
#' @return The top card in the deck will be dealt, providing the face, suite and value of the card.
#'
#' @export
#'
#' @examples
#' dealt_cards <- deal_card(cards = cards_df)
#'
#' deal_card(cards = cards_df)
deal_card <- function(cards = cards_df) {
  cards[1, ]
}
