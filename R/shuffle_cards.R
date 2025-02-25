#' Re-shuffle a deck of virtual playing cards
#'
#' @description Randomly reorders a deck of 52 playing cards, ensuring a fair shuffle.
#' The shuffled deck object can then be used in other functions such as `GambleR::deal_card()`.
#'
#' @param cards A data frame containing a standard deck of 52 playing cards.
#' If no deck is provided, a fresh one will be generated.
#'
#' @return A shuffled deck of playing cards as a data frame.
#'
#' @export
#'
#' @examples
#' # Shuffle a fresh deck
#' shuffled_deck <- GambleR::shuffle_cards()
#'
#' # Shuffle an existing deck
#' shuffled_deck <- GambleR::shuffle_cards(cards = deck_of_cards)
shuffle_cards <- function(cards = NULL) {

  # if no deck is provided, generate a standard 52-card deck
  if (is.null(cards)) {
    # load the previously saved deck of cards data-frame
    cards <- GambleR::deck_of_cards
  }

  # check if the input deck is valid
  if (!is.data.frame(cards) || nrow(cards) != 52 || !all(c("face", "suit", "card_value") %in% colnames(cards))) {
    stop("🃏 Invalid deck provided. Please supply a valid 52-card deck.")
  }

  # shuffle the deck
  message("🔀 Shuffling deck...")
  shuffled_deck <- cards[sample(nrow(cards)), ]
  message("Deck shuffled.")

  return(shuffled_deck)
}

