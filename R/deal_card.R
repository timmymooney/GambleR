#' Deal one or more cards from a deck of playing cards
#'
#' @description Deals the top card(s) from a given deck, returning both the dealt card(s) and the updated deck.
#'
#' @param cards A data frame representing a deck of cards. If not provided, a freshly shuffled deck derived from `GambleR::shuffle_cards()` will be used each time the function is called with no `cards` object provided.
#' @param n_cards The number of cards to deal. Default is 1.
#'
#' @param verbose A logical input (TRUE or FALSE), to determine whether the function prints the output of the cards. This argument is TRUE by default.
#'
#' @return A list with two elements:
#'   - `dealt`: The card(s) dealt.
#'   - `remaining_deck`: The updated deck with dealt cards removed.
#'
#' @export
#'
#' @examples
#' # Deal a single card from a fresh deck
#' dealt_card <- GambleR::deal_card()
#' dealt_card <- dealt_card$dealt
#' remaining_deck <- dealt_card$remaining_deck
#'
#' # Deal two cards from an existing deck
#' dealt_cards <- GambleR::deal_card(cards = remaining_deck,
#'                                   n_cards = 2)
#' dealt_cards <- dealt_cards$dealt
#' updated_deck <- dealt_cards$remaining_deck
deal_card <- function(cards = NULL,
                      n_cards = 1,
                      verbose = TRUE) {

  # if no deck is provided, use a fresh shuffled deck output by the shuffle_cards() function
  if (is.null(cards)) {
    cards <- GambleR::shuffle_cards()
  }

  # validate the deck
  if (!is.data.frame(cards) || !all(c("face", "suit", "card_value") %in% colnames(cards))) {
    stop("🃏 Invalid deck provided. Please supply a valid 52-card deck.")
  }

  # check if enough cards are left
  if (n_cards > nrow(cards)) {
    stop("Not enough cards left in the deck!")
  }

  # deal the top `n_cards`
  dealt <- cards[1:n_cards, ]
  remaining_deck <- cards[-(1:n_cards), ]

  # print dealt cards only if verbose = TRUE
  if (verbose) {
    message("\n🃟 Dealt card(s):")
    print(dealt)
  }

  # return both dealt cards and the remaining deck
  return(invisible(list(dealt = dealt, remaining_deck = remaining_deck)))
}
