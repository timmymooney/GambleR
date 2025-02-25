#' Standard Deck of Playing Cards
#'
#' @description A complete 52-card deck including face values, suits, and assigned card values.
#'
#' @format A data frame with 52 rows and 3 variables:
#' \describe{
#'   \item{face}{Character: The rank of the card ("Ace", "2", ..., "King").}
#'   \item{suit}{Character: The suit of the card ("♥Hearts", "♦Diamonds", "♣Clubs", "♠Spades").}
#'   \item{card_value}{Numeric: The assigned value (Ace = 1, 2-10 as is, Jack/Queen/King = 10).}
#' }
#'
#' @examples
#' # Load the standard deck
#' deck <- GambleR::deck_of_cards
#'
#' # Shuffle the deck
#' shuffled_deck <- GambleR::shuffle_cards(deck)
"deck_of_cards"
