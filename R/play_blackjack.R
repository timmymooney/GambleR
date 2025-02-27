# calculate_hand_value ---------------------------------------------------------

calculate_hand_value <- function(hand) {

  # sum the values, treating Aces as 1 initially
  total_value <- sum(hand$card_value)

  # count the number of Aces in the hand
  num_aces <- sum(hand$face == "Ace")

  # adjust Aces from 1 to 11 if it doesn't cause a bust
  while (num_aces > 0 && total_value + 10 <= 21) {
    total_value <- total_value + 10
    num_aces <- num_aces - 1
  }

  return(total_value)
}

# player_turn ------------------------------------------------------------------

player_turn <- function(deck, hand) {

  while (TRUE) {
    message("\nYour current hand:")
    print(hand)
    hand_value <- calculate_hand_value(hand)
    message("Hand value: ", hand_value)

    # ask the player what they want to do
    action <- readline("▶️ STICK or TWIST? (s/t): ")

    if (tolower(action) == "s") {
      message("🤗 You chose to STICK")
      break
    } else if (tolower(action) == "t") {
      message("👹 You chose to TWIST...")
      new_card <- GambleR::deal_card(deck,
                                     n_cards = 1,
                                     verbose = TRUE)
      hand <- rbind(hand, new_card$dealt)
      deck <- new_card$remaining_deck

      # check if the player busted
      if (calculate_hand_value(hand) > 21) {
        #message("🃏 BUST! You went over 21.")
        break
      }
    } else {
      message("⚠️ Invalid input. Please type 's' to stick or 't' to twist.")
    }
  }
  return(list(hand = hand, remaining_deck = deck))
}

# dealer_turn ------------------------------------------------------------------

dealer_turn <- function(deck, hand) {

  message("\n🤵 Dealer reveals their second card:")
  print(hand)

  while (calculate_hand_value(hand) < 17) { # need change this, so it's more realistic!🤔
    message("🃏 Dealer twists...")
    new_card <- GambleR::deal_card(deck,
                                   n_cards = 1,
                                   verbose = FALSE)
    hand <- rbind(hand, new_card$dealt)
    deck <- new_card$remaining_deck
    message("Dealer's new hand:")
    print(hand)

    # check if dealer busts
    if (calculate_hand_value(hand) > 21) {
      message("❌ Dealer busts! You win automatically.")
      return(list(hand = hand, remaining_deck = deck, dealer_bust = TRUE))  # ensure ealer_bust is always included
    }
  }

  message("✅ Dealer chose to STICK")
  return(list(hand = hand, remaining_deck = deck, dealer_bust = FALSE))  # explicitly return dealer_bust = FALSE
}

# determine_winner -------------------------------------------------------------

determine_winner <- function(player_hand, dealer_hand) {

  player_value <- calculate_hand_value(player_hand)
  dealer_value <- calculate_hand_value(dealer_hand)

  message("Final Scores:")
  message("Your total: ", player_value)
  message("Dealer's total: ", dealer_value)

  if (player_value > 21) {
    return("dealer")
  } else if (dealer_value > 21) {
    return("player")
  } else if (player_value > dealer_value) {
    return("player")
  } else if (dealer_value > player_value) {
    return("dealer")
  } else {
    return("draw")
  }
}

# play_blackjack ---------------------------------------------------------------

#' Step up and face the dealer in Blackjack!
#'
#' @param bet A positive integer representing the amount you're willing to gamble (and probably lose). Default is `10`.
#' @param currency A string representing the currency the user wishes to play with. Defaults to GBP (`£`), but feel free to flex in dollars, euros, or even Monopoly money.
#'
#' @return A log of the game, showing the cards drawn and the final outcome.
#' @export
#'
#' @examples
#' # Playing it safe with the default £10 stake
#' play_blackjack()
#'
#' # Big-dogging it
#' play_blackjack(bet = 500,
#'                currency = "£")
play_blackjack <- function(bet = 10,
                           currency = "£") {

  # first, set up the game and deal cards
  deck <- GambleR::shuffle_cards()
  player_hand <- GambleR::deal_card(deck,
                                    n_cards = 2,
                                    verbose = FALSE)
  deck <- player_hand$remaining_deck
  dealer_hand <- GambleR::deal_card(deck,
                                    n_cards = 2,
                                    verbose = FALSE)
  deck <- dealer_hand$remaining_deck

  message("Dealing cards...")

  # show dealer's first card
  message("\n[Dealer's face-up card:]")
  print(dealer_hand$dealt[1, ])

  # player's turn
  player_result <- player_turn(deck, player_hand$dealt)

  # if logic, stating that if the player busts, game ends
  if (calculate_hand_value(player_result$hand) > 21) {
    return(paste0("🃏 BUST! You lose: ", currency, bet))
  }

  # dealer's turn
  dealer_result <- dealer_turn(deck, dealer_hand$dealt)

  # if dealer busts, player wins
  if (dealer_result$dealer_bust) {
    return(paste0("🎉 Dealer busts! You win! Prize: ", currency, bet * 2))
  }

  # now determine the winner
  winner <- determine_winner(player_result$hand, dealer_result$hand)

  if (winner == "player") {
    return(paste0("🤑 You won: ", currency, bet * 2))
  } else if (winner == "dealer") {
    return(paste0("😔 Dealer wins. You lose: ", currency, bet))
  } else {
    return("🤝 It's a draw.")
  }
}
