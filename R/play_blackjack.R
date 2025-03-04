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

player_turn <- function(deck,
                        hand) {

  while (TRUE) {
    message("\nYour current hand:")
    print(hand)
    hand_value <- calculate_hand_value(hand)
    message("Hand value: ", hand_value)

    # ask the player what they want to do
    action <- readline("▶️ STICK or TWIST? (s/t): ")

    if (tolower(action) == "s") {
      cat("🤗 You chose to STICK\n")
      Sys.sleep(2)  # delay before dealer acts
      break
    } else if (tolower(action) == "t") {
      cat("👹 You chose to TWIST...\n")
      Sys.sleep(2)  # delay before player acts
      new_card <- GambleR::deal_card(deck,
                                     n_cards = 1,
                                     verbose = TRUE)
      hand <- rbind(hand,
                    new_card$dealt)
      deck <- new_card$remaining_deck

      # check if the player busted
      if (calculate_hand_value(hand) > 21) {
        break
      }
    } else {
      message("⚠️ Invalid input. Please type 's' to stick or 't' to twist.")
    }
  }
  return(list(hand = hand,
              remaining_deck = deck))
}

# dealer_turn ------------------------------------------------------------------

dealer_turn <- function(deck,
                        hand,
                        player_value) {

  cat("🤵 Dealer reveals their second card...\n")
  Sys.sleep(2)  # delay before dealer acts
  print(hand)

  while (TRUE) {
    dealer_value <- calculate_hand_value(hand)

    # dealer busts if going over 21
    if (dealer_value > 21) {
      message("❌ Dealer busts! You win automatically.")
      return(list(hand = hand,
                  remaining_deck = deck,
                  dealer_bust = TRUE))
    }

    # dealer MUST stick at 17+ - official casino rules
    if (dealer_value >= 17 || dealer_value >= player_value) {
      cat("✅ Dealer sticks.\n")
      Sys.sleep(2)  # delay before dealers cards are shown
      break
    }

    # dealer must twist at 16 or lower
    cat("🃏 Dealer twists...\n")
    Sys.sleep(2)  # delay before dealers card is shown
    new_card <- GambleR::deal_card(deck,
                                   n_cards = 1,
                                   verbose = FALSE)
    hand <- rbind(hand,
                  new_card$dealt)
    deck <- new_card$remaining_deck
    message("Dealer's new hand:")
    print(hand)
  }

  return(list(hand = hand,
              remaining_deck = deck,
              dealer_bust = FALSE))
}

# determine_winner -------------------------------------------------------------

determine_winner <- function(player_hand,
                             dealer_hand) {

  player_value <- calculate_hand_value(player_hand)
  dealer_value <- calculate_hand_value(dealer_hand)

  message("Final Scores:\n")
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
#' @description
#' Blackjack is a classic casino game where the goal is to beat the dealer by getting as close to 21 as possible without going over.
#'
#' ## House Rules:
#' - Aces can count as **1 or 11**.
#' - Face cards (Jack, Queen and King) are worth **10**.
#' - You can **stick** (stay with your current hand) or **twist** (hit for another card).
#' - Dealer must **twist on 16 or lower**.
#' - If a players first two cards make **21**, and contain an Ace it's Blackjack! (Automatic win unless dealer also has it, resulting in a draw).
#'
#' @param bet A positive integer representing the amount you're willing to gamble (and probably lose). Default is `10`.
#' @param currency A string representing the currency the user wishes to play with. Defaults to GBP (`£`), but feel free to flex in dollars, euros, or even Monopoly money.
#'
#' @return A log of the game, showing the cards drawn and the final outcome.
#' @export
#'
#' @examples
#' # Playing it safe with the default £10 stake
#' GambleR::play_blackjack()
#'
#' # Big-doggin' it
#' GambleR::play_blackjack(bet = 500,
#'                         currency = "£")
play_blackjack <- function(bet = 10,
                           currency = "£") {

  deck <- GambleR::shuffle_cards()
  player_hand <- GambleR::deal_card(deck,
                                    n_cards = 2,
                                    verbose = FALSE)
  deck <- player_hand$remaining_deck
  dealer_hand <- GambleR::deal_card(deck,
                                    n_cards = 2,
                                    verbose = FALSE)
  deck <- dealer_hand$remaining_deck

  cat("Dealing cards...")
  Sys.sleep(2)  # delay before showing dealt cards

  # show dealer's first card only
  message("\n[Dealer's face-up card:]")
  print(dealer_hand$dealt[1, ])

  # calculate initial hand values
  player_value <- calculate_hand_value(player_hand$dealt)
  dealer_value <- calculate_hand_value(dealer_hand$dealt)

  # check for Blackjack (Ace + 10-value card)
  player_blackjack <- (nrow(player_hand$dealt) == 2 &&
                         player_value == 21 &&
                         stringr::str_detect(string = player_hand$dealt$face, pattern = "Jack|Queen|King|10"))

  dealer_blackjack <- (nrow(dealer_hand$dealt) == 2 &&
                         dealer_value == 21 &&
                         stringr::str_detect(string = dealer_hand$dealt$face, pattern = "Jack|Queen|King|10"))

  # instant win conditions for blackjack
  if (player_blackjack && dealer_blackjack) {
    return("😨 Both you and the dealer have Blackjack! It's a draw.")
  } else if (player_blackjack) {
    return(paste0("🍾 Blackjack!!! You win ", currency, bet * 2.5))  # Typically pays 3:2
  } else if (dealer_blackjack) {
    return(paste0("💔 Dealer has Blackjack, you lose ", currency, bet))
  }

  # player's turn
  player_result <- player_turn(deck,
                               player_hand$dealt)
  player_value <- calculate_hand_value(player_result$hand)

  # if player busts, game ends immediately
  if (player_value > 21) {
    return(paste0("🃏 BUST! You lose ", currency, bet))
  }

  # dealer's turn
  dealer_result <- dealer_turn(deck,
                               dealer_hand$dealt,
                               player_value)

  # if dealer busts, player wins
  if (dealer_result$dealer_bust) {
    return(paste0("💸 You win ", currency, bet * 2))
  }

  # determine the winner
  winner <- determine_winner(player_result$hand,
                             dealer_result$hand)

  if (winner == "player") {
    return(paste0("🤑 You won ", currency, bet * 2))
  } else if (winner == "dealer") {
    return(paste0("😔 Dealer wins. You lose ", currency, bet))
  } else {
    return("🤝 It's a draw.")
  }
}

