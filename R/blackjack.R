#' blackjack
#'
#' A blackjack game with the most important features
#' including betting, ace conversion and blackjack.
#' Run the function without any inputs and follow the steps in the console.
#'
#' @returns Called for fun. No return value.
#' @export
#'
#' @examples
#' \dontrun{
#' blackjack()
#' }
blackjack <- function() {
  # Betting
  balance <- 100
  while (balance > 0) {
    cat("Your balance is",balance,"\n")
    bet <- readline("How much would you like to bet: ")
    bet <- as.numeric(bet)
    while (is.na(bet)) {
      cat("We dont accept checks, place a valid bet\n")
      bet <- readline("How much would you like to bet: ")
      bet <- as.numeric(bet)
    }
    while (balance - as.numeric(bet) < 0) {
      cat("You are not that rich! Place a smaller bet\n")
      bet <- readline("How much would you like to bet: ")
      bet <- as.numeric(bet)
    }

  # Create and shuffle deck
  deck <- rep(c(2:10, rep(10,3) ,11), 4)
  deck <- sample(deck)

  # deal hands
  player <- c(deck[1], deck [3])
  dealer <- c(deck[2], deck [4])
  deck <- deck[-(1:4)]

  # check for blackjack
  if (sum(player) == 21 && sum(dealer) == 21){
    balance <- balance + (bet*1.5)
    cat("**********************************************","\n")
    cat("                                              ","\n")
    cat("Two blackjacks? It's a tie! Players bet is returned\n")
    cat("                                              ","\n")
    cat("**********************************************","\n")
    next
  }
  else if (sum(player) != 21 && sum(dealer) == 21){
    balance <- balance - bet
    cat("**********************************************","\n")
    cat("                                              ","\n")
    cat("The dealer got a blackjack, player loses!\n")
    cat("                                              ","\n")
    cat("**********************************************","\n")
    next
  }
  else if (sum(player) == 21 && sum(dealer) != 21) {
    cat("**********************************************","\n")
    cat("                                              ","\n")
    cat("Blackjack! Player wins 1,5x of his bet!\n")
    cat("                                              ","\n")
    cat("**********************************************","\n")
    next
  }

  # players turn
  cat("Dealers first card: ",dealer[1],"\n")
  while (sum(player) < 21) {
   cat("**********************************************","\n")
   cat("Your hand is",player,"\n","Total = ",sum(player),"\n")
   input <- readline("Do you want to hit or stay? (h/s): ");
   while (input != "s" && input != "h") {
     input <- readline("Do you want to hit or stay? (h/s): ")
   }
   if (input == "h") {
     player <- c(player, deck[1])
     cat("You drew: ",deck[1],"\n")
     deck <- deck[-1]
    if (sum(player) > 21 && 11 %in% player) {
      player[which(player ==11)[1]] <- 1
      cat("Your ace got converted from 11 to 1","\n")
    }
    cat("----------------------------------------------","\n")
   }
   else {
     cat("----------------------------------------------","\n")
     break
   }
  }

  # dealers turn
  while (sum(dealer) < 17) {
    dealer <- c(dealer , deck[1])
    deck <- deck[-1]
    if (sum(dealer) > 21 && 11 %in% dealer) {
      dealer[which(dealer ==11)[1]] <- 1
      cat("Your ace got converted from 11 to 1\n")
    }
  }

  # Results
  cat("player hand = ",player,"| Total = ",sum(player),"\n")
  cat("Dealer hand = ",dealer,"| Total = ",sum(dealer),"\n")
  cat("----------------------------------------------","\n")
  cat("**********************************************","\n")
  cat("                                              ","\n")
   if (sum(player) > 21) {
     cat("Bust! Player loses!","\n")
     balance <- balance - bet
     }
    else if (sum(dealer) > 21) {
    cat("Dealer bust! Player wins!","\n")
    balance <- balance + bet
     }
    else if (sum(player) > sum(dealer)){
      cat("Player wins!","\n")
      balance <- balance + bet
     }
    else if (sum(dealer) > sum(player)) {
      cat("Dealer wins!","\n")
      balance <- balance - bet
     }
    else cat("Hands are equal, it's a tie!","\n")
    cat("                                              ","\n")
    cat("**********************************************","\n")

  # Decide if palyer continues or not
  if (balance <=0) {
    cat("Womp womp, you gambled all of your money\nand got kicked out of the casino!\n")
    break
  }
  continue <- readline("Would you like to continue? (y/n): ")
  if (continue == "n") {
    cat("Good run! Your final balance is: ",balance,"\n")
    break
    }
  }
}
