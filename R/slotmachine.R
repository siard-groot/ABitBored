#' slotmachine
#'
#' Take a spin and gamble all your points. This slot machine has certainly not
#' been rigged, you can check the source code. The starting balance
#' is 100 points and can be increased up to five fold per spin.
#' Run the function without input to play.
#'
#' @returns Called for fun. No return value.
#' @export
#'
#' @examples
#' \dontrun {
#' slotmachine.R
#' }
slotmachine <- function() {
  # Setup
  balance <- 100
  bet <- 10
  # Define symbols
  symbols <- c(rep("CHE", 5),
               rep("LEM", 4),
               rep("ORA", 3),
               rep("BEL", 2),
               rep("BAR", 1),
               rep("777", 1))
  # Define winning conditions
  check_win <- function(spin, balance, bet) {
    if (all(spin == "CHE")) {
      balance <- (balance + 2 * bet)
      cat("Three of a kind! You win 20 points!\n")
    }
    else if (all(spin == "LEM")){
      balance <- (balance + 3 * bet)
      cat("Three of a kind! You win 20 points!\n")
    }
    else if (all(spin == "ORA")) {
      balance <- (balance + 5 * bet)
      cat("Three of a kind! You win 50 points!\n")
    }
    else if (all(spin == "BEL")) {
      balance <- (balance + 10 * bet)
      cat("Three of a kind! You win 100 points!\n")
    }
    else if (all(spin == "BAR")) {
      balance <- (balance + 20 * bet)
      cat("Big win! You win 200 points!\n")
    }
    else if (all(spin == "777")) {
      balance <- (balance + 50 * bet)
       cat("JACKPOT! You win 500 points!\n")
    }
    else if (length(unique(spin)) == 2) {
      balance <- (balance + 1.5 * bet)
      cat("A pair! You win 15 points!\n")
    }
    return(balance)
  }
  # Main game loop
  while (balance > 0) {
    cat("Your balance is", balance, "\n")
    input <- readline("The fee is 10 points, would you like to continue? (y/n): ")
    if (input == "y") {
      balance <- balance - bet
      spin <- sample(symbols, 3, replace = TRUE)
      readline("Press (enter) to spin the slotmachine")
      cat("--------------------\n")
      cat("|", spin[1], "|", spin[2], "|", spin[3], "|\n")
      cat("--------------------\n")
      balance <- check_win(spin, balance, bet)
      if (balance <= 0) {
        cat("Good run! Your final balance is: ",balance,"\n")
        break
      }
    }
    else {
      cat("You exit the casino with your pockets as full and as empty as when you entered\n")
      break
  }
  }
}
