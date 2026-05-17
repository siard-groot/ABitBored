#' Tic Tac Toe
#'
#' A multiplayer game of Tic Tac Toe. The function automatically alternates
#' between turns and declares a winner when a win condition is met.
#' Run the function without input and follow the steps in the console,
#' alternating between players.
#'
#' @returns Called for fun. No return value.
#' @export
#'
#' @examples
#' \dontrun{
#' tictactoe()
#' }
tictactoe <- function() {
  # Print the grid
  grid <- matrix(1:9, nrow =3, ncol=3, byrow=TRUE)
  print_grid <- function(grid) {
    for (i in 1:3) {
      cat(" ",grid[i,1],"|",grid[i,2],"|",grid[i, 3],"\n")
      if (i < 3) cat(" ---|---|---\n")
  }
}
# Play the game
turn <- "player X"
result <- FALSE
while (result == FALSE) {
  # Turn player 1
  if (turn == "player X") {
    cat("Turn: ",turn,"\n")
    print_grid(grid)
    input <- as.integer(readline("Choose placement: "))
    while (!input %in% 1:9) {
      input <- as.integer(readline("Choose placement: "))
    }
    if (input %in% grid) {
    grid[which(grid == input)[1]] <- "X"
    }
    turn <- "player O"
  }
  # Turn player 2
  else {
    cat("Turn: ",turn,"\n")
    print_grid(grid)
    input <- as.integer(readline("Choose placement: "))
    while (!input %in% 1:9) {
      input <- as.integer(readline("Choose placement: "))
    }
    if (input %in% grid) {
      grid[which(grid == input)[1]] <- "O"
    }
    turn <- "player X"
  }
  # Check for winner
  check_winner <- function(mark) {
    (grid[1,1] == mark && grid[1,2] == mark && grid[1,3] == mark) ||
    (grid[2,1] == mark && grid[2,2] == mark && grid[2,3] == mark) ||
    (grid[3,1] == mark && grid[3,2] == mark && grid[3,3] == mark) ||
    (grid[1,1] == mark && grid[2,1] == mark && grid[3,1] == mark) ||
    (grid[1,2] == mark && grid[2,2] == mark && grid[3,2] == mark) ||
    (grid[1,3] == mark && grid[2,3] == mark && grid[3,3] == mark) ||
    (grid[1,1] == mark && grid[2,2] == mark && grid[3,3] == mark) ||
    (grid[3,1] == mark && grid[2,2] == mark && grid[1,3] == mark)
  }
  if (check_winner("X") == TRUE) {
    print_grid(grid)
    result <- TRUE
    winner <- "player X"
  }
  else if (check_winner("O") == TRUE) {
    print_grid(grid)
    result <- TRUE
    winner <- "player O"
  }
}
cat("\n")
cat(strrep("*", nchar(winner[1])+15), "\n")
cat(strrep(" ", nchar(winner[1])+15), "\n")
cat(winner," wins!\n")
cat(strrep(" ", nchar(winner[1])+15), "\n")
cat(strrep("*", nchar(winner[1])+15), "\n")
}
