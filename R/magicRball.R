#' magicRball
#'
#' This magic R ball will provides wisdom to anyone who seeks it.
#' It has numerous data science inspired responses to any question.
#' Run the function without any inputs and follow the steps in the console.
#'
#' @returns Called for fun. No return value.
#' @export
#'
#' @examples
#' \dontrun{
#' magicRball()
#' }
magicRball <- function() {
  responses <- c(# affirmative
                  "p-value is <0.001)",
                  "Without a doubt",
                  "The server agrees",
                  "It is scientifically proven",
                  "The probabilities are in your favour",
                  "With some p-hacking, yes",
                  "Every scientist agrees unanimously",
                  "The null hypothesis was rejected",
                  "The response has been peer reviewed and accepted. Yes.",
                  sample(paste0("R-squared is 0.9",1:9)),
                  "Results were published in Nature. The answer is yes",
                  # unsure
                  "There is no known function to answer this question",
                  "Probabilities could not be quantified",
                  "Unsure, try python",
                  "The answer is 42",
                  "Initial data suggests yes, but data could not be replicated",
                  "Correlation does not imply causation",
                  # negative
                  "The server capacity is full, try again later",
                  "Toolcalling unsuccesfull",
                  "Statistically unlikely",
                  "The numbers did not align",
                  "I obtained a p-value of ... one",
                  "Insufficient data, increase sample size",
                  "Too many outliers present to predict reliably")

   # Question input
  cat("You have come to seek the wisdom of the magic R ball, reveal your question\n")
  readline("What knowledge do thy seek: ")
  readline("Shake the magicRball (enter): ")
  cat("The ball starts shaking vigorously!\n")
  cat("You watch intently as the R on the magic ball starts\n")
  cat("to emit a faint but ever increasingly bright glow.\n")
  cat("You shield your eyes against the blinding light...\n")
  readline("(enter)")

  # Print ball
  ball <- "
                      00000000000000
               200000000000000000000007
            000000000000004         000006
          00000000000000                0000
        00000000000000        0000000     0000
       00000000000000        0000000000    0000
      000000000000000       0000  05000     6000
     0000000000000000       0000   000       0000
    00000000000000000       000000000        00000
    000000000000000000      0000  000        20000
   00000000000000000000     0000   000       000000
   000000000000000000000    0000    000     0000000
   000000000000000000000000               000000000
   00000000000000000000000000000       000000000000
    0000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000
     00000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000
       0000000000000000000000000000000000000000
        70000000000000000000000000000000000000
          0000000000000000000000000000000000
             0000000000000000000000000000
                0000000000000000000000
                     000000000000

  "
cat(ball,"\n")
  cat("As you take a peek through the shivers of your fingers,\nyou notice the big R has been replaced with a string of text")
  input <- readline("Read the text? (y/n): ")
  while (input != "y" && input != "n") {
    input <- readline("Read the text? (y/n): ")
  }
  if (input == "y") {
  # Generate response
  responses <- sample(responses)
  cat(strrep("*", nchar(responses[1])+2), "\n")
  cat(strrep(" ", nchar(responses[1])+2), "\n")
  cat(responses[1],"\n")
  cat(strrep(" ", nchar(responses[1])+2), "\n")
  cat(strrep("*", nchar(responses[1])+2), "\n")
  }
  else {
    cat("The text dissapears as you exit the function, the eluded knowlege is forever lost to time\n")
  }
}
