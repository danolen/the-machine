## Simulate the probabilities of a team winning a game
simulate_game <- function(homeScorePred, awayScorePred, homeORaway = c("home", "away"), max_score, drawAllowed = c(TRUE, FALSE)) {
  library(tidyverse)
  score_matrix = dpois(0:max_score, awayScorePred) %o% dpois(0:max_score, homeScorePred)
  dplyr::if_else(drawAllowed == TRUE,
          dplyr::if_else(homeORaway == "home", 
                  sum(score_matrix[lower.tri(score_matrix)]), 
                  sum(score_matrix[upper.tri(score_matrix)])),
          dplyr::if_else(homeORaway == "home", 
                  sum(score_matrix[upper.tri(score_matrix)]) + (sum(diag(score_matrix))/2), 
                  sum(score_matrix[lower.tri(score_matrix)]) + (sum(diag(score_matrix))/2)))
}