## Simulate the probabilities of a team covering a given spread
simulate_spread <- function(homeScorePred, awayScorePred, homeORaway = c("home", "away"), spread, max_score) {
  library(tidyverse)
  score_matrix = dpois(0:max_score, homeScorePred) %o% dpois(0:max_score, awayScorePred)
  colnames(score_matrix) = 0:max_score
  rownames(score_matrix) = 0:max_score
  score_matrix = tibble::rownames_to_column(as.data.frame(score_matrix)) %>% 
    reshape2::melt(id = "rowname")
  colnames(score_matrix) = c("home_score", "away_score", "prob")
  score_matrix = dplyr::mutate(score_matrix,
                        home_score = as.numeric(home_score),
                        away_score = as.numeric(away_score) - 1,
                        home_margin = away_score - home_score,
                        away_margin = home_score - away_score)
  dplyr::if_else(homeORaway == "home",
          sum(subset(score_matrix, home_margin < spread)$prob),
          sum(subset(score_matrix, away_margin < spread)$prob))
}