## Simulate the probabilities of a game going over/under a given total
simulate_total <- function(homeScorePred, awayScorePred, overORunder = c("over", "under"), total, max_score) {
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
                        total_score = home_score + away_score)
  dplyr::if_else(overORunder == "over",
          sum(subset(score_matrix, total_score > total)$prob),
          sum(subset(score_matrix, total_score < total)$prob))
}