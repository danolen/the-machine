## Simulate the probabilities of a team going over/under a given team total
simulate_team_total <- function(ScorePred, overORunder = c("over", "under"), team_total, max_score) {
  library(tidyverse)
  score_prob = dpois(0:max_score, ScorePred)
  score = 0:max_score
  score_matrix = cbind(score, score_prob) %>%
    as.data.frame()
  dplyr::if_else(overORunder == "over",
          sum(subset(score_matrix, score > team_total)$score_prob),
          sum(subset(score_matrix, score < team_total)$score_prob))
}