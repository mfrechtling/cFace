setwd("F:/Work/R/cFace")
source("./scripts/functions.R")
league <- "nba"
home <- "Sacramento Kings"
away <- "Detroit Pistons"

print(get_prob(league, home, away, FALSE))