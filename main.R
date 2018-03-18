setwd("F:/Work/R/cFace")
source("./scripts/functions.R")
league <- "nba"
home <- "Minnesota Timberwolves"
away <- "Houston Rockets"

print(get_prob(league, home, away, FALSE))