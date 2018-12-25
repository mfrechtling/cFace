updateScores <- function()
{
        raw <- read.csv("./data/nba/raw_scores.csv", header=TRUE)
        scores <- data.frame(raw$Home, raw$Home.Score, raw$Visitor, raw$Visitor.Score, rep(0, length(raw$Home)), rep(0, length(raw$Home)))

        teams <- 






        colnames(scores) <- c("Home", "Score", "Away", "Score", "Elo.Home", "Elo.Away")
        write.table(scores, "./data/nba/scores.csv", row.names = FALSE, col.names = TRUE, sep=",")
}

updateElo <- function()
{
}
