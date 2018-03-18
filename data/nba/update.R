updateScores <- function()
{
        raw <- read.csv("./data/nba/raw_scores.csv", header=TRUE)
        scores <- data.frame(raw$Home, raw$Home.Score, raw$Visitor, raw$Visitor.Score)
        colnames(scores) <- c("Home", "Score", "Away", "Score")
        write.table(scores, "./data/nba/scores.csv", row.names = FALSE, col.names = TRUE, sep=",")
}

updateElo <- function()
{
}