updateScores <- function()
{
        raw <- read.csv("./data/nhl/raw_scores.csv", header=TRUE)
        scores <- data.frame(raw$Home, raw$G.1, raw$Visitor, raw$G)
        colnames(scores) <- c("Home", "Score", "Away", "Score")
        write.table(scores, "./data/nhl/scores.csv", row.names = FALSE, col.names = TRUE, sep=",")
}

updateElo <- function()
{
        raw <- read.csv("./data/nhl/raw_elo.csv", header=TRUE)
        elo <- data.frame(raw$Elo, raw$Team)
        colnames(elo) <- c("ELO", "Team")
        write.table(elo, "./data/nhl/elo.csv", row.names=FALSE, col.names = TRUE, sep=",")
}