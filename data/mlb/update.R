updateScores <- function()
{
        raw <- read.csv("./data/mlb/raw_scores.csv", header=TRUE)
        scores <- data.frame(raw$team1, raw$score1, raw$team2, raw$score2)
        scores <- scores[complete.cases(scores),]
        colnames(scores) <- c("Home", "Score", "Away", "Score")
        write.table(scores, "./data/mlb/scores.csv", row.names = FALSE, col.names = TRUE, sep=",")
}

updateElo <- function()
{
}
