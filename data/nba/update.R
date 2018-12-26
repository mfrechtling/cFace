updateScores <- function()
{
        raw <- read.csv("./data/nba/raw_scores.csv", header=TRUE)
        scores <- data.frame(raw$Home, raw$Home.Score, raw$Visitor, raw$Visitor.Score, rep(0, length(raw$Home)), rep(0, length(raw$Home)))
        colnames(scores) <- c("Home", "Score", "Away", "Score", "Elo.Home", "Elo.Away")
        teams <- as.character(unique(scores$Home))
        elo_vec <- as.numeric(rep(1300, length(teams)))
        elo <- data.frame(teams, elo_vec)
        print(elo)
        for (i in 1:nrow(scores))
        {
            print(i)
            home <- scores$Home[i]
            away <- scores$Away[i]

            print(home)
            print(away)

            scores$Elo.Home[i] <- elo[scores$Home[i], 2]
            scores$Elo.Away[i] <- elo[scores$Away[i], 2]
            elo_new <- update_elo(scores[i, 2], scores[i, 4], elo[scores$Home[i], 2], elo[scores$Away[i], 2])
            print(elo_new)
            elo[as.character(scores$Home[i]), 2] <- elo_new[1]
            elo[as.charactre(scores$Away[i]), 2] <- elo_new[2]
        }
        #write.table(scores, "./data/nba/scores.csv", row.names = FALSE, col.names = TRUE, sep=",")
}

updateElo <- function()
{
}

update_elo <- function(home_score, away_score, elo_home, elo_away)
{
    r_h <- 10^(elo_home / 400)
    r_a <- 10^(elo_away / 400)
    e_h <- r_h / (r_h + r_a)
    e_a <- r_a / (r_h + r_a)
    rp_h <- r_h + (30*(home_score - e_h))
    rp_a <- r_a + (30*(away_score - e_a))
    return(c(rp_h, rp_a))
}
