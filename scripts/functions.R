monteCarlo <- function(home_mus, home_sds, away_mus, away_sds, n=1000, elo_home=1300, elo_away=1300, allowDraw=TRUE)
{
        home_coeff = elo_home / (elo_home + elo_away)
        away_coeff = elo_away / (elo_home + elo_away)
        components <- sample(1:2, prob=c(home_coeff, away_coeff), size=n, replace=TRUE)
        
        home_samples <- rnorm(n, mean=home_mus[components], sd=home_sds[components])
        away_samples <- rnorm(n, mean=away_mus[components], sd=away_sds[components])
        
        margin <- round(home_samples - away_samples, digits=0)
        if (!allowDraw)
        {
                margin <- margin[margin != 0]
                if (length(margin) < n)
                {
                        margin <- c(margin, monteCarlo(home_mus, home_sds, away_mus, away_sds, n - length(margin), elo_home, elo_away, allowDraw))
                }
        }
        
        return(margin)
}

weight.func <- function(x)
{
        return((1000^x)/1000)
}

weight.mean <- function(x)
{
        w <- seq(1/length(x), 1, 1/length(x))
        w <- weight.func(w)
        w <- w / sum(w)
        return(weighted.mean(x, w))
}

weight.var <- function(x)
{
        w <- seq(1/length(x), 1, 1/length(x))
        w <- weight.func(w)
        w <- w / sum(w)
        return(weighted.var(x, w))
}

weighted.var <- function(x, w, na.rm = FALSE)
{
        if (na.rm) { 
                w <- w[i <- !is.na(x)] 
                x <- x[i] 
        } 
        sum.w <- sum(w) 
        sum.w2 <- sum(w^2) 
        mean.w <- sum(x * w) / sum(w) 
        (sum.w / (sum.w^2 - sum.w2)) * sum(w * (x - mean.w)^2, na.rm = na.rm) 
}

get_stats_df <- function(league)
{
        tokens <- c("./data", league, "scores.csv")
        scores <- read.csv(paste(tokens, collapse = "/"), header=TRUE)
        away_scored_mean <- aggregate(scores[,4], list(scores$Away), weight.mean)
        away_scored_var <- aggregate(scores[,4], list(scores$Away), weight.var)
        
        away_conceeded_mean <- aggregate(scores[,2], list(scores$Away), weight.mean)
        away_conceeded_var <- aggregate(scores[,2], list(scores$Away), weight.var)
        
        
        home_scored_mean <- aggregate(scores[,2], list(scores$Home), weight.mean)
        home_scored_var <- aggregate(scores[,2], list(scores$Home), weight.var)
        
        home_conceeded_mean <- aggregate(scores[,4], list(scores$Home), weight.mean)
        home_conceeded_var <- aggregate(scores[,4], list(scores$Home), weight.var)
        
        stats_df <- data.frame(home_scored_mean$x, 
                               home_scored_var$x, 
                               home_conceeded_mean$x, 
                               home_conceeded_var$x, 
                               away_scored_mean$x, 
                               away_scored_var$x, 
                               away_conceeded_mean$x, 
                               away_conceeded_var$x, 
                               row.names = away_scored_mean$Group.1)
        colnames(stats_df) <- c("home_scored_mean", 
                                "home_scored_var", 
                                "home_conceeded_mean", 
                                "home_conceeded_var", 
                                "away_scored_mean", 
                                "away_scored_var", 
                                "away_conceeded_mean", 
                                "away_conceeded_var")
        return(stats_df)
        
}

get_elo_df <- function(league)
{
        tokens <- c("./data", league, "elo.csv")
        elo <- read.csv(paste(tokens, collapse = "/"), header=TRUE)
        elo_df <- data.frame(elo$ELO)
        rownames(elo_df) <- elo$Team
        return(elo_df)
}

get_results <- function(home, away, stats_df, elo_df, n=1000, allowDraw=TRUE)
{
        home_mus <- c(stats_df[home,]$home_scored_mean, stats_df[away,]$away_conceeded_mean)
        away_mus <- c(stats_df[home,]$home_conceeded_mean, stats_df[away,]$away_scored_mean)
        home_sds <- sqrt(c(stats_df[home,]$home_scored_var, stats_df[away,]$away_conceeded_var))
        away_sds <- sqrt(c(stats_df[home,]$home_conceeded_var, stats_df[away,]$away_scored_var))
        res <- monteCarlo(home_mus, home_sds, away_mus, away_sds, n, elo_df[home, ], elo_df[away, ], allowDraw)
        
        mean_margin <- mean(res)
        if (mean_margin > 0)
        {
                test_type <- "greater"
                test_index <- 1
        } else{
                test_type <- "less"
                test_index <- 2
        }
        
        results <- t.test(res, alternative=test_type, conf.level = 0.99)
        
        home_win_prob <- round((1 / (length(which(res > 0)) / n)) * (1.91/2), digits=2)
        away_win_prob <- round((1 / (length(which(res < 0)) / n)) * (1.91/2), digits=2)
        
        dens <- density(res)
        if (league == "nba") {
                home_big_win_prob <- round((1 / integrate(approxfun(dens),lower=11,upper=max(res))$value) * (1.91/2),digits=2)
                home_little_win_prob <- round((1 / integrate(approxfun(dens),lower=1,upper=10)$value) * (1.91/2),digits=2)
                away_big_win_prob <- round((1 / integrate(approxfun(dens),lower=min(res),upper=-11)$value) * (1.91/2),digits=2)
                away_little_win_prob <- round((1 / integrate(approxfun(dens),lower=-10,upper=-1)$value) * (1.91/2),digits=2)
        } else {
                home_big_win_prob <- 0
                home_little_win_prob <- 0
                away_big_win_prob <- 0
                away_little_win_prob <- 0
        }
        
        results_df <- data.frame(home=character(),
                                 away=character(),
                                 home_odds=double(),
                                 away_odds=double(),
                                 margin=double(),
                                 conf_int = double(),
                                 home_little_win=double(),
                                 home_big_win=double(),
                                 away_little_win=double(),
                                 away_big_win=double(),
                                 stringsAsFactors = FALSE)
        
        results_df[1,1:2] <- c(home, away)
        results_df[1,3:10] <- c(home_win_prob, 
                                away_win_prob, 
                                results$estimate, 
                                results$conf.int[test_index], 
                                home_little_win_prob, 
                                home_big_win_prob,
                                away_little_win_prob,
                                away_big_win_prob)
        return(results_df)
}

get_prob <- function(league, home, away, allowDraw=TRUE)
{
        tokens <- c("./data", league, "update.r")
        source(paste(tokens, collapse = "/"))
        
        updateScores()
        updateElo()
        
        stats_df <- get_stats_df(league)
        elo_df <- get_elo_df(league)
        
        res <- get_results(home, away, stats_df, elo_df, n=1000000, allowDraw)
}