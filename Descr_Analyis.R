## Set Random data 

set.seed(127)
lambda <- 0.2
n <- 40
sample_size <- 1000
simulated_sample <- replicate(sample_size, rexp(n, lambda))
means_exponentials <- apply(simulated_sample, 2, mean)

##  Meam Comparison
sample_mean <- mean(means_exponentials)
theo_mean <- 1 / lambda
sample_mean
theo_mean
mat.mean  <- (sample_mean / theo_mean)
mat.mean

##  Variance Comparison
sample_var <- var(means_exponentials)
theo_var  <- (1 / lambda)^2 / (n) 
sample_var
theo_var
mat.Var  <- (sample_var / theo_var)

sample_sd <- sd(means_exponentials)
theo_sd  <- 1/(lambda * sqrt(n))
sample_sd
theo_sd
mat.sd  <- (sample_sd / theo_sd)
mat.sd

## Distribution _ Plotting 
plotdata <- data.frame(means_exponentials)
m <- ggplot(plotdata, aes(x =means_exponentials))
m <- m + geom_histogram(aes(y=..density..), colour="grey",
                        fill = "grey66")
m <- m + labs(title = "Distribution of means of 40 Samples", x = "Mean of 40 Samples", y = "Density")
m <- m + geom_vline(aes(xintercept = sample_mean, colour = "sample"))
m <- m + geom_vline(aes(xintercept = theo_mean, colour = "theoretical"))
m <- m + stat_function(fun = dnorm, args = list(mean = sample_mean, sd = sample_sd), color = "gold1", size = 1.0)
m <- m + stat_function(fun = dnorm, args = list(mean = theo_mean, sd = theo_sd), colour = "red", size = 1.0)
m