# Bayesian Model Estimatation
# https://m-clark.github.io/models-by-example/bayesian-basics.html

library(tidyverse)

# prior, likelihood, and posterior distributions
## soccer goal example, binomial example using probability of goal

# Set up ------------------------------------
# outcomes
shots <- c('goal','goal','goal','miss','miss',
           'goal','goal','miss','miss','goal')

# convert to numeric, 0/1
shots_01 <- as.numeric(shots == 'goal') # did not know this function

N <- length(shots)                 # sample size
n_goals <- sum(shots == 'goals')   # number of shots made (goals??)
n_missed <- sum(shots == 'miss')   # misses

# specify binomial distribution
## calculate theta θ between .5 - .85 with N = 10
set.seed(1234)

x1 <- rbinom(1000, size = 10, p =.5)    # probabiltiy of goal is 0.5
x2 <- rbinom(1000, size = 10, p = .85) # probabiltiy of goal is 0.85

mean(x1)

qplot(x1, geom = 'histogram')

mean(x2)

qplot(x2, geom = 'histogram')

# Prior ----------------------------------
# We don't actually know theta, so we are going to estimate it
## We'll fist supply some "prior" values for it to consider
theta = seq(from   = 1/(N + 1),
            to     = N/(N + 1),
            length = 10)

# Prior here is 0.5 representing a null prior, or the most likely (either or)
### here, the example is going with a triangle distribution, representing the 
# probability desnity function (possible theta values) which put high 
# probabiltiy values around the central value.
# https://en.wikipedia.org/wiki/Triangular_distribution
p_theta = pmin(theta, 1 - theta)

# uniform
# p_theta = dunif(theta)

# beta prior with mean = .5
# p_theta = dbeta(theta, 10, 10)

# Normalize so that values sum to 1
p_theta = p_theta / sum(p_theta) 

# Likelihood -----------------------------
p_data_given_theta = choose(N, n_goals) * theta^n_goals * (1-theta)^n_missed

# Posterior ------------------------------
p_data = p_data_given_theta*p_theta  # marginal probability of the data

p_theta_given_data = p_data_given_theta*p_theta / sum(p_data)  # Bayes theorem theorem

p_df <- data.frame(theta = theta,
                   prior = p_theta,
                   likelihood = p_data_given_theta,
                   posterior = p_theta_given_data)
p_df <- round(p_df, 3)
p_df

# my answers are differnt to the example's? 
# Not sure why, but the lesson is there: peaks in table show where to look

posterior_mean = sum(p_theta_given_data*theta) # theirs is .56 which makes sense
posterior_mean

