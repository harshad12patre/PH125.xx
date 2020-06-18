#1a.What is the probability of guessing correctly for one question?
1/5

#1b What is the expected value of points for guessing on one question?
1/5 * 1 + -0.25* 4/5

# 1c What is the expected score of guessing on all 44 questions?
mu <- 44 * (1/5 * 1 + -0.25* 4/5)
mu

#1d What is the standard error of guessing on all 44 questions?
er <- sqrt(44) * abs(1+0.25) * sqrt(1/5*4/5)
er

#1e Use the Central Limit Theorem to determine the probability that a guessing student
#  scores 8 points or higher on the test.
1-pnorm(8,mu,er)

#1f Set the seed to 21, then run a Monte Carlo simulation of 10,000 students guessing
#  on the test.
# What is the probability that a guessing student scores 8 points or higher?
set.seed(21)
X <- replicate(10000, {
  sum(sample(c(1,-0.25), size=44, replace=TRUE, prob=c(1/5,4/5)))
})
sum(X>=8)/10000



# Suppose that the number of multiple choice options is 4 and that there is no
#  penalty for guessing - that is, an incorrect question gives a score of 0.
#2a What is the expected value of the score when guessing on this new test?
mu <- 44 * (1/4 * 1 + 0* 3/4)

#2b What is the probability of scoring over 30 when guessing? Report your answer
#  using scientific notation with 3 significant digits in the format x.xx*10^y.
p <- seq(0.25, 0.95, 0.05)
exp_val <- sapply(p, function(x){
  mu <- n * a*x + b*(1-x)
  sigma <- sqrt(n) * abs(b-a) * sqrt(x*(1-x))
  1-pnorm(35, mu, sigma)
})

min(p[which(exp_val > 0.8)])

# 2cConsider a range of correct answer probabilities p <- seq(0.25, 0.95, 0.05)
#  representing a range of student skills.
# What is the lowest p such that the probability of scoring over 35 exceeds 80%?
p <- seq(0.25, 0.95, 0.05)

fu <- function(p){
  # calculate the expected value at given p
  expected_value <- 44 * (1*p + 0*(1-p))
  # calculate the standard error at given p
  standard_error <- sqrt(44) * abs(1 - 0) * sqrt(p*(1 - p))
  # calculate likelihood of score of 35 or greater
  1-pnorm(35, expected_value, standard_error)
}

sapply(p, FUN=fu)


# A casino offers a House Special bet on roulette, which is a bet on five pockets
#  (00, 0, 1, 2, 3) out of 38 total pockets. The bet pays out 6 to 1. In other
#  words, a losing bet yields -$1 and a successful bet yields $6. A gambler wants
#  to know the chance of losing money if he places 500 bets on the roulette House
#  Special.
#3a What is the expected value of the payout for one bet?
(6*5/38 + -1*(1 - 5/38))

#3bWhat is the standard error of the payout for one bet?
abs(-1 - 6) * sqrt(5/38*(1 - 5/38))

#3c What is the expected value of the average payout over 500 bets? Remember there
#  is a difference between expected value of the average and expected value of
#  the sum. Same as one bet.
(6*5/38 + -1*(1 - 5/38))

#3d What is the standard error of the average payout over 500 bets? Remember there
# is a difference between the standard error of the average and standard error of
# the sum.
(abs(-1 - 6) * sqrt(5/38*(1 - 5/38)))/sqrt(500)

#3e What is the expected value of the sum of 500 bets?
mu <- 500 * (6*5/38 + -1*(1 - 5/38))
mu

#3f What is the standard error of the sum of 500 bets?
er <- sqrt(500) * (abs(-1 - 6) * sqrt(5/38*(1 - 5/38)))
er

#3g Use pnorm with the expected value of the sum and standard error of the sum to
# calculate the probability of losing money over 500 bets,  Pr(???????0) .
pnorm(0, mu, er)





