#1.One ball will be drawn at random from a box containing: 3 cyan balls, 5 magenta balls, and 7 yellow balls.

#What is the probability that the ball will be cyan?
A <- rep(c("cyan","magenta","yello"),times=c(3,5,7))
mean(A=="cyan")
#2. What is the probability that the ball will not be cyan?
 mean(A!= "cyan")
#3.What is the probability that the first draw is cyan and that the second draw is not cyan?
 p_2 <- 1 - (cyan - 1) / (cyan + magenta + yellow - 1)
 p_1 * p_2
#4. Now repeat the experiment, but this time, after taking the first draw and recording the color, return it back to the box and shake the box. We call this sampling with replacement.
 
 #What is the probability that the first draw is cyan and that the second draw is not cyan?
Event <- mean(A!="cyan") * mean(A=="cyan") 
Event
cyan <- 3
magenta <- 5
yellow <- 7
p <- cyan+magenta+yellow
# The variable 'p_1' is the probability of choosing a cyan ball from the box on the first draw.
p_1 <- cyan / (cyan + magenta + yellow)

# Assign a variable 'p_2' as the probability of not choosing a cyan ball on the second draw with replacement.
p_2 <- 1-p_1
# Calculate the probability that the first draw is cyan and the second draw is not cyan using `p_1` and `p_2`.
p_1*p_2
