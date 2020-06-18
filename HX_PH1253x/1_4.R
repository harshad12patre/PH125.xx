library(gtools)
library(tidyverse)


#In the 200m dash finals in the Olympics, 8 runners compete for 3 medals (order matters). In the 2012 Olympics, 3 of the 8 runners were from Jamaica and the other 5 were from different countries. The three medals were all won by Jamaica (Usain Bolt, Yohan Blake, and Warren Weir).Use the information above to help you answer the following four questions.

#1a.How many different ways can the 3 medals be distributed across 8 runners?
str(permutations(8,3))

#1b.How many different ways can the three medals be distributed among the 3 runners from Jamaica?
str(permutations(3,3))

#1c.What is the probability that all 3 medals are won by Jamaica?
df <- combinations(8,3)
# 56 total combinations, only one with 1, 2, 3

#1d.Run a Monte Carlo simulation on this vector representing the countries of the 8 runners in this race:

runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
#For each iteration of the Monte Carlo simulation, within a replicate() loop, select 3 runners representing the 3 medalists and check whether they are all from Jamaica. Repeat this simulation 10,000 times. Set the seed to 1 before running the loop.

#Calculate the probability that all the runners are from Jamaica.
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
set.seed(1)

# Run Monte Carlo 10k
B <- 10000
results <- replicate(B, {
  winners <- sample(runners, 3)
  (winners[1] %in% "Jamaica" & winners[2] %in% "Jamaica" & winners[3] %in% "Jamaica")
})
mean(results)



#A restaurant manager wants to advertise that his lunch special offers enough choices to eat different meals every day of the year. He doesn't think his current special actually allows that number of choices, but wants to change his special if needed to allow at least 365 choices.

A meal at the restaurant includes 1 entree, 2 sides, and 1 drink. He currently offers a choice of 1 entree from a list of 6 options, a choice of 2 different sides from a list of 6 options, and a choice of 1 drink from a list of 2 options.

#2a.How many meal combinations are possible with the current menu?
combinations(6,2)
6*15*2

#2b.The manager has one additional drink he could add to the special.How many combinations are possible if he expands his original special to 3 drink options?
6*15*3

#2c.The manager decides to add the third drink but needs to expand the number of options. The manager would prefer not to change his menu further and wants to know if he can meet his goal by letting customers choose more sides.How many meal combinations are there if customers can choose from 6 entrees, 3 drinks, and select 3 sides from the current 6 options?
combinations(6,3)
6*3*20

#2d.The manager is concerned that customers may not want 3 sides with their meal. He is willing to increase the number of entree choices instead, but if he adds too many expensive options it could eat into profits. He wants to know how many entree choices he would have to offer in order to meet his goal.- Write a function that takes a number of entree choices and returns the number of meal combinations possible given that number of entree options, 3 drink choices, and a selection of 2 sides from 6 options.- Use sapply() to apply the function to entree option counts ranging from 1 to 12.What is the minimum number of entree options required in order to generate more than 365 combinations?
f <- function(entree){
  print(3*15*entree)
}

# Use sapply to apply the function to entree option counts ranging from 1 to 12.
# What is the minimum number of entree options required in order to generate more
#  than 365 combinations?
options <- seq(1:12)
sapply(options, f)


#2e.The manager isn't sure he can afford to put that many entree choices on the lunch menu and thinks it would be cheaper for him to expand the number of sides. He wants to know how many sides he would have to offer to meet his goal of at least 365 combinations.- Write a function that takes a number of side choices and returns the number of meal combinations possible given 6 entree choices, 3 drink choices, and a selection of 2 sides from the specified number of side choices.- Use sapply() to apply the function to side counts ranging from 2 to 12.What is the minimum number of side options required in order to generate more than 365 combinations?
ff <- function(sides){
  3*6*nrow(combinations(sides,2))
}

# Use sapply to apply the function to side counts ranging from 2 to 12.
# What is the minimum number of side options required in order to generate
#  more than 365 combinations?
options <- 2:12
sapply(options, ff)

head(esoph)


#Case-control studies help determine whether certain exposures are associated with outcomes such as developing cancer. The built-in dataset esoph contains data from a case-control study in France comparing people with esophageal cancer (cases, counted in ncases) to people without esophageal cancer (controls, counted in ncontrols) that are carefully matched on a variety of demographic and medical characteristics. The study compares alcohol intake in grams per day (alcgp) and tobacco intake in grams per day (tobgp) across cases and controls grouped by age range (agegp).
#The following three parts have you explore some basic characteristics of the dataset.

#Each row contains one group of the experiment. Each group has a different combination of age, alcohol consumption, and tobacco consumption. The number of cancer cases and number of controls (individuals without cancer) are reported for each group.

#3a.How many groups are in the study?
nrow(esoph)


#3b.How many cases are there?
#Save this value as all_cases for later problems.
all_cases <- sum(esoph$ncases)

#3c.How many controls are there?
#Save this value as all_controls for later problems.
all_controls <- sum(esoph$ncontrols)

#The following four parts ask you to explore some probabilities within this dataset related to alcohol and tobacco consumption.

#4a.What is the probability that a subject in the highest alcohol consumption group is a cancer case?
esoph %>% filter(alcgp == "120+") %>%
  summarize(sum_cases=sum(ncases), tot=sum(ncontrols) + sum(ncases), probability=sum_cases/tot)

#4b.What is the probability that a subject in the lowest alcohol consumption group is a cancer case?
esoph %>% filter(alcgp == "0-39g/day") %>%
  summarize(sum_cases=sum(ncases), tot=sum(ncontrols)+sum(ncases), probability=sum_cases/tot)

#4c.Given that a person is a case, what is the probability that they smoke 10g or more a day?
esoph %>% summarize(tot_cases = sum(ncases))
esoph %>% filter(tobgp != "0-9g/day") %>%
  summarize(smoking10_cases = sum(ncases))
122/200
#4d.Given that a person is a control, what is the probability that they smoke 10g or more a day?
esoph %>% summarize(tot_cases = sum(ncontrols))
esoph %>% filter(tobgp != "0-9g/day") %>%
  summarize(smoking10_cases = sum(ncontrols))
450/975


#The following four parts look at probabilities related to alcohol and tobacco consumption among the cases.

#5a.For cases, what is the probability of being in the highest alcohol group?
esoph %>% filter(alcgp == "120+") %>%
  summarize(sum_cases=sum(ncases))
45/all_cases

#5b.For cases, what is the probability of being in the highest tobacco group?
esoph %>% filter(tobgp == "30+") %>%
  summarize(sum_cases=sum(ncases))
31/all_cases

#5c.For cases, what is the probability of being in the highest alcohol group and the highest tobacco group?
esoph %>% filter(alcgp == "120+" & tobgp =="30+") %>%
  summarize(sum_cases = sum(ncases))
10/all_cases


#5d.For cases, what is the probability of being in the highest alcohol group or the highest tobacco group?
esoph %>% filter(alcgp == "120+" | tobgp =="30+") %>%
  summarize(sum_cases = sum(ncases))
66/all_cases


#The following six parts look at probabilities related to alcohol and tobacco consumption among the controls and also compare the cases and the controls.


#6a.For controls, what is the probability of being in the highest alcohol group?
esoph %>% filter(alcgp == "120+") %>%
  summarize(contr_sum = sum(ncontrols), probability = contr_sum/all_controls)

#6b.How many times more likely are cases than controls to be in the highest alcohol group?
esoph %>% filter(alcgp == "120+") %>%
  summarize(contr_sum = sum(ncontrols), case_sum = sum(ncases),
            co_prob = contr_sum/all_controls, ca_prob = case_sum/all_cases,
            ratio = ca_prob/co_prob)

#6c. For controls, what is the probability of being in the highest tobacco group?
esoph %>% filter(tobgp == "30+") %>%
  summarize(contr_sum = sum(ncontrols), probability = contr_sum/all_controls)

# 6d.For controls, what is the probability of being in the highest alcohol group and
#  the highest tobacco group?
esoph %>% filter(tobgp == "30+" & alcgp == "120+") %>%
  summarize(contr_sum = sum(ncontrols), probability = contr_sum/all_controls)

# 6e.For controls, what is the probability of being in the highest alcohol group or
#  the highest tobacco group?
esoph %>% filter(tobgp == "30+" | alcgp == "120+") %>%
  summarize(contr_sum = sum(ncontrols), probability = contr_sum/all_controls)

# 6f.How many times more likely are cases than controls to be in the highest alcohol
#  group or the highest tobacco group?
esoph %>% filter(alcgp == "120+" | tobgp == "30+") %>%
  summarize(contr_sum = sum(ncontrols), case_sum = sum(ncases),
            co_prob = contr_sum/all_controls, ca_prob = case_sum/all_cases,
            ratio = ca_prob/co_prob)