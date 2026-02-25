# Sara Reathaford
# AQMSS2
# Homework 3

# STAR Dataset

# 2.1 Data Preparation

install.packages("tidyverse")
library(tidyverse)
install.packages("dplyr")
library(dplyr)
install.packages("modelsummary")
library(modelsummary)
install.packages("marginaleffects")
library(marginaleffects)

# a) Load star.csv and create the same factor variables as in Assignment 2: classtype
# with labels "Small", "Regular", "Regular+Aide", and race with labels "White", "Black", etc.

starraw <- read.csv("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/star/star.csv")

df <- starraw 

df <- df %>% mutate(classtype = factor(classtype, levels = c(1,2,3), labels = c("Small", "Regular", "Regular+Aide")), race = factor(race, levels = 1:6, labels = c("White", "Black", "Asian", "Hispanic", "Native American", "Other")))

# b) Create a binary variable small equal to 1 if the student was in a small class and 0 otherwise

df <- df %>% mutate(small = if_else(classtype == "Small", 1,0))

# c) Drop observations with missing values on hsgrad. How many observations remain?

df <- df %>% drop_na(hsgrad)
nrow(df)
# 3074 observations remain

# d) Compute the high school graduation rate overall and by class type. In a comment,
# describe the differences

#overall
mean(df$hsgrad)
# 0.8332786

# by class type
df %>% group_by(classtype) %>% summarize(grad_rate = mean(hsgrad))
# Small = 0.836
# Regular = 0.825
# Regular+Aide = 0.839

# Graduation rates are high overall (about 83%). Students in small classes have a
# slightly higher graduation rate than those in regular classes (83.6% vs 82.5%), but
# the difference is very small. The regular+aide group has a slightly higher graduation
# rate of 83.9%. There is overall no large difference in graduation across class types based
# on these descriptive averages alone.

# 2.2 LPM and logit

# a) Estimate an LPM predicting hsgrad from small: lpm1 = lm(hsgrad ~ small, data = df).

lpm1 <- lm(hsgrad ~ small, data = df)
broom:: tidy(lpm1)
# Students in small classes have an estimated grad probability 0.00375 higher than non-small classes
# p = 0.80 --> very large 

# b) Estimate a logit model with the same predictor: logit1 = glm(hsgrad ~ small, family = binomial, data = df).

logit1 <- glm(hsgrad ~ small, family = binomial, data = df)
broom::tidy(logit1)
# small = 0.0271
# intercept = 1.60

# The logit coefficient on small is 0.0271, indicating a very small increase in the log-odds
# of graduating for students in small classes. Converting to odd ratios, students in small classes
# have 2.7% higher odds of graduating. However, because the p-value = 0.8, it is too high to
# be statistically significant. The logit results are consistent with the LPM findings above. 


# c) Interpret the LPM coefficient on small: what is the estimated difference in graduation
# probability between small and non-small classes?

# The LPM coefficient on small is 0.00375, meaning that students in small classes are estimated
# to be about 0.4 pp more likely to graduate than students in non-small classes. 


# d) Compute the AME from the logit using avg slopes(logit1). How does it compare to
# the LPM coefficient?

avg_slopes(logit1)

# the average marginal effects of small from the logit model is 0.00375, which  means students
# in small classes are about 0.4 pp more likely to graduate than those in non-small classes. This is very similar 
# to the LPM coefficient, indicating that the LPM provides a similar estimate of the treatment effect in this case. 


# 2.3 Adding Controls

# a) Estimate both LPM and logit with controls:lpm2 = lm(hsgrad ~ small + race + yearssmall, data = df)
# logit2 = glm(hsgrad ~ small + race + yearssmall,
             #family = binomial, data = df).

lpm2 <- lm(hsgrad ~ small + race + yearssmall, data = df)

logit2 <- glm(hsgrad ~ small + race + yearssmall, family = binomial, data = df)
logit2

# b) Compare the coefficient on small between the bivariate and controlled models. Does
# it change much? What does this tell you about the randomization?

broom::tidy(lpm1)
# small = 0.00375
broom::tidy(lpm2)
# small = - 0.0756
# Small classes now appear to reduce graduation by 7.6 pp

# The coeff on small changes when controls are added. In the bivariate model, the effect is close to 0, while in the controlled model
# the coefficient becomes negative and statistically significant. This change may reflect the inclusion of yearssmall, which captures
# the treatment intensity by accounting for the years spent in small classes. This means that
# randomization worked for the assignment, but not exposure intensity, so therefore the coeffs aren't similar. 

# c) Interpret the coefficient on yearssmall from the logit model. Use avg slopes() to
# convert to a marginal effect.

avg_slopes(logit2, variables = "yearssmall")

# estimate = 0.0283

# The AME of yearssmall is 0.0283, meaning that each additional year spent in small class increases the probability of graduating by about 2.8 pp.
# The effect is statistically significant, suggesting that longer exposure to small classes is associated with higher grad rate. 


# 2.4 Predicted Probabilities

# a) Using the controlled logit model, compute predicted graduation probabilities for:
# • A White student in a small class with 3 years in small classes
# • A Black student in a regular class with 0 years in small classes
# Use predictions(logit2, newdata = datagrid(...)). Report the estimates and 95% CIs.


pred <- predictions(logit2, newdata = data.frame(small = c(1,0), yearssmall = c(3,0), race = factor(c("White", "Black"), levels = levels(logit2$model$race))))
pred

# White student in small class for 3 years: 0.869 predicted probability of graduation. At 95% CI: 0.845-0.890.
# Black student in regular clas with 0 years in small classes: 0.729 predicted prob of grad. At 95% CI: 0.695-0.762


# b) Plot predicted graduation probabilities across yearssmall for small vs. non-small classes:
# plot predictions(logit2, condition = c("yearssmall", "small")). Save the plot.

# code to try to fix "Hispanic" variable
df <- df %>% mutate(race= factor(race))
race_levels <- levels(df$race)
table(df$race)
race_levels <- levels(logit2$model$race)

plot1 <- plot_predictions(logit2, condition = c("yearssmall", "small"))
ggsave("plot1.png")

# 2.5 Interactions

# a) Does the small class effect on graduation differ by race? Estimate:
# logit3 = glm(hsgrad ~ small * race + yearssmall,
             # family = binomial, data = df).

logit3 <- glm(hsgrad ~ small * race + yearssmall, family = binomial, data = df)
logit3

# Small class effect differ slightly by race for Black and Other students, and the data must be 
# sparse or separated for Asian and Native American students, which is why the estimates are unreliable.

# b) Use avg slopes(logit3, variables = "small", by = "race") to compute the marginal
# effect of small separately for each racial group.

avg_slopes(logit3, variable = "small", by = "race")


# c) In a comment, discuss: is the small class effect larger for some groups than others?

# Yes, the small class effect varies across racial groups. It is the strongest (-0.0765) for Black students, moderate (-0.103) for white students,
# uncertain for Asian students, and insignificant for Native American and Other students.

# 2.6 Presenting results and discussion

# a) Create a table with modelsummary() comparing all four models (LPM bivariate, LPM
# controlled, logit bivariate, logit controlled). Use robust SEs for the LPM models.

table <- modelsummary(list("LPM bivariate" = lpm1, "LPM controlled" = lpm2, "Logit bivariate" = logit1, "Logit controlled" = logit2), vcov = list("robust", "robust", NULL, NULL))
ggsave("table.png")

# b) Create a coefficient plot with modelplot().

modelplot <- modelplot(list(lpm1, lpm2, logit1, logit2))
ggsave("modelplot.png")

# c) In a comment (5–10 sentences), discuss:
# • What does the STAR data suggest about the effect of small class sizes on high
# school graduation?
# • How do the LPM and logit results compare? Do they tell a similar or different story?
# • Why is this experimental evidence more credible than an observational study?


# The STAR data suggests that there are small differences in graduation probabilities across
# class types based on raw averages (which we solved at the beginning), but the overall descriptive
# graduation rate was about 83%. Students in small classes show a slightly higher graduation
# rate of 83.6%, than those in regular classes (82.5%), but the regular+aide group was higher than
# both at 83.9%. 

#Additionally, the bivariate LPM shows a very small positive effect of being in a 
# small class (0.00375), but it is not statistically significant. The logit model confirms the result
# with the AME also around 0.4 pp. Therefore, the LPM and logit models share a similar result when
# only the treatment indicator is considered. 

# When controls were introduced (race and years spent in small classes), the results shift. The 
# coefficient on small class assignment becomes negative in the controlled LPM (-0.0756), which controls
# for the exposure intensity (yearssmall). However, the yearssmall shows a positive effect in the logit
# model, suggesting that each additional year in a small class, will increase the probability of graduating
# by about 2.8 pp. When comparing models with modelsummary and modelplot, we can show that LPM and 
# logit results are consistent in size and direction, though the logit is nonlinear.

# The design of STAR as an experiment, using random assignment to class types, has a greater 
# credibility to causal interpretations than observational studies because it reduces confounding bias






