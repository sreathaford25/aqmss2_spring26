# Sara Reathaford
# Class Assignment 3

# In-Class ANES Voter Turnout

install.packages("tidyverse")
library(tidyverse)
library(modelsummary)
install.packages("marginaleffects")
library(marginaleffects)

# 1.1 Setup and data preparation
# upload dataset

anesraw <- read.csv("https://github.com/franvillamil/AQM2/raw/refs/heads/master/datasets/anes/anes_timeseries_2020.csv")
anesraw

print("NOTE: mutate vs transmute")
print("NOTE: case_when and ifelse")

class(NA_character_)
class(NA_real_)
class(NA)

# a) key variables to define:

df <- anesraw %>% transmute(
  voted = ifelse(V202109x < 0, NA, V202109x), age = ifelse(V201507x < 0, NA, V201507x), female = case_when(V201600 == 2 ~ 1, V201600 == 1 ~ 0, TRUE ~ NA_real_), education = case_when(V201511x == 1 ~ 10, V201511x == 2 ~ 12, V201511x == 3 ~ 14, V201511x == 4 ~ 16, V201511x == 5 ~ 20, TRUE ~ NA_real_), income = ifelse(V201617x < 0, NA, V201617x), party_id = ifelse(V201231x < 0, NA, V201231x))

# b)  Drop observations with missing values on any of these variables. How many observations remain?

# WRONG WAY:
# summary(df)
# nrow(df)
# 8280
# df = na.omit(df)
# with na.omit, we are losing too many observations, so it is a bad idea to use it
# nrow(df)
# 6733 observations


# CORRECT WAY BELOW: 
df = subset(df, !is.na(income))
# df = df %>% filter(!is.na(income))
# two functions above do the same thing, better way to deal with NAs
nrow(df)
# 7664

# c) Compute the overall turnout rate (proportion of voted == 1) and print summary statistics for all variables.

mean(df$voted, na.rm = TRUE)
# 0.861693
summary(df)

# 1.2 Exploratory Visualization

# a) Create a bar chart showing the turnout rate by education level (hint: compute the mean of voted for each value of education, then use geom col()).

turnout_by_edu = df %>% group_by(education) %>% summarize(turnout = mean(voted, na.rm = TRUE))

ggplot(turnout_by_edu, aes(x = factor(education), y = turnout)) + geom_col() + labs(x = "Years of Education", y = "Turnout Rate")

# b) In a comment, describe the pattern. Does turnout increase with education?

# Turnout increases with education: respondents with more years of education are more likely to report voting. The pattern is monotonic.


# 1.3 Linear Probability Model

# a) Estimate an LPM with voted as the outcome and age, education, income, and female as predictors: lpm = lm(voted ~ age + education + income + female, data = df).
# b) Print the results using broom::tidy().
lpm = lm(voted ~ age + education + income + female, data = df)
broom::tidy(lpm)

# c) Interpret the coefficient on education in a comment. What does it mean in terms of probability?

#  The coefficient on education represents the estimated change in the probability of voting for each additional year of education, holding the other variables constant.

# d) Check the predicted probabilities: how many are below 0 or above 1? Report the minimum and maximum predicted values.

preds_lpm = predict(lpm)
sum(preds_lpm < 0)
# 0

sum(preds_lpm > 1)
# 802

range(preds_lpm)
# 0.5147577 1.1709925

# 1.4 Logistic Regression

# a) Estimate a logit model with the same predictors:logit = glm(voted ~ age + education + income + female,family = binomial, data = df).

logit = glm(voted ~ age + education + income + female, family = binomial, data = df)

nd = data.frame(age = c( 25, 50), education = c(10, 10), income = rep(20, 2), female = rep(1,2))
predict(logit, newdata = nd)

logit2 = glm(voted ~ age + education + income + female, family = binomial, data = df)


exp(0.8154817) / (1 + exp(0.8154817))
# 0.6932764 = probability

exp(1.7304635) / (1 + exp(1.7304635))
# 0.8494717 = probability

predict(logit, newdata = nd, type = "response")
# 0.6932764 0.8494717



# b) Print the results using broom::tidy().
broom::tidy(logit)



# c) Compute the odds ratios using exp(coef(logit)). Interpret the odds ratio for education in a comment.

exp(coef(logit))

# The odds ratio for education indicates the multiplicative change in the odds of voting for each additional year of education. An odds ratio above 1 means more education is associated with higher odds of voting.

# d) Verify all predicted probabilities are bounded:

preds_logit = predict(logit, type = "response")
range(preds_logit)
# 0.2510479 0.9945045
# all predicted probabilities are between 0 and 1


# 1.5 Comparing LPM and logit

# a) Compute average marginal effects for the logit model using marginaleffects::avg slopes(logit).

avg_slopes(logit)

# b) Compare the AMEs to the LPM coefficients. How similar are they? Discuss in a comment.

#  The AMEs from the logit model are similar to the LPM coefficients, as expected when predicted probabilities are mostly in a moderate range. Both approaches tell a broadly similar story about the relationship between each predictor and voter turnout.

# c) Create a table with modelsummary() showing the LPM and logit side by side. Use robust standard errors for the LPM:
# modelsummary(list("LPM" = lpm, "Logit" = logit), vcov = list("robust", NULL)).

modelsummary(list("LPM" = lpm, "Logit" = logit), vcov = list("robust", NULL), output = "markdown")



# 1.6 Predicted Probabilities

# a) Use plot predictions(logit, condition = "education") to plot the predicted probability of voting across education levels. Save the plot.

p1 = plot_predictions(logit, condition = "education")
p1
ggsave("pred_prob_education.png", p1, width = 6, height = 4)

# b) Create a second plot showing predicted probabilities across age for men and women separately:plot predictions(logit, condition = c("age", "female")).

p2 = plot_predictions(logit, condition = c("age", "female"))
p2
ggsave("pred_prob_age_gender.png", p2, width = 6, height = 4)

# c) In a comment, describe the patterns. How does the effect of age differ from the effect of education?

# Education shows a clear positive relationship with turnout. Age also has a positive effect. The plot by gender shows that both men and women follow similar age-turnout patterns, with any gender gap being modest relative to the age effect.


# 1.7 Presenting Results

# a) Create a coefficient plot comparing the LPM and logit models using modelplot().
# b) Save the plot

p3 = modelplot(list("LPM" = lpm, "Logit" = logit), vcov = list("robust", NULL))
p3
ggsave("coefplot_lpm_logit.png", p3, width = 6, height = 4)

# c) In a comment: for this dataset, do the LPM and logit lead to different substantive conclusions? When might the differences matter?

#  For this dataset, the LPM and logit lead to similar substantive conclusions: age, education, and income are all positively associated with turnout, and gender has a modest or negligible effect. The differences between LPM and logit matter more when predicted probabilities are close to the boundaries (0 or 1). In this sample, turnout is relatively common, so the linear approximation works reasonably well.









