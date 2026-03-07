# Sara Reathaford
# AQMSS2
# Homework Assignment 5


library(tidyverse)
library(modelsummary)
library(marginaleffects)
library(dplyr)
library(ggplot2)
install.packages("fixest")
library(fixest)
install.packages("plm")
library(plm)

# -----------------------------------------------------------------------------
# 2.1 Data Exploration

df <- haven:: read_dta("teaching_evals.dta")

# a) How many unique instructors and courses are in the data? Use n distinct(df$InstrID)
  # and n distinct(df$CourseID). What is the average number of observations (courseyear pairs) per instructor? In a comment, note whether this looks like a short or long
  # panel.

length(unique(df$InstrID))
# 48
length(unique(df$CourseID))
# 254

# average # of observations (courseyear pairs) per instructor

df %>% group_by(InstrID) %>% summarize(n = n()) %>% summarize(avg_obs = mean(n))
# 17.5 observations per instructor

# There are 48 unique instructors and 254 unique courses in the dataset. The average
  # number of instructors appears in about 17.5 course year observations.
  # This suggests the dataset is a short panel because instructors are observed only
  # a limited number of times over time.

# Short: many individuals, few time periods
# Long: few individuals, many time periods

# b) Create a scatter plot of Eval (y-axis) against Apct (x-axis). Add a regression line with
  # geom smooth(method = "lm"). In a comment, describe the cross-sectional relationship
  # between grading generosity and evaluations: is it positive or negative? Does this pattern surprise you?


plot1 <- ggplot(df, aes(x = Apct, y = Eval)) + geom_point() + geom_smooth(method = "lm") + theme_minimal() + labs(x = "% of students receiving A or A-", y = "Average Course Evaluation")
plot1
ggsave("plot1.png")

# The scatter plot shows a positive relationship between the percentage of A grades
  # and average course evaluations. Courses where a larger share of students receive
  # A grades tend to have slightly higher evaluation scored. However, the points are 
  # widely scattered around the regression line, suggesting that the relationship is weak.
  # Most evaluation scores are clustered between 4 and 5, indicating that evaluations are
  # generally high regardless of grading generosity.


# ------------------------------------------------------------------------------


# 2.2 Pooled OLS baseline

# a) Estimate a pooled OLS model with all three regressors:
  # m1 = lm(Eval ~ Apct + Enrollment + Required, data = df). Report the results using summary() or modelsummary(). In a comment, interpret the coefficient on Apct: a
  # one-percentage-point increase in the share of A grades is associated with how much of
  # a change in evaluation scores?

m1 <- lm(Eval ~ Apct + Enrollment + Required, data = df)
summary(m1)
modelsummary(list(m1), stars = TRUE)

# Apct: B = 0.359***
# A one percentage point increase in the share of A grades is associated with 0.359 increase in evaluation scores. 


# b) In a comment, explain why the OLS estimate of Apct might be biased. What unobserved characteristics of instructors could simultaneously drive both grading generosity and evaluation scores? Give at least two concrete examples. Is the expected bias
  # upward or downward?

# The OLS estimate may be biased because instructors differ in unobserved ways. For example,
  # more engaging or nice instructors may both receive higher evaluations and be more generous
  # in grading. Similarly, instructors teaching easier subjects may give higher grades and receive
  # better evaluations. Because these characteristics are not included in the regression, they
  # may bias the estimated relationship between grading generosity and evaluations, creating an 
  # upward bias.

# -----------------------------------------------------------------------------------

# 2.3 Fixed Effects Models

# a) Estimate a model with instructor fixed effects, and a two-way model adding year fixed
  # effects:

m_instr <- feols(Eval ~ Apct + Enrollment + Required | InstrID, data = df)
m_twfe <- feols(Eval ~ Apct + Enrollment + Required | InstrID + Year, data = df)

# b) Compare all three models (m1, m instr, m twfe) in a single table with standard errors
  # clustered by instructor:

modelsummary(list("Pooled OLS" = m1, "Instructor FE" = m_instr, "Two-Way FE" = m_twfe),
             vcov = ~ InstrID, stars = TRUE, gof_map = c("r.squared", "nobs"))


# c) Interpret the coefficient on Apct in the instructor-FE model (m instr). In a comment,
  # explain what the instructor fixed effect is controlling for. Is the FE coefficient on Apct
  # larger or smaller than in the pooled OLS? What does this tell us about the direction of
  # omitted variable bias in the pooled OLS estimate — are more lenient graders systematically
  # better or worse evaluators in terms of their unobserved characteristics?


# In the instructor FE model, the coeff on Apct is 0.306. This means that a one unit
  # increase in the share of A grades is associated with a 0.306 increase in course
  # evaluation scores. This model is holding enrollment and whether the course is required
  # constant. The instructor FE model controls for all time-invariant characteristics
  # of instructors, such as teaching ability, personality, grading philosophy, or reputation.
  # The model therefore compares evaluation scores for the same instructor across different
  # courses of years. The coeff on Apct is smaller in the instructor FE model (0.306) than
  # in the pooled OLS model (0.359). This suggests that the pooled OLS estimate was upward
  # biased. Instructors who tend to give more A grades may also have unobserved characteristics,
  # such as being nicer or more engaging, which led to higher evaluation scores. 


#-----------------------------------------------------------------------------------

# 2.4 Random Effects and the Hausman Test

# a) Estimate a random effects model using plm. The random effects model assumes that
  # the unobserved instructor-level heterogeneity is uncorrelated with the regressors:

pdata <- pdata.frame(df, index = c("InstrID", "CourseID"))
m_re <- plm(Eval ~ Apct + Enrollment + Required, data = pdata, model = "random")

# assumes instructor characteristics are NOT correlated with regressors

# b) Run the Hausman test to assess whether fixed or random effects is more appropriate. The Hausman test checks whether the random effects assumption (no correlation
  # between unobservables and regressors) holds:

m_fe_plm <- plm(Eval ~ Apct + Enrollment + Required, data = pdata, model = "within")
phtest(m_fe_plm, m_re)

# chisq = 4.9169
# df = 3
# p-value = 0.178

# If p < 0.05, reject random effects, prefer FE
# 0.178 > 0.05, fail to reject random effects


# c) In a comment, interpret the Hausman test result. What is the null hypothesis? Is it
  # rejected? Based on the test and on the substantive reasoning from the previous sub
  # section, should you prefer the fixed effects or the random effects estimator for this
  # dataset? Explain in 3–5 sentences.

# The null hypothesis of the Hausman test is that the random effects estimator is consistent,
  # meaning that the instructor-specific unobserved effects are not correlated with regressors.
  # The p-value is 0.178, so we fail to reject the null hypothesis. This suggests that the 
  # random effects model could be appropriate. However, it is plausible that the instructor
  # characteristics such as teaching ability are correlated with grading behavior. This means
  # that the FE model is likely to be more reliable for this dataset. 
