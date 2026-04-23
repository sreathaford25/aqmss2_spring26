# Sara Reathaford
# AQMSS2 
# Assignment 10 HW


# Using HW Assignment 4 for the data and analysis


# Wealth and Infant Mortality

# download necessary packages
library(modelsummary)
library(marginaleffects)
library(tidyverse)
library(dplyr)
library(broom)
library(ggplot2)
install.packages("readstata13")
library(readstata13)
library(haven)

# 2.1 Data Exploration

# a) Load the dataset and print summary statistics for all variables. How many countries
# are in the data?

df = read_dta("https://github.com/franvillamil/AQM2/raw/refs/heads/master/datasets/other/infantmortality.dta")
summary(df)
nrow(df)
# countries = 101 obs

# b) Create a histogram of infant and a histogram of income. Are either of them rightskewed?

# infant mortality
infmort_g1 <- ggplot(df, aes(x = infant)) + geom_histogram() + theme_minimal()
infmort_g1
# right skew

# income
income_g1 <- ggplot(df, aes(x = income)) + geom_histogram() + theme_minimal()
income_g1
# right skew

# c) Create a scatter plot of infant (y-axis) against income (x-axis), coloring points by
# region. Describe the relationship in a comment

inf_inc_plot <- ggplot(df, aes(x = income, y = infant, color = region)) + geom_point() + geom_smooth(method = "lm", se = FALSE)
inf_inc_plot
ggsave("inf_inc_plot.pdf")

# Income is heavily right skewed, with most countries clustered a low income levels and a few high income outliers stretching the distribution to the right.
# Therefore, it would be valuable to use the log(income) to control for the country effects and residual patterns

# d) Create the same scatter plot but using log(income) on the x-axis and log(infant) on
# the y-axis. Does the log-log relationship look more linear?

inf_inc_logplot <- ggplot(df, aes(x= log(income), y = log(infant), color = region)) + geom_point() + geom_smooth(method = "lm", se = FALSE)
inf_inc_logplot
ggsave("inf_inc_plogplot.pdf")

# The log-log relationship does appear more linear. We see negative relationships for all regions other than Africa, which is consistent among the log incomes.

# 2.2 Comparing Specifications

# a) Estimate a level-level model:
# m1 = lm(infant ~ income, data = df).

m1 <- lm(infant ~ income, data = df)
summary(m1)
# Infant = B0 + B1Income
# B1 = change in infant mortality for $1 increase in income

# b) Estimate a log-log model:
# m2 = lm(log(infant) ~ log(income), data = df).

m2 <- lm(log(infant) ~ log(income), data = df)
summary(m2)

# log(infant) = B0 + B1log(income)
# a 1% increase in income -> B1% change in infant mortality


# c) Interpret the coefficient on income in each model:
#• In m1: what is the predicted change in infant mortality for a $1,000 increase in
#income?
# • In m2: recall that the log-log coefficient is an elasticity. What does it mean here?
#(e.g., “A 10% increase in income is associated with a % change in infant mortality.”)

# m1 --> B = -0.020907
# In the level-level model, a $1,000 increase in income is associated with a 20.9 decrease in 
# infant deaths per 1,000 births. The relationship is statistically significant (p < 0.0001), 
# but the model explains only about 11% of the variation in infant mortality (R^2 = 0.1093)

# m2 --> B = -0.51179
# In the log-log model, the coefficient on log(income) is -0.512, which can be interpreted as an elasticity.
# A 1% increase in income is associated with a 0.51% decrease in infant mortality. A 10% increase in income
# is associated with approx. a 5.1% reduction in infant mortality. The relationship is statistically significant
# because p < 0.001, AND the model explains about 50% of the variation (R^2 = 0.4971).


# d) Create a residuals vs. fitted values plot for both models. Which specification has a
#better residual pattern? Discuss in a comment.

# m1

m1_aug <- augment(m1)

m1_aug_plot <- ggplot(m1_aug, aes(x = .fitted, y = .resid)) + geom_point() + geom_hline(yintercept = 0, linetype = "dashed") + labs(x = "Fitted Values", y = "Residuals", title = "Residuals vs. Fitted: Level-Level (m1)")
m1_aug_plot
ggsave("m1_aug_plot.pdf")

# m2

m2_aug <- augment(m2)

m2_aug_plot <- ggplot(m2_aug, aes(x = .fitted, y = .resid)) + geom_point() + geom_hline(yintercept = 0, linetype = "dashed") + labs(x = "Fitted Values", y = "Residuals", title = "Residuals vs. Fitted: level-log (m2)")
m2_aug_plot
ggsave("m2_aug_plot.pdf")

# the m2_aug plot has a better residual pattern because with the level-log model, 
# the residuals are more spread and less skewed like in the level-level model.

# 2.3 Multiple Regression with Controls

# a) Estimate a log-log model with controls for region and oil-exporting status:
# m3 = lm(log(infant) ~ log(income) + region + oil, data = df).

m3 <- lm(log(infant) ~ log(income) + region + oil, data = df)
summary(m3)

# b) Print the results. In a comment, interpret the coefficient on log(income): does controlling for region and oil status change the income effect?

summary(m3)
# B = -0.33985 (elasticity)
# A 1% increase in income is associated with about a 0.34% decrease in infant mortality,
# holding region and oil exporting status constant. Compared to the previous log-log model (m2),
# the effect became smaller (m2 = -0.515). This means that part of the income effect was
# actually capturing regional differences. Once we control for geography and oil, income still
# matters, but less strongly than before. 

# c) Interpret the coefficient on the Africa region indicator (relative to the reference category). What does it tell you about infant mortality in Africa, controlling for income?

coef(m3)
# Europe
exp(coef(m3)["regionEurope"])
# BEurope = 0.3556432
1 - 0.3556432
# 0.6443568
# European countries have about 64% lower infant mortality than African countries when controlling for income and oil.

# Americas
100 * (exp(coef(m3)["regionAmericas"]) -1)
# -42.29558
# Countries in the Americas have about 42% lower infant mortality than African countries when controlling for income and oil.

# Asia
100 * (exp(coef(m3)["regionAsia"]) -1)
# -50.97903
# Countries in Asia have about 51% lower infant mortality than African countries when controlling for income and oil.

# The negative coefficients for the Americas, Asia, and Europe indicate that these regions have
# significantly lower infant mortality than Africa, even when controlling for income and oil status.
# This suggests that geography and regional factors play an important role beyond income alone.

# d) Compute average marginal effects using avg slopes(m3). Focus on the AME of income
# and report it in a comment

avg_slopes(m3)

# AME income = -0.00159
# On average, a $1 increase in income is associated with a 0.00159 decrease in infant deaths per 1000
# births, holding region and oil constant. When scaled at an increase of $1000, the infant deaths are
# associated with about a 1.6% decrease. This is smaller than the estimate from the level-level model,
# because it reflects the diminishing returns relationship which is captured from the log-log model. 


# 2.4 Interaction: oil x status income

# a) Estimate a model with an interaction between oil status and log income:
# m4 = lm(log(infant) ~ log(income) * oil + region, data = df).

m4 <- lm(log(infant) ~ log(income) * oil + region, data = df)
summary(m4)

# log(income) = -0.43234
# for non-oil countries, a 1% increase in income is associated with a 0.43% decrease in infant mortality. 

# log(income):oilyes = 0.80764
# This interaction term tells us how much the slope changes for oil countries. 

# log(income) + log(income):oilyes = oil_slope
-0.43234 + 0.80764
# oil slope = 0.3753
# For oil exporting countries, a 1% increase in income is associated with a 0.38% increase in infant mortality.


# b) Use avg slopes(m4, variables = "income", by = "oil") to compute the marginal
# effect of income separately for oil-exporting and non-oil countries.

avg_slopes(m4, variables = "income", by = "oil")

# In non-oil countries, a $1 increase in income is associated with a 0.00209 decrease in infant deaths per 1000 births.
# A $1000 increase in income reduces infant mortality by about 2.1 deaths per 1000 births in non-oil countries.

# In oil exporting countries, a $1 increase in income is associated with a 0.00111 increase in infant mortality.
# A $1000 increase is associated with about 1.1 more infant deaths per 1000 births.


# c) In a comment, discuss: does the relationship between income and infant mortality
# differ for oil-exporting countries? What might explain this?

# Yes, the relationship between income and infant mortality does change between oil and non-oil countries.
# For non-oil countries, a $1000 increase in income is associated with about 2.1 LESS deaths per 1000 births. 
# For oil countries, a $1000 increase is associated with 1.1 MORE infant deaths per 1000 births. This then suggests
# that income growth does not help health outcomes in oil-dependent economies. I'm thinking that this could be due
# to lack of resources or weak institutions. 


# d) Plot how the marginal effect of income varies by oil status:
# plot slopes(m4, variables = "income", condition = "oil"). Save the plot.

m4_plot <- plot_slopes(m4, variables = "income", condition = "oil")
ggsave("m4_plot.pdf")


# 2.5 Predicted Values for Specific Scenarios

# a) Using model m3 (without interaction), compute predicted infant mortality rates for:
# • A non-oil African country with income = $1,000
# • A non-oil European country with income = $20,000
# • An oil-exporting country in the Americas with income = $10,000
# Note: since the outcome is log(infant), you need to exponentiate the predictions to
# get infant mortality in the original scale. Use exp() on the estimate column.

pred <- predictions(m3, newdata= datagrid(income = c(1000, 20000, 10000), region = c("Africa", "Europe", "Americas"), oil = c("no", "no", "yes")))
pred

pred$infant_predicted <- exp(pred$estimate)
pred$infant_predicted

# b) In a comment, discuss the predicted values. Are they plausible? How large is the gap
# between the African and European scenarios?

# Non-oil African country, income = 1000
# 66.99 infant deaths per 1000 births

# Non-oil European country, income = 20000
# 8.61 infant deaths per 1000 births

# Oil exporting American country, income = 10000
# 33.53 infant deaths per 1000 births

# These predicted values represent a gap of roughly 58 deaths per 1000 births (67-9).
# These predicted values are plausible and reflect the income and regional disparities.
# The large gap between the African and European scenarios suggests that both wealth and
# geography play an important role in explaining the differences in infant mortality between countries. 


# 2.6 Publication-quality Visualization

# a) Create a prediction plot showing predicted infant mortality across income levels, separately by region:

plot_m3 <- plot_predictions(m3, condition = c("income", "region")) + labs(x = "Income (USD per capita)", y = "Predicted Infant Mortality", title = "Predicted Infant Mortality by Income and Region") + theme_minimal()

plot_m3

ggsave("infant_mortality_plot.pdf", plot_m3)


# b) In a comment (5–10 sentences), discuss: what does this plot tell a general audience
# about the relationship between wealth and infant mortality? What role does geography play? 
# What are the main limitations of this analysis (e.g., omitted variables, reverse causality, ecological fallacy)?


# The prediction plot shows a strong negative relationship between income and infant mortality across all regions.
# As income increases, predicted infant mortality declines, especially at lower income levels. However, the slopes and
# levels differ across regions. African countries are predicted to have consistently higher infant mortality than countries
# in the Americas, Asia, and especially Europe at similar income levels. This suggests that geography and regional characteristics
# play an important role rather than only income alone.

# The main limitation of this analysis is that it is observational and cross-sectional, so we cannot make strong causal claims. There
# may be omitted variables like health infrastructure, education levels, government structure, or conflict such as war that affect
# both income and infant mortality. Additionally, reverse causality is possible, since healthier populations may also contribute to
# higher economic growth. The results may also not reflect individual level relationships within countries.


# 2.7 Diagnostics and Robust Inference

# a) Create a residuals vs. fitted values plot for m3. Does the plot suggest heteroskedasticity?

aug3 <- augment(m3)
m3_aug_plot <- ggplot(aug3, aes(x = .fitted, y = .resid)) + geom_point() + geom_hline(yintercept = 0) + theme_minimal()
m3_aug_plot
ggsave("m3_aug_plot.pdf")

# The results do not appear to fan out, but there does appear to be a possible 3 clusters with a few outliers that do not fit within the clusters.



# b) Create a regression table comparing all four models with robust standard errors:

regress_tbl <-modelsummary(list("Level" = m1, "Log-Log" = m2, "Controls" = m3, "Interaction" = m4), vcov = "robust", stars = TRUE, gof_map = c("r.squared", "nobs"), output = "regress_tbl.tex")
regress_tbl



# c) Compare the robust and default standard errors for m3. Run modelsummary() with and
# without vcov = "robust". Do the conclusions change? Why use robust SEs?

modelsummary(m3)
modelsummary(m3, vcov = "robust")

# Comparing the default and robust standard errors, the coeff estimates remain the same, but the robust standard errors
# are generally larger. This can reduce statistical significant for some variables. Robust standard errors are preferred
# especially in this cross-country analysis because the assumption of constant error variance (homoskedasticity) is unlikely to hold.
# Using robust standard erros ensures the inference to be more reliable even if there is heteroskedasticity. 
