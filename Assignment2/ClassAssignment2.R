#Sara_Reathaford
#Class_Assignment2


install.packages("tidyverse")
install.packages("dplyr")
library(dplyr)
library(magrittr)
library(ggplot2)
install.packages("modelsummary")
library(modelsummary)
install.packages("sandwich")
library(sandwich)


# 1.1 Setup and data preparation

qog_raw <- read.csv('https://www.qogdata.pol.gu.se/data/qog_std_cs_jan26.csv')

# a) renaming variables

df = qog_raw %>% 
  select(country = cname, epi = epi_epi, women_parl = wdi_wip, gov_eff = wbgi_gee, green_seats = cpds_lg)

# b) Drop observations with missing values on any of these variables. How many countries remain?

nrow(df)
 # 194 countries 

# using na.omit() drops too many observations because the variable 'green-seats' is only available for a small subset of countries. Therefore, it is better to keep the full sample of variables and R will work with the missing values.

df_clean <- df %>% na.omit()

nrow(df_clean)
# with the cleaned data from removing NA's, we are left with only 36 observations, which is way too small to draw conclusions in this case.

# c) Print summary statistics for all variables

summary(df)

# 1.2 Exploratory Visualization
# a) Create a scatter plot of women parl (x-axis) vs. epi (y-axis)
# b) Add a linear fit using geom smooth(method = "lm")

ggplot(df, aes(x = women_parl, y = epi)) + geom_point() + geom_smooth(method = "lm") + labs(x = "Women in Parliament as %", y = "EPI Score")

# c) In a comment, describe what you see. Is there a relationship? what is its direction?

# Based on the line of best fit added, there is a clear positive relationship. The more women that there are in Parliament tend to have a higher EPI score. 
 

# 1.3 Bivariate Regression

# a) Run a bivariate regression: lm(epi ~ women parl, data = df).

m1 = lm(epi ~ women_parl, data = df)

# b) Extract the results using broom::tidy().

tidy(m1)

# c) Interpret the coefficient on women_parl in a comment. What is the predicted difference in EPI between a country at the 25th percentile and one at the 75th percentile of women in parliament?

nd = data.frame(women_parl = quantile(df$women_parl, c(0.25, 0.75), na.rm = TRUE))

# The coefficient on women_parl shows that the predicted change in EPI score for each additional pp of women in parliament. To get the predicted difference in EPI for a country at 25th and 75th percentile, we can use the predict function.

p25 = quantile(df$women_parl, 0.25, na.rm=TRUE)
p75 = quantile(df$women_parl, 0.75, na.rm = TRUE)

pred = predict(m1, newdata = data.frame(women_parl = c(p25, p75)))
pred[2] - pred[1]

# 75%
# 5.638584


# 1.4 Multiple Regression
# a) Run a multiple regression adding gov eff as a control

m2 <- lm(epi ~ women_parl + gov_eff, data = df)
tidy(m2)

# b) Compare the coefficient on women_parl between the bivariate and multiple regression. Does it change? In what direction? Explain in a comment what this suggests.

# The coeff on women_parl decreases greatly when gov_eff is included. This suggests that part of the bivariate association was driven by gov effectiveness being correlated with both women in parliament and environmental performance due to omitted variable bias.


# 1.5 Demonstrating OVB

# The omitted variable bias formula says: Bbar1 = Bhat1 + Bhat2 x δ, where Bbar1 is the bivariate coefficient, Bhat1 and Bhat2 are the multiple regression coefficients, and δ is the coefficient from regressing the omitted variable on the included variable.

# a) From the regressions above, write down β1 (bivariate), β1 (multiple), and β2 (multiple)
# extract the relevant coeff:

beta1_biva = tidy(m1) %>% filter(term == "women_parl") %>% pull(estimate)
beta1_mult = tidy(m2) %>% filter(term == "women_parl") %>% pull(estimate)
beta2_mult = tidy(m2) %>% filter(term == "gov_eff") %>% pull(estimate)

# b) Run the auxiliary regression: lm(gov eff ~ women parl, data = df). Extract ˜δ.

aux = lm(gov_eff ~ women_parl, data = df)
delta = tidy(aux) %>% filter(term == "women_parl") %>% pull(estimate)

# c) Verify the OVB formula: check that B1 + B2 x δ = Bbar1 (up to rounding)
# right: beta1_mult + beta2_mult * delta

round(beta1_mult +beta2_mult * delta, 4)

# 0.3307

#left: beta1_biva

round(beta1_biva, 4)

# 0.3078

#both values match, confirming the OVB formula

#d) In a comment, interpret what this means: why did the coefficient on women_parl change when we added gov_eff?

# The coefficient changed because of the inflated bivariate estimate. The bias is positive because gov_eff is pos correlated with both women_parl (δ > 0) and with epi (Bhat2 >0)


#1.6 Robust Standard Errors

# a) Using modelsummary(), print the multiple regression results with default (classical) standard errors.

modelsummary(m2, output = "markdown")

# b) Now print the same model with robust standard errors: modelsummary(model, vcov = "robust").

modelsummary(m2, vcov = "robust", output = "markdown")

# c) Compare the SEs. Do they differ substantially? Do any conclusions change?

# Based on this sample, the SEs are don't differ by much, and the conclusions don't change typically


#1.7 Presenting Results

# a) Create a table comparing the bivariate and multiple regression models side by side, using robust SEs for both: modelsummary(list(m1, m2), vcov = "robust").

modelsummary(list("Bivariae" = m1, "Multiple" = m2), vcov = "robust", output = "markdown")

# b) Create a coefficient plot using modelsummary::modelplot() comparing both models.

modelplot <- modelplot(list("Bivariae" = m1, "Multiple" = m2), vcov = "robust")

# c) Save the plot using ggsave().

ggsave("modelplot.png", plot = modelplot)



# 1.8 Extra: Effect size

# a) How could we know whether the effect of women parl is big or small?

# To determine the effect of women_parl, we could standardize both variables and redo the regression to get a standardized beta coefficient.



