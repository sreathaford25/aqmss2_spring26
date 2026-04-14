# Sara Reathaford
# Homework 9
# Survival Analysis

install.packages("survival")
library(survival)
library(broom)
library(ggplot2)
library(marginaleffects)

# load the dataset

lung <- lung

# recorde status --> dead (1 = event, 0 = censored)

lung$dead <- lung$status - 1

# 2.1 Kaplan-Meier survival curves

# a) Explore the data. In a comment, report the total number of observations, the number
  # of events (deaths), and the number of censored cases. What proportion of patients are
  # censored? Is this a lot or a little? Think about what this means: the censored patients’
  # true survival times are unknown but at least as long as their observed times.

n_total <- nrow(lung) # 228
n_events <- sum(lung$dead == 1, na.rm = TRUE) # 165
n_censored <- sum(lung$dead == 0, na.rm = TRUE) # 63

prop_censored = n_censored / n_total # 0.2763158 --> 28%

# About 28% of the patients are censored, which is a moderate amount. This means that for these
  # patients, we only know they survived at least as long as their observed time, but not their 
  # exact survival time. This is why survival analysis is needed. 

# b) Estimate the overall Kaplan-Meier survival curve using survfit() with formula Surv(time,
  # dead) ~ 1. The summary() output shows, at each event time, the number at risk, the
  # number of events, the estimated survival probability, and the confidence interval. In a
  # comment, report the estimated median survival time. What does this number mean in
  # plain language?

km_all <- survfit(Surv(time, dead) ~ 1, data = lung)

summary(km_all)
summary(km_all)$table["median"]
# median = 310

# The estimated median survival time is about 310 days, meaning that 50% of the patients are
  # expected to die before this time and 50% survive longer. 

# c) Estimate separate Kaplan-Meier curves by sex using survfit() and plot them with
  # ggplot2. Hint: use broom::tidy() to convert the survfit object to a data frame, then
  # plot with geom step() and geom ribbon() for confidence intervals. Save the plot as a
  # PDF. Also run a log-rank test using survdiff(). In a comment, describe what you see:
  # which group survives longer? Does the confidence interval for the two groups overlap? 
  # Report the log-rank test p-value and explain what it tests (whether the survival 
  # curves are statistically different).
km_sex <- survfit(Surv(time, dead) ~ sex, data = lung)

km_df <- broom::tidy(km_sex)

# 1 = male, 2 = female

ggplot(km_df, aes(x = time, y = estimate, color = strata)) + 
  geom_step() + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = strata)) +
  labs(x = "Time (days)", y = "Survival probability", color = "Sex", fill = "Sex") +
  theme_minimal()

ggsave("km_sex_plot.pdf")

# log rank test

survdiff(Surv(time, dead) ~ sex, data = lung)

# Females typically survive longer (curve higher)
# log rank p-value = 0.001

# The Kaplan-Meier curves show the females have higher survival probabilities over time
  # compared to males. The confidence intervals overlap somewhat but are generally separated.
  # The log rank test is statistically significant (p < 0.01), indicating that the survival curves
  # differ between men and women. 

#---------------------------------------------------------------------------------------------------------

# 2.2 Cox Proportional Hazards Model

# a) Fit a Cox proportional hazards model predicting survival from age, sex, and ph.ecog
  # using coxph() from survival. The output shows both raw coefficients (log-hazard scale) 
  # and exponentiated coefficients (hazard ratios). In a comment, report and interpret the 
  # hazard ratio for sex. Recall from class: a hazard ratio below 1 means lower hazard 
  # (longer survival), above 1 means higher hazard (shorter survival). What does the hazard 
  # ratio for sex tell us about survival differences between men and women? Is it statistically 
  # significant?

cox_model <- coxph(Surv(time, dead) ~ age + sex + ph.ecog, data = lung)
summary(cox_model)

# hazard ratio sex = 0.552
# HR < 1 --> lower hazard --> longer survival

# The hazard ratio for sex is below 1, indicating that females have a lower risk of death compared to 
  # males. This suggests that women tend to survive longer than men. The effect is statistically significant. 

# b) Interpret the hazard ratio for ph.ecog. In a comment, explain what a one-unit increase
  # in ECOG performance score (i.e., moving toward worse physical functioning) does
  # to the hazard of death. Express this as a percentage change (e.g., “X% higher/lower hazard”).

# ph.ecog = 1.5900

(1.5900 - 1) * 100 = # 59% 

# A one-unit increase in ECOG score is associated with about a 59% increase in the hazard of death. 

# c) The Cox model assumes proportional hazards: the effect of each covariate is constant
  # over time. Test this assumption using cox.zph(). In a comment, report the p-value
  # for each covariate and the global test. A significant p-value suggests the proportional
  # hazards assumption is violated for that variable. Do any variables violate the assumption? 
  # If so, what would this mean in substantive terms (e.g., the effect of age changes over the 
    # course of the disease)?
  
ph_test <- cox.zph(cox_model)
ph_test

# if p < 0.05 = violation, if p > 0.05, assumption holds

# age: p = 0.66 --> no violation
# sex: p = 0.13 --> no violation
# ph.ecog: p = 0.15 --> no violation
# global: p = 0.22 --> no violation

# The proportional hazards test shows that none of the covariates violate the assumption, as all
  # p-values are greater than 0.05. The global test is not statistically significant, which suggests 
  # that the model as a whole satisfies the proportional hazards assumption. This suggests that the
  # effects of age, sex, and ECOG performance status on the hazard of death remain constant over time. 

# d) Write a short summary paragraph as a comment in your R script (4–6 sentences).
  # Cover: (1) whether the Kaplan-Meier analysis suggested survival differences by sex;
  # (2) which predictors are significant in the Cox model and the direction of their effects;
  # (3) whether the proportional hazards assumption holds; (4) one substantive conclusion
  # about factors predicting lung cancer survival.

# The Kaplan-Meier analysis indicates clear survival differences by sex, with females exhibiting higher 
  # survival probabilities over time than males, and the log-rank test suggests this difference is statistically
  # significant. In the Cox proportional hazards model, sex and ph.ecog are statistically significant
  # preidctors of survival and age is not. Females have lower hazard of death rates than males (HR = 0.58), which 
  # suggests longer survival, whereas worse health status ph.ecog is associated with higher risk of death (59% increase).
  # Age has a small positive effect on hazard but is not statistically significaly. The proportional hazards assumption
  # holds for all variables, as none of the tests are statistically significant and the global test (p = 0.22) confirms that
  # model is appropriate. Overall, the results suggest that survival among lung cancer patients is strongly influenced
  # by baseline health status and sex, with poorer physical condition increasing mortality risk and females experiencing
  # better survival outcomes. 



