# Sara Reathaford
# HW Assignment 6

library(dplyr)
library(tidyr)
library(ggplot2)
library(fixest)
library(modelsummary)
install.packages("did")
library(did)

# 2.1 Data Setup and Visualization
# Load dataset

data(mpdta)

# a) How many counties are in the data? How many unique treatment cohorts (distinct
  # values of first.treat) are there? Use table(mpdta$first.treat) to see how many
  # counties adopted treatment in each year. In a comment, explain what “staggered treatment adoption” 
  # means in this context: why is it a problem to simply compare treated
  # vs. untreated counties?

length(unique(mpdta$countyreal))
# There are 500 counties in the data

table(mpdta$first.treat)
# There are 4 treatments
# 0 (before treatment) = 1545
# 2004 = 100 counties
# 2006 = 200 counties
# 2007 = 655 counties

# Staggered treatment adoption means that the treatment is adopted in different waves, like the different
  # years in this case. This can be problematic to simply compare the treatments because each treatment was
  # adopted at different times, so you must account for time.


# b) Plot average log teen employment (lemp) over years, separately for each treatment
  # cohort. Use the following code:

mpdta_avg <- mpdta %>% 
  mutate(cohort = factor(first.treat, levels = c(0, 2004, 2006, 2007), labels = c("Never Treated", "Adopted 2004", "Adopted 2006", "Adopted 2007"))) %>%
  group_by(year, cohort) %>%
  summarise(mean_lemp = mean(lemp, na.rm = TRUE))


avglempplot = ggplot(mpdta_avg, aes(x = year, y = mean_lemp, color = cohort)) + geom_line() +
  geom_point() + theme_minimal() + labs(x = "Year", y = "Log teen employment", color = "Treatment cohort")

avglempplot

ggsave("avglempplot.png")

# The graph shows clear differences in the levels of log teen employment across cohorts, with 2006 cohort consistently being
  # the highest values, and the never treated having the lowest. This indicates baseline heterogeneity across counties. Focusing on the
  # pre-treatment trends, the cohorts do not appear to follow perfectly parallel paths. The 2004 cohort exhibits a more pronounced downward
  # trend prior to treatment, while other groups remain relatively stable. This raises some concerns about the validity of the parallel trends
  # assumption, although the differences are not extreme. After treatment, however, the cohorts display different patterns, suggesting that treatment 
  # effects may vary across groups and over time .

#-------------------------------------------------------------------------------------------------------------------------------------------------------------


# 2.2 Naive TWFE vs. Callaway-Santánna estimator

# a) Estimate a naive TWFE model treating all treated counties as a single group.
  # (Check the data, maybe you need to create a time-varying treatment indicator first, call
  #  it treated post. It should indicate treatment units after treatment comes into effect.)
  # Hint: (first.treat > 0 & year >= first.treat)
  # Report and interpret the coefficient on treated post. In a comment, note that this
  # model pools all treatment cohorts together — what implicit assumption is it making
  # about the treatment effect across cohorts and over time?


mpdta = mpdta %>% mutate(treated_post = ifelse(first.treat > 0 & year >= first.treat, 1, 0))

naive_twfe = feols(lemp ~ treated_post | countyreal+year, data = mpdta, cluster = ~countyreal)

naive_twfe

# treated_post = -0.036549

# The coefficient on treated_post is -0.036549 represents a decrease of 3.65% on employment
  # and is statistically significant with a value of 0.0038934. The implicit assumption is that
  # the treatment effect is identical across every cohort and that there is no difference between
  # the cohort years. This is naive because we know that the treatment is staggered, so we cannot
  # simply compare the effects.


# b) Now use the Callaway-Santanna (2021) estimator, which estimates group-time aver-
  # age treatment effects separately for each cohort and time period, using never-treated
  # counties as the control group:

cs_out = att_gt(yname = "lemp", gname = "first.treat", idname = "countyreal", tname = "year", xformla = ~1, data = mpdta, control_group = "nevertreated")

summary(cs_out)

# Aggregate to an overall ATT

overall_agg <- aggte(cs_out, type = "simple")
overall_agg

# The overall ATT is -0.04, which is the average ATT for all groups in their different post-treatment
  # periods, and weighing the effects by size. It controls by not using already treated units. The TWFE from
  # before is about the same as the overall ATT (-0.0365 vs -0.04)


# c) Examine the event-study version of the Callaway-Santanna results:

cs_dyn = aggte(cs_out, type = "dynamic")
ggdid(cs_dyn)

ggsave("cs_dyn.png")

# In this plot, we can see that the pre-treatment effects are close to 0 (and include 0), providing support for the parallel trends assumption.
  # Then once treatment occurs, we see a negative trend that grows overtime, which represents the dynamic time effects of treatment. 


#-----------------------------------------------------------------------------------------------------------------

# 2.3 Pre-testing the parallel trends assumption

# a) Re-run the CS estimator with bootstrapped standard errors to obtain valid uniform
  # confidence bands and a joint pre-test:

cs_out_bt = att_gt(yname = "lemp", gname = "first.treat", idname = "countyreal", tname = "year", xformla = ~1, data = mpdta, control_group = "nevertreated", bstrap = TRUE, cband = TRUE)
summary(cs_out_bt)

# p-value for pre-test = 0.16812

# This test evaluated the parallel trends assumption pre-treatment. The H0 is this case would be that all pre-treatment ATT values = 0. 
  # This means that before any policy was enforced, the treated and control groups were moving the same. However, due to the large p-value we
  # got from running this test, we FAIL TO REJECT the Null. 

# b) Visualize all group-time ATT estimates — both pre- and post-treatment — with:

ggdid(cs_out_bt)
ggsave("mpdta_att_gt.pdf", width = 10, height = 6)

# We see that all pre-treatment ATT estimates are indistinguishable from 0 because the confidence bands cross the horizontal line at 0. This means
  # that it adds support for the parallel trends assumption because all started at the same place. 


# c) In a comment (2–3 sentences), reflect on the limitations of pre-testing. Even if we can-
  # not reject parallel trends in the pre-period, can we be certain the assumption holds
  # during the post-treatment period? What is the pre-test actually telling us, and what is
  # it not telling us?

# Although the pre-treatment estimates are indistinguishable from 0, we cannot be certain that the 
  # parallel trends assumption hold in the post-treatment period. The pre-test only examines whether
  # treated and control groups followed similar trends before treatment, but it does not guarantee that
  # these trends would have continued in the absence of treatment. With the large p-value, we just fail to reject
  # the null hypothesis of parallel trends in the pre-period. This does not prove that the assumption is true. 


#--------------------------------------------------------------------------------------------------------------------------------------

# 2.4 Comparing Control Group Specifications

# a) Re-estimate the CS model using not-yet-treated counties as the control group:

cs_out_nyt = att_gt(yname = "lemp", gname = "first.treat", idname = "countyreal", tname = "year", xformla = ~1, data = mpdta, control_group = "notyettreated")

aggte(cs_out_nyt, type = "simple")

# ATT = -0.0398

# The never-treated estimate from 2.2b was -0.0365, but this ATT of "notyettreated" is -0.0398. These results are
  # very similar in both size and magnitude. This suggests that the results are robust to the choice of control group. 

# b) Produce and save an event-study plot for this specification:

cs_dyn_nyt = aggte(cs_out_nyt, type = "dynamic")
ggdid(cs_dyn_nyt)
ggsave("mpdta_event_study_nyt.pdf", width = 7, height = 4)

# In comparison to the trends seen in 2.2c, the graphs look exactly the same to me. Using the broader control group did not
  # change the conclusions drastically, if at all. 

# c) In a comment (2–3 sentences), discuss the trade-off between the two control group
  # choices. Under what conditions would you prefer never-treated as the control group?
  # When might not-yet-treated be preferable despite the additional assumption it requires?


# Using never-treated units as the control group provides a cleaner comparison because units are never
  # exposed to treatment and therefore are unlikely to be affected by it. However, this approach relies on
  # smaller sample size, which can reduce precision. 

# In contrast, using not-yet treated units expands the control group and can improve statistical power, but it requires
  # the additional assumption that these units are not influenced by their future treatment before it occurs (maybe anticipation placebo). 
  # If this assumption is violated, the estimates may be biased.

#----------------------------------------------------------------------------------------------------------------------------------------------

# 2.5 Why does TWFE fail in staggered settings?

# a) In a comment (3–5 sentences), explain intuitively why the naive TWFE estimator can
  # produce misleading results in staggered DiD settings. What is the “forbidden comparison” problem? 
  # Which units get used as the control group in a way that is problematic,
  # and why is that a problem if treatment effects are heterogeneous across cohorts or over time?

# The naive TWFE estimator can produce misleading results in staggered DiD settings because it uses already-treated units as control groups for newly treated
  # units. This creates "forbidden comparisons," where units that have been affected by the treatment are incorrectly used if they were untreated. 
  # This is problematic because once a unit is treated, its outcomes are no longer a valid counterfactual for untreated outcomes. If treatment effects
  # If TE vary across cohorts or change over time, TWFE will mix these effects in a way that can bias the overall estimate. So, TWFE does not recover a clear
  # causal effect in staggered settings and instead produces a weighted average of comparisons.


# b) Compare the TWFE estimate from question 2.2a to the Callaway-Sant´ anna estimate
  # from question 2.2b. Are they similar or different? In a comment, based on the event-
  # study pre-trends from question 2.2c, which estimate do you find more credible and why?

# 2.2a TWFE = -0.03654
# 2.2b CSA = -0.04

# The TWFE estimate differs from the Callaway-Santanna estimate, which suggests that the naive TFWE approach may be biased
  # in this setting. Given the staggered adoption of the treatment and the evidence of heterogeneous trends across cohorts, the TWFE 
  # estimator is likely combining treated vs treated units. Based on the event study results, the pre-treatment estimates are generally 
  # close to zero, supporting the parallel trends assumption for the Callaway-Santanna approach. Therefore, this appraoch is more credible
  # because it accounts for treatment timing and avoids the forbidden comparison problem in the TWFE. This can also be compared with the trends
  # seen in 2.2c.









