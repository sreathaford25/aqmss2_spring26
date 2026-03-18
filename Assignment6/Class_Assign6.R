# Sara Reathaford
# Class - Assignment 6

# Card-Krueger Minimum Wage

library(dplyr)
library(tidyr)
library(ggplot2)
library(fixest)
library(modelsummary)
install.packages("did")
library(did)

#----------------------------------------------------------------------------------

# 1.1 Data Setup and Exploration

df = read.csv("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/other/minwage.csv")

# a) Create a NJ dummy variable that equals 1 if location is not "PA" and 0 otherwise. Report the number of restaurants in NJ vs. PA using table(). Then compute the average
  # wageBefore and wageAfter separately for NJ and PA restaurants using group by() and
  # summarise(). In a comment, note whether wages in NJ increased relative to PA after
  # the policy change.

df = df %>% mutate(NJ = ifelse(location != "PA", 1, 0))
table(df$NJ)
# 0 1
# 67 291

# NJ restaurants (coded 1) outnumber PA restaurants (coded 0) because the NJ sample spans four sub-regions
  # (centralNJ, northNJ, shoreNJ, southNJ) while PA is treated as a single region


df %>%
  group_by(NJ) %>%
  summarise(
    mean_wage_before = mean(wageBefore, na.rm = TRUE),
    mean_wage_after = mean(wageAfter, na.rm = TRUE))

# Before the policy change, average starting wages in NJ and PA were nearly identical (both close to the federal
  # minimum of $4.25). After the NJ minimum wage rose to $5.05, NJ wages increased noticeably while PA wages
  # remained flat, confirming the policy raised wages in the treated state.


# b) Compute the simple DiD estimate manually using the following steps:
  # • For NJ: mean(fullAfter) - mean(fullBefore)
  # • For PA: mean(fullAfter) - mean(fullBefore)
  # • DiD = (NJ after − NJ before) − (PA after − PA before)
  # In a comment, interpret the result in words. What does this number say about the
  # effect of the minimum wage increase on employment?

means = df %>%
  group_by(NJ) %>%
  summarise(
    before = mean(fullBefore, na.rm = TRUE),
    after = mean(fullAfter, na.rm = TRUE),
    change = after - before)
means

nj_change = means$change[means$NJ == 1]
pa_change = means$change[means$NJ == 0]
did_est = nj_change - pa_change
cat("DiD estimate:", round(did_est, 3), "\n")

did_est

# DiD estimate = 2.927

# The DiD estimate is the difference in within-group changes. A positive value means full-time employment grew
  # more (or fell less) in NJ than in PA after the minimum wage increase, which contradicts the standard prediction that
  # higher minimum wages reduce employment.

# c) To run regressions, reshape the data to long format (one row per restaurant-period)
  # using the following code:

df_long = df %>%
  mutate(id = row_number()) %>%
  pivot_longer(
    cols = c(fullBefore, fullAfter),
    names_to = "period",
    values_to = "full_emp") %>%
  mutate(
    post = ifelse(period == "fullAfter", 1, 0),
    NJ = ifelse(location != "PA", 1, 0))

nrow(df_long)
# 176

nrow(df)
# 358

# The long-format dataset has exactly twice as many rows as the original. The DiD regression requires long format
  # because the interaction post × NJ is the DiD estimator: it captures how the within-NJ change in employment (post
  # - pre) differs from the corresponding within-PA change.



#--------------------------------------------------------------------------------------------

# 1.2 DiD Regression


# a) Estimate the DiD regression using fixest:

# Report the results using modelsummary(). Identify and interpret the coefficient on the
  # interaction term post:NJ: this is the DiD estimator. Compare it to your manual calculation 
  # from question 1.1b — they should match.

m_did = feols(full_emp ~ post * NJ, data = df_long, cluster = ~id)

modelsummary(m_did, stars = TRUE, gof_map = c("nobs", "r.squared"),
             output = "markdown")

m0 = feols(full_emp ~ post * NJ, data = df_long)
m1 = lm(full_emp ~ post * NJ, data = df_long)

df_long$postNJ = df_long$post * df_long$NJ
df_long$treated = ifelse(df_long$NJ == 1 & df_long$post == 1,
                         1,0)

m2 = feols(full_emp ~ treated | id + post, data = df_long)
m3 = lm(full_emp ~ treated + factor(id) + factor(post), data = df_long)

modelsummary(list(m0,m1,m2,m3), stars = TRUE)

# post = -2.493 is diff between counterfactural and general obs
# NJ = -2.693 diff in time pre-treatment
# post x NJ = 2.927 the interaction term we are trying to measure

# The coefficient on post:NJ is the DiD estimator and should match the manual calculation from 1.1b. The post
  # coefficient captures the pre–post change in PA (the counterfactual trend), the NJ coefficient captures the baseline
  # NJ–PA gap, and the interaction captures the additional change in NJ relative to that trend.

# Both coefficients are 2.927


# b) Add chain fixed effects to absorb time-invariant differences across fast food chains:
  # m_did_fe = feols(full_emp ~ post * NJ | chain, data = df_long, cluster = ~id)
  # Compare the two models in a single modelsummary() table. Does controlling for chain
  # type change the DiD estimate noticeably? In a comment, explain what the chain fixed
  # effects are absorbing and why controlling for them may or may not matter here

m_did_fe = feols(full_emp ~ post * NJ | chain, data = df_long, cluster = ~id)
modelsummary(
  list("DiD" = m_did, "DiD + Chain FE" = m_did_fe),
  stars = TRUE, gof_map = c("nobs", "r.squared"),
  output = "markdown")


# The DiD estimate does not change when chain fixed effects are added. Chain FEs absorb baseline differences in
  # staffing levels across fast-food chains (e.g., Wendy’s may have structurally different employment levels than KFC),
  # but since chain type is roughly balanced across states, controlling for it has little impact on the DiD coefficient.



# c) In a comment, state the parallel trends assumption for this specific example. What
  # would we need to observe about NJ and PA employment trends in the pre-period to
  # be confident in the DiD estimate? Give one concrete example of something that could
  # violate this assumption (i.e., something that would affect NJ but not PA employment
  # independently of the minimum wage change).

# The parallel trends assumption here requires that, absent the NJ minimum wage increase, employment trends in
  # NJ and PA fast-food restaurants would have been the same from February to November 1992. This is plausible
  # because both states share a similar economic environment and the two surveys were close together in time, limiting
  # opportunities for diverging trends. A concrete violation would occur if NJ experienced an independent economic
  # shock during this period — for instance, if a major employer opened or closed plants in NJ between the two survey
  # waves, this would change NJ employment for reasons unrelated to the minimum wage, biasing the DiD estimate.


#--------------------------------------------------------------------------------------------------------


# 1.3 Wages as a Validation Check

# a) Repeat the DiD analysis using wages as the outcome instead of employment. Reshape
  # the data for wages and estimate the model:
  # Report the results. Did the minimum wage increase actually raise wages in NJ relative
  # to PA? Is the sign and magnitude of the DiD coefficient what you would expect?

df_long_wage = df %>%
  mutate(id = row_number()) %>%
  pivot_longer(
    cols = c(wageBefore, wageAfter),
    names_to = "period",
    values_to = "wage") %>%
  mutate(
    post = ifelse(period == "wageAfter", 1, 0),
    NJ = ifelse(location != "PA", 1, 0))

m_wage = feols(wage ~ post * NJ, data = df_long_wage, cluster = ~id)

modelsummary(m_wage, stars = TRUE, gof_map = c("nobs", "r.squared"),
             output = "markdown")

# The interaction coefficient post:NJ is positive and statistically significant: wages rose substantially in NJ relative to
  # PA after the policy change, and the magnitude is consistent with the $0.80 minimum wage increase ($5.05 - $4.25).
  # This is precisely the sign and magnitude one would expect if the law was actually binding.


# b) In a comment, explain why the wage result is important for interpreting the employment DiD. If wages had not risen in NJ after the law change, what would that imply
  # about the employment result? Why is it reassuring (or not surprising) that wages did
  # rise in NJ?

# The wage DiD serves as a “first stage” or manipulation check. If wages had not risen in NJ after the minimum
  # wage increase, it would be unclear whether the study is truly estimating the effect of a minimum wage change at all
  # — the law might not have been binding, or firms might have already been paying above the new minimum. The fact
  # that wages did rise in NJ gives us confidence that the treatment actually occurred as intended, so the employment
  # 4 DiD can be credibly interpreted as a causal response to the minimum wage increase rather than a spurious or null
  # comparison.




