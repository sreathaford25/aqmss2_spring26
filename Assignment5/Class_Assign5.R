# Sara Reathaford
# AQMSS2
# In-class Assignment 5
# Presidential Approval

library(tidyverse)
library(modelsummary)
library(marginaleffects)
library(dplyr)
library(ggplot2)
install.packages("fixest")
library(fixest)

# 1.1 Setup and data exploration

# a) Load the dataset. How many unique states and years are in the data? Use length(unique())
# or n distinct() to check. Is the panel balanced (i.e., does every state appear the same
  # number of times)?

df <- read.csv("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/presidential_approval/presidential_approval.csv")

length(unique(df$State))
# 50
length(unique(df$Year))
#32

table(table(df$State))

# The panel is balanced: every state appears the same number of times (one observation per state-year)

# b) Compute summary statistics for PresApprov and UnemPct using summary() or modelsummary::datThen plot PresApprov over Year for a few selected states (e.g., California, Texas, New
  # York) to visualize the panel structure:

summary(df$PresApprov)
summary(df$UnemPct)

df_sub = df %>%
  filter(State %in% c("California", "Texas", "NewYork"))

ggplot(df_sub, aes(x = Year, y = PresApprov, color = State)) +
  geom_line() +
  theme_minimal() +
  labs(x = "Year", y = "Presidential approval (%)", color = "State")

# The three states move closely together over time, tracking the same large swings in approval. This parallel movement
  # suggests that common national factors (e.g., the incumbent president’s party, economic cycles, foreign policy events)
  # are the dominant driver of approval, while state-level differences are relatively stable

# c) Create a scatter plot of PresApprov (y-axis) against UnemPct (x-axis) across all state-year
  # observations. Add a regression line with geom smooth(method = "lm"). In a comment,
  # describe the cross-sectional relationship: does higher unemployment seem to be associated with lower or higher approval ratings?

ggplot(df, aes(x = UnemPct, y = PresApprov, color = State)) +
  geom_point(alpha = 0.4) +
  #geom_smooth(method = "lm") +
  theme_minimal() + theme(legend.position = "none")
  labs(x = "Unemployment rate (%)", y = "Presidential approval (%)")

# Across state-year observations, higher unemployment is associated with lower presidential approval. However,
  # this cross-sectional pattern pools observations across states and years, so it reflects both within-state variation over
  # time and permanent between-state differences in unemployment levels and approval — making it difficult to draw
  # causal conclusions.

# 1.2 Pooled OLS

# a) Estimate a pooled OLS model regressing presidential approval on unemployment:
  # m pooled = lm(PresApprov ~ UnemPct, data = df). Report the results using summary()
  # or modelsummary(). In a comment, interpret the coefficient on UnemPct: what does it
  # say about the relationship between unemployment and approval?

m_pooled <- lm(PresApprov ~ UnemPct, data = df)
summary(m_pooled)

# The coefficient on UnemPct is negative: a one-percentage-point increase in the unemployment rate is associated with
  # a decrease of that magnitude in the presidential approval rating. This relationship is statistically significant, but it
  # conflates variation across states with variation within states over time.


# b) Add South as a control:
  # m pooled2 = lm(PresApprov ~ UnemPct + South, data = df). Does controlling for
  # whether a state is in the South change the coefficient on UnemPct? In a comment, explain why or why not.

m_pooled2 = lm(PresApprov ~ UnemPct + South, data = df)
summary(m_pooled2)

# Controlling for southern state status changes the coefficient on UnemPct only modestly. This suggests that the bivariate OLS estimate was not strongly confounded by the North–South distinction: southern states differ systematically
  # from the rest in their approval levels, but this difference is not strongly correlated with the unemployment-approval
  # association in this pooled specification


# c) In a comment, reflect on the limitations of pooled OLS for this type of data. What kinds
  # of unobserved, time-invariant differences across states might bias the estimate of the
  # unemployment effect? Give two or three concrete examples.

#  Pooled OLS is problematic for panel data because it ignores unobserved, time-invariant differences across states
  #that may be correlated with unemployment. For example: (1) states with historically weaker economies may have
  # structurally higher unemployment and different political cultures that shape baseline approval; (2) states in particular regions may have persistent partisan leanings that affect how residents evaluate the president independently of
  # economic conditions; (3) states with large unionized labor forces may have both higher unemployment sensitivity
  # and different approval baselines. All of these would produce omitted variable bias in the pooled OLS estimate.


# 1.3 Entity Fixed Effects

# a) Estimate a model with state fixed effects using fixest:
# Report the results alongside the pooled OLS model in a single modelsummary() table.
  # How does the coefficient on UnemPct change compared to pooled OLS?

m_fe <- feols(PresApprov ~ UnemPct | State, data = df)
modelsummary(list("Pooled OLS" = m_pooled, "State FE" = m_fe),
             vcov = ~State,
             stars = TRUE,
             gof_map = c("r.squared", "nobs"),
             output = "markdown")

# The coefficient on UnemPct changes relative to pooled OLS. The state fixed effects model compares approval within
  # the same state across different years, removing the influence of any time-invariant state characteristics.

# b) In a comment, explain what the state fixed effects are absorbing. Note that the South
  # variable drops out of the model — why can’t it be estimated when state fixed effects
  # are included? What does this imply about any variable that does not vary within a
  # state over time?

# State fixed effects absorb all time-invariant differences across states — including geography, political culture,
  # long-run economic structure, and regional identity. This is precisely why South drops from the model: it does not
  # vary within a state over time, so its effect is indistinguishable from the state-specific intercept (fixed effect). Any
  # time-invariant variable is collinear with the set of state dummies and cannot be estimated separately.

# c) What does the coefficient on UnemPct now identify? In a comment, explain the intuition: the state FE estimator compares approval ratings within the same state across
  # different years, rather than across different states. How does this differ from the pooled
  # OLS interpretation?

# The coefficient on UnemPct in the state FE model identifies a within-state effect: it measures how approval
  # changes in a given state when its unemployment rate rises or falls, compared to that state’s own average. This
  # is fundamentally different from pooled OLS, which compares states with different unemployment levels to each
  # other. The FE estimator controls for all stable state-level confounders (observed or not) but cannot account for
  # time-varying omitted variables.


# 1.4 Two-way Fixed Effects

# a) Add year fixed effects to absorb common time shocks (e.g., national economic conditions, wars, presidential scandals) that affect all states simultaneously:
  #  m_twfe = feols(PresApprov ~ UnemPct | State + Year, data = df)

m_twfe = feols(PresApprov ~ UnemPct | State + Year, data = df)


# b) Compare all three models in a single modelsummary() table with standard errors clustered by state:

modelsummary(
  list("Pooled OLS" = m_pooled, "State FE" = m_fe, "Two-Way FE" = m_twfe),
  vcov = ~State,
  stars = TRUE,
  gof_map = c("r.squared", "nobs"),
  output = "markdown")

# c) In a comment, discuss what the year fixed effects are controlling for. Does adding them
  # change the coefficient on UnemPct? If so, what does that suggest about the role of common time trends in driving the relationship between unemployment and approval?

# Year fixed effects absorb common time shocks: national economic cycles, presidential scandals, wars, or any other
  # event that affects approval in all states simultaneously in a given year. If national unemployment rises during a
  # recession, both the unemployment rate and presidential approval will move together in all states at once — not
  # because of a state-level effect but because of the shared macro environment. Adding year dummies removes this
  # source of confounding and identifies the effect of a state’s unemployment relative to the national average in each
  # year. If the coefficient on UnemPct changes noticeably after adding year FEs, it suggests that common time trends
  # were partly driving the relationship estimated with state FEs alone.








