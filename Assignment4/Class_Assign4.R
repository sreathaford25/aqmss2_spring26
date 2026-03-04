# Sara Reathaford
# AQMSS2
# In-Class Assignment 4


# Part 1 - Corruption and Wealth

# 1. Setup and data exploration

# a) load the dataset

library(dplyr)
library(broom)
library(ggplot2)
library(modelsummary)
library(marginaleffects)
install.packages("readstata13")
library(readstata13)
library(haven)

df = read_dta("https://raw.githubusercontent.com/franvillamil/AQM2/refs/heads/master/datasets/other/corruption.dta")

# b) drop observations with missing values on the key variables

df = df %>% filter(!is.na(ti_cpi) & !is.na(undp_gdp))
nrow(df)
# 170

# c) summary statistics

summary(df$ti_cpi)

sd(df$ti_cpi)
## 2.105143

summary(df$undp_gdp)

sd(df$undp_gdp)
## 9986.849

# The corruption index ranges from its minimum to its maximum on the 0–10 scale. GDP per capita has a large
# standard deviation relative to its mean and a maximum far above the median, indicating right skewness.

# 2 Exploratory visualization

# a) scatterplot of corruption vs GDP per capital (level)

ggplot(df, aes(x = undp_gdp, y = ti_cpi)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "GDP per capita (PPP)", y = "Corruption Perceptions Index")

# b) The relationship is positive—richer countries tend to be less corrupt—but the pattern is clearly non-linear. Most
# countries cluster at low GDP values, and the linear fit does not capture the curvature well

# c) Scatter plot with log-transformed GDP:

ggplot(df, aes(x = log(undp_gdp), y = ti_cpi)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "log(GDP per capita)", y = "Corruption Perceptions Index")

# The log transformation spreads out the lower-income countries and compresses the upper tail, producing a much
# more linear relationship.

# 3 Bivariate Regression

# a–b) Estimate the level-level model:

m1 = lm(ti_cpi ~ undp_gdp, data = df)
  tidy(m1)

# The coefficient on undp_gdp gives the predicted change in the corruption index for a one-dollar increase in GDP per
  #capita. For a $10,000 increase, multiply the coefficient by 10,000:

  coef(m1)["undp_gdp"] * 10000
# undp_gdp = 1.729782
  
# c) Predicted corruption at the 25th and 75th percentiles of GDP:
  
  q25 = quantile(df$undp_gdp, 0.25)
  q75 = quantile(df$undp_gdp, 0.75)
  c(q25, q75)

  predictions(m1, newdata = datagrid(undp_gdp = c(q25, q75))) 
 
  # The difference in predicted corruption between a country at the 75th percentile and one at the 25th percentile of
  # GDP captures the interquartile range effect. The confidence intervals indicate the precision of these predictions.
  
  
# 4 Non-linear specifications
  
# a–b) Log model:
  
  m2 = lm(ti_cpi ~ log(undp_gdp), data = df)
  tidy(m2)

  # In a level-log model, a 1% increase in GDP per capita is associated with a change of β1/100 in the corruption index.
  # For a doubling of GDP (log(2) ≈ 0.693):
  
  coef(m2)["log(undp_gdp)"] * log(2)
  
  plot_predictions(m2, condition = "undp_gdp")  
  
# c) Quadratic model:
  
m3 = lm(ti_cpi ~ undp_gdp + I(undp_gdp^2), data = df)
  tidy(m3)

plot_predictions(m3, condition = "undp_gdp")
  
# d) Compare R2:
  
  r2 = c(
    "Level-Level" = summary(m1)$r.squared,
    "Level-Log" = summary(m2)$r.squared,
    "Quadratic" = summary(m3)$r.squared)
  r2
  
#The log specification fits the data best, consistent with the scatter plots showing a concave relationship. A non-linear
  #specification is appropriate because the marginal return to additional GDP diminishes at higher income levels:
    #moving from $1,000 to $5,000 matters more for governance quality than moving from $25,000 to $29,000
  
# 5 Marginal Effects

# a) Average marginal effect of GDP in the log model:
  
  avg_slopes(m2, variables = "undp_gdp")
  
# b) The AME differs from the raw coefficient on log(undp_gdp) because the marginal effect of GDP in a level-log
  #model depends on the level of GDP: ∂y/∂x = β/x. The AME averages this over all observed values. It tells us the
  #average predicted change in the corruption index for a one-dollar increase in GDP across all countries in the sample.
  
# c) Marginal effects of the quadratic model at specific GDP values:

  slopes(m3, variables = "undp_gdp",
         newdata = datagrid(undp_gdp = c(2000, 10000, 30000)))
  
# The marginal effect of GDP on corruption diminishes as countries become richer. At low GDP levels, an additional
  #dollar of income has a larger predicted effect on corruption than at high GDP levels. This is consistent with the
  # concave shape of the relationship.
  
# Prediction Plots

# a) Prediction plot for the log model:
  
  p1 = plot_predictions(m2, condition = "undp_gdp")
  p1
  
  ggsave("pred_plot_m2.png", p1, width = 6, height = 4)
  
# b) Prediction plot for the quadratic model:
  
  p2 = plot_predictions(m3, condition = "undp_gdp")
  p2

  ggsave("pred_plot_m3.png", p2, width = 6, height = 4)
  

# c) Both models tell a similar story: corruption decreases sharply with initial increases in GDP and then levels off at
  # higher income levels. The log model produces a smoother curve, while the quadratic model can curve back upward
  # at very high GDP values (a feature of the parabolic functional form that may not be substantively meaningful).
  
  
# Residual Diagnostics
  
# a) Residuals vs. fitted for the level-level model:
  
  m1_aug = augment(m1)
  
  ggplot(m1_aug, aes(x = .fitted, y = .resid)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "Fitted values", y = "Residuals", title = "Residuals vs Fitted: Level-Level (m1)")
  
 # The residual plot shows a clear curved pattern, indicating that the linear specification misses the non-linear
  #relationship. The spread of residuals also appears to increase with fitted values, suggesting heteroskedasticity. 
  
  
#  b) Residuals vs. fitted for the log model:
  
  m2_aug = augment(m2)
  
  ggplot(m2_aug, aes(x = .fitted, y = .resid)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "Fitted values", y = "Residuals", title = "Residuals vs Fitted: Level-Log (m2)")
  
  
# The log transformation substantially improves the residual pattern. The curvature is reduced, though some
 # heteroskedasticity may remain.  
  

# c) Cook’s distance for influential observations:
  
  n = nrow(df)
  threshold = 4 / n
  
  cooks_d = cooks.distance(m2)
  influential = which(cooks_d > threshold)
  df$cname[influential]
  
  plot(m2, which = 4)
  
# d) Influential observations should not be removed automatically. They may represent genuine cases (e.g., very
  #wealthy or very corrupt countries) rather than data errors. A recommended robustness check would be to reestimate the model excluding these observations and compare the coefficients. If the results are similar, the original
#estimates are robust.
  
  
  
# 8 Publication-quality table
  
# a) Regression table comparing all three models:

 table <- modelsummary(
    list("Level-Level" = m1, "Level-Log" = m2, "Quadratic" = m3),
    vcov = "robust",
    stars = TRUE,
    gof_map = c("r.squared", "nobs"),
    output = "markdown")

 ggsave("table_class_assign4.png")
 
 
# b) The level-log model (m2) is the preferred specification. It has the highest R2, produces the best residual
 # diagnostics, and its functional form has a clear substantive interpretation: the relationship between wealth and
  #corruption is one of diminishing returns. The log transformation also avoids the quadratic model’s problem of an
  # eventual sign reversal at extreme values.
  
  
  
  
  
  
  