# Sara Reathaford
# Assignment 8 HW
# Spatial Lag Model

library(sf)
install.packages("spData")
library(spData)
install.packages("spdep")
library(spdep)
install.packages("spatialreg")
library(spatialreg)
library(ggplot2)

data(world)

# 2.1 Spatial Lag Model (SLM)

# a) Report the estimated ρˆ (rho) parameter and its p-value, and report the coefficient 
  # on log gdp. Is ρ statistically significant?

world = world[!is.na(world$gdpPercap) & !is.na(world$lifeExp), ]
world = world[world$continent != "Antarctica", ]
world$log_gdp = log(world$gdpPercap)

slm_fit = lagsarlm(lifeExp ~ log_gdp, data = world, listw = listw, zero.policy = TRUE)

summary(slm_fit)

# The p (rho) = -0.0042561 with a p-value of 0.805. The p (rho) is negative and not statistically
  # significant, indicating that there is not a presence of spatial dependence in life expectancy.
  # This means that a country's life expectancy is not correlated with the life expectancy of its neighbors. 

# The log_gdp coefficient = 5.54820 and is highly statistically significant, indicating a strong positive
  # relationship between GDP per capita and life expectancy. Overall, while GDP per capita is an important
  # predictor of life expectancy, the SLM provides no evidence that life expectancy itself diffuses across
  # neighboring countries. 

# b) In a comment (2–3 sentences), interpret ρˆ. Recall from class that the SLM captures
  # genuine spatial diffusion: the outcome of unit i is partly determined by its neighbors’
  # outcomes. If ρ >ˆ 0, what does this mean about the relationship between a country’s
  # life expectancy and its neighbors’ life expectancy?

# The parameter p captures spatial diffusion in life expectancy. In this case, p is very close to 0 (-0.004) and
  # not statistically significant. This suggests that a country's life expectancy is not systematically related to
  # the life expectancy of its neighbors. There is not evidence of spatial spillovers or diffusion in the outcome variable.

# This contrasts with the SEM, where spatial dependence was found in the error term. Therefore, spatial correlation
  # appears to arise from unobserved factors rather than direct interaction between countries' life expectancy levels. 

# c) In a comment, explain why the coefficient on log gdp in the SLM output is not the
  # marginal effect of GDP on life expectancy. Recall from class: solving y = ρWy+Xβ+ε
  # for y gives y = (I − ρW)−1(Xβ + ε). What does this equilibrium matrix (I − ρW)−1
  # imply for how a change in xi propagates through the network?

# In the SLM, the coefficient on log_gdp is not the marginal effect because the changes in GDP can propagate through
  # the spatial networks via feedback loops. Specifically, the model implies that a change in one country affects its 
  # neighbors, which in turn feed back into the original country. However, in this case, the p is very close to 0 and not
  # statistically significant, so the spatial feedback mechanism is negligible. As a result, the coefficient on log_gdp is 
  # very close to the true marginal effects. 

# SEM > SLM

# SLM: p = -0.004 (no spatial lag dependence), AIC = 967.93 (worse than OLS)

# SEM: lambda = 0.76 (strong spatial error dependence), AIC = 894.7
  # hidden regional factors are affecting nearby countries

# LM test for residual autocorrelation: p < 0.001 

#-------------------------------------------------------------------------------------------------------------------------------

# 2.2 Direct and Indirect Effects

# a) Compute the equilibrium direct and indirect effects using the impacts() function,
  # passing the SLM fit and the spatial weights. Use R = 500 for simulation-based standard errors 
  # (and set a seed for reproducibility). In a comment, report the direct effect,
  # the indirect effect, and the total effect of log gdp. How does the direct effect compare
  # to the raw log gdp coefficient from the SLM output and to the OLS coefficient?

set.seed(123)

impacts_slm = impacts(slm_fit, listw = listw, R = 500)

summary(impacts_slm)

# The direct effect of log_gdp is about 5.54, meaning that a 1 unit increase in GDP per cap increases
  # a country's life expectancy by 5.54 years, holding all other factors constant. 

# The indirect effect is very small and slightly negative = -0.0235, and its distribution includes zero
  # according to the quantile distribution, indicating that it is not statistically significant. 

# The total effect is approximately 5.52, which is almost identical to the direct effect. This reflects the fact
  # that there are essentially no spatial spillovers in the model. Compared to the OLS estimate, the direct effect is
  # nearly identical. This is because the spatial autoregressive parameter p is close to 0, so there is no meaningful
  # spatial feedback. 


# b) In a comment (2–3 sentences), explain the substantive meaning of the indirect effect.
  # Recall from class: the indirect effect captures the spillover from unit i’s x to all other
  # units’ y, after the spatial feedback loop reaches equilibrium. If log GDP per capita in
  # Country A increases by 1 unit, what does the indirect effect say about life expectancy
  # in neighboring countries?

# The indirect effect captures the spillover impact of a country's GDP on life expectancy in other countries.
  # In this case, the indirect effect is very close to 0 and not statistically significant. This means that 
  # increased in GDP per capita in one country do not have a meaningful impact on the life expectancy in 
  # neighboring countries. This suggests that improvement in economic conditions are largely contained within
  # countries and do not cross borders. 


# c) The total effect is larger than the direct effect. In a comment, explain whether this is an
  # expected feature of the SLM. Under what conditions would the indirect effect be larger
  # or smaller? (Hint: think about what happens to the spillover term as ρ approaches 0
  # versus as ρ grows larger.)

# The total effect is usually larger than the direct effect because it includes the direct and indirect
  # effects on other countries. However, because the indirect effect is negative, but still close to 0, 
  # the total effect is similar to the direct effect. This happens because the spatial autoregressive p
  # is close to 0, and the spillovers are negligible. If p were larger, we would expect stronger spillovers
  # and a larger gap between direct and total effects. 

#---------------------------------------------------------------------------------------------------------------------

# 2.3 Model Comparison

# a) Compare OLS, SEM, and SLM using AIC(). Lower AIC indicates better fit, penalized
  # for model complexity. In a comment, report the three AIC values. Which model has
  # the lowest AIC? Does this agree with your LM-test-based model choice in question 1.3b?

AIC(ols_fit, sem_fit, slm_fit)

# The SEM model has the lowest AIC by a large margin, indicating that it provides the best fit
  # to the data, even after accounting for model complexity. This result is consistent with the LM 
  # tests from earlier, which suggested choosing the SEM over the SLM.

# b) Write a short summary paragraph as a comment in your R script (5–8 sentences). Include all of the following: 
  # (1) whether spatial autocorrelation was present in the OLS residuals and how strong it was; (2) which spatial 
  # model you selected based on the LM tests and why; (3) how the key coefficient estimate on log gdp differs across 
  # OLS, SEM, and SLM; (4) what the SLM implies about life expectancy spillovers across borders; (5) one limitation of 
  # using queen contiguity weights for country-level data (think about what the matrix misses).

# The OLS model showed clear evidence of spatial autocorrelation in the residuals, since Moran's I was significant.
  # This means that the OLS assumption of independent errors is violated and suggests that there is spatial structure
  # in the data. Based on the LM tests, the SEM model was the better fit because the robust test for error dependence
  # was more significant than the spatial lag model. The coefficient in the SEM (about 3.96) compared to OLS and SLM 
  # (around 5.5). This suggests that OLS was overestimating the effect by not accounting for spatial dependence. The SLM
  # results showed that p (rho) is basically 0 and not significant, meaning there is no real evidence that life expectancy
  # spreads across neighboring countries. This is also reflected in the indirect effects, which are essentially 0. If we 
  # look at model fit, the SEM also performs the best, with a much lower AIC (around 894.7) compared to both OLS and SLM. 
  # This supports the idea that spatial dependence is coming from unobserved regional factors rather than spillovers in life 
  # expectancy. One limitation of using queen contiguity weights is that they leave out island countries and treat all neighbors
  # the same, even if they are very far apart, which could potentially miss some important spatial relationships. 





