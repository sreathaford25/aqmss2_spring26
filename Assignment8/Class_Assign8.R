# Sara Reathaford
# Class Assignment 8
# Spatial Data II

library(sf)
install.packages("spData")
library(spData)
install.packages("spdep")
library(spdep)
install.packages("spatialreg")
library(spatialreg)
library(ggplot2)

data(world)

# 1.1 Setup and OLS baseline

# a) Load the world dataset. Remove rows with missing gdpPercap or lifeExp, and remove
  # Antarctica. How many observations remain? Log-transform gdpPercap and store it as
  # a new column log gdp:

world = world[!is.na(world$gdpPercap) & !is.na(world$lifeExp), ]
world = world[world$continent != "Antarctica", ]
world$log_gdp = log(world$gdpPercap)

# 160

# After removing observations with missing gdpPercap or lifeExp and dropping Antarctica, 160 countries remain.
  # We log-transform GDP per capita because the raw variable is strongly right-skewed: 
  # a handful of very rich countries have values far above the bulk of the distribution. 
  # The log transformation compresses the upper tail and makes the relationship between GDP and
  # life expectancy more linear, which is an assumption of OLS.

# b) Fit an OLS regression of life expectancy (lifeExp) on log GDP per capita (log gdp):

ols_fit = lm(lifeExp ~ log_gdp, data = world)
summary(ols_fit)

# The coefficient on log_gdp is positive and statistically significant (p < 0.001). 
  # It means that a one-unit increase in log GDP per capita — roughly a doubling of 
  # GDP per capita — is associated with higher life expectancy by approximately that
  # many years on average. The model explains a substantial share of cross-country variation 
  # in life expectancy, as reflected by the R².

# c) Save OLS residuals and map them:

world$ols_resid = residuals(ols_fit)

ggplot(world) +
  geom_sf(aes(fill = ols_resid), color = "white", linewidth = 0.2) +
  scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#d6604d",
                       midpoint = 0, name = "OLS residual") +
  theme_void() +
  labs(title = "OLS residuals: life expectancy ~ log GDP per capita")

ggsave("ols_residuals_map.pdf", width = 10, height = 5)

# The residual map reveals clear geographic clustering. Sub-Saharan Africa shows a concentration of negative
  # 2 residuals — countries with lower life expectancy than the model predicts given 
  # their income level, likely due to high HIV/AIDS prevalence and disease burden. 
  # Western Europe and parts of East Asia display positive residuals, indicating that these 
  # regions achieve higher life expectancy than income alone predicts. This non-random geographic
  # pattern in the residuals is a visual signal of spatial autocorrelation

# People in Africa are living less than they should based on their GDP per capita, but people in Central
  # and Latin America and the Mediterranean are living longer than they should based on their GDP. 
#------------------------------------------------------------------------------------------------------

# 1.2 Spatial weights matrix

# a) Create a queen contiguity neighborhood and row-standardized spatial weights. Recall
  # from class (and Assignment 7) that queen contiguity defines neighbors as any polygon
  # sharing at least one point:

nb = poly2nb(world, queen = TRUE)
listw = nb2listw(nb, style = "W", zero.policy = TRUE)
summary(nb)

# Some countries have zero neighbors in the contiguity matrix. These are island nations (e.g., New Zealand, Japan,
  # Caribbean states, Pacific island states) that share no land boundary or common border point with any other polygon
  # in the dataset. Queen contiguity requires at least one shared point; islands surrounded by ocean have none, so they
  # are isolated nodes in the weights graph. The zero.policy = TRUE argument allows these units to remain in the
  # analysis despite having no neighbors.

# b) Test Moran’s I on the OLS residuals:

moran.test(world$ols_resid, listw = listw, zero.policy = TRUE)

# The Moran’s I statistic is positive and the p-value is well below 0.05, indicating statistically 
  # significant positive spatial autocorrelation in the OLS residuals. Countries close to each other 
  # tend to have similar residuals — either both overestimated or both underestimated — which violates 
  # the OLS assumption of independent errors. Ignoring this pattern yields inefficient estimates and 
  # invalid standard errors.

# ------------------------------------------------------------------------------------------------------------------------------

# 1.3 Lagrangian Multiplier Tests

# Recall from class that when Moran’s I on residuals is significant, we face a choice: should we
  # use the Spatial Error Model (SEM) or the Spatial Lag Model (SLM)? The Lagrange Multiplier
  # (LM) tests help guide this decision. Run all four tests at once:

lm_tests = lm.LMtests(ols_fit, listw = listw,
                      test = c("LMerr", "LMlag", "RLMerr", "RLMlag"),
                      zero.policy = TRUE)
summary(lm_tests)

# a) Report the test statistics and p-values for LMerr and LMlag. Are both significant? In a
  # comment, recall from class what each of these tests is checking: LMerr tests for spatial
  # dependence in the error term (λ ̸= 0), while LMlag tests for a spatially lagged dependent
  # variable (ρ ̸= 0).

# LMerr tests whether there is spatial dependence in the error term (λ ̸= 0 in the SEM). LMlag tests whether a
  # spatially lagged dependent variable belongs in the model (ρ ̸= 0 in the SLM). Both tests are significant (p < 0.05),
  # meaning both types of spatial dependence appear to be present in some form when tested individually. When both
  # standard LM tests are significant, we turn to the robust versions to discriminate.

# b) Report the robust versions RLMerr and RLMlag. The robust tests control for the presence
  # of the other type of dependence. Which is more significant? Based on the LM decision
  # rule from class (if both LM tests are significant, compare the robust versions), which
  # model would you choose: SEM or SLM? Write your reasoning in a comment.

# The robust tests (RLMerr, RLMlag) each control for the presence of the other type of spatial dependence. Comparing
  # them: if RLMerr is more significant than RLMlag, the evidence favors the SEM; if RLMlag dominates, the SLM is
  # preferred. Based on the decision rule from class — select the model whose robust test is more significant — the
  # output above guides the choice between the two spatial models for Part 2


#------------------------------------------------------------------------------------------------------------------------------

# 1.4 Spatial Error Model (SEM)

# Based on the diagnostics above, fit the Spatial Error Model using errorsarlm() from spdep:

sem_fit = errorsarlm(lifeExp ~ log_gdp, data = world,
                     listw = listw, zero.policy = TRUE)
summary(sem_fit)

# a) Report the estimated coefficient on log gdp from the SEM and compare it to the OLS
  # estimate. Has the coefficient changed? Report the λˆ (lambda) parameter and its pvalue. 
  # Is it statistically significant?

# The coefficient on log_gdp from the SEM and the OLS estimate are both reported above. The SEM coefficient
  # may shift somewhat from OLS because the error-structure correction absorbs spatial confounding. The λˆ (lambda)
  # parameter captures spatial autocorrelation in the errors; if it is positive and statistically significant, the SEM has
  # identified genuine spatial dependence in the residual variation.

# b) In a comment (2–3 sentences), explain what λ represents in the SEM. Recall from class:
  # the SEM says u = λWu + ε, meaning the error at each unit is partly a function of
  # neighbors’ errors. If λ > 0 and significant, what does this tell us about the structure of
  # the unmeasured factors driving life expectancy?

# In the SEM, λ governs the spatial autoregressive process in the disturbances: u = λWu + ε. A positive and
  # significant λ means that the unmeasured factors driving life expectancy are spatially correlated — omitted variables
  # such as regional disease environments, cultural practices around healthcare, or cross-border health infrastructure
  # are themselves geographically clustered. The SEM filters this spatial correlation out of the residuals without positing
  # that life expectancy itself directly diffuses across borders.


# c) Check whether the SEM has removed the spatial autocorrelation from the residuals.
  # Save the SEM residuals and run Moran’s test again:

world$sem_resid = residuals(sem_fit)
moran.test(world$sem_resid, listw = listw, zero.policy = TRUE)

# Comparing this Moran’s I to the one from question 2b, the SEM substantially reduces the spatial autocorrelation in
  # the residuals. The test statistic is now much closer to zero and the p-value is no longer significant (or much less so),
  # indicating that the spatial error correction has absorbed most of the geographic clustering that OLS left behind in its
  # residuals.

#----------------------------------------------------------------------------------------------------------------------------------

# 1.5 Distance-based weights: an alternative neighborhood

# So far we have used queen contiguity to define neighbors: two countries are neighbors if
  # their polygons share at least one point. But this misses island nations entirely and treats
  # all shared-border pairs as equally connected regardless of distance. An alternative is to
  # define neighbors based on geographic proximity: two countries are neighbors if the distance
  # between their centroids is below a threshold.


# a) Compute the centroids of every country and build a distance-based neighborhood in
  # which two countries are neighbors if their centroids are within 300 km of each other.
  # Use the following code:

coords = st_centroid(st_geometry(world))
nb_dist = dnearneigh(coords, d1 = 0, d2 = 300)

summary(nb_dist)


# Earlier in the course we said that computing distances requires projecting to a planar CRS 
  # (e.g. UTM). That advice applies when you work within a limited area where a single projection 
  # is accurate. Here we have a global dataset: no single planar projection preserves distances 
  # everywhere on Earth. The function dnearneigh() handles this automatically — when it receives 
  # an sf object with a geographic CRS (WGS84), it computes great-circle distances on the ellipsoid, which
  # are accurate worldwide. The 300 km threshold is therefore interpreted in kilometers
  # without needing to reproject.

#  How many countries now have zero neighbors? Is this number higher or lower
  # than before? Why might that be?

# The distance based neighborhood produces a different structure compared to queen contiguity. 
  # In this case, 114 out of 160 countries have 0 neighbors, which is a large share of the sample.
  # This is higher than with queen contiguity which is a bit surprising. The 300km threshold is quite
  # restrictive on a global scale and many countries (island nations, or water separation) do not have
  # another country within 300km of their centroid. Therefore, this network is fragmented because there are
  # 127 disconnected subgraphs, meaning that the countries are isolated. The distance based weights are sensitive
  # to the choice of distance threshold. A larger threshold might have produced a more connected network. 

# b) Create row-standardized weights from the distance-based neighborhood and fit a SEM
  # using the same formula (lifeExp ~ log gdp):

listw_dist = nb2listw(nb_dist, style = "W", zero.policy = TRUE)

sem_dist = errorsarlm(lifeExp ~ log_gdp, data = world,
                      listw = listw_dist, zero.policy = TRUE)

summary(sem_dist)


# In a comment, report λˆ and its p-value. Compare the log gdp coefficient and λˆ from
  # this model to the contiguity-based SEM in question 1.4a. Are the results substantially
  # different? What does this tell you about the sensitivity of spatial models to the 
  # definition of the neighborhood?

# The coefficient on log_gdp is 5.47 in the distance based model, compared to 3.96 in the contiguity based SEM.
  # Both are highly significant with p < 0.001, but the distance based model produces a larger estimate. This suggests
  # that the estimated effect of GDP on life expectancy is somewhat sensitive to how spatial relationships are defined. 

# The spatial autocorrelation parameter of lambda differs from 0.76 in the contiguity based SEM, but only about 0.42 in the 
  # distance based SEM. Both are statistically significant, but the contiguity based model indicates much stronger spatial
  # dependence in the error term. This could be due to the structure of the spatial weights matrix. The contiguity matrix is more
  # connected, whereas the distance matrix with the 300km threshold is sparse, with many countries having no neighbors. This could
  # produce a result where the distance based model captures weaker spatial dependence.

# c) Run Moran’s I on the residuals of this distance-based SEM (using listw dist). In a
  # comment, does this model also succeed in removing spatial autocorrelation from the
  # residuals? Compare to your answer in 1.4c.

world$sem_dist_resid = residuals(sem_dist)

moran.test(world$sem_dist_resid, listw = listw_dist, zero.policy = TRUE)

# Moran's I for the residuals of the distance based SEM is about -0.0015 with a p value of 0.449, 
  # meaning that this means that it is not statistically significant. Therefore, we fail to reject
  # the null hypothesis of no spatial autocorrelation. There is no evidence of spatial clustering in the 
  # residuals. Compared to the OLS model, this suggests that the SEM with distance weights has successfully
  # removed spatial dependence from the residuals. This result is similar to the contiguity SEM, which also
  # reduced spatial autocorrelation. However, the result should be interpreted cautiously because the distance
  # weights matrix is sparse and many countries have no neighbors.







