# Sara Reathaford
# In-Class Assignment 9


# install packages
install.packages("carData")
library(carData)
install.packages("MASS")
library(MASS)
install.packages("nnet")
library(nnet)
library(marginaleffects)
#upload data
data(BEPS)

# 1.1 Ordered logit: perceptions of the national economy

# a) Explore the economic.cond.national variable and convert it to an ordered factor:
  # In a comment, report the distribution across the five categories. Which category is
  # most common? In a second comment, explain why using OLS on this variable would
  # be problematic: think about the equal-spacing assumption and what it implies about
  # the difference between “got much worse” (1) and “got a little worse” (2) versus the
  # difference between “stayed the same” (3) and “got a little better” (4).

table(BEPS$economic.cond.national)
BEPS$econ_ord = factor(BEPS$economic.cond.national, ordered = TRUE)

# The distribution is concentrated between the middle categories 2, 3, and 4 with category 3 
  # being the most common. Less respondents answered on the extremes of 1 and 5. Using OLS
  # here would be problematic because it assumed that each response is equally spaced, when this
  # is not the case with a Likert scale. Instead, an ordered logit would avoid this assumption by
  # estimating the threshold parameters that let the data determine the scale. 

# b) Fit an ordered logit model predicting econ ord from age, gender, Europe, and political.knowledge

m_ologit = polr(econ_ord ~ age + gender + Europe + political.knowledge,
                data = BEPS, Hess = TRUE)

summary(m_ologit)

# In a comment, report the raw coefficient on Europe and its sign.
  # Based on the sign convention, does higher support for European integration predict
  # more optimistic views of the national economy?

# The raw coefficient on Europe is -0.122693, but after applying the sign reversal, this would
  #imply a positive association. Respondents with a stronger pro-EU attitudes tend to perceive 
  # national economic decisions as improved.

# age is not significant, gender is a bit significant, Europe is more sig, and political knowledge is most.
  # can use the t-value and distributions to determine significance 

# c) Compute average marginal effects (AMEs) using marginaleffects:

avg_slopes(m_ologit)

#  In a comment, interpret the AME for Europe on the probability of each category. Does a one-unit increase in
  # pro-Europe attitude increase or decrease the probability of perceiving the economy as improved (category 4 or 5)? 
  # By approximately how much on average across respondents? Note that the AMEs across all five categories for any 
  # given predictor must sum to zero — use this as a sanity check.

# The AMEs show the average change in the probability of each response category associated with a one-unit increase in
  # each predictor. For Europe, the AMEs on the lower categories (1 and 2) are negative, while the AMEs on the higher
  # categories (4 and 5) are positive, consistent with a positive association between pro_EU sentiment and more optimistic
  # economic assessments. As a sanity check, the AMEs for any given predictor must sum to zero across the five categories
  # because probabilities are constrained to sum to one. 

# d) ) Compute predicted probabilities for the five response categories at the mean of all covariates, separately for male 
  # and female respondents:

predictions(m_ologit, newdata = datagrid(gender = c("female", "male")))

# In a comment, compare the predicted probabilities for the most pessimistic category (1 = got much worse) and the most 
  # optimistic category (5 = got much better) for each gender. Are there notable differences by gender? What does this 
  # suggest about gender gaps in economic perceptions?

0.0267+ 0.1874 # female 1 + 2

0.0222 + 0.1615 # male 1 + 2

0.2141 - 0.1837 # female - male = about 3%

preds = tidy(predictions(m_ologit, by = "gender"))

# The predicted probabilities for the most pessimistic category (1 = got much worse) and the most optimistic category
  # (5 = got much better) are shown separately for female and male respondents, with all other covariates held at
  # their sample means. Any gender differences in these predicted probabilities should be modest, given that gender
  # does not appear to be a strong driver of economic perceptions relative to the other covariates in the model. The
  # overlapping confidence intervals for male and female respondents across most categories suggest that the gender
  # gap in economic optimism is not large in this dataset.

#--------------------------------------------------------------------------------------------------------------------------------------

# 1.2 Multinomial logit: vote choice

# a) Set Conservative as the reference category and fit a multinomial logit predicting vote from economic assessments and leader evaluations:

BEPS$vote = relevel(BEPS$vote, ref = "Conservative")
m_mlogit = multinom(vote ~ economic.cond.national + Blair + Hague +
                      Kennedy + Europe, data = BEPS, trace = FALSE)
summary(m_mlogit)

#  In a comment, describe the direction of the Blair coefficient in the Labour vs. Conservative equation. What does a positive coefficient on
  # Blair (feelings toward Tony Blair) in the Labour equation imply about the relationship
  # between Blair approval and the likelihood of voting Labour relative to Conservative?

# The model produces two sets of log-odds coefficients: Labour vs. Conservative and Liberal Democrat vs. Conservative. The coefficient on 
  # Blair in the Labour vs. Conservative equation is strongly positive: higher approval of Tony Blair is associated with substantially greater 
  # log-odds of voting Labour rather than Conservative. This makes intuitive sense — Blair was the Labour leader, so voters who rated him favorably 
  # were much more likely to have voted for his party. By contrast, the Blair coefficient in the Liberal Democrat vs. Conservative equation is 
  # expected to be smaller or near zero, since Blair approval does not strongly differentiate Liberal Democrat voters from Conservatives.

# b) Compute AMEs across all predictors and all outcome categories:

avg_slopes(m_mlogit)

# In a comment, report the AME of Blair on the probability of voting Labour. Interpret it in plain language: holding other variables constant, 
  # how does a one-unit increase in Blair approval change the probability of voting Labour on average across respondents?

# The AME of Blair on the probability of voting Labour is positive and substantial. A one-unit increase in Blair
  # approval (on the 1–5 scale) is associated with a meaningful increase in the average probability of voting Labour,
  # holding all other variables constant. This reflects the strong personalization of vote choice in 1997: feelings toward
  # the party leader were a major driver of vote intention, and Blair in particular was unusually popular relative to his
  # Conservative counterpart.

# c) The multinomial logit assumes Independence of Irrelevant Alternatives (IIA): the odds ratio between any two alternatives is unaffected 
  # by the presence or absence of other alternatives. Recall from class the red bus / blue bus example, where IIA fails because two alternatives 
  # (red bus and blue bus) are close substitutes. In a comment of 2–3 sentences, explain what IIA means for this application with Conservative, 
  # Labour, and Liberal Democrat as alternatives. Do you think IIA is likely to hold here — or are any two of these parties close substitutes in 
  # the minds of British voters? Explain your reasoning.

# c) The multinomial logit assumes Independence of Irrelevant Alternatives (IIA): the odds ratio between any two alternatives (e.g., Labour vs. 
  # Conservative) is unaffected by the presence or characteristics of the third alternative (Liberal Democrats). In the red bus / blue bus analogy, 
  # IIA fails because two alternatives are near-perfect substitutes and removing one simply shifts its probability to the other rather than distributing 
  # it proportionally. For British party choice, IIA is a moderate concern: Labour and the Liberal Democrats are both centre-left parties, sharing
  # some ideological space, so some voters may treat them as partial substitutes in a way IIA cannot accommodate. The Conservatives, however, occupy a 
  # clearly distinct ideological position (right-wing), so the three-party menu is not as degenerate as two buses of different colours. Overall, IIA is 
  # plausible for Conservative vs. the others but is a more legitimate worry for the Labour/Liberal Democrat distinction.


# ------------------------------------------------------------------------------------------------------------------------------------------------------------
# NEW DATA
install.packages("pscl")
library(pscl)
library(ggplot2)
install.packages("AER")
library(AER)
library(MASS)
library(marginaleffects)
data(bioChemists)

# 1.3 Poisson Regression: publication counts

# a) Explore the outcome variable art:

summary(bioChemists$art)
var(bioChemists$art)
pdf("art_histogram.pdf", width = 6, height = 4)
hist(bioChemists$art, breaks = 20, main = "Distribution of articles",
     xlab = "Number of articles", col = "gray80")
dev.off()

ggplot(bioChemists, aes(x = art)) +
  geom_histogram(binwidth = 1, fill = "#294b66", color = "white") +
  theme_minimal() +
  labs(title = "Publications in last 3 years of PhD",
       x = "Number of articles", y = "Count")

# In a comment, report the mean and variance of art. A key diagnostic for count data is
  # whether the variance substantially exceeds the mean — this is called overdispersion
  # and violates the Poisson assumption that mean equals variance. Note whether you
  # observe this pattern here.

# The distribution of art is right-skewed, with a mode at zero and a long upper tail. The mean is around 1.69 while
  # the variance is approximately 3.71 — roughly twice the mean. Under the Poisson assumption, the variance should
  # equal the mean; a ratio substantially above 1 indicates overdispersion. This pattern is a first signal that a standard
  # Poisson model may underestimate uncertainty and produce anti-conservative standard errors.

# b) Fit a Poisson regression of art on all predictors:
m_pois = glm(art ~ fem + mar + kid5 + phd + ment,
             data = bioChemists, family = poisson)
summary(m_pois)

exp(coef(m_pois)["ment"])

# In a comment, answer the following two questions: (1) Report the coefficient on ment
  # and exponentiate it with exp() to obtain the incidence rate ratio (IRR). Interpret it: a
  # one-unit increase in mentor articles multiplies expected student articles by approximately how much? (2) Report the residual deviance and degrees of freedom from the
  # summary() output and compute their ratio. Recall from class that under a correctly
  # specified Poisson model this ratio should be close to 1; a ratio substantially above 1
  # (say, > 2) suggests overdispersion.

# The incidence rate ratio (IRR) for ment is 1.026: each additional article published by the mentor is associated
  # with a multiplicative increase in expected student articles by that factor, holding all else constant. The effect is
  # modest but positive, suggesting that more productive mentors slightly boost student output. The residual deviance
  # is substantially larger than the residual degrees of freedom (their ratio is well above 2), which is another clear
  # diagnostic signal of overdispersion — the Poisson model does not adequately capture the variation in publication
  # counts.

# c) Test for overdispersion formally:
dispersiontest(m_pois)

# In a comment, report the estimated dispersion parameter and the p-value. Is there statistically significant evidence of overdispersion? 
  # What does this imply for the validity of the Poisson standard errors you computed above?

# The dispersion test strongly rejects the null hypothesis of equidispersion (p < 0.001). The estimated dispersion
  # parameter is well above 1, confirming that the variance in art substantially exceeds its mean. This means the
  # Poisson standard errors are too small: the model underestimates uncertainty, inflates test statistics, and produces
  # p-values that are misleadingly small. A model that explicitly accounts for overdispersion — such as the negative
  # binomial — is needed.

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------

# 1.4 Negative Binomial Regression

# a) Fit the negative binomial model with the same formula:
m_nb = glm.nb(art ~ fem + mar + kid5 + phd + ment,
              data = bioChemists)
summary(m_nb)

# In a comment, compare the coefficient on ment to the Poisson estimate from the Poisson model above. Has it changed substantially? Report the estimated overdispersion
  # parameter theta from the NB output. Is the overdispersion modest or severe?
  
# The coefficient on ment is similar to the Poisson estimate, indicating that the point estimate is reasonably stable.
  # The key difference is in the standard errors: the negative binomial model produces larger, more honest uncertainty
  # estimates. The estimated overdispersion parameter theta (shown in the summary) quantifies how much the
  # variance exceeds the Poisson prediction; a smaller theta means more severe overdispersion. Here theta is
  # moderate, indicating meaningful but not extreme extra-Poisson variation.

# b) Compare model fit using AIC:
AIC(m_pois, m_nb)
# m_pois = 3314.113
# m_nb = 3135.917

# In a comment, report both AIC values. Which model has the lower AIC? Recall from
  # earlier in the course that AIC penalizes model complexity, so a lower AIC for the NB
  # model (which has one additional parameter) means the improvement in fit outweighs
  # the added complexity. What does this comparison imply: is overdispersion a problem
  # worth addressing for this dataset?

# The negative binomial AIC is substantially lower than the Poisson AIC, despite the NB model having one additional
  # parameter (theta). Under AIC, the improvement in fit more than compensates for the added complexity. This
  # confirms that overdispersion is a genuine feature of the data, not noise, and that the negative binomial is the more
  # appropriate model for these publication counts.

# c) Compute predicted article counts for male vs. female researchers, holding all other
  # variables at their sample means:

predictions(m_nb, newdata = datagrid(fem = c("Men", "Women")))

# In a comment, report the predicted number of articles for men and women (with confidence intervals). 
  # How large is the gender gap in predicted publications? Is this difference statistically distinguishable 
  # given the uncertainty intervals?

# The predicted number of articles for men exceeds that for women, holding marital status, number of young
  # children, PhD prestige, and mentor productivity constant at their sample means. The confidence intervals provide
  # information on whether this gender gap is statistically distinguishable: if the intervals do not overlap, the difference
  # is significant at conventional levels. The gap reflects a persistent within-group gender difference in publication
  # productivity that is not simply an artefact of other observable characteristics

# d) Write a short summary paragraph as a comment in your R script (4–6 sentences). Cover
  # all of the following: (1) whether Poisson regression is adequate for this dataset or
  # whether the negative binomial is needed, and why; (2) the interpretation of the ment
  # incidence rate ratio — what does mentor productivity tell us about student productivity?; 
  # (3) which predictors are statistically significant in the negative binomial model; (4)
  # one substantive conclusion about the factors driving publication productivity among
  # PhD students in biochemistry.

# The Poisson model does not fit this dataset well because there is clear evidence of overdispersion.
  # The variance to mean ratio is about twice as large as expected, the residual deviance greatly exceeds 
  # the degrees of freedom, and a formal dispersion test strongly rejects equidispersion (p < 0.001). A 
  # negative binomial model is therefore more appropriate because it account for this extra variation and 
  # provides a better fit. The incidence rate ratio for ment is slightly above 1, indicating that higher
  # mentor productivity is associated with a small but meaningful increase in student publication output.
  # In the negative binomial model, ment, fem, and kid5 are statistically significany predictors while phd 
  # and mar are not. Substantively, these results suggest that publication productivity among PhD students in 
  # biochemistry is shaped by mentorship, gender, family responsibilities, with more productive mentors
  # boosting output and caregiving demands for reducing it. 
