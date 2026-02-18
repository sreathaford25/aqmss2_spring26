Sara Reathaford
Homework1
STAR dataset

# The Project STAR (Student/Teacher Achievement Ratio) experiment randomly assigned students in Tennessee to small classes (13–17 students), regular classes (22–25 students), or regular classes with a teacher’s aide. We use data from this experiment to practice applied regression.

install.packages("tidyverse")
library(tidyverse)
install.packages("modelsummary")
library(modelsummary)

# 2.1 Data Preparation

# a) upload dataset
star <- read.csv("https://github.com/franvillamil/AQM2/raw/refs/heads/master/datasets/star/star.csv")

# b) Create a factor variable for classtype with labels: "Small", "Regular", "Regular+Aide"

df <- star

df <- df %>% mutate(classtype = factor(classtype, levels = c(1,2,3), labels = c("Small", "Regular", "Regular+Aide")))

# c) Create a factor variable for race with labels: "White", "Black", "Asian", "Hispanic",
"Native American", "Other".

df <- df %>% mutate(race = factor(race, levels = c(1,2,3,4,5,6), labels = c("White", "Black", "Asian", "Hispanic", "Native American", "Other")))

# d) Create a binary variable "small" that equals 1 if classtype == "Small" and 0 otherwise.

df <- df %>% mutate(small = ifelse(classtype == "Small", 1,0))

# e) Report the number of observations and the number of non-missing observations for g4reading and g4math.

nrow(df)
# 6325 rows

sum(!is.na(df$g4reading))
# 2353
sum(!is.na(df$g4math))
# 2395

# Total observations of students = 6325
# Non-missing reading scores = 2353
# Non-missing math scores = 2395

#2.2 Comparing Groups

# a) Calculate the mean 4th grade reading score by class type. Which group scores highest?

df %>% group_by(classtype) %>% summarize(mean_reading = mean(g4reading, na.rm = TRUE))

# Small = 723
# Regular = 720
# Regular+Aide = 721

# The students in small classes have the highest average reading score. The difference across groups is relatively small, but suggest a slight advantage for small class sizes.

# b) Run a bivariate regression of g4reading on small. Interpret the coefficient.

m1_read <- lm(g4reading ~ small, data = df)
m1_read

# coefficients: intercept = 720.3, small = 3.1

# The intercept represents the reading scores of students NOT in small classes, which is about 720.3. The coefficient on small indicates that students assigned to small classes score about 3.1 points higher in reading on average compared to students in non-small classes. Because this is a bivariate regression, the coefficient is equivalent to the difference in means reading scores between small and non-small classes.

# c) Verify that the regression coefficient equals the difference in means between small and regular+aide classes. (Hint: compare with the grouped means from part a.)

mean_small <- mean(df$g4reading[df$small ==1], na.rm = TRUE)
mean_small
# mean_small = 723.3912

mean_notsmall <- mean(df$g4reading[df$small == 0], na.rm = TRUE)
mean_notsmall
# mean_notsmall = 720.2913

mean_small - mean_notsmall
# 3.099851 =~ 3.1
# This difference matches the regression coefficient we calculated before.

# d) Repeat the bivariate regression for g4math. Is the pattern similar?

m1_math <- lm(g4math ~ small, data = df)
m1_math
# Intercept = 708.5940, small = 0.5912

# The intercept represents the predicted math score for students not in small classes which is about 708.5940, and the coefficient on small indicates that students in small classes score about 0.6 points higher in math on average compared to students in non-small classes. Compared to the reading scores, the effect of small classes on math scores appears much smaller, suggesting a weaker difference between groups for math performance. 


# 2.3 Adding Controls

# a) Run a multiple regression of g4reading on small, race, and yearssmall.

m2_read <- lm(g4reading ~ small + race + yearssmall, data = df)
m2_read

# intercept = 724.386, small = -4.000, yearssmall = 2.170

# b) Compare the coefficient on small with the bivariate model. Does it change much? What does this tell you about the quality of the randomization?

# The coefficient on small is now about -4.0, meaning that after controlling for race and years spent in small classes, the students in smaller classes are predicted to score about 4 points lower in reading compared to students in non-small classes. Compared to the bivariate model where small = 3.1, the coefficient changes greatly in both magnitude and direction. This could suggest that differences in race composition and exposure to small classes were influential. The STAR study is a randomized experiment, so we should expect the coefficient on small to remain stable after adding controls. A large change may indicate that the sample composition or missing data is affecting the estimates. It could also mean that the randomization wasn't really random or that the controls absorb the variation differently.  

# c) Interpret the coefficient on yearssmall. What does it capture?

#The coefficient on yearssmall is about 2.170, which indicates that each additional year a student is in a small class, their readings scores will improve by 2.17 points.


# 2.4 Interactions

# a) Does the effect of being in a small class differ by race? Fit the following model: lm(g4reading ~ small * race + yearssmall, data = df).

m3_read <- lm(g4reading ~ small * race + yearssmall, data = df)
m3_read

# int = 724.680, small = -5.318, yearssmall = 2.249

# b) Print the results using broom::tidy().

broom:: tidy(m3_read)

# c) What is the estimated effect of a small class for White students? For Black students? (Use the coefficients to calculate.)

# *reference category is white students*

coef(m3_read)["small"]
# = -5.317517
# For white students, being in a small class is associated with about 5.3 points LOWER on reading scores compared to non-small classes.

coef(m3_read)["small"] + coef(m3_read)["small:raceBlack"]
# = 1.656508
-5.317517 + 6.974

# For Black students, the effect of small classes in the main effect (-5.32) PLUS the interaction term (+6.97). This results in about a 1.7 increase in reading scores.

# d) In a comment, discuss whether the interaction is substantively meaningful.

# The interaction terms allow the effect of small classes to vary between the different racial groups. However, most of the interaction coefficients are not statistically significant AND they have large standard errors, which suggests limited evidence that the effect of small class sizes differs by race. 


# 2.5 Presenting Results

# a) Create a table with modelsummary() comparing all your reading score models (bivariate, multiple, interaction), using robust standard errors

modelsummary(list("Bivariate" = m1_read, "Multiple" = m2_read, "Interpretation" = m3_read), vcov = "robust", output = "reading_models_table.html")

# b) Create a coefficient plot with modelplot() for the three models.

modelplot(list("Bivariate" = m1_read, "Multiple" = m2_read, "Interaction" = m3_read), vcov = "robust")
ggsave("reading_models_plot.png")

# c) Save both outputs.

# saved above to wd

# 2.6 Brief Discussion

# In a comment (5–10 sentences), discuss:
##a) What does the STAR data suggest about the effect of small class sizes on student achievement?
##b) Why is this evidence more credible than a typical observational study of class size?
##c) Are there any limitations or caveats based on what you observed in the data?


# The STAR data shows mixed evidence on the effects of small class sizes. In the bivariate model, the students in small classes scored about 3 points higher in reading, suggesting a positive relationship. However, once race and years spent in small classes were controlled for, the coefficient became negative, indicating that the comparison may have been capturing differences in student composition or other omitted variables rather than a solely a class size effect. The inclusion of yearssmall is important because it measures the total exposure, which could account for much of the observed advantage in the bivariate model. Since STAR was a randomized experiment, the results are more credible that an observational class study because it reduces selection bias. This allows us to interpret the differences more confidently as causal rather than only correlational. However, missing data reduces the sample size and may influence estimates. Additionally, the interaction results provide little evidence that the effects of small classes differ strongly across racial groups. The large change in the small class coefficient across models suggests that the estimated effect depends on which variables are included. Additionally, most race interaction terms are not statistically significant, meaning ther is little evidence that the effect of small classes differ across racial groups. 





