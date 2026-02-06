install.packages("ggplot2")
library(ggplot2)
gapminder <- read.csv("data/gapminder.csv")
head(gapminder)
str(gapminder)

# creating a plot to show how life expectancy has changed over time

countries <- c("Albania", "Costa Rica", "Norway", "Serbia", "Tunisia")
df <- gapminder[gapminder$country %in% countries, ]

ggplot(df, aes(x = year, y = lifeExp, color = country)) + geom_line() + geom_point() + labs(x = "Year", y = "Life expectancy", title = "Life expectancy over time") + theme_minimal()

ggsave("Assignment1/ass1_plot.png", width = 7, height = 5)
