library(MASS)
library(tidyverse)
library(ggplot2)
library(GGally)
library(corrplot)

# loading the data
human <- read.table("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human1.txt", header = TRUE, sep = ",")
str(human)

# The dataframe consists of 195 observations and 19 variables. The data represent several indicators from most countries in the world. Each observation therefore represents one country plus there are observations representing whole regions. Countries are ordered from the best to the worst. There are variables concerning health and knowledge & empowerment.

# transforming the GNI variable to numeric
human <- human %>%
  mutate(GNI = as.numeric(GNI))

#excluding unimportant variables
keep <- c("Country", "Edu2.FM", "Labo.FM", "Edu.Exp", "Life.Exp", "GNI", "Mat.Mor", "Ado.Birth", "Parli.F")
human1 <- select(human, one_of(keep))

# omitting NA values
human1 <- na.omit(human1)

# define the last indice we want to keep
last <- nrow(human1) - 7

# choose everything until the last 7 observations
human1 <- human1[1:last, ]

# add countries as rownames
rownames(human1) <- human1$Country

# omitting 'Country' column
human1 <- select(human1, -Country)

# saving the df
write.csv(human1, file = "data/human.csv", row.names = TRUE)


ggpairs(human2)
cor(human2) %>% corrplot(type = "upper")

scaled <- scale(human2)
pca_human <- prcomp(scaled)
biplot(pca_human, choices = 1:2, cex = c(0.8, 1), col = c("grey40", "deeppink2"))        
summary(pca_human)