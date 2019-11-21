install.packages("corrplot")
library(MASS)
library(tidyverse)
library(ggplot2)
library(GGally)
library(corrplot)
data("Boston")

dim(Boston)
str(Boston)

summary(Boston)
ggpairs(Boston, 
        mapping = aes(alpha = 0.6), 
        lower = list(combo = wrap("facethist", bins = 30)))
?ggpairs

cor_matrix<-cor(Boston) 
cor_matrix %>%
  round(digits = 2)
corrplot(cor_matrix, method="circle", type = "upper", cl.pos = "b", tl.pos = "d", tl.cex = 0.6)
pairs(Boston)
