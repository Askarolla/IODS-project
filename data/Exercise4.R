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

scaled_boston <- scale(Boston)
summary(scaled_boston)
class(scaled_boston)
scaled_boston <- as.data.frame(scaled_boston)


# create a quantile vector of crim, create a categorical variable 'crime', remove original from dataset
bins <- quantile(scaled_boston$crim)
crime <- cut(scaled_boston$crim, breaks = bins, include.lowest = TRUE, labels = c("low", "med_low", "med_high", "high"))
table(crime)
scaled_boston <- dplyr::select(scaled_boston, -crim)
scaled_boston <- data.frame(scaled_boston, crime)

#take sample, create test & train sets
ind <- sample(nrow(scaled_boston),  size = nrow(scaled_boston) * 0.8)
train <- scaled_boston[ind,]
test <- scaled_boston[-ind,]



# linear discriminant analysis
lda.fit <- lda(crime ~ ., data = train)
# the function for lda biplot arrows
lda.arrows <- function(x, myscale = 1, arrow_heads = 0.1, color = "red", tex = 0.75, choices = c(1,2)){
  heads <- coef(x)
  arrows(x0 = 0, y0 = 0, 
         x1 = myscale * heads[,choices[1]], 
         y1 = myscale * heads[,choices[2]], col=color, length = arrow_heads)
  text(myscale * heads[,choices], labels = row.names(heads), 
       cex = tex, col=color, pos=3)
}

classes <- as.numeric(train$crime)
plot(lda.fit, dimen = 2, col = classes, pch = classes)
lda.arrows(lda.fit, myscale = 1)

#remove crime from test
correct_classes <- test$crime
test <- dplyr::select(test, -crime)

#predict and cross tabulate the results
lda.pred <- predict(lda.fit, newdata = test)
table(correct = correct_classes, predicted = lda.pred$class)

65/102

#loading Boston and scaling it again
data("Boston")
scaled_boston <- scale(Boston)
scaled_boston <- as.data.frame(scaled_boston)
#calculating the distances
distb <- dist(scaled_boston)
#figuring out the optimal number of clusters
twcss <- sapply(1:10, function(k){kmeans(scaled_boston, k)$tot.withinss})
# visualize the results
qplot(x = 1:10, y = twcss, geom = 'line')

km <-kmeans(scaled_boston, centers = 2)
pairs(scaled_boston, col = km$cluster)