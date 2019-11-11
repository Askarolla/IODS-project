# Theodor Petřík, November 10, 2019. Regression and model validation.

#packages
install.packages("gdata")
library(gdata)
library(dplyr)
library(ggplot2)
library(GGally)
#getting the data
url <-"http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt"
download.file(url, destfile = "datal")
data <- read.table("datal", header = TRUE, sep="\t", stringsAsFactors = TRUE)

#wrangling
dim(data)
str(data)

#there are 183 observations of 60 variables. The variables are all int except for the last one which is factor (gender)

#making the analysis data set
# creating new vars and scaling them
d_sm <- data$D03 + data$D11+ data$D19 + data$D27
d_ri <- data$D07 + data$D14 + data$D22 + data$D30
d_ue <- data$D06 + data$D15 + data$D23 + data$D31
deep <- (d_sm + d_ri + d_ue)/12

su_lp <- data$SU02 + data$SU10+ data$SU18 + data$SU26
su_um <- data$SU05 + data$SU13 + data$SU21 + data$SU29
su_sb <- data$SU08 + data$SU16 + data$SU24 + data$SU32
surf <- (su_lp + su_um + su_sb)/12

st_os <- data$ST01 + data$ST09+ data$ST17 + data$ST25
st_tm <- data$ST04 + data$ST12 + data$ST20 + data$ST28
stra <- (st_os + st_tm)/8

data$Attitude <- data$Attitude / 10
#making the new set and filtering out observations where point == 0
analysis_data <- data.frame(data$gender, data$Age, data$Attitude, deep, stra, surf, data$Points)

#renaming columns
names(analysis_data)[1] <- "gender"
names(analysis_data)[2] <- "age"
names(analysis_data)[3] <- "attitude"
names(analysis_data)[7] <- "points"

analysis_data <-analysis_data %>%
  dplyr::filter(points != 0)

#saving the dataset
write.csv(analysis_data, file = "learning2014.csv", row.names = FALSE)

#testing that it works
test <- read.csv("learning2014.csv")
str(test)

##Analysis
rm(list = ls(all = TRUE)) #clear environment
data <- read.csv("learning2014.csv")

data1 <- read.table("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/learning2014.txt ", sep = ",", header = TRUE)
#summaries and overview
summary(data)
ggpairs(data, lower = list(combo = wrap("facethist", bins = 20)))
data %>%
  filter(gender == "M") %>%
  summarise(IQR(points))

#big model
model1 <- lm(points ~ attitude +  stra + surf, data)
summary(model1)

#small model - statistically not significant variables are ommited
model2 <- lm(points ~ attitude, data)
summary(model2)
