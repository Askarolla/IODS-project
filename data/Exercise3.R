# Theodor Petřík, November 17, theme:Logistic regression

#libraries
library(dplyr)
library(ggplot2)

#loading the data & exploring the structure
mat <- read.csv("data/student-mat.csv", header = TRUE, sep = ";")
por <- read.csv("data/student-por.csv", header = TRUE, sep = ";")

dim(mat)
str(mat)
dim(por)
str(por)

join_by <- c("school", "sex", "age", "address", "famsize", "Pstatus", "Medu", "Fedu", "Mjob", "Fjob", "reason", "nursery","internet")
math_por <- inner_join(mat, por, by = join_by, suffix = c(".mat", ".por"))
joined_columns <- select(math_por, one_of(join_by))
notjoined_columns <- colnames(mat)[!colnames(mat) %in% join_by]

for(column_name in notjoined_columns) {
  # select two columns from 'math_por' with the same original name
  two_columns <- select(math_por, starts_with(column_name))
  # select the first column vector of those two columns
  first_column <- select(two_columns, 1)[[1]]
  # if that first column vector is numeric...
  if(is.numeric(first_column)) {
    # take a rounded average of each row of the two columns and
    # add the resulting vector to the alc data frame
    alc[column_name] <- round(rowMeans(two_columns))
  } else { # else if it's not numeric...
    # add the first column vector to the alc data frame
    alc[column_name] <- first_column
  }
}

# glimpse at the new combined data
glimpse(alc)

#new variables
alc <- alc %>% 
  mutate(alc_use = (Dalc + Walc)/2) %>% 
  mutate(high_use = alc_use > 2)

#saving the tidied dataset
write.csv(alc, file = "data/alc.csv")
?write.csv
