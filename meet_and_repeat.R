library(tidyverse)
library(tidyr)

###BPRS
BPRS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BPRS.txt", sep  =" ", header = T)

names(BPRS)
str(BPRS)
summary(BPRS)

#change to factors
BPRS$treatment <- factor(BPRS$treatment)
BPRS$subject <- factor(BPRS$subject)

#wide to long
BPRSL <-  BPRS %>% gather(key = weeks, value = bprs, -treatment, -subject)
BPRSL <-  BPRSL %>% mutate(week = as.integer(substr(weeks, 5,5)))

#summaries
glimpse(BPRSL)
summary(BPRSL)

ggplot(BPRSL, aes(x = week, y = bprs, linetype = subject)) +
  geom_line() +
  scale_linetype_manual(values = rep(1:10, times=4)) +
  facet_grid(. ~ treatment, labeller = label_both) +
  theme(legend.position = "none") + 
  scale_y_continuous(limits = c(min(BPRSL$bprs), max(BPRSL$bprs)))
### RATS
rats <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt", header = TRUE, sep = '\t')

names(rats)
summary(rats)
str(rats)

#factoring
rats$ID <- as.factor(rats$ID)
rats$Group <- as.factor(rats$Group)

#wide to long
ratsl <- rats %>%
  gather(key = WD, value = Weight, -ID, -Group) %>%
  mutate(Time = as.integer(substr(WD, 3,4))) 

glimpse(ratsl)
summary(ratsl)

ggplot(ratsl, aes(x = Time, y = Weight, group = ID)) +
  geom_line(aes(linetype = Group)) +
  scale_x_continuous(name = "Time (days)", breaks = seq(0, 60, 10)) +
  scale_y_continuous(name = "Weight (grams)") +
  theme(legend.position = "top")

#save the dataframes
write.csv(BPRSL, "data/bprsl.csv", row.names = F)
write.csv(ratsl, "data/rats.csv", row.names = F)


##############################
library(tidyverse)
library(ggplot2)

#loading the data
rats <- read.csv("data/rats.csv")
bprsl <- read.csv("data/bprsl.csv")
str(rats)

#factoring
rats$ID <- as.factor(rats$ID)
rats$Group <- as.factor(rats$Group)

# Standardise the variable bprs
rats <- rats %>%
  group_by(Time) %>%
  mutate(stdrats = (Weight - mean(Weight))/sd(Weight) ) %>%
  ungroup()

glimpse(rats)

#plot
ggplot(rats, aes(x = Time, y = stdrats, linetype = ID)) +
  geom_line() +
  scale_linetype_manual(values = rep(1:10, times=4)) +
  facet_grid(. ~ Group, labeller = label_both) +
  scale_y_continuous(name = "standardized rats")

# Number of weeks, baseline (week 0) included
n <- rats$Time %>% unique() %>% length()

# Summary data with mean and standard error of rats by group and time 
ratss <- rats %>%
  group_by(Group, Time) %>%
  summarise( mean = mean(Weight), se = sd(Weight)/sqrt(n) ) %>%
  ungroup()

# Plot the mean profiles
ggplot(ratss, aes(x = Time, y = mean, linetype = Group, shape = Group)) +
  geom_line() +
  scale_linetype_manual(values = c(1,2,3)) +
  geom_point(size=3) +
  scale_shape_manual(values = c(1,2,3)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se, linetype="1"), width=0.3) +
  theme(legend.position = c(0.8,0.8)) +
  scale_y_continuous(name = "mean(Weight) +/- se(Weight)")

#############################

bprsl$treatment <- factor(bprsl$treatment)
bprsl$subject <- factor(bprsl$subject)

str(rats)


# create a regression model RATS_reg
RATS_reg <- lm(Weight ~ Time + Group, RATSL)

# print out a summary of the model
summary(RATS_reg)

# access library lme4
library(lme4)

# Create a random intercept model
RATS_ref <- lmer(Weight ~ Time + Group + (1 | ID), data = RATSL, REML = FALSE)

# create a random intercept and random slope model
RATS_ref1 <- lmer(Weight ~ Time + Group + (Time | ID), data = RATSL, REML = FALSE)

# print a summary of the model
summary(RATS_ref1)

# perform an ANOVA test on the two models
anova(RATS_ref1, RATS_ref)

# create a random intercept and random slope model with the interaction
RATS_ref2 <- lmer(Weight ~ Time + Group + (Time | ID) + Time*Group, data = RATSL, REML = FALSE)

# print a summary of the model
summary(RATS_ref2)

# perform an ANOVA test on the two models
anova(RATS_ref2, RATS_ref1)

# draw the plot of RATSL with the observed Weight values
ggplot(RATSL, aes(x = Time, y = Weight, group = ID)) +
  geom_line(aes(linetype = Group)) +
  scale_x_continuous(name = "Time (days)", breaks = seq(0, 60, 20)) +
  scale_y_continuous(name = "Observed weight (grams)") +
  theme(legend.position = "top")

# Create a vector of the fitted values
Fitted <- fitted(RATS_ref2)

# Create a new column fitted to RATSL
RATSL <- RATSL %>%
  mutate(Fitted = Fitted)

# draw the plot of RATSL with the Fitted values of weight
ggplot(RATSL, aes(x = Time, y = Fitted, group = ID)) +
  geom_line(aes(linetype = Group)) +
  scale_x_continuous(name = "Time (days)", breaks = seq(0, 60, 20)) +
  scale_y_continuous(name = "Fitted weight (grams)") +
  theme(legend.position = "top")
