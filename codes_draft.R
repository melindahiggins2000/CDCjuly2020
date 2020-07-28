# code for class exercises and on your own work
# https://archive.ics.uci.edu/ml/datasets/abalone

# load data from SPSS
library(haven)
abalone <- read_sav("abalone.sav")

library(foreign)
abalone <- read.spss("abalone.sav",
                     to.data.frame=TRUE)

# load data from SAS
library(haven)
abalone <- read_sas("abalone_sas.sas7bdat", NULL)

library(foreign)
abalone <- read.xport("abalone.xpt")

# load data from Excel
library(readxl)
abalone <- read_excel("abalone.xlsx")

# Load the abalone dataset using readr
# read in dataset
library(readr)
abalone <- readr::read_csv("abalone.csv")

# notice abalone created and loaded into environment
# click on it to View it
View(abalone)

# load dplyr package
library(dplyr)

# view top 6 rows
abalone %>% 
  head()

# arrange by length
# and look at top 6 rows
abalone %>%
  arrange(length) %>%
  head()

# sort descending use desc()
# arrange by length - descending
# and look at top 6 rows
abalone %>%
  arrange(desc(length)) %>%
  head()

# or look at bottom 6 rows
abalone %>%
  arrange(length) %>%
  tail()

# get summary stats of height
abalone %>%
  select(height) %>%
  summary()

# get summary stats of rings
abalone %>%
  select(rings) %>%
  summary()

library(ggplot2)

# make a histogram of height
ggplot(abalone, aes(x=height)) +
  geom_histogram()

# make a histogram of height
ggplot(abalone, aes(x=height)) +
  geom_histogram(color="black", fill="green")

# make a plot of abalone length by height
ggplot(abalone, aes(x=length, y=height)) +
  geom_point(color="green") +
  geom_abline(intercept=0, slope=1, color="blue")

# make a plot of abalone whole weight and shucked weight
ggplot(abalone, aes(x=wholeWeight, y=shuckedWeight)) +
  geom_point(color="purple") +
  geom_abline(intercept=0, slope=1, color="blue")

# find cases where height = 0
abalone %>%
  filter(height==0)



# get names of variables, column headings
names(abalone)

# change the name of the 1st column to casenum
varnames <- names(abalone)
varnames
varnames[1]
names(abalone)[1] <- "casenum"
names(abalone)

# Load dplyr package
library(dplyr)
library(ggplot2)

# Create abaloneMod from abalone and add new variable age equal to rings + 1.5
# Add 3 more variables to abaloneMod pctShucked pctViscera pctShell for each as percent of wholeWeight
# Add new character variable agecat to abaloneMod labeled as "< 10.5" and "10.5 and older"
# Add new logical variable adult to abaloneMod for sex not equal to "I"
# Add new variable sexAF as a factor of sex to abaloneMod
# Add new sexF variable as a factor to abaloneMod with levels "F" "I" "M" and labels "Female" "Infant" "Male"
# Add ordered factor sexOF variable to abaloneMod with levels in order "I" "F" "M"; labels "Infant" "Female" "Male"
abaloneMod <- abalone %>%
  mutate(age = rings + 1.5) %>%
  mutate(pctShucked = shuckedWeight * 100 / wholeWeight) %>%
  mutate(pctViscera = visceraWeight * 100 / wholeWeight) %>%
  mutate(pctShell = shellWeight * 100 / wholeWeight) %>%
  mutate(agecat = ifelse(test = age < 10.5, 
                         yes = "< 10.5", 
                         no = "10.5 and older")) %>%
  mutate(adult = sex != "I")

# Keep cases with height > 0
# Keep cases where shuckedWeight is less than wholeWeight
# Keep cases where length is > both height and diameter
abaloneKeep <- abaloneMod %>%
  filter(height > 0) %>%
  filter(shuckedWeight < wholeWeight) %>%
  filter((length > height) & (length > diameter))

# ==============================
library(Hmisc)

# Save output from Hmisc::describe, view class of output
abhmisc <- abaloneKeep %>% 
  select(wholeWeight, shuckedWeight, shellWeight) %>% 
  Hmisc::describe()

# Get structure of abhmisc
str(abhmisc)

# View shuckedWeight statistics from abhmisc
abhmisc$shuckedWeight

# View shuckedWeight extremes from abhmisc
abhmisc$shuckedWeight$extremes

# ==================================

# Load psych package
library(psych)

# From ex3.6, Correlations between age and weights with psych::corr.test()
abage <- abaloneKeep %>% 
  select(age, wholeWeight, shuckedWeight, shellWeight, visceraWeight) %>%
  psych::corr.test()

# From ex3.7, Perform pooled t-test of length by adult
abttest <- t.test(length ~ adult, data = abaloneKeep, var.equal = TRUE)

# From ex3.9
# Create frequency table of sex by agecat
tablesexage <- abaloneKeep %>% 
  with(table(sex, agecat))

# Chi-square test using tablesexage
cssexage <- chisq.test(tablesexage)

# List names of elements in abage
names(abage)

# Display Pearson's r's and p-values from abage
abage$r
abage$p

# Display statistic, parameter and conf.int from abttest
abttest$statistic
abttest$parameter
abttest$conf.int

# Display expected values and p.value from cssexage
cssexage$expected
cssexage$p.value

# do anova exercise ==============================

abaloneKeep %>%
  group_by(sex) %>%
  group_by(N = n(), add = TRUE) %>%
  select(age, sex) %>%
  summarise_all(funs(mean, sd, var))

library(car)
car::leveneTest(age ~ sex, data = abaloneKeep)

abaov <- aov(age ~ sex, data = abaloneKeep)
abaov

summary(abaov)

par(mfrow = c(2,2))
plot(abaov)
par(mfrow = c(1,1))

print(abaov)

TukeyHSD(abaov)

# skip these...
abresid <- data.frame(abaov$fitted.values,
                      abaov$residuals)
names(abresid) <- c("fittedvals", "resid")

ggplot(abresid, aes(resid)) +
  geom_histogram(fill = "light blue", color = "black")

ggplot(abresid, aes(fittedvals, resid)) +
  geom_point()

abaov$coefficients
abaov$df.residual

# using lm ===========================

lmshucked <- lm(age ~ shuckedWeight, data = abaloneKeep)
lmshucked$coefficients

smrylmshucked <- summary(lmshucked)
str(smrylmshucked)
smrylmshucked$r.squared
smrylmshucked$adj.r.squared

par(mfrow = c(2,2))
plot(lmshucked)
par(mfrow = c(1,1))

# skip these...
car::residualPlot(lmshucked)
car::residualPlots(lmshucked)
car::influencePlot(lmshucked)

# visualize data
ggplot(abaloneKeep, aes(shuckedWeight, age)) +
  geom_point() +
  geom_smooth(method = "lm")

# final models ===================

lmshucked <- lm(age ~ shuckedWeight, data = abaloneKeep)
lmshell <- lm(age ~ shellWeight, data = abaloneKeep)
smrylmshucked <- summary(lmshucked)
smrylmshell <- summary(lmshell)

smrylmshucked$r.squared
smrylmshell$r.squared
AIC(lmshucked, lmshell)


library(psych)
library(Hmisc)
library(gmodels)
library(GGally)

lmshellI <- lm(age ~ shellWeight, 
               subset = (sex == "I"),
               data = abaloneKeep)
slmI <- summary(lmshellI)

lmshellF <- lm(age ~ shellWeight, 
               subset = (sex == "F"),
               data = abaloneKeep)
slmF <- summary(lmshellF)

lmshellM <- lm(age ~ shellWeight, 
               subset = (sex == "M"),
               data = abaloneKeep)
slmM <- summary(lmshellM)

slmI$r.squared
slmF$r.squared
slmM$r.squared

ggplot(abaloneKeep, aes(shellWeight, age)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~sex)

