# Load the abalone dataset using readr
library(readr)
abalone <- readr::read_csv("http://assets.datacamp.com/production/repositories/2299/datasets/5a80498a0bba2da70fd4af7764c3ab4e71e32dda/abalone.csv")
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
  mutate(adult = sex != "I") %>%
  mutate(sexAF = as.factor(sex)) %>%
  mutate(sexF = factor(sex, 
                       levels = c("F", "I", "M"), 
                       labels = c("Female", "Infant", "Male"))) %>%
  mutate(sexOF = factor(sex, 
                        levels = c("I", "F", "M"), 
                        labels = c("Infant", "Female", "Male"), 
                        ordered = TRUE))

# check
abaloneMod <- abaloneMod %>%
  mutate(flag_length_lt_ht = length < height) %>%
  mutate(flag_length_lt_diam = length < diameter) %>%
  mutate(keep_ht_diam = ((length > height) & (length > diameter))) %>%
  mutate(keepht = (height > 0)) %>%
  mutate(keep_shWt_lt_whWt = (shuckedWeight < wholeWeight))

# Keep cases with height > 0
# Keep cases where shuckedWeight is less than wholeWeight
# Keep cases where length is > both height and diameter
abaloneKeep <- abaloneMod %>%
  filter(height > 0) %>%
  filter(shuckedWeight < wholeWeight) %>%
  filter((length > height) & (length > diameter))

dim(abaloneKeep)

sum(abaloneKeep$length <= abaloneKeep$height)
sum(abaloneKeep$length <= abaloneKeep$diameter)
sum(abaloneKeep$shuckedWeight >= abaloneKeep$wholeWeight)
sum(abaloneKeep$visceraWeight >= abaloneKeep$wholeWeight)
sum(abaloneKeep$shellWeight >= abaloneKeep$wholeWeight)

## ==================================================
## begin ch3
## pre-code

# Load the abalone dataset using readr
library(readr)
abalone <- readr::read_csv("http://assets.datacamp.com/production/repositories/2299/datasets/5a80498a0bba2da70fd4af7764c3ab4e71e32dda/abalone.csv")
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
  mutate(adult = sex != "I") %>%
  mutate(sexAF = as.factor(sex)) %>%
  mutate(sexF = factor(sex, 
                       levels = c("F", "I", "M"), 
                       labels = c("Female", "Infant", "Male"))) %>%
  mutate(sexOF = factor(sex, 
                        levels = c("I", "F", "M"), 
                        labels = c("Infant", "Female", "Male"), 
                        ordered = TRUE))

# Keep cases with height > 0
# Keep cases where shuckedWeight is less than wholeWeight
# Keep cases where length is > both height and diameter
abaloneKeep <- abaloneMod %>%
  filter(height > 0) %>%
  filter(shuckedWeight < wholeWeight) %>%
  filter((length > height) & (length > diameter))

# code for ch3 - tests

# descriptive stats

ggplot(abaloneKeep, aes(length)) +
  geom_histogram()

ggplot(abaloneKeep, aes(diameter)) +
  geom_histogram()

# there is still 1 possible outlier
ggplot(abaloneKeep, aes(height)) +
  geom_histogram()

ggplot(abaloneKeep, aes(wholeWeight)) +
  geom_histogram()

ggplot(abaloneKeep, aes(shuckedWeight)) +
  geom_histogram()

ggplot(abaloneKeep, aes(visceraWeight)) +
  geom_histogram()

ggplot(abaloneKeep, aes(shellWeight)) +
  geom_histogram()

# odd skip around 14-15 rings - is this real?
ggplot(abaloneKeep, aes(rings)) +
  geom_histogram()

# no skips - what is happening?
abaloneKeep %>% with(table(rings))

# summarise and show all rows of tbl_df
# notice only gap is at 28
abaloneKeep %>% 
  group_by(rings) %>% 
  summarise(freq=n()) %>%
  print(n = Inf)

# fix breaks number of bins
# the number of rings go from 1 to 29
# set bins equal to 29 and re-run
# no more gaps
ggplot(abaloneKeep, aes(rings)) +
  geom_histogram(bins = 29)

# or look at age = rings + 1.5
ggplot(abaloneKeep, aes(age)) +
  geom_histogram()

# check percents of weight
# somewhat normalized, more symmetric but have positive kurtosis
ggplot(abaloneKeep, aes(pctShucked)) +
  geom_histogram()
ggplot(abaloneKeep, aes(pctViscera)) +
  geom_histogram()
ggplot(abaloneKeep, aes(pctShell)) +
  geom_histogram()

# look at summary stats
abaloneKeep %>%
  select(length, height, diameter) %>%
  summary()

# try describe from Hmisc
library(Hmisc)
abaloneKeep %>%
  select(length, height, diameter) %>%
  Hmisc::describe()

# try describe from psych
library(psych)
abaloneKeep %>%
  select(length, height, diameter) %>%
  psych::describe()

# get mean and sd for length
abaloneKeep %>%
  summarise(meanlt = mean(length),
            sdlt = sd(length))

# just get mean and sd for each var
abaloneKeep %>%
  select(length, height, diameter) %>%
  summarise_all(funs(mean, sd))

# just get mean and sd for each var
abaloneKeep %>%
  summarise_at(c("shuckedWeight","wholeWeight"),
               funs(median, min, max))

# get stats by sex
# look at summary stats
abaloneKeep %>%
  group_by(sex) %>%
  select(length, height, diameter) %>%
  summarise_all(funs(mean, sd))

# get stats by sex
# look at summary stats
abaloneKeep %>%
  group_by(sex) %>%
  select(shuckedWeight, age) %>%
  summarise_all(funs(mean, sd))

# look by adult versus infant
abaloneKeep %>%
  group_by(adult) %>%
  select(shuckedWeight, age) %>%
  summarise_all(funs(mean, sd))

# correlations with age
abaloneKeep %>%
  select(age, length, height, diameter) %>%
  cor()

# correlations with age
abaloneKeep %>%
  select(age, wholeWeight, 
         shuckedWeight, visceraWeight, 
         shellWeight) %>%
  cor()

# correlations with age
abaloneKeep %>%
  select(age, pctShucked, pctViscera, 
         pctShell) %>%
  cor()

# regular chisq
table1 <- abaloneKeep %>%
  with(table(agecat, sex))
chisq.test(table1)

# regular chisq
table2 <- abaloneKeep %>%
  with(table(adult, sex))
chisq.test(table2)

# crosstabs
# need to explain with option
# defaults to SAS style format
library(gmodels)
abaloneKeep %>%
  select(sex, agecat) %>%
  with(gmodels::CrossTable(sex, agecat,
                           chisq=TRUE,
                           prop.r = FALSE,
                           prop.t = FALSE,
                           prop.chisq = FALSE))

abaloneKeep %>%
  select(sex, adult) %>%
  with(gmodels::CrossTable(sex, adult,
                           chisq=TRUE,
                           prop.r = FALSE,
                           prop.t = FALSE,
                           prop.chisq = FALSE))

# create CrossTabs table, show expected counts, column percentages
abaloneKeep %>%
  with(gmodels::CrossTable(agecat, sex,
                           chisq=TRUE,
                           prop.r = FALSE,
                           prop.t = FALSE,
                           prop.chisq = FALSE))


mtcars

mtcars %>%
  transmute(displ_l = disp / 61.0237)


# get min, mean, sd, median, and max for age
abaloneKeep %>%
  select(age) %>%
  summarise_all(funs(min, mean, sd, median, max))

# get min, mean, sd, median, and max for age by sex as group
abaloneKeep %>%
  group_by(sex) %>%
  select(age) %>%
  summarise_all(funs(min, mean, sd, median, max))

# get median, 25th and 75th percentiles for wholeWeight by adult
abaloneKeep %>% group_by(adult) %>%
  select(wholeWeight) %>%
  summarise(medwwt = median(wholeWeight),
            q1wwt = quantile(wholeWeight, probs = 0.25),
            q3wwt = quantile(wholeWeight, probs = 0.75))

# ch3 tests and correlations

abaloneKeep %>% 
  select(shuckedWeight, wholeWeight) %>%
  cor()

library(psych)

abaloneKeep %>% 
  select(age, shuckedWeight) %>%
  psych::corr.test()

abc <- abaloneKeep %>% 
  select(age, shuckedWeight) %>%
  psych::corr.test()
print(abc)
print(abc, digits = 6, short = FALSE)

# view more digits for r and p-values 
abc$r
abc$p

abc.p <- psych::corr.p(abc$r, abc$n)
print(abc.p, digits = 3, short = FALSE)

# -----------

abc.m <- abaloneKeep %>% 
  select(age, wholeWeight, shellWeight, 
         visceraWeight, shuckedWeight) %>%
  psych::corr.test()
print(abc.m, digits = 3, short = FALSE)
abc.m$r
abc.m$p

abc.pm <- psych::corr.p(abc.m$r, abc.m$n)
print(abc.pm, digits = 3, short = FALSE)

# t-tests
# unpooled
t.test(formula = length ~ adult, 
       data = abaloneKeep)

# pooled
t.test(formula = length ~ adult, 
       data = abaloneKeep,
       var.equal = TRUE)

# bartlett test equal variances
# or var.test
# assumes normality, more than 2 groups
bartlett.test(length ~ adult, 
              data = abaloneKeep)

# assumes normality - 2 groups
# like SAS's Folder F test
var.test(length ~ adult, 
         data = abaloneKeep)

# car package levene's test
# more than 2 groups, more robust deviations normality
library(car)
leveneTest(length ~ as.factor(adult), 
           data = abaloneKeep)

abaloneKeep %>%
  select(length, adult) %>%
  group_by(adult) %>%
  group_by(N = n(), add = TRUE) %>%
  summarise_all(funs(mean, sd))

# chi-square tests
# compare proportions of adults between sex groups

# compare proportion of adult between age groups; create table object, then run chisq.test
tableSexAgecat <- abaloneKeep %>%
  with(table(sex, agecat))
chisq.test(tableSexAgecat)

cs1 <- chisq.test(tableSexAgecat)
cs1$observed
cs1$expected

# create CrossTabs table, show expected counts, column percentages
abaloneKeep %>%
  with(gmodels::CrossTable(sex, agecat,
                           chisq=TRUE,
                           prop.r = FALSE,
                           prop.t = FALSE,
                           prop.chisq = FALSE))

# save the output from the summary() function for length, height and diameter, in object summaryDim
summaryDim <- abaloneKeep %>%
  select(length, height, diameter) %>%
  summary()

# display the minimum for height, element at row 1 column 2
summaryDim[1,2]

# save the output from the psych::describe() function for length, height and diameter, in object describeDim
describeDim <- abaloneKeep %>%
  select(length, height, diameter) %>%
  psych::describe()

# print the variable names, n, mean and sd for each variable
describeDim[,c("vars","n","mean","sd")]

# save results of unpooled t-test of wholeWeight by agecat in object ttest1
ttest1 <- t.test(height ~ agecat, data = abaloneKeep)

ttest1$statistic
ttest1$parameter
ttest1$p.value
ttest1$conf.int

# skip these
ttest1$null.value
ttest1$alternative
ttest1$method
ttest1$data.name

formatC(ttest1$p.value, format = "e")

# compare proportion of adult between age groups; create table object, then run chisq.test
tableSexAgecat <- abaloneKeep %>%
  with(table(sex, agecat))
chisq.test(tableSexAgecat)

cs1 <- chisq.test(tableSexAgecat)
cs1$observed
cs1$expected

names(cs1)
cs1$statistic
cs1$parameter
cs1$p.value
cs1$method

ggplot(abaloneKeep, aes(sex, rings)) +
  geom_boxplot()

abaloneKeep %>%
  select(sex, rings, age) %>%
  group_by(sex) %>%
  summarise_all(funs(mean, sd, median))

# get spearman's rho correlations 
# between age, length, height and diameter
# use base r cor() function
# save spearman's rho using cor() function as spearcor
spearcor <- abaloneKeep %>%
  select(age, length, height, diameter) %>%
  cor(method = "spearman")

# what is the class of spearcor
class(spearcor)

# run Mann Whitney test of diameter by adult
wcox <- wilcox.test(diameter ~ adult,
                    data = abaloneKeep)

wcox$statistic
wcox$p.value

abaloneKeep %>%
  select(rings, adult, length, height, diameter,
         wholeWeight, shuckedWeight,
         shellWeight, visceraWeight) %>%
  cor()

abaloneKeep %>%
  select(rings, adult, length, height, diameter,
         wholeWeight, shuckedWeight,
         shellWeight, visceraWeight) %>%
  cor(method = "spearman")

ttRings <- t.test(rings ~ adult, data = abaloneKeep,
       var.equal = TRUE)

dRings <- (2*ttRings$statistic)/sqrt(ttRings$parameter)

ttDiameter <- t.test(diameter ~ adult, data = abaloneKeep,
       var.equal = TRUE)

dDiameter <- (2*ttDiameter$statistic)/sqrt(ttDiameter$parameter)

# test impact of factors - try sexOF

ggplot(abaloneKeep, aes(sexOF, wholeWeight)) +
  geom_boxplot()

# anova test - ordered factors
# this automatically run .L and .Q
# linear and quadratic contrasts - in the coefficients
aov1 <- aov(wholeWeight ~ sexOF, data = abaloneKeep)
aov1
summary(aov1)
aov1$coefficients

# anova test - regular factor
# note female is first - used as contrast group by default
aov2 <- aov(wholeWeight ~ sexF, data = abaloneKeep)
aov2
summary(aov2)
aov2$coefficients
anova(aov2)
# intercept is mean whole weight of Females
# sexInfant is avg whole weight difference from females
# sexMale is avg whole weight difference from females

abaloneKeep %>%
  pull(wholeWeight) %>%
  mean()

abaloneKeep %>%
  select(wholeWeight, sexOF) %>%
  group_by(sexOF) %>%
  summarise_all(funs(mean))

# lm
lm1 <- lm(wholeWeight ~ sexF, data = abaloneKeep)
lm1
summary(lm1)
anova(lm1)

lm1of <- lm(wholeWeight ~ sexOF, data = abaloneKeep)
lm1of
summary(lm1of)
anova(lm1of)

abaloneKeep1 <- abaloneKeep %>%
  mutate(sexOFr = factor(sexOF, ordered = FALSE))

table(abaloneKeep1$sexOF)
table(abaloneKeep1$sexOFr)

class(abaloneKeep1$sexOF)
class(abaloneKeep1$sexOFr)

lm1aof <- lm(wholeWeight ~ sexOFr, data = abaloneKeep1)
lm1aof
summary(lm1aof)
anova(lm1aof)
lm1aof$coefficients
# now intercept is mean whole weight of infants
# and others are females diff from infants
# and males diff from infants

abaloneKeep1 %>%
  group_by(sexOFr) %>%
  select(wholeWeight) %>%
  summarise_all(funs(mean))

# skip for now
# load car package
library(car)
clm1 <- car::Anova(lm1)
clm1

clm1t3 <- car::Anova(lm1, type=3)
clm1t3

## use davis dataset for ch2 lesson 3 slides

library(car)
head(Davis)

# readr::write_csv(Davis, "davis.csv")

davismod <- Davis %>%
  mutate(bmi = weight / ((height/100))^2)

head(davismod)

davismod <- davismod %>%
  mutate(diffht = repht - height) %>%
  mutate(difflow = diffht <= -3)

davismod %>%
  select(sex, height, repht, 
         diffht, difflow) %>%
  head()

davismod <- davismod %>%
  mutate(bmicat = 
           ifelse(test = bmi < 25,
                  yes = "1. underwt/norm",
                  no = ifelse(bmi < 30,
                              "2. overwt",
                              "3. obese")))

table(davismod$bmicat)

head(davismod)

hist(davismod$bmi)

head(arrange(davismod, bmi))
tail(arrange(davismod, bmi))

davismod %>%
  pull(bmi) %>%
  is.numeric()

davismod %>%
  pull(bmicat) %>%
  is.numeric()

davismod %>%
  pull(difflow) %>%
  is.logical()

davismod %>%
  pull(difflow) %>%
  is.vector()

is.data.frame(davismod)
is.matrix(davismod)

davismod %>%
  pull(difflow) %>%
  head()

davismod %>%
  pull(difflow) %>%
  as.numeric() %>%
  head()

davismod %>%
  select(weight, height) %>%
  head(4)

davismod %>%
  select(weight, height) %>%
  as.matrix() %>%
  class()

davismod %>%
  head(10)

davismod %>% 
  pull(bmi) %>%
  summary()

ggplot(davismod, aes(bmi)) +
  geom_dotplot()

davismod %>%
  arrange(bmi) %>%
  tail()

ggplot(davismod, 
       aes(weight, height)) + 
  geom_point() + 
  geom_abline(intercept=0, slope=1)

daviskeep <- davismod %>% 
  filter(bmi < 100)

daviskeep %>%
  arrange(bmi) %>%
  tail()

ggplot(daviskeep, aes(bmi)) +
  geom_dotplot()

# display rows 15 to 21
davismod %>% 
  slice(15:21)

# get 2nd element in bmi from davismod
davismod %>%
  slice(2) %>%
  pull(bmi)

davismod$bmi[2]


abaloneMod %>% pull(age) %>% median()

# -----------------------------------------------------
# quick checks - after removing factor exercises
# moving them into ch 4

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

