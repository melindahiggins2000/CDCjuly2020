# ==================================================
# quick checks - after removing factor exercises
# moving them into ch 4
# ==================================================

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
  mutate(adult = sex != "I")

# Keep cases with height > 0
# Keep cases where shuckedWeight is less than wholeWeight
# Keep cases where length is > both height and diameter
abaloneKeep <- abaloneMod %>%
  filter(height > 0) %>%
  filter(shuckedWeight < wholeWeight) %>%
  filter((length > height) & (length > diameter))

# ==================================================
# davis dataset for examples
# ==================================================

## use davis dataset for ch3

library(car)
head(Davis)

# readr::write_csv(Davis, "davis.csv")

davismod <- Davis %>%
  mutate(bmi = weight / ((height/100))^2) %>%
  mutate(diffht = repht - height) %>%
  mutate(difflow = diffht <= -3) %>%
  mutate(bmicat = 
           ifelse(test = bmi < 25,
                  yes = "1. underwt/norm",
                  no = ifelse(bmi < 30,
                              "2. overwt",
                              "3. obese"))) %>%
  mutate(bmigt25 = ifelse(bmi > 25,
                          "2. overwt/obese",
                          "1. underwt/norm"))

daviskeep <- davismod %>% 
  filter(bmi < 100)

davismod %>% arrange(bmi) %>% tail()
daviskeep %>% arrange(bmi) %>% tail()

# =============================================
# begin ch4
# =============================================

# skip factors for now
daviskeep %>% 
  select(bmicat, bmigt25) %>%
  Hmisc::describe()

# numeric coding with labels
daviskeep <- daviskeep %>%
  mutate(bmistr = ifelse(test = bmi < 25,
                          yes = "underwt/norm", 
                          no = ifelse(bmi < 30, 
                                      "overwt", "obese"))) %>%
  mutate(bmicode = ifelse(test = bmi < 25,
                         yes = 1, 
                         no = ifelse(bmi < 30, 
                                     2, 3))) %>%
  mutate(bmiF = factor(bmicode,
                       levels = c(1, 2, 3),
                       labels = c("underwt/Norm",
                                  "overwt",
                                  "obese")))
head(daviskeep)

str(daviskeep)

ggplot(daviskeep, aes(bmistr, weight)) +
  geom_boxplot()

ggplot(daviskeep, aes(as.factor(bmistr), weight)) +
  geom_boxplot()

ggplot(daviskeep, aes(bmiF, weight)) +
  geom_boxplot()

table(daviskeep$bmicat)
table(daviskeep$bmicode)
table(daviskeep$bmiF)

# =================================================
# start w/o factors
# =================================================

daviskeep <- daviskeep %>%
  mutate(diff_ht_repht = height - repht) %>%
  mutate(diff_wt_repwt = weight - repwt)

ggplot(daviskeep, aes(diff_ht_repht)) +
  geom_histogram()

ggplot(daviskeep, aes(diff_wt_repwt)) +
  geom_histogram()

ggplot(daviskeep, aes(sex, diff_ht_repht)) +
  geom_boxplot()

ggplot(daviskeep, aes(sex, diff_wt_repwt)) +
  geom_boxplot()

ggplot(daviskeep, aes(weight, repwt)) +
  geom_point(aes(colour = sex)) +
  geom_abline(intercept = 0, slope = 1)

ggplot(daviskeep, aes(weight, repwt)) +
  geom_point(aes(colour = sex)) +
  geom_abline(intercept = 0, slope = 1) +
  facet_wrap(~sex)

daviskeep %>%
  select(sex, diff_wt_repwt, diff_ht_repht) %>%
  summary()

daviskeep %>%
  select(sex, diff_wt_repwt, diff_ht_repht) %>%
  group_by(sex) %>%
  group_by(N = n(), add = TRUE) %>%
  summarise_all(funs(mean, sd), na.rm = TRUE)
  
head(daviskeep)

lm1 <- lm(repwt ~ weight, data = daviskeep)
lm1
summary(lm1)

lm1$coefficients

plot(lm1)

car::residualPlot(lm1)

car::residualPlots(lm1)

ggplot(daviskeep, aes(weight, repwt)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(daviskeep, aes(weight, repwt)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~sex)

lm1resid <- as.data.frame(lm1$residuals)
names(lm1resid) <- "residuals"

ggplot(lm1resid, aes(residuals)) +
  geom_histogram(color = "black", fill = "light blue")

davisM <- daviskeep %>%
  filter(sex == "M")

davisF <- daviskeep %>%
  filter(sex == "F")

lmM <- lm(repwt ~ weight, data = davisM)
lmM
summary(lmM)

lmF <- lm(repwt ~ weight, data = davisF)
lmF
summary(lmF)

t.test(diff_wt_repwt ~ sex, data = daviskeep)
t.test(diff_ht_repht ~ sex, data = daviskeep)

aovwts <- aov(diff_wt_repwt ~ sex, data=daviskeep)
aovwts
summary(aovwts)

daviskeep <- daviskeep %>%
  mutate(bmi3tile = ntile(bmi, 3))

table(daviskeep$bmi3tile)

quantile(daviskeep$bmi, probs = c(0.333, 0.667))

daviskeep <- daviskeep %>%
  mutate(bmicut3 = cut(bmi, c(0, 20.6, 23.2, 200)))

head(daviskeep)

table(daviskeep$bmicut3)

ggplot(daviskeep, aes(bmicut3, diff_wt_repwt)) +
  geom_boxplot(aes(color = bmicut3)) +
  geom_jitter(aes(color = bmicut3))

daviskeep %>%
  group_by(bmicut3) %>%
  select(diff_wt_repwt) %>%
  summarise_all(funs(mean, sd), na.rm = TRUE)

aov1 <- aov(diff_wt_repwt ~ bmicut3, data = daviskeep)
summary(aov1)
plot(aov1)
print(aov1)
TukeyHSD(aov1)
aov1$coefficients
aov1$df.residual

ggplot(daviskeep, aes(bmi, diff_wt_repwt)) +
  geom_point() +
  geom_smooth(method = "lm")

cor(daviskeep$bmi, daviskeep$diff_wt_repwt, 
    use = "complete.obs")

cor(daviskeep$bmi, daviskeep$diff_wt_repwt, 
    use = "complete.obs",
    method = "spearman")

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

# ==============================

x <- TRUE
age <- c("child", "young", "old")
a <- c(5.0, 3.1, 2.4)
b <- c(4.1, 2.2, 5.4)
m <- matrix(c(a, b),
            nrow = 3,
            ncol = 2)
l <- list(x, age, m)
names(l) <- c("xval","agevec","mtx")
str(l)

# Run describe() from Hmisc package for sex and bmi
davisHmisc <- daviskeep %>% 
  select(sex, bmi) %>% 
  Hmisc::describe()

# Show structure of Hmisc::describe() output
str(davisHmisc)

# Save correlation output from psych::corr.test()
daviscorr <- daviskeep %>% 
  select(bmi, weight, height) %>%
  psych::corr.test()

# Display structure of daviscorr
str(daviscorr)


# Stats by Group - descriptives for ANOVA
daviskeep %>%
  group_by(bmicat) %>%
  group_by(N = n(), add = TRUE) %>%
  select(diffht, bmicat) %>%
  summarise_all(funs(mean, sd, var))

daviskeep %>%
  group_by(bmicat) %>%
  group_by(N = n(), add = TRUE) %>%
  select(diffht, bmicat) %>%
  summarise_all(funs(mean, sd, var), na.rm=TRUE)

# Perform ANOVA of age by sex, save output as abaov
davisaov <- aov(diffht ~ bmicat, data = daviskeep)

# Show summary of abaov
summary(davisaov)

# Perform TukeyHSD posthoc pairwise tests on abaov
TukeyHSD(davisaov)



# Run lm() of age by shuckedWeight, save output as lmshucked
davislm <- lm(diffht ~ bmi, data = daviskeep)
davislm
str(davislm)
names(davislm)

# Display coefficients element from lmshucked
davislm$coefficients
davislm$coefficients[2]

# Save summry() output of lmshucked
summary(davislm)
smrydavislm <- summary(davislm)
names(smrydavislm)

# Show r.squared and adj.r.squared elements of smrylmshucked
smrydavislm$r.squared
smrydavislm$adj.r.squared
smrydavislm$coefficients



# Run lm() for age by shuckedWeight and by shellWeight
lmdiffhtbmi <- lm(diffht ~ bmi, data = daviskeep)
lmdiffhtwt <- lm(diffht ~ weight, data = daviskeep)

# Run summary() for each model fit and save results
smrylmdiffhtbmi <- summary(lmdiffhtbmi)
smrylmdiffhtwt <- summary(lmdiffhtwt)

# Display `r.squared` and compare AICs for both models
smrylmdiffhtbmi$r.squared
smrylmdiffhtwt$r.squared
AIC(lmdiffhtbmi, lmdiffhtwt)


# Plot of diffht by weight model fits by sex
ggplot(daviskeep, aes(weight, diffht)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~sex) +
  ggtitle("Height differences predicted by weight, model fit by sex")

# Run lm() for age by shuckedWeight and by shellWeight
davislmF <- lm(diffht ~ weight, 
                  subset = (sex == "F"), 
                  data = daviskeep)
davislmM <- lm(diffht ~ weight, 
                  subset = (sex == "M"), 
                  data = daviskeep)

# Run summary() for each model fit and save results
smrylmdiffhtwtF <- summary(lmdiffhtwtF)
smrylmdiffhtwtM <- summary(lmdiffhtwtM)

# Display `r.squared` and compare models by sex
smrylmdiffhtwtF$r.squared
smrylmdiffhtwtM$r.squared
