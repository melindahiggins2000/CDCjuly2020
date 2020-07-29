# my first object created =====
obj1 <- 2+2
obj1

# next block of code =====
idnum <- c(1,2,3,4,5)
age <- c(45,34,52,41,48)
pet <- c("dog","dog","cat","parrot","cat")

# combine 3 vectors =====
mydata <- data.frame(idnum, age, pet)
View(mydata)
# view(mydata) # R is tYPe case sensitive

names(mydata)

# look at pet
library(dplyr)
mydata %>%
  select(pet)

# also use the $ selector
mydata$pet

# why do these look different
# ALWAYS check class() of objects
class(pet)
class(mydata)
class(abalone)

pet_dplyr <- mydata %>%
  select(pet)

pet_base <- mydata$pet

class(pet_dplyr)
class(pet_base)

# with dplyr, try pull() instead of select()
# select() use 2+ variables
# pull() use with 1 variable
pet_pull <- mydata %>%
  pull(pet)

class(pet_pull)

# can also choose variable by index
# [row, col] index
# look at all rows, 3rd column
mydata[,3]
mydata[2,3]


