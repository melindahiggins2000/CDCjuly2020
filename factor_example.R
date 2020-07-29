pet <- c("dog","dog","cat","parrot","cat")
petnum <- c(1,1,2,3,2)

pet_factor <- factor(petnum,
                     levels = c(1,2,3),
                     labels = c("dog","cat","parrot"))

data.frame(pet, petnum, pet_factor)

petas <- as.factor(pet)

data.frame(pet, petnum, pet_factor, petas)

table(pet)
table(petnum)
table(pet_factor)
table(petas)

