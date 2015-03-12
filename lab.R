#lab0
head(present)
ncol(present)
present$girls #view girls counts
plot(present$year, present$girls)
#total
present$total<-present$boys+present$girls
head(present)
plot(present$year, present$total)
present[present$total==max(present$total), ]
#proportion of boys over time:
present$pro_boys<-present$boys/(present$total)
plot(present$year, present$pro_boys)
present$boys > present$girls#returns logical TRUE or FALSE 
#boy to girl ratio:
present$btg<-present$boys/present$girls
plot(present$year, present$btg)

present$diff<-abs(present$boys-present$girls)
present[present$diff==max(present$diff),]

#lab1
source("http://www.openintro.org/stat/data/cdc.R")
names(cdc)
dim(cdc)
class(cdc$genhlth)
levels(cdc$genhlth)
class(cdc$smoke100)
summary(cdc$smoke100)
male<-cdc[cdc$gender=="m", ]
nrow(male)
exce<-cdc[cdc$genhlth=="excellent",]
nrow(exce)/nrow(cdc)
gender_smokers = table(cdc$gender,cdc$smoke100)
mosaicplot(gender_smokers)
subset<-cdc[cdc$age<23 & cdc$smoke100>=1,]
nrow(subset)

bmi = (cdc$weight / cdc$height^2) * 703
boxplot(bmi ~ cdc$genhlth)

hist(bmi)
hist(bmi, breaks = 50)

plot(cdc$weight, cdc$wtdesire)

#lab2
load(url("http://www.openintro.org/stat/data/kobe.RData"))
head(kobe)
kobe_streak <- calc_streak(kobe$basket)
barplot(table(kobe_streak))




























































































































