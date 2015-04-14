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

outcomes <- c("heads", "tails")
sample(outcomes, size = 1, replace = TRUE)
sim_fair_coin <- sample(outcomes, size = 100, replace = TRUE)
sim_fair_coin
table(sim_fair_coin)
sim_unfair_coin <- sample(outcomes, size = 100, replace = TRUE, prob = c(0.2, 0.8))



outcomes <- c("H", "M")
sim_basket <- sample(outcomes, size = 1, replace = TRUE)
kobe$basket
sim_basket


#lab3A

load(url("http://www.openintro.org/stat/data/ames.RData"))
area <- ames$Gr.Liv.Area
price <- ames$SalePrice

#lab4
load(url("http://bit.ly/dasi_nc"))
summary(nc)
head(nc)
source("http://bit.ly/dasi_inference")
inference(nc$gained, type = "ci", method = "simulation", conflevel = 0.90,
          est = "mean", boot_method = "perc")

inference(y = nc$weight, x = nc$habit, est = "mean", type = "ht", null = 0, 
          alternative = "twosided", method = "theoretical", order = c("smoker","nonsmoker"))


load(url("http://bit.ly/dasi_gss_ws_cl"))
inference(y = gss$wordsum, x = gss$class, est = "mean", type = "ht", alternative = "greater", 
          method = "theoretical")


#lab 5
source("http://bit.ly/dasi_inference")
load(url("http://www.openintro.org/stat/data/atheism.RData"))
head(atheism)

#lab 6
load(url("http://www.openintro.org/stat/data/mlb11.RData"))
head(mlb11)
plot(runs~at_bats, data=mlb11)
cor(mlb11$runs, mlb11$at_bats)
plot(homeruns~wins, data=mlb11)
plot_ss(x = mlb11$at_bats, y = mlb11$runs, showSquares = TRUE)
fit<-lm(runs ~ at_bats, data = mlb11)
summary(fit)

plot(mlb11$runs ~ mlb11$at_bats)
abline(fit)

fit2<-lm(homeruns~runs, data=mlb11)
summary(fit2)
plot(mlb11$homeruns~mlb11$runs)
abline(fit2)


































































































