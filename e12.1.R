library(foreign)
library(sem)

data <- read.dta("fertility.dta")

# Part (a)
reg1 <- lm(weeksm1~morekids, data=data)
cat("Part (a):\n")
cat("Effect of morekids on weeksworked: ",reg1$coefficients[2], "\n\n")

# Part (c)
reg2 <- glm(morekids~ samesex, data= data, family="binomial")
cat("Part (c):\n")
cat("Increased odds of morekids due to samesex: ", exp(reg2$coefficients[2] ), "\n\n")

# Part (d)
cat("Part (d):\n")
cat("Condition 1: Instrument relevance \n")
cat("Correlation b/w morekids and samesex: ", cor(data$morekids, data$samesex), "\n\n")
cat("Condition 2: Instrument exogeneity \n")
cat("Correlation b/w residuals of morekids and samesex: ", cor(resid(reg2), data$samesex), "\n\n")


# Part (e)
cat("Part (e):\n")
reg3 <- lm(morekids~ samesex, data= data) 
cat("F statistic: ", summary(reg3)$fstatistic[1] , "\n\n")

# Part (f)
cat("Part (f):\n")
reg4 <- tsls(weeksm1~morekids, instruments =~samesex, data= data) 
cat("Effect of morekids on weeksworked with samesex as an instrument: ", reg4$coefficients[2] , "\n\n")

# Part (g)
cat("Part (g):\n")
reg5 <- lm(weeksm1~morekids+agem1+black+hispan+othrace, data=data)
reg6 <- tsls(weeksm1~morekids+agem1+black+hispan+othrace, instruments =~samesex+agem1+black+hispan+othrace, data= data) 

cat("Effect of morekids on weeksworked with control variables non IV: ", reg5$coefficients[2] , "\n\n")
cat("Effect of morekids on weeksworked with control variables IV: ", reg6$coefficients[2] , "\n\n")