require(foreign) 
data <- read.dta("birthweight_smoking.dta") 

# Part (a)
reg1 = lm(birthweight~smoker, data=data)
cat("Part (a):\n")
cat("Effect of smoking on birthweight: ",reg1$coefficients[2], "\n\n")

# Part (b)
# (i)
reg2 = lm(birthweight~smoker+alcohol+nprevist, data=data)
cat("Part (b):\n")
cat("(i)\n")
cat("Effect of smoking, alcohol and nprevist on birthweight: ", "\n")
cat(reg2$coefficients)
cat("\n\n")

cat("(ii)\n")
cat("Difference in coefficient of smoking: ", reg2$coefficients[2] - reg1$coefficients[2], "\n\n")

cat("(iii)\n")
cat("Birthweight of Jane's child: ", reg2$coefficients[1] + reg2$coefficients[2]*1 + reg2$coefficients[3]*0 + reg2$coefficients[4]*8, "\n\n")

cat("(iv)\n")
cat("R2 : ", summary(reg2)$r.squared , "\n")
cat("Adj-R2 : ", summary(reg2)$adj.r.squared , "\n\n")

# Part (c)
reg3 <- lm(smoker~alcohol+nprevist,data=data) 
reg4 <- lm(birthweight~alcohol+nprevist,data=data) 
x1 <- reg3$residuals      
y1 <- reg4$residuals 
cat("(c)\n")
cat("Coefficent of smoking: ", lm(y1~x1)$coefficients[2], "\n\n")

# Part (d)
reg5 <- lm(birthweight~smoker+alcohol+tripre0+tripre2+tripre3,data=data)
cat("(ii)\n")
cat("Coefficient of tripre0: ", reg5$coefficients[4], "\n\n")

cat("(iv)\n")
cat("Adj-R2 : ", summary(reg5)$adj.r.squared , "\n\n")





