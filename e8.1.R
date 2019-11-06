library("readxl")
data <- read_excel("lead_mortality.xlsx", sheet = "Data")

# part a
cat("Part (a):\n")
inf0 <- subset(data$infrate, data$lead == 0)
m0 <- mean(inf0)
inf1 <- subset(data$infrate, data$lead == 1)
m1 <- mean(inf1)
diff <- m1 - m0
tstat <- diff / (sd(data$infrate))
pval <- 2 * pt(tstat, df = dim(data)[1] - 1, lower.tail = FALSE)
cat("Average infrate @ lead = 0:", m0, "\n")
cat("Average infrate @ lead = 1:", m1, "\n")
cat("Differene between infrate @ lead = 1 and infrate @ lead = 0 :", diff, "\n")
cat("corresponding p-value :", pval, "\n\n")

# part b i
data$leadxph <- data$ph * data$lead
model1 <- lm(infrate ~ ph + lead + leadxph, data = data)
cat("Part (b.i):\nModel summary:\n")
cat(paste(replicate(50, "-"), collapse = ""), "\n")
print(summary(model1))
cat(paste(replicate(50, "-"), collapse = ""), "\n\n")

# part b ii
data$m1est <- predict(model1)
ph0 <- subset(data$ph, data$lead == 0)
infest0 <- subset(data$m1est, data$lead == 0)
ph1 <- subset(data$ph, data$lead == 1)
infest1 <- subset(data$m1est, data$lead == 1)
png(filename = "e8_1_plot.png")
plot(ph0, infest0, xlab = "pH", ylab = "Inf", col = "green", type = "b")
lines(ph1, infest1, xlab = "pH", ylab = "Inf", col = "red", type = "b")
legend("topright", legend = c("Lead = 0", "Lead = 1"), col = c("green", "red"), lty = 1)
invisible(dev.off())

# part b v
phavg <- mean(data$ph)
cat("Part (b.v):\n")
cat("Average pH:", phavg, "\n")
sdph <- sd(data$ph)
c1 <- coef(model1)["lead"] + phavg * coef(model1)["leadxph"]
cat("Effect of lead @ average pH:", c1, "\n")
cat("Standard deviation of pH:", sdph, "\n")
c2 <- coef(model1)["lead"] + (phavg + sdph) * coef(model1)["leadxph"]
c3 <- coef(model1)["lead"] + (phavg - sdph) * coef(model1)["leadxph"]
cat("Effect of lead @ average ph  - standard deviation of ph:", c3, "\n")
cat("Effect of lead @ average ph  + standard deviation of ph:", c2, "\n")
cat("\n")

# part b vi
cat("Part (b.vi):\n")
t <- qt(c(.975), df = 168)
coef_cov <- vcov(model1)
stddev <- sqrt(coef_cov["lead", "lead"] + 6.5^2 * coef_cov["leadxph", "leadxph"] + 2 * 6.5 * coef_cov["lead", "leadxph"])
b <- coef(model1)["lead"] + 6.5 * coef(model1)["leadxph"]
left <- coef(model1)["lead"] + 6.5 * coef(model1)["leadxph"] - stddev * t
right <- coef(model1)["lead"] + 6.5 * coef(model1)["leadxph"] + stddev * t
cat("95% confindence interval - [", left, right, "]\n")
cat("\n")

# part c
model4 <- lm(ph ~ hardness + temperature + precipitation, data = data)
cat("Part (c):\nModel summary:\n")
cat(paste(replicate(50, "-"), collapse = ""), "\n")
print(summary(model4))
cat(paste(replicate(50, "-"), collapse = ""), "\n")
