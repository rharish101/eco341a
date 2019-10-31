nba <- read.table("NBASAL.RAW")
colnames(nba) <- c("marr", "wage", "exper", "age", "coll", "games", "minutes", "guard", "forward", "center", "points", "rebounds", "assists", "draft", "allstar", "avgmin", "lwage", "black", "children", "expersq", "agesq", "marrblck")

# Part (i)
model1 <- lm(points ~ exper + age + coll + expersq, data=nba)
cat("Part (i):\nModel summary:\n")
cat(paste(replicate(50, "-"), collapse=""), "\n")
print(summary(model1))
cat(paste(replicate(50, "-"), collapse=""), "\n\n")

# Part (ii)
a <- coef(model1)["expersq"]
b <- coef(model1)["exper"]
limit <- floor(-b / (2 * a))  # lowest value of parabola
cat("Part (ii):\nExperience limit is:", limit, "\n\n")

# Part (iii)
cat("Part (iii):\n")
cat("p-value of coefficient of 'coll': ", summary(model1)$coefficients["coll", 4], "\n\n")

# Part (iv)
model2 <- lm(points ~ exper + age + coll + expersq + agesq, data=nba)
cat("Part (iv):\n")
cat("p-value of coefficient of 'agesq': ", summary(model2)$coefficients["agesq", 4], "\n\n")

# Part (v)
model3 <- lm(lwage ~ points + exper + expersq + age + coll, data=nba)
cat("Part (v):\nModel summary:\n")
cat(paste(replicate(50, "-"), collapse=""), "\n")
print(summary(model3))
cat(paste(replicate(50, "-"), collapse=""), "\n\n")

# Part (vi)
# Setting age and coll's coef to 0 by removing them
restr_model3 <- lm(lwage ~ points + exper + expersq, data=nba)
# `deviance` returns RSS
rss_u <- deviance(model3)
rss_r <- deviance(restr_model3)
# q=2, n=`dim(nba)[1]`, k=2
df2 <- dim(nba)[1] - 2 - 1  # 2nd d.o.f. for F-distr
f_stat <- ((rss_r - rss_u) / 2) / (rss_u / df2)
p_val <- pf(f_stat, 2, df2, lower.tail=F)
cat("Part (vi):\n")
cat("p-value of F-statistic for joint significance of 'age' and 'coll':", p_val, "\n")
