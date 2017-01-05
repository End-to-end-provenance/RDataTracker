1: ddg.start("setup")
2: library(Ecdat)
3: library(mosaic)
4: x <- DoctorAUS
5: ddg.finish("setup")
6: ddg.start("test1")
7: x$sex2 <- "Male"
8: x$sex2[x$sex == "1"] <- "Female"
9: x$Age <- (x$age) * 100
10: x$Income <- (x$income) * 10000
11: ddg.finish("test1")
12: ddg.start("ddg.chunk_1")
13: histogram(~Age, data = x)
14: ddg.finish("ddg.chunk_1")
15: ddg.start("ddg.chunk_2")
16: histogram(~illness, data = x)
17: histogram(~doctorco, data = x)
18: ddg.finish("ddg.chunk_2")
19: ddg.start("ddg.chunk_3")
20: histogram(~doctorco, data = x)
21: x$doctorcobin <- 1
22: x$doctorcobin[x$doctorco == "0"] <- 0
23: histogram(~doctorcobin, data = x)
24: ddg.finish("ddg.chunk_3")
25: ddg.start("test2")
26: histogram(~doctorcobin, data = x)
27: ddg.finish("test2")
28: ddg.start("test3")
29: x$sex2 <- "Male"
30: x$sex2[x$sex == "1"] <- "Female"
31: x$Income <- (x$income) * 1e+05
32: ddg.finish("test3")
33: ddg.start("ddg.chunk_4")
34: histogram(~Income, data = x)
35: ddg.finish("ddg.chunk_4")
36: ddg.start("ddg.chunk_5")
37: bwplot(Age ~ Income, data = x, ylab = "Age groups (of five years)")
38: lmage <- lm(sqrt(income) ~ poly(age, 2, raw = T), data = x)
39: plot(x$age * 100, sqrt(x$income), xlab = "Age", ylab = "Sqrt of Income, $10,000")
40: lines(x$age * 100, predict(lmage))
41: ddg.finish("ddg.chunk_5")
42: ddg.start("ddg.chunk_6")
43: x$Age2 <- x$Age^2
44: Quadratic <- lm(Income ~ Age + Age2, data = x)
45: ddg.finish("ddg.chunk_6")
46: ddg.start("ddg.chunk_7")
47: summary(Quadratic)
48: ddg.finish("ddg.chunk_7")
49: ddg.start("ddg.chunk_8")
50: x$illnessbin <- 1
51: x$illnessbin[x$illness == 0] <- 0
52: glmill <- glm(illnessbin ~ income + sex + age + chcond, data = x, 
52:     family = "binomial")
53: summary(glmill)
54: x$logitillness <- predict(glmill)
55: x$probabilityillness <- exp(x$logitillness)/(1 + exp(x$logitillness))
56: xyplot(probabilityillness ~ income * 10000, groups = chcond, 
56:     auto.key = TRUE, data = x, type = c("p", "r"), xlab = "Income", 
56:     ylab = "Probability of Illness", main = "Probability of Illness in Past 2 Weeks to Income")
57: ddg.finish("ddg.chunk_8")
