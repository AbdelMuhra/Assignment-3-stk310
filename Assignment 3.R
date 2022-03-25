# Read data into R from CSV file.

pracA3 <- read.csv("videos.csv", header = T)

x <- pracA3$Song.Length
y <- pracA3$File.Size

n <- nrow(pracA3)
# Question 1

# OLS regression with inference.
lrm <- lm(y ~ x, data = pracA3)
(slrm <- summary(lrm))

# Calculate 90% confidence intervals for beta1 & beta2.
alpha <- 0.1

df <- slrm$df[2]
(tvalue_10 <- qt(alpha/2, df, lower.tail = FALSE))

(beta1hat <- slrm$coef[1,1])

(se_beta1hat <- slrm$coef[1,2])

(lcl_beta1 <- beta1hat - tvalue_10 * se_beta1hat)

(ucl_beta1 <- beta1hat + tvalue_10 * se_beta1hat)

(beta2hat <- slrm$coef[2,1])

(se_beta2hat <- slrm$coef[2,2])

(lcl_beta2 <- beta2hat - tvalue_10 * se_beta2hat)

(ucl_beta2 <- beta2hat + tvalue_10 * se_beta2hat)

# Test H0: beta2 = 0.07.
beta2_null <- 0.07
(testvalue <- (beta2hat - beta2_null) / se_beta2hat)

(pvalue <- 2 * pt(testvalue, df, lower.tail = FALSE))

# Calculate 95% confidence interval for sigma-square.
alpha <- 0.05
(mse <- slrm$sigma ^ 2)
(chi2value_lt <- qchisq(alpha/2, df))

(chi2value_ut <- qchisq(alpha/2, df, lower.tail = FALSE))

(lcl_sigsq <- df * mse / chi2value_ut)

(ucl_sigsq <- df * mse / chi2value_lt)

# ANOVA
xbar <- mean(x)
ybar <- mean(y)
yhat <- lrm$fitted.values
res <- lrm$residuals
(ess <- beta2hat ^ 2 * sum((x - xbar) ^ 2))

(rss <- sum(res ^ 2))

(tss <- sum((y - ybar) ^ 2))

(Fvalue <- slrm$fstatistic[1])

(pvalue <- slrm$coef[2,4])

# Mean & individual prediction.>> alpha <- 0.05
x0 <- 184
(yhat0 <- unname(lrm$fitted.values[3]))

(tvalue_5 <- qt(alpha/2, df, lower.tail = FALSE))
(se_meanpred <- sqrt(mse * (1 / n + ((x0 - xbar) ^ 2) / sum((x - xbar) ^ 2))))

(lcl_meanpred <- yhat0 - tvalue_5 * se_meanpred)

(ucl_meanpred <- yhat0 + tvalue_5 * se_meanpred)
(se_indpred <- sqrt(mse * (1 + 1 / n + ((x0 - xbar) ^ 2) / sum((x - xbar) ^ 2))))

(lcl_indpred <- yhat0 - tvalue_5 * se_indpred)

(ucl_indpred <- yhat0 + tvalue_5 * se_indpred)


# Question 2
# OLS regression through origin.
lrm0 <- lm(y ~ 0 + x, data = pracA3)
(slrm0 <- summary(lrm0))

# Question 3
# OLS regression for log-linear model.
lnx <- log(x)
lny <- log(y)
loglrm <- lm(lny ~ lnx, data = pracA3)
(sloglrm <- summary(loglrm)) 
 
(alphahat <- sloglrm$coef[1,1])

(beta1hat <- exp(alphahat))

(beta2hat <- sloglrm$coef[2,1])  
 
 