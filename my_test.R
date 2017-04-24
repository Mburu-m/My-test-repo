y = rnorm(300)
x = rpois(300, lambda = 2)

fit = lm(y~x)
summary(fit)
par(mfrow = c(2,2))
plot(fit)
