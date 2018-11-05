
tests = read.table("2.a_Test_Scores.txt")

#Basic numerical EDA for states dataset.
summary(tests)
sd(tests$Test.Score)
sd(tests$Hours.Studied)
cor(tests$Test.Score, tests$Hours.Studied)

#Basic graphical EDA for tests dataset.
plot(tests$Hours.Studied, tests$Test.Score)

#####Fitting a simple linear regression.#####

model.simple = lm(Test.Score ~ Hours.Studied, data = tests)


summary(model.simple) #Investigating the model and assessing some diagnostics.
plot(model.simple)

influencePlot(model.simple)


#Constructing confidence and prediction bands for the scope of our data.
newdata = data.frame(Hours.Studied = seq(1, 3, length = 100))
conf.band = predict(model.simple, newdata, interval = "confidence")
pred.band = predict(model.simple, newdata, interval = "prediction")

plot(tests$Hours.Studied, tests$Test.Score,
     xlab = "Hours Studied", ylab = "Test Score",
     main = "Simple Linear Regression Model\nTests Dataset")
abline(model.simple, lty = 2)
lines(newdata$Hours.Studied, conf.band[, 2], col = "blue") #Plotting the lower confidence band.
lines(newdata$Hours.Studied, conf.band[, 3], col = "blue") #Plotting the upper confidence band.
lines(newdata$Hours.Studied, pred.band[, 2], col = "red") #Plotting the lower prediction band.
lines(newdata$Hours.Studied, pred.band[, 3], col = "red") #Plotting the upper prediction band.
legend("topleft", c("Regression Line", "Conf. Band", "Pred. Band"),
       lty = c(2, 1, 1), col = c("black", "blue", "red"))


#####Adding a quadratic term.#####

model.quadratic = lm(Test.Score ~ Hours.Studied + I(Hours.Studied^2), data = tests)

summary(model.quadratic) #Investigating the model and assessing some diagnostics.
plot(model.quadratic)
influencePlot(model.quadratic)

#Constructing confidence and prediction bands for the scope of our data.
conf.band = predict(model.quadratic, newdata, interval = "confidence")
pred.band = predict(model.quadratic, newdata, interval = "prediction")

plot(tests$Hours.Studied, tests$Test.Score,
     xlab = "Hours Studied", ylab = "Test Score",
     main = "Quadratic Regression Model\nTests Dataset")
lines(tests$Hours.Studied[order(tests$Hours.Studied)],
      model.quadratic$fitted.values[order(tests$Hours.Studied)], lty = 2)
lines(newdata$Hours.Studied, conf.band[, 2], col = "blue") #Plotting the lower confidence band.
lines(newdata$Hours.Studied, conf.band[, 3], col = "blue") #Plotting the upper confidence band.
lines(newdata$Hours.Studied, pred.band[, 2], col = "red") #Plotting the lower prediction band.
lines(newdata$Hours.Studied, pred.band[, 3], col = "red") #Plotting the upper prediction band.
legend("topleft", c("Regression Line", "Conf. Band", "Pred. Band"),
       lty = c(2, 1, 1), col = c("black", "blue", "red"))


#####Adding a factor.#####

model.factor = lm(Test.Score ~ Hours.Studied + Gender, data = tests)

summary(model.factor) #Investigating the model and assessing some diagnostics.
plot(model.factor)
influencePlot(model.factor)

col.vec = c(rep("pink", 250), rep("blue", 250))

plot(tests$Hours.Studied, tests$Test.Score, col = col.vec,
     xlab = "Hours Studied", ylab = "Test Score",
     main = "Linear Regression Model w/ Factor\nTests Dataset")
abline(model.factor$coefficients[1], #Intercept for females.
       model.factor$coefficients[2], #Slope for females.
       lwd = 3, lty = 2, col = "pink")
abline(model.factor$coefficients[1] + model.factor$coefficients[3], #Intercept for males.
       model.factor$coefficients[2], #Slope for males.
       lwd = 3, lty = 2, col = "blue")
legend("topleft", c("Female Regression", "Male Regression"),
       lwd = 3, lty = 2, col = c("pink", "blue"))