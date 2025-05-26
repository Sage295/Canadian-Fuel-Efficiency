#Coded by: Sejal Mogalgiddi, Joaquin Hidalgo-Estrada

#opening and importing data set
df <- data.frame(fuel)
# printing out the data set to see the first few values.
#doing this helps us see if data set has been imported properly
head(df)

# we assigned the independent and dependent variable to x and y to make it
#easier to read and type later.
x <- fuel$`City (L/100 km)` 
y <- fuel$`Highway (L/100 km)`

#this model helps us find the data needed for the hypothesis test, p-values, etc
model = lm(y ~ x, data=fuel)
# prints scatter plot
plot(x,y)
abline(model)
# helps us find the least squares regression line
print(model)
# finds all the necessary plots needed to determine if our assumptions are violated or not
plot(model)
#prints p, values, t-statistics, etc
summary(model)
# to find r and r^2
cor.test(x,y)


#transformation of log(y)
model2 <- lm(I(log(y) ~ x), data=fuel)
plot(x, log(y))
abline(model2)
plot(model2)
#prints p, values, t-statistics, etc
summary(model2)

#transformation of y^2
model3 <- lm(I((y^2) ~ x), data=fuel)
plot(x, (y^2))
abline(model3)
plot(model3)
#prints p, values, t-statistics, etc
summary(model3)

#transformation of x^2
model4 <- lm(y ~ I(x^2), data=fuel)
plot(x^2, y)
abline(model4)
plot(model4)
#prints p, values, t-statistics, etc
summary(model4)

#transformation of log(y) and x^2
model5 <- lm(I(log(y) ~ I(x^2)), data=fuel)
plot((x^2), log(y))
abline(model5)
plot(model5)
#prints p, values, t-statistics, etc
summary(model5)

#transformation of log(x)
model6 <- lm(y~ I(log(x)), data=fuel)
plot(log(x), y)
abline(model6)
plot(model6)
#prints p, values, t-statistics, etc
summary(model6)




