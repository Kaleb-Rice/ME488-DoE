library(MontgomeryDAE)
Table3.1

summary(aov(Observation ~ Power, data=Table3.1))  #create model object using aov
anova(Observation ~ Power, data=Table3.1) #this won't work!

model <- lm(Observation ~ Power, data=Table3.1)
summary(model)
anova(model) #showing this is the same as line 4

options(contrasts = c("contr.sum", "contr.poly")) # Set R to use sum-to-zero contrasts for factor variables
model2 <- lm(Observation ~ Power, data=Table3.1)
summary(model2)
options(contrasts = c("contr.treatment", "contr.poly")) #reset R back to its default baseline coding

model.tables(aov(Observation ~ Power, data=Table3.1), type = "effects")

water_pH <- read.csv("E:/ME488/RobsData/water_pH.txt")
water_pH # Show data
summary(aov(pH ~ brand, data=water_pH))  #create model object using aov

# Residual checking

qqnorm(resid(model))
qqline(resid(model), col='orange')

op <- par(mfrow=c(1, 2))
plot(resid(model), main='Residual vs. Run Order')
plot(x=predict(model), y=resid(model), main='Predicted vs. Residual')

bartlett.test(Observation ~ Power, data=Table3.1)

#Example 3.5
Example3.5
model <- aov(Observation ~ EstimationMethod, data=Example3.5)
summary(model)

plot(predict(model), resid(model), main='Predicted vs. Residual')

library(car)
leveneTest(Observation ~ EstimationMethod,data=Example3.5)

#Transformations
y <- sqrt(Example3.5$Observation)
model <- aov(y ~ EstimationMethod, data=Example3.5)
summary(model)
plot(predict(model), resid(model), main='Predicted vs. Residual')

y <- log(Example3.5$Observation)
model <- aov(y ~ EstimationMethod, data=Example3.5)
summary(model)
plot(predict(model), resid(model), main='Predicted vs. Residual')

y <- 1/(sqrt(Example3.5$Observation))
model <- aov(y ~ EstimationMethod, data=Example3.5)
summary(model)
plot(predict(model), resid(model), main='Predicted vs. Residual')

y <- 1/(Example3.5$Observation)
model <- aov(y ~ EstimationMethod, data=Example3.5)
summary(model)
plot(predict(model), resid(model), main='Predicted vs. Residual')

#Contrasts

library(emmeans)  # emmeans gives estimated marginal means, which is better for real-world data!
model <- lm(Observation ~ Power, data=Table3.1)
emmodel <- emmeans(model, ~ Power)
summary(emmodel)

contrast_model <- contrast(emmodel, list('C1'=c(1, -1, 0, 0),'C2'=c(1, 1, -1, -1),'C3'=c(0, 0, 1, -1)))
summary(contrast_model)
confint(contrast_model, level = 0.95)

contrast_model <- contrast(emmodel, list('C1'=c(-1, 1, 0, 0),'C2'=c(-1, -1, 1, 1),'C3'=c(0, 0, -1, 1)))
summary(contrast_model)
confint(contrast_model, level = 0.95)

pairs(emmodel, adjust="tukey")

# Calculate Fisher LSD
MSE <- sigma(model)^2   
df  <- df.residual(model)
n   <- 5
LSD <- qt(0.975, df = df) * sqrt(2 * MSE / n)  # Calculate LSD at alpha = 0.05
LSD
