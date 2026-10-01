library(MontgomeryDAE)
Table10.2

model <- lm(Viscosity ~ ReactionTemperature + CatalystFeedRate, data = Table10.2)
summary(model)

qqnorm(residuals(model), main = "Normal Probability Plot of Residuals")
qqline(residuals(model), col = "red", lwd = 2) # Adds the reference line

plot(Table10.2$Viscosity,residuals(model))
abline(h = 0, col = "black")
plot(Table10.2$ReactionTemperature,residuals(model))
abline(h = 0, col = "black")
plot(Table10.2$CatalystFeedRate,residuals(model))
abline(h = 0, col = "black")

model2 <- lm(Viscosity ~ ReactionTemperature, data = Table10.2)
print(anova(model2,model))
summary(model2) # Just for interest!
qf(0.05,1,13,lower.tail= FALSE)

print(Table10.3 <- data.frame('y'=Table10.2$Viscosity,'Predicted'=predict(model),'Residual'=resid(model),'StandardizedResids'=rstandard(model),'hii'=hatvalues(model),'StudentizedResids'=rstudent(model),'PRESSResids'=resid(model)/(1-hatvalues(model))))

PRESSResids <- resid(model)/(1-hatvalues(model)) #Eqn 10.50
PRESS <- sum(PRESSResids^2) # PRESS (sum)
SST <- sum(anova(model)$`Sum Sq`) #SST
R2 <- 1-(PRESS/SST)