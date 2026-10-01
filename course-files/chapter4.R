library(MontgomeryDAE)
Table4.3

rand.model <- aov(Flicks ~ ExtrusionPressure, data=Table4.3)
summary(rand.model)

blocked.model <- aov(Flicks ~ Error(BatchOfResin) + ExtrusionPressure,data=Table4.3)
summary(blocked.model)

library(emmeans)
model <- aov(Flicks ~ ExtrusionPressure + BatchOfResin, data=Table4.3)
exmodel <- emmeans(model, ~ ExtrusionPressure)
summary(exmodel)
pairs(exmodel, adjust="tukey")

# Run model adequacy
op <- par(mfrow = c(2, 2)) # 2 rows by 2 columns

# Plot 1: Normal Probability Plot
qqnorm(resid(model), main = 'Normal Probability Plot')
qqline(resid(model), col = 'orange')

# Plot 2: Fitted vs. Residuals
plot(x = predict(model), y = resid(model), main = 'Fitted vs. Residual',
     xlab = 'Fitted Values', ylab = 'Residuals')
abline(h = 0, col = 'orange', lty = 1)

# Plot 3: Pressure vs. Residuals
plot(x = as.numeric(Table4.3$ExtrusionPressure), y = resid(model), 
     main = 'Extrusion Pressure vs. Residual', xlab = 'Extrusion Pressure', ylab = 'Residuals')

# Plot 4: Batch vs. Residuals
plot(x = as.numeric(Table4.3$BatchOfResin), y = resid(model), 
     main = 'BatchOfResin vs. Residual', xlab = 'BatchOfResin', ylab = 'Residuals')

par(op) # Reset plotting layout back to default

resid(blocked.model) # this won't work

#REML
library(lme4)
library(lmerTest)
model <- lmer(Flicks ~ -1 + ExtrusionPressure + (1|BatchOfResin),data=Table4.3,REML=TRUE) # Copied directly from Haskell's book
summary(model)

confint(model)^2

# Latin Squares
Table4.9
model <- aov(BurnRate ~ Batch + Operator + Formulation, data=Table4.9)
summary(model)

clean.model <- aov(BurnRate ~ Error(Batch + Operator) + Formulation, data=Table4.9) # Haskell argues this is cleaner?
summary(clean.model)

# Run model adequacy

op <- par(mfrow = c(2, 2)) # 2 rows by 2 columns

# Normal Probability Plot
qqnorm(resid(model), main = 'Normal Probability Plot')
qqline(resid(model), col = 'orange')

# Fitted vs. Residuals
plot(x = predict(model), y = resid(model), main = 'Fitted vs. Residual',
     xlab = 'Fitted Values', ylab = 'Residuals')
abline(h = 0, col = 'orange', lty = 1)

# Burn Rate vs. Residuals
plot(x = as.numeric(Table4.9$BurnRate), y = resid(model), 
     main = 'Burn Rate vs. Residual', xlab = 'Burn Rate', ylab = 'Residuals')

par(op) # Reset plotting layout back to default

#Generate other Latin Squares (might be useful???)
library(agricolae)
trt <- c("A", "B", "C", "D","E")
outdesign <- design.lsd(trt)
print(outdesign$sketch)

#Replicate Table 4.9 - same batch, same operators (for ease of use)
Table4.9_rep <- rbind(transform(Table4.9, Replicate = factor(1)), transform(Table4.9, Replicate = factor(2)))
Table4.9_rep
clean.model2 <- aov(BurnRate ~ Error(Batch + Operator) + Formulation, data=Table4.9_rep)
summary(clean.model2)
summary(clean.model)

#Graeco-Latin Square
Table4.20
model <- aov(BurnRate ~ Batch + Operator + TestAssembly + Formulation, data=Table4.20)
summary(model)
