library(MontgomeryDAE)

Table5.1$Temperature <- factor(Table5.1$Temperature) # Make material type factor not numeric!
Table5.1

null.model <- aov(BatteryLife~1,data=Table5.1) # Let's look at null model first!
full.model <- aov(BatteryLife ~ MaterialType * Temperature, data = Table5.1) # different format from Haskell's book!
anova(null.model,full.model)

summary(model)

interaction.plot(Table5.1$Temperature,Table5.1$MaterialType,Table5.1$BatteryLife)
interaction.plot(Table5.1$MaterialType,Table5.1$Temperature,Table5.1$BatteryLife)

# Run some Tukey tests!
library(emmeans)
emmodel <- emmeans(full.model, ~ Temperature) # note error message!
emm <- emmeans(full.model, ~ MaterialType | Temperature) # Run Tukey for all temperatures!
pairs(emm)

emmodel <- emmeans(full.model, ~ MaterialType) # note error message!
emm <- emmeans(full.model, ~ Temperature | MaterialType) # Run Tukey for all materials
pairs(emm)

# Run model adequacy
op <- par(mfrow = c(2, 2)) # 2 rows by 2 columns

# Plot 1: Normal Probability Plot
qqnorm(resid(full.model), main = 'Normal Probability Plot')
qqline(resid(full.model), col = 'orange')

# Plot 2: Fitted vs. Residuals
plot(x = predict(full.model), y = resid(full.model), main = 'Fitted vs. Residual',
     xlab = 'Fitted Values', ylab = 'Residuals')
abline(h = 0, col = 'orange', lty = 1)

# Plot 3: MaterialType vs. Residuals
plot(x = as.numeric(Table5.1$MaterialType), y = resid(full.model), 
     main = 'MaterialType vs. Residual', xlab = 'Material Type', ylab = 'Residuals')

# Plot 4: Temperature vs. Residuals
plot(x = as.numeric(Table5.1$Temperature), y = resid(full.model), 
     main = 'Temperature vs. Residual', xlab = 'Temperature', ylab = 'Residuals')

par(op) # Reset plotting layout back to default
