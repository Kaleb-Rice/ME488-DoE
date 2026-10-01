library(MontgomeryDAE);library(pwr)

Table6.1
full.model <- aov(Response ~ A * B, data = Table6.1)
summary(full.model)
2*coef(full.model)[-1]
full.model = lm(Response ~ A + B, data = Table6.1) # remove interaction (*) and generate model
summary(full.model)

# Run model adequacy
op <- par(mfrow = c(2, 2)) # 2 rows by 2 columns

# Plot 1: Normal Probability Plot
qqnorm(resid(full.model), main = 'Normal Probability Plot')
qqline(resid(full.model), col = 'orange')

# Plot 2: Fitted vs. Residuals
plot(x = predict(full.model), y = resid(full.model), main = 'Fitted vs. Residual',xlab = 'Fitted Values', ylab = 'Residuals')
abline(h = 0, col = 'orange', lty = 1)

# Plot 3: A vs. Residuals
plot(x = as.numeric(Table6.1$A), y = resid(full.model), main = 'A vs. Residual', xlab = 'A', ylab = 'Residuals')

# Plot 4: B vs. Residuals
plot(x = as.numeric(Table6.1$B), y = resid(full.model), main = 'B vs. Residual', xlab = 'B', ylab = 'Residuals')

par(op) # Reset plotting layout back to default

pwr.f2.test(u = 1, v = 4, f2 = 8, sig.level = 0.05)  #assumes f^2 = 8^6 = 48
pwr.f2.test(u = 1, v = 4, f2 = 8/6, sig.level = 0.05)
pwr.f2.test(u = 1, v = 4, f2 = 8/6, sig.level = 0.1)
pwr.f2.test(u = 1, v = 8, f2 = 12/10, sig.level = 0.05) # 3 replicates

#Example 6.1 - a 2^3 design
Table6.4

model = aov(EtchRate ~ Gap * Flow * Power, data = Table6.4)
2*coef(model)[-1]
summary(model)

# Run model adequacy

# Normal Probability Plot
qqnorm(resid(model), main = 'Normal Probability Plot')
qqline(resid(model), col = 'orange')

# Fitted vs. Residuals
plot(x = predict(model), y = resid(model), main = 'Fitted vs. Residual',xlab = 'Fitted Values', ylab = 'Residuals')
abline(h = 0, col = 'orange', lty = 1)

# Run order vs residuals - check for time-order covariance
plot(x = seq_along(resid(model)), y = resid(model), main = 'Run Order vs. Residual',xlab = 'Run Order', ylab = 'Residuals')
abline(h = 0, col = 'orange', lty = 1)

# Plot : Gap vs. Residuals # this is pointless because data is coded!
plot(x = as.numeric(Table6.4$Power), y = resid(model), main = 'Power vs. Residual', xlab = 'Power', ylab = 'Residuals')

# Example 6.2- 2^4 single replicate
Table6.10
model <- aov(Filtration ~ Temperature * Pressure * Formaldehyde * StirringRate, data=Table6.10)
summary(model)
model2 <- aov(Filtration ~ (Temperature * Pressure * StirringRate) + Formaldehyde, data = Table6.10) # remove formaldehyde interaction, get back some DOF!
summary(model2)

# Normal Probability plot for Example 6.2 - mostly copied from Haskell, but some tweaks as well

X <- model.matrix(~ -1 + Temperature*Pressure*Formaldehyde*StirringRate, data=Table6.10)
effects <- colSums(X * Table6.10$Filtration) / (0.5 * nrow(Table6.10)) # modified from Haskell's code
names(effects) <- colnames(X)
locs <- qqnorm(effects, main='Normal Probability Plot')
qqline(effects, col='orange')
ix <- 1:length(effects)  # or seq_along(effects)
text(x = locs$x[ix], y = locs$y[ix], labels = names(effects[ix]), pos = 4)

# Half Normal probability plot - from Haskell's book
# Here's another useful library =)
library(DoE.base)
vals <- halfnormal(model, alpha=0.1)

#Refine model by removing pressure!
model <- aov(Filtration ~ Temperature * Formaldehyde * StirringRate, data=Table6.10)
summary(model)

# Run model adequacy

# Normal Probability Plot
qqnorm(resid(model), main = 'Normal Probability Plot')
qqline(resid(model), col = 'orange')

#Other examples

# Example 6.3
model63 <- aov(DrillRate ~ DrillLoad * FlowRate * RotationalSpeed * Mud, data=Example6.3)
summary(model63)
halfnormal(model63,alpha=0.1)
2*coef(model63)[-1]
model63b <- aov(DrillRate ~  FlowRate * RotationalSpeed * Mud, data=Example6.3)
summary(model63b)

# Example 6.4
model64 <- aov(Defects ~ Temperature * ClampTime * ResinFlow * ClosingTime, data=Example6.4)
halfnormal(model64,alpha=0.1)
2*coef(model64)[-1]
model64b <- aov(Defects ~ Temperature + ResinFlow , data=Example6.4)
summary(model64b)
