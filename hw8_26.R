# ===================================== #
# Problem 1: Biomass Example #
# ===================================== #
biomass <- read.csv("biomass.txt",header=T)        # Reads in the biomass data.
summary(biomass)                                   # Summarizes the data set.

# Part (a)
# ========
source("pairs.panels.r")                   # Loads the pairs.panels function into memory
pairs.panels(biomass)                              # Creates a matrix of scatterplots/correlations


# Part (b)
# ========
library(olsrr)
reg <- lm(biomass~pH+Potassium+Sodium,data=biomass)
allsubsets <- ols_step_all_possible(reg)
allsubsets
plot(allsubsets)

# PRESS obtained by REPEATEDLY running the ols_press function

# Visualize Cp plot
plot((allsubsets[["result"]]$n+1),allsubsets[["result"]]$cp, # Plots Cp v.s p
     main="Cp Plot",xlab="p", ylab="Cp",
     xlim=c(1,4), ylim=c(0,8),
     pch=19, cex=1.4) 
abline(0, 1,lwd=2)                   # Add reference line y=x


# Part (c)
# ========
ols_step_backward_p(reg, p_val = 0.05)    # Backward Elimination
ols_step_forward_p(reg, p_val = 0.05)     # Forward Selection


# ==================== #
# Problem 2 #
# ==================== #
protein <- read.csv("protein.txt",header=T)

reg1 <- lm(protein~copper*zinc+I(copper**2)+I(zinc**2),data=protein)
summary(reg1)
reg2 <- lm(protein~copper*zinc+I(copper**2),data=protein)
summary(reg2)
reg3 <- lm(protein~copper*zinc,data=protein)
summary(reg3)


