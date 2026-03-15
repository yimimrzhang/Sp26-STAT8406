# =========== #
# Homework #7 #
# =========== #

# ============================================= #
# Problem 1: Bat data #
# ============================================= #
bats <- read.csv("bats.txt",header=T)     # Reads in the bat data
attach(bats)                                   # Promotes the data set
plot(logmass,logenergy,xlab="Log Mass",ylab=   # Scatterplot of log energy expend.
       "Log Energy Expenditure",cex.axis=1.5,         #   vs. log body mass with a separate
     pch=c(4,16,1)[unclass(as.factor(type))],cex=1.5, #   plotting symbol for the three
     cex.lab=1.6,mgp=c(2.7,1,0))                      #   types of species

reg1.out <- lm(logenergy[type=="Non-echolocating bats"]~
                 logmass[type=="Non-echolocating bats"])      # Regression for non-echo bats
abline(reg1.out,lwd=2,lty=3)                   # Plots the regression line
reg2.out <- lm(logenergy[type=="Non-echolocating birds"]~
                 logmass[type=="Non-echolocating birds"])     # Regression for non-echo birds
abline(reg2.out,lwd=2,lty=1)                   # Plots the regression line
reg3.out <- lm(logenergy[type=="Echolocating bats"]~
                 logmass[type=="Echolocating bats"])          # Regression for echo bats
abline(reg3.out,lwd=2,lty=2)                   # Plots the regression line
legend(1.8,0.5,c("Non-echo bats",              # Places a legend on the plot to
                 "Non-echo birds","Echo bats"),pch=c(4,16,1), #   distinguish the three species
       lwd=c(2,2,2),lty=c(3,1,2),cex=1.3)           #   types, at (1.8,0.5)

reg4.out <- lm(logenergy~logmass*type)         # Interaction model
summary(reg4.out)                              # Summary of interaction model
anova(reg4.out)                                # ANOVA for interaction model

reg5.out <- lm(logenergy~logmass+type)         # Additive model
anova(reg5.out, reg4.out)

# ============================ #
# Problem 2: Sludge #
# ============================ #
sludge <- read.csv("sludge.txt",header=T) # Reads in the sludge data
summary(sludge)                                # Data summary
attach(sludge)                                 # Attaches the sludge data

# Part (a)
# ========
plot(soilconc,plantconc,cex.axis=1.5,cex=1.5,  # Plot of plant content vs. soil content
     pch=c(1,2,16)[unclass(as.factor(type))],cex.lab=1.6,    
     xlab="Soil Mercury Content",ylab="Plant Mercury Content",
     mgp=c(2.7,1,0))
legend(1.5,150,c("Barley","Corn","Wheat"),     # Places a legend on the plot
       pch=c(1,2,16),cex=1.5)

# Part (c)
# ========
reg1 <- lm(plantconc~(soilconc+ I(soilconc^2))*type)  # Fits a quadratic model for the three crop types
summary(reg1)                                  # Regression summary
anova(reg1)                                    # Regression ANOVA

reg2 <- lm(plantconc~soilconc*type + I(soilconc^2)) # Fits a quadratic model with the same quadratic coefficients 
                                                    # for the three crop types
anova(reg2,reg1)                       

