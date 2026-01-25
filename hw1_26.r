# ========== #
# Homework 1 #
# ========== #

# ==================== #
# Problem 1#
# ==================== #
sizes <- read.csv("sizes.txt",header=T)   # Reads in sizes data
attach(sizes)                             # Allows to access variable names 
                                          #   without calling the data frame

# Parts (a,b): Scatterplot of height vs. armspan for females and males,
#              Regression equations for both females and males,
# ==================================================================
plot(armspan,height,pch=1,cex=1.5,             # Plots height vs. armspan with open circles
     xlab="Armspan (cm)",ylab="Height (cm)",      #   (pch=1), axis labels, and text sizes set
     cex.lab=1.5,cex.axis=1.5,mgp=c(2.7,1,0),     #   to 1.5 times the default size
     main="Height vs. Armspan",cex.main=1.8)
points(armspan[sex=="F"],height[sex=="F"],     # Overlays female points with a closed
       pch=16,cex=1.5)                              #   circle (pch=16)
regF <- lm(height[sex=="F"]~armspan[sex=="F"]) # LS regression for females only
regM <- lm(height[sex=="M"]~armspan[sex=="M"]) # LS regression for males only
abline(regF,lwd=2)                             # Regression line for females
abline(regM,lwd=2,lty=2)                       # Regression line (dashed) for males
legend(183,170,legend=c("Female","Male"),      # Places a legend at (180,170) to distinguish
       lty=c(1,2),pch=c(16,1),lwd=c(2,2),cex=1.6)   #   female and male line types
summary(regF)
summary(regM)

# Part (c): Prediction of Dr Zhang's height
# =========================================
Zhangpred <- regF$coef[1]+regF$coef[2]*165       # Dr. Zhang's predicted height
resid(regF)                                      # Residuals from the female regression

# Part (d): Plot, regression of height on foot length for both sexes
# ==================================================================
plot(footlen,height,pch=1,cex=1.5,             # Plots height vs. foot length with open circles
     xlab="Foot Length (cm)",ylab="Height (cm)",  #   (pch=1), axis labels, and text sizes set
     cex.lab=1.5,cex.axis=1.5,mgp=c(2.7,1,0),     #   to 1.5 times the default size
     main="Height vs. Foot Length",cex.main=1.8)
points(footlen[sex=="F"],height[sex=="F"],     # Overlays female points with a closed
       pch=16,cex=1.5)                              #   circle (pch=16)
regF <- lm(height[sex=="F"]~footlen[sex=="F"]) # LS regression for females only
regM <- lm(height[sex=="M"]~footlen[sex=="M"]) # LS regression for males only
abline(regF,lwd=2)                             # Regression line for females
abline(regM,lwd=2,lty=2)                       # Regression line for males
legend(26.2,168,legend=c("Female","Male"),     # Places a legend at (26.2,168) to distinguish
       lty=c(1,2),pch=c(16,1),lwd=c(2,2),cex=1.6)   #   female and male line types
summary(regF)
summary(regM)

# Part (e): Prediction of Dr Zhang's height
# =========================================
Zhangpred <- regF$coef[1]+regF$coef[2]*24        # Dr. Zhang's predicted height
resid(regF)                                      # Residuals from the male regression




# ==================== #
# Problem 2 #
# ==================== #
gesell <- read.csv("gesell.txt",header=T)  # Reads in Gesell data
summary(gesell)                                 # Data summary

# Part (a): Scatterplot of Gesell scores vs. ages
# ===============================================
age <- gesell$age                               # Ages of children
score <- gesell$score                           # Gesell scores
plot(age,score,cex.axis=1.5,cex.lab=1.8,pch=16, # Scatterplot of score vs.
     xlab="Age at First Word (months)",cex=1.5,    #   age with axis labels
     ylab="Gesell Adaptive Score",mgp=c(2.7,1,0),  #   and a title
     main="Gesell Adaptive Score vs. Age at First Word (Months)",cex.main=1.6)

# Part (b): Regression of Gesell score on age, plotted on scatterplot
# ===================================================================
reg <- lm(score~age)         # Least squares regression of score on age
summary(reg)                 # Regression summary
abline(reg$coef,lwd=2)       # Plots the regression line

# Part (c): Regression residuals
# ==============================
reg$resid                    # Vector of model residuals

# Part (e): Removes child #18 (age=42) and repeats previous analyses
# ==================================================================
age2 <- age[age<42]          # Keeps all ages except the one at age 42
score2 <- score[age<42]      # Keeps all scores except the one at age 42
reg2 <- lm(score2~age2)      # Regression without the high leverage value
summary(reg2)                # Regression summary
abline(reg2$coef,lwd=2,lty=8)# Plots the new regression line
text(30,105,"The dashed line is the regression",cex=1.3)
text(30,101,"line without case 18.",cex=1.3)
