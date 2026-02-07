# ========== #
# Homework 2 #
# ========== #

# ==================== #
# Problem 1 #
# ==================== #
recov <- read.csv("recovery.txt",       # Reads in the biological recovery data
                  header=T)
attach(recov)                                # Promotes the biological recovery data

# Part (a): Scatterplot of Biological Recovery vs. Time
# =====================================================
plot(time,recovery,pch=16,cex.axis=1.5,      # Plots the recovery percentages vs. time
     xlab="Time (minutes)",mgp=c(2.7,1,0),      #   with x- and y-axis labels and a title
     ylab="Biological Recovery (percent)",
     main="Biological Recovery vs. Time",
     cex.lab=1.6,cex.main=1.8,cex=1.5)

# Part (b): Scatterplot of Log Biological Recovery vs. Time
# =========================================================
logrec <- log(recov$recovery)                # Computes the log recovery percentages
plot(time,logrec,pch=16,cex.axis=1.5,        # Plots the log recovery percentages vs.
     xlab="Time (minutes)",mgp=c(2.7,1,0),      #   time with axis labels and a title
     ylab="Log Biological Recovery (percent)",
     main="Log Biological Recovery vs. Time",
     cex.lab=1.6,cex.main=1.8,cex=1.5)

# Part (c): Regression of Log Recovery on Time
# ============================================
reg.out <- lm(logrec~time)                   # Regression of log recovery on time
summary(reg.out)                             # Prints regression summary

# Part (d): ANOVA Table
# =====================
anova(reg.out)                               # Prints the ANOVA table



# ========= #
# Problem 2 #
# ========= #
city <- read.csv("citysize.txt",header=T) # Reads in the citysize data
attach(city)                              # Allows to access variable names 
                                          #   without calling the data frame
 
# Part (a): Scatterplot of Expenditure vs. City Size
# =====================================================
plot(size,expenditure,pch=16,cex.axis=1.5,      # Plots expenditure vs. city size
     xlab="City Size (1000s of People)",        # with x- and y-axis labels and a title
     ylab="Expenditure ($1000)", mgp=c(2.7,1,0),
     main="Expenditure vs. City Size",
     cex.lab=1.6,cex.main=1.8,cex=1.5)
lines(lowess(size,expenditure,f=0.6),lwd=2,     # Overlays lowess fit with f=0.6
      col=2,lty=8)


# Part (b): Compute Pearson's correlation
# ========================================
cor(size,expenditure)   # Pearson's correlation


# Part (c): Regression of Expenditure on City Size, plotted on scatterplot
# ==========================================================================
out <- lm(expenditure~size)  # Least squares regression of expenditure on city size
summary(out)                 # Regression summary
abline(out$coef,lwd=2)       # Plots the regression line


# Part (d): Residual Plot
# ========================
resid <- out$resid           # Puts the residuals in a vector called "resid"
plot(size,resid,pch=16,cex.axis=1.5,      # Plots the residuals vs. time
     xlab="City Size (1000s of People)",  # with x- and y-axis labels and a title
     ylab="Residuals",mgp=c(2.7,1,0),
     main="Residual Plot",
     cex.lab=1.6,cex.main=1.8,cex=1.5)
abline(h=0, lwd=2, lty=3)                 # Adds horizontal reference line at 0
lines(lowess(size,resid,f=0.5),lwd=2,     # Overlays lowess fit with f=0.6
      col=2,lty=8)

