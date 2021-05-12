
# =============================
# FIRST READ THE DATA INTO R
# =============================

baseball <- read.table(file=
                         "http://www.amstat.org/publications/jse/datasets/baseball.dat.txt",
                       header = F,
                       col.names=c("salary", "x1", "x2", "x3", "x4", "x5",
                                   "x6", "x7","x8", "x9", "x10", "x11", "x12", "x13",
                                   "x14", "x15", "x16", "ID"))
baseball$logsalary <- log(baseball$salary);
baseball <- baseball[, -c(1, 18)] # REMOVE salary AND ID
dim(baseball); head(baseball)
dat <- baseball

# THE NULL MODEL WITH INTERCEPT ONLY
fit.null <- lm(x1 ~ 1, data=dat);

# MODEL WITH ALL FIRST-ORDER INTERACTION
fit.whole <- lm(baseball, data=dat,  x=TRUE, y=TRUE)

n <- NROW(dat); n


# =================================
# STEPWISE PROCEDURE
# =================================

# -------------------
# STEPWISE SELECTION
# -------------------
fit.stepwise <-  step(fit.null, scope=list(lower=fit.null, upper=fit.whole),
                      direction="both", k = 2)  
fit.stepwise$anova  
summary(fit.stepwise)

# FITTING THE HOAGLIN AND VELLEMAN (HV, 1995) MODEL
fit <- fit.stepwise;
 
p <- length(coef(fit))-1

#install.packages("car");
library(car);

# =============================
# MODEL DIAGNOSTICS
# =============================

# =============================
# I. ASSUMPTION CHECKING
# =============================

# OBTAIN THE STUDENTIZED JACKKNIFE RESIDUALS 
r.jack <- rstudent(fit)

# -----------
# NORMALITY
# -----------

par(mfrow=c(1,2),mar=c(8,4,8,4)) 

# The fisrt plot: Histogram 
hist(r.jack, xlab="Jackknife Residual", col="green4",
     main="(a) Histogram") 

# The second plot: qq plot for studentized jackknife residuals
qqPlot(fit, pch=19, cex=.8, col="orange", main="(b) Q-Q Plot") 

# THE SHAPIRO-WILKS NORMALITY TEST
shapiro.test(r.jack) 

# ----------------------------------
# HOMOSCEDASTICITY (EQUAL VARIANCE)
# ----------------------------------

# Breusch-Pagan Test for Non-Constant Error Variance 
ncvTest(fit)

# Plot Absolute Jackknife Residuals vs. Fitted values 
par(mfrow=c(1,1),mar=c(4, 4, 4, 4)) 
spreadLevelPlot(fit, pch=20, cex=0.5, col="green4", 
                main="Absolute Jackknife Residuals vs. Fitted values")

# -----------------
# INDEPENDENCE
# -----------------

# Durbin-Watson Test for Autocorrelated Errors
durbinWatsonTest(fit)

# ------------
# LINEARITY
# ------------

# Evaluate Nonlinearity VIA THE component + residual plot (partial residual)
crPlots(fit, main="Partial Residual Plots")

# ============================
# II. OUTLIER DETECTION
# ============================

infl <- influence.measures(fit); 
infl.mat <- as.data.frame(infl$infmat)
infl.sum <- summary(influence.measures(fit)); 
infl.sum 

# Interactive Plot for Identifying Influential Points
# Press ESC to stop when you are done with identification
influencePlot(fit, id=list(method="noteworthy"), 
              col="blue", 
              main="Influence Plot", 
              sub="Circle size is proportial to Cook's d")

# ===============================
# III. CHECK ON MULTICOLINEARITY 
# ===============================

# CONDITION NUMBER WITHOUT INTERCEPT 
fitEdit <- update(fit, ~. - 1);
kappa(lm(fitEdit, x=TRUE)$x);

# COMPUTE VIF USING FUNCTION vif DIRECTLY
vif(fit) 

# ===============
# PREDICTION 
# ===============
 
player.new <- baseball[1, ]

# 95% PREDICTION INTERVAL FOR PREDICTING INDIVIDUAL RESPONSE
predict(fit, newdata = data.frame(player.new), interval = "prediction", se.fit = T, conf.level = 0.95)


