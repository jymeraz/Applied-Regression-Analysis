
# =============================
# FIRST READ THE DATA INTO R
# =============================

# FROM A LOCAL FILE
tampalms <- read.table("tampalms.dat", header=F,
                       col.names=c("appraised", "sale"))

# LOG TRANSFORMATION FOR RESPONSE AND PREDICTOR VARIABLES
x <- log(tampalms$appraised)
y <- log(tampalms$sale)


# ================================
# EXPLORTARY DATA ANALYSIS (EDA)
# ================================

# SCATTERPLOT
par(mfrow=c(1,1), mar=c(7, 5, 7, 5))
plot(x, y, main = "Scatterplot of Appraised Value vs. Sale Price", xlab="Appraised Value", ylab="Sale Price", col="green4", 
     pch=18, cex=.8, cex.lab=1.2, col.lab="gray15")

# CORRELATION 
cor(x, y)
cor.test(x, y, alternative = "two.sided", 
         method = "pearson", conf.level=.95)


# =======================================
# FITTING SIMPLE LINEAR REGRESSION (SLR)
# =======================================

fit <- lm(y~x);   
# TWO STANDARD COMPUTER OUTPUTS
summary(fit); anova(fit)

# ADDING THE LS-FITTED LINE
par(mfrow=c(1,1), mar=c(7, 5, 7, 5))
plot(x, y, main = "Scatterplot with LS Fitted Line", xlab="Appraised Value", ylab="Sale Price", col="green4", 
     pch=18, cex=0.9, cex.lab=1.2, col.lab="gray15")
abline(lsfit(x,y), col="red", lwd=2)


# ==========================
# MODEL DIAGNOSTICS
# ==========================

# FITTED Y HAT AND RESIDUALS
y.hat <- fitted(fit)
r <- resid(fit)
dat.sheet <- data.frame(x, y, fitted=y.hat, residual=r)
dat.sheet

# RESIDUAL VS FITTED
plot(y.hat, r, pch=18, col="grey25", main="Residual vs. Fitted", 
     xlab=expression(hat(y)))
abline(h=0, col="red")


# =============================
# Confidence and Prediction 
# ==============================

# CONFIDENCE BAND (CB) AND PREDICTION BAND (PB) 
plot.CB(x, y, prediction.band=TRUE, working.hotelling=TRUE, 
        confidence.level=0.95, ylab="Sale Price", xlab="Appraised Value")


# =======================
# FUNCTION plot.CB() 
# =======================

plot.CB <- function(x, y, prediction.band=TRUE, working.hotelling=TRUE, 
                    confidence.level=0.95, xlab="x", ylab="y", legend=TRUE){
  
  # ERROR CHECKING STEPS
  fit <- lm(y~x)
  x0 <- min(x)-sd(x); x1 <- max(x) + sd(x); 
  y0 <- min(y)-2*sd(y); y1 <- max(y) + 2*sd(y)
  new <- data.frame(x= seq(x0, x1, length=100)) 
  CI95 <- predict(fit, newdata=new, se.fit=TRUE,interval="confidence", level=confidence.level); 
  
  par(mar=rep(4,4), mfrow=c(1, 1))
  plot(c(x0, x1), c(y0, y1), type="n", ylab=ylab, xlab=xlab, 
       main="LS Fitted Line with Confidence/Prediction Bands", cex.lab=1.2)
  polygon(c(new$x, rev(new$x)), c(CI95$fit[,2], rev(CI95$fit[,3])),
          col = "burlywood1", border = NA)
  points(x, y, pch=20, col="green4")
  abline(lsfit(x,y), lwd=2) 
  abline(v=min(x), col="gray35", lty=2)
  abline(v=max(x), col="gray35", lty=2)
  lines(new$x, CI95$fit[,2], lty=2, col="red", lwd=1.5)
  lines(new$x, CI95$fit[,3], lty=2, col="red", lwd=1.5)
  
  # PREDICTION BAND
  if (prediction.band) {
    PI95 <- predict(fit, newdata=new, se.fit=TRUE,interval="prediction", 
                    level=confidence.level)
    lines(new$x, PI95$fit[,2],lty=1, col="blue", lwd=1.5)
    lines(new$x, PI95$fit[,3],lty=1, col="blue", lwd=1.5)
  } 
  
  # WORKING-HOTELLING JOINT CONFIDENCE BAND
  if (working.hotelling) {
    n <- length(x)
    W.Hoteling <-  sqrt(2 * qf(confidence.level, 2, n-2))
    LB <- CI95$fit[, 1] - W.Hoteling*CI95$se.fit
    UB <- CI95$fit[, 1] + W.Hoteling*CI95$se.fit
    lines(new$x, LB,lty=1, col="red", lwd=1.2)
    lines(new$x, UB,lty=1, col="red", lwd=1.2)
  }
  if (prediction.band && working.hotelling && legend){
    legend(x0, y1, c("LS fitted line", "naive CB", "PB", "Working-Hotelling CB"), 
           lty=c(1, 2, 1, 1), col=c("black", "red", "blue", "red"), lwd=1, cex=0.8)
  }
}

#
