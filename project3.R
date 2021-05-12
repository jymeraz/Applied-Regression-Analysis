########################
# INPUT THE DATA 
########################

#?iris
dat <- iris[, c(1, 2)]
head(dat); dim(dat)

x.groups <- c("setosa", "versicolor", "virginica")
RnD <- rep(x.groups , c(50, 50, 50))
# MAKE SURE THAT THE LEVELS OF RnD ARE ORDERED IN A DESIGNATED WAY
RnD <- factor(RnD, levels=x.groups, ordered=TRUE)
dat <- data.frame(dat=dat, RnD=RnD);

########################
# ONE-WAY ANOVA 
########################

# EDA: PARALLEL BOXPLOT PLUS JITTER DOT PLOTS
# --------------------------------------------
par(mfrow=c(1,1), mar=rep(5,4))
boxplot(dat.Sepal.Length ~ RnD,  data=dat, boxwex=0.3,
        col="lightgreen", notch=F, 
        ylab="Sepal Length", xlab="Iris Species")

# REGRESSION WITH DUMMY VARIABLES
# --------------------------------
# THE FIRST GROUP IS THE BASELINE GROUP
contrasts(dat$RnD) <- contr.treatment(x.groups, base=1)
fitLength <- lm(dat.Sepal.Length~RnD, data=dat)
summary(fitLength); anova(fitLength)

# SCATTER PLOT
y <- iris[, c(1)] 
x <- iris[, c(2)]

regulator <- rep(c("Set", "Ver", "Vir"), c(50, 50, 50))
dat2 <- data.frame(x, y, regulator)


# PLOT OF ANCOVA Data
par(bg="white", mfrow=c(1, 1), mar=rep(5, 4))
plot(range(x), range(y), type="n", ylab="Length", xlab="Width",
     main="Plot of Iris Data")
level <- sort(unique(regulator))

for (i in 1:length(level)){
  temp <- dat2[dat2$regulator==level[i],]
  points(temp$x,temp$y, pch=16, col=i)
  abline(lsfit(x=temp$x, y=temp$y), lty=i, col=i)
}

########################
# ANCOVA 
########################

# REGRESSION WITH DUMMY VARIABLES
# --------------------------------
# INTERACTION MODEL
fit1 <- lm(y ~ x*factor(regulator), data=dat, x=T)

# ANCOVA MODEL
fit2 <- lm(y ~ x + factor(regulator), data=dat, x=T) 
summary(fit2); anova(fit2)

fit4 <- lm(y ~ x, data=dat, x=T) 

# ANCOVA
anova(fit2, fit4)
# CHECK THE ASSUMPTION OF NO INTERACTION
anova(fit1, fit2)

########################
# TWO-WAY ANOVA 
########################

# ?ToothGrowth
dat <- ToothGrowth
dim(dat); head(dat)

len <- dat[, c(1)]
supp <- rep(c("VC", "OJ"), c(30, 30))
dose <- c(rep(c("0.5", "1.0", "2.0"), c(10, 10, 10)), 
          rep(c("0.5", "1.0", "2.0"), c(10, 10, 10)))

# MAKE SURE THE LEVELS OF bone ARE ORDERED 
dose <- factor(dose, levels=c("0.5", "1.0", "2.0"), ordered=TRUE)
dat <- data.frame(dat=dat, supp=supp, dose=dose)

# TABLE OF CELL MEANS
aggregate(len, by=list(dose, supp), FUN=mean)

# INTERACTION PLOT
par(bg="white", mfrow=c(1, 1), mar=rep(4, 7))
interaction.plot(dose, supp, len, col = 2:3, lty=1,
                  leg.bty = "o", leg.bg="gray80", lwd=2, main="dose vs mean of len")

# REGRESSION WITH DUMMY VARIABLES
# ---------------------------------

fit1 <- lm(len~factor(supp)*factor(dose), x=T, data=dat)
fit2 <- lm(len~factor(supp) + factor(dose), x=T, data=dat)
fit3 <- lm(len~factor(supp), x=T, data=dat)
fit4 <- lm(len~factor(dose), x=T, data=dat)

# TEST OF INTERACTION
anova(fit1, fit2)
# TEST OF MAIN EFFECT OF SUPP
anova(fit2, fit4)
# TEST OF MAIN EFFECT OF DOSE
anova(fit2, fit3)






