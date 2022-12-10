PM.data <- subset(data, data$Dim1 == "Total")
PM.data <- PM.data[order(PM.data$SpatialDimValueCode),]
PM.data.urban.rural <- subset(data, data$Dim1 != "Total")
install.packages('lme4')
library(lme4)
install.packages('dplyr')
library(dplyr)

#Asthma

asthma.sex <- data.frame(Asthma_Sex)
colnames(asthma.sex)
colnames(asthma.sex)[3] = "SpatialDimValueCode"
colnames(asthma.sex)[5] = "Period"
asthma <- merge(PM.data, asthma.sex, by=c("Period","SpatialDimValueCode"))
GDP <- data.frame(GDP_Per_Capita)
colnames(GDP)[2] = "SpatialDimValueCode"
total.asthma <- merge(asthma, GDP, by=c("Period","SpatialDimValueCode"))

#Rheumatic Heart Disease

rheum.sex <- data.frame(Rheum_Sex)
colnames(rheum.sex)
colnames(rheum.sex)[3] = "SpatialDimValueCode"
colnames(rheum.sex)[5] = "Period"
rheum <- merge(PM.data, rheum.sex, by=c("Period","SpatialDimValueCode"))
total.rheum <- merge(rheum, GDP, by=c("Period","SpatialDimValueCode"))

#Hypertensive Heart Disease

hyper.sex <- data.frame(Hyper_Sex)
colnames(hyper.sex)
colnames(hyper.sex)[3] = "SpatialDimValueCode"
colnames(hyper.sex)[5] = "Period"
hyper <- merge(PM.data, hyper.sex, by=c("Period","SpatialDimValueCode"))
total.hyper <- merge(hyper, GDP, by=c("Period","SpatialDimValueCode"))

#control for sex, time trends and GDP per capita



#control for sex, time trends and GDP per capita

model.asthma <- lmer(Age.standardized.death.rate.per.100.000.standard.population
                     ~ Value + GDP.per.capita + Period + Sex + (0 + GDP.per.capita|Value)
                        + (0 + Sex|Value) + (0 + Period|Value), total.asthma)
summary(model.asthma)
anova(model.asthma)
coef.asthma <- data.frame(coef(summary(model.asthma)))
coef.asthma$p.value <- 2 * (1 - pnorm(abs(coef.asthma$t.value)))
coef.asthma

model.rheum <- lmer(Age.standardized.death.rate.per.100.000.standard.population
                    ~ Value + GDP.per.capita + Period + Sex + (0 + GDP.per.capita|Value)
                    + (0 + Sex|Value) + (0 + Period|Value), total.rheum)
summary(model.rheum)
anova(model.rheum)
coef.rheum <- data.frame(coef(summary(model.rheum)))
coef.rheum$p.value <- 2 * (1 - pnorm(abs(coef.rheum$t.value)))
coef.rheum

model.hyper <- lmer(Age.standardized.death.rate.per.100.000.standard.population
                    ~ Value + GDP.per.capita + Period + Sex + (0 + GDP.per.capita|Value)
                    + (0 + Sex|Value) + (0 + Period|Value), total.hyper)
summary(model.hyper)
anova(model.hyper)
coef.hyper <- data.frame(coef(summary(model.hyper)))
coef.hyper$p.value <- 2 * (1 - pnorm(abs(coef.hyper$t.value)))
coef.hyper

library(gridExtra)
grid.table(coef.asthma)
grid.table(coef.rheum)

pdf("boxcox.pdf")
boxcox(hyper0, lambda = seq(-1, 1, length = 10))
dev.off()

pdf("scatterplotmatrix.asthma.pdf")
scatterplotMatrix(~ Death.rate.transform + Value +
                    GDP.per.capita + Period , total.asthma.pos,
                  spread=FALSE, smoother.args=list(lty=2))
dev.off()


pdf("scatterplotmatrix.rheum.pdf")
scatterplotMatrix(~ Death.rate.transform + Value +
                                         GDP.per.capita + Period , total.rheum.pos,
                                       spread=FALSE, smoother.args=list(lty=2))
dev.off()

pdf("scatterplotmatrix.hyper.pdf")
scatterplotMatrix(~ Death.rate.transform + Value +
                    GDP.per.capita + Period , total.hyper.pos,
                  spread=FALSE, smoother.args=list(lty=2),)
dev.off()

pdf("influenceplot.pdf")
par(mfrow=c(3,1))
influencePlot(wls.asthma, id=list(method="identify") )
influencePlot(wls.rheum, id=list(method="identify") )
influencePlot(wls.hyper, id=list(method="identify") )
dev.off()

