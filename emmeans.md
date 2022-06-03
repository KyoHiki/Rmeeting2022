``` r
## Install libaries
library(emmeans)
library(ggplot2)

d <- read.table("Data_benthos.txt", sep = ",", header = T) #This data is only partially available on this Github page for various reasons. I appreciate your understanding.
names(d); head(d)
```
[1] "River"   "Insecta" "Ni"      "Zn"      "Cu"      "pH"      "TOC"     "Soil"    "Speed"  
   River Insecta    Ni     Zn    Cu  pH TOC   Soil Speed  
1 River1    0.66  0.80   2.83  0.54 7.6 2.9  Large  0.17  
2 River1    0.54 50.01 111.74 27.69 7.5 4.6  Large  0.38  
3 River1    0.82 44.17  41.16  7.90 7.5 4.5  Small  0.32  
4 River2    0.79  0.50  42.94 11.86 7.8 1.9 Middle  0.58  
5 River2    0.72 19.92   5.77  6.21 7.8 1.4 Middle  0.83  
6 River2    0.88  1.15   5.03 15.55 7.6 1.1 Middle  0.80  

``` r
d$Soil <- factor(d$Soil,
                 levels = c("Small", "Middle", "Large")
                 )

## Develop multiple linear regression model----
mod <- glm(Insecta ~ log10(Ni) + log10(Zn) + log10(Cu)
           + pH + TOC + Soil + Speed + River,
           family = gaussian(link = "logit"),
           data = d, na.action = na.fail)

summary(mod)

``` 
Call:  
glm(formula = Insecta ~ log10(Ni) + log10(Zn) + log10(Cu) + pH + 
    TOC + Soil + Speed + River, family = gaussian(link = "logit"), 
    data = d, na.action = na.fail)  
  
Deviance Residuals:   
[1]  0  0  0  0  0  0  0  0   0   
  
Coefficients: (2 not defined because of singularities)  
            Estimate Std. Error t value Pr(>|t|)  
(Intercept)  17.7556        NaN     NaN      NaN  
log10(Ni)    -1.0865        NaN     NaN      NaN  
log10(Zn)     1.1730        NaN     NaN      NaN  
log10(Cu)    -1.2242        NaN     NaN      NaN  
pH           -2.2928        NaN     NaN      NaN  
TOC          -0.1279        NaN     NaN      NaN  
SoilMiddle   -3.8079        NaN     NaN      NaN  
SoilLarge    -1.6001        NaN     NaN      NaN  
Speed         7.8911        NaN     NaN      NaN  
RiverRiver2       NA         NA      NA       NA  
RiverRiver3       NA         NA      NA       NA  
  
(Dispersion parameter for gaussian family taken to be NaN)  
     Null deviance: 1.0309e-01  on 8  degrees of freedom  
Residual deviance: 5.6823e-30  on 0  degrees of freedom  
AIC: -580.3  
  
Number of Fisher Scoring iterations: 1   
  
``` r
#Calculate estimated marginal means----
x.Ni <- seq(from = min(d$Ni), max(d$Ni), length = 100)
x.TOC <- seq(from = min(d$TOC), max(d$TOC), length = 100)

EMM.Ni <- confint(emmeans(mod,
                          "Ni",
                          at = list(Ni = x.Ni)
                          ),
                  parm,
                  level = 0.95
                  )

EMM.TOC <- confint(emmeans(mod,
                           "TOC",
                           at = list(TOC = x.TOC)
                           ),
                   parm,
                   level = 0.95
                   )

res <- data.frame(Ni = x.Ni,
                  TOC = x.TOC,
                  emm.Ni = exp(EMM.Ni$emmean)/(1 + exp(EMM.Ni$emmean)),
                  LCL.Ni = exp(EMM.Ni$lower.C)/(1 + exp(EMM.Ni$lower.C)),
                  UCL.Ni = exp(EMM.Ni$upper.CL)/(1 + exp(EMM.Ni$upper.CL)),
                  emm.TOC = exp(EMM.TOC$emmean)/(1 + exp(EMM.TOC$emmean)),
                  LCL.TOC = exp(EMM.TOC$lower.C)/(1 + exp(EMM.TOC$lower.C)),
                  UCL.TOC = exp(EMM.TOC$upper.CL)/(1 + exp(EMM.TOC$upper.CL))
                  )

rm(EMM.Ni, EMM.TOC)

summary(res)
```

```r
cairo_pdf(filename = "Fig_TOC.pdf")

par(mgp = c(1.0, 0.7, 0), xaxs = "i", yaxs = "i")

plot(0, 0,
     xlab = "", ylab = "",
     xaxt = "n", yaxt = "n",
     type = "n",
     xlim = c(0, 7), ylim = c(0, 1),
     frame.plot = F)

lines(res$TOC, res$emm.TOC, col = "black", lwd = 2.5, lty = 1)
lines(res$TOC, res$LCL.TOC, col = "black", lwd = 2.5, lty = 2)
lines(res$TOC, res$UCL.TOC, col = "black", lwd = 2.5, lty = 2)

box("plot", lty = 1)

mtext("Predicted diversity index value",
      side = 2, line = 2.7, cex = 1.2, col = "black", las = 0)
mtext("TOC concentration (mg/L)",
      side = 1, line = 2.1, cex = 1.2, col = "black", las = 0)

axis(1,
     at = seq(0, 7, 1), label = seq(0, 7, 1),
     tck = -0.015, cex.axis = 1.2, las = 1)
axis(2,
     at = seq(0, 1, 0.2), label = seq(0, 1, 0.2),
     tck = -0.015, cex.axis = 1.2, las = 1)

dev.off()
```

```r
#Plot EMM for Ni change----
cairo_pdf(filename = "Fig_Ni.pdf")

par(mgp = c(1.0, 0.7, 0), xaxs = "i", yaxs = "i")

plot(0, 0,
     xlab = "", ylab = "",
     xaxt = "n", yaxt = "n",
     type = "n",
     xlim = c(log10(0.1), log10(300)), ylim = c(0, 1),
     frame.plot = F)

lines(log10(res$Ni), res$emm.Ni, col = "black", lwd = 2.5, lty = 1)
lines(log10(res$Ni), res$LCL.Ni, col = "black", lwd = 2.5, lty = 2)
lines(log10(res$Ni), res$UCL.Ni, col = "black", lwd = 2.5, lty = 2)

box("plot", lty = 1)

mtext("Predicted diversity index value",
      side = 2, line = 2.7, cex = 1.2, col = "black", las = 0)
mtext("Ni concentration (mg/L)",
      side = 1, line = 2.1, cex = 1.2, col = "black", las = 0)

axis(1,
     at = seq(log10(0.1), log10(300), 1), label = c("0.1", "1", "10", "100"),
     tck = -0.015, cex.axis = 1.2, las = 1)
axis(2,
     at = seq(0, 1, 0.2), label = seq(0, 1, 0.2),
     tck = -0.015, cex.axis = 1.2, las = 1)

dev.off()
```

```r
#Heatmap of EMM for TOC and Ni changes----
EMM <- numeric(10000)
xx.Ni <- 10^seq(from = min(log10(d$Ni)), max(log10(d$Ni)), length = 100)

for (i in 1:100) {
  temp <- confint(emmeans(mod,
                          "Ni", "TOC",
                          at = list(Ni = x.Ni,
                                    TOC = x.TOC[i])
                          ),
                  parm,
                  level = 0.95
                  )
  
  EMM[((i - 1) * 100 + 1):((i - 1) * 100 + 100)] <- temp$emmean
  }

res2 <- data.frame(Ni = rep(log10(xx.Ni), times = 100),
                   TOC = rep(x.TOC, each = 100),
                   emm = exp(EMM)/(1 + exp(EMM))
                   )

rm(EMM, i)

summary(res2)
```

```r

p <- ggplot(res2, aes(y = Ni, x = TOC, z = emm))
p <- p + theme_minimal()
p <- p + theme(plot.margin = unit(rep(1.1, 4), "lines"),
               axis.title.x = element_text(size = 12 * 1.8, vjust = -1.5),
               axis.title.y = element_text(size = 12 * 1.8, vjust = 4.5),
               axis.text.x = element_text(size = 12 * 1.8, colour = "black"),
               axis.text.y = element_text(size = 12 * 1.8, colour = "black")
               )
p <- p + scale_x_continuous(breaks = seq(0, 7, 1), labels = seq(0, 7, 1))
p <- p + scale_y_continuous(breaks = seq(log10(0.1), log10(300), 1), labels = c("0.1", "1", "10", "100"))
p <- p + xlab("TOC concentration (mg/L)")
p <- p + ylab("Ni concentration (mg/L)")
p <- p + geom_raster(aes(fill = emm), interpolate = F)
p <- p + scale_fill_gradientn(colours = c(rgb(255/256, 149/256, 48/256),
                                          rgb(224/256, 255/256, 238/256),
                                          rgb(47/256, 128/256, 255/256)),
                              limits = c(0.2, 1)
                              )
p <- p + stat_contour(breaks =  c(0.3, 0.45, 0.60, 0.75, 0.90),
                      aes(y = Ni, x = TOC, z = emm),
                      color="black",
                      size = 0.6)
p <- p + theme(legend.title = element_text(size = 13 * 1.8),
               legend.text = element_text(size = 12 * 1.8),
               legend.spacing.y = unit(0.75 * 1.8, 'cm'),
               legend.text.align = 1,
               legend.key.size = unit(0.85 * 1.8, 'cm')
               )
p <- p + labs(fill = "Predicted\n DI value")

png(file = 'Heatmap.png', h = 8.27 * 100 * 1.8, w = 8.27 * 125 * 1.8, res = 200)

print(p)

dev.off()
```
