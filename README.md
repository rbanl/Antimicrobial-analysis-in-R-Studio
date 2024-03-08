# Antimicrobial-analysis-in-R-Studio
I compared the effectiveness of two brands of mouthwashes (Listerine and Colgate) against 2 different species of bacteria(Escherichia Coli and Staphylococcus Aureus). 


AIMS: This report discusses results of an experiment comparing the effectiveness of two brands of mouthwashes (Listerine and Colgate) against 2 different species of bacteria(Escherichia Coli and Staphylococcus Aureus). The report also discusses whether the efficacies of a mouthwash varies with the bacteria present.  
METHODS AND RESULTS : Aseptic techniques  were used along with a sensitivity disc to produce zones of inhibition , which were measured after overnight incubation. Data was analysed with a two-way ANOVA test , Listerine mouthwash had the larger mean diameter of inhibition. Staphylococcus Aureus proved to be more resistant to growth inhibition.

CONCLUSION: Listerine had more antimicrobial capacity, but it might not be better mouthwash depending on the customer’s needs and oral flora 


# Please see code below:

HYPOTHESIS	
Any one mouthwash will be more effective 	
 and the effectiveness of the mouthwash depends on the bacteria	
	
	
INTRODUCTION	
This analysis involves the data from the  antimicrobial.txt document	
It invlolves an investigation comparing the effect of 2 mouthwashes on 2 bacteria species	
Data contains diameter of zone of inhibition ,mouthwashes and bacteria used,	
in a 3-column format	
	
	
Data was organised into 3 columns	
data.frame':	60 obs. of  3 variables:
 $ diameter : int  7 8 9 3 3 6 5 6 7 5 ...	
 $ bacteria : Factor w/ 2 levels "ecoli","staph": 1 1 1 1 1 1 1 1 1 1 ...	
 $ mouthwash: Factor w/ 2 levels "colgate","listerine": 1 1 1 1 1 1 1 1 1 1 ...	
	
	
                           SETUP	
	
 data are in ../data	
dataAM <- read.table("antimicr.txt", header = TRUE)	
setwd("C:/Users/Robert Benjamin-Laing/Desktop/Y3869236/analysis")	
	
	
 packages	
 for figures	
library(ggplot2)	
	
	
 functions	
 summarySE calculates the mean, number of cases, se, sd and ci for	
 a response variable 	
source("../functions/summarySE.R")	
	
	
                       IMPORT AND TIDY DATA	
	
 data are in ../data	
dataAM <- read.table("antimicr.txt", header = TRUE)	
	
 check structure	
str(dataAM)	
data.frame':	60 obs. of  3 variables:
$ diameter : int  7 8 9 3 3 6 5 6 7 5 ...	
$ bacteria : Factor w/ 2 levels "ecoli","staph": 1 1 1 1 1 1 1 1 1 1 ...	
$ mouthwash: Factor w/ 2 levels "colgate","listerine": 1 1 1 1 1 1 1 1 1 1 ...	
 Treatment combination summary. Summary will be useful for figure	
	
 names of the different mouthwash	
levels(dataAM$mouthwash)	
Listerine,"Colgate"	
	
 names of the different bacteria	
levels(dataAM$bacteria)	
e.coli,"s.aureus"	
	
 EXPLORATORY ANALYSIS	
shapiro.test((dataAM$diameter))	
Test indicates data is non-parametric(W = 0.95992, p-value = 0.04661 )	
	
hist(dataAM$diameter)	
Histogram looks fairly parametric  	
	
plot(mod,which = 1)	
Variances are roughly equal	
	
	
	
dataAMsummary <- summarySE( data = dataAM ,measurevar = "diameter",groupvars = c("mouthwash","bacteria"))	
dataAMsummary	
  mouthwash bacteria  N  diameter       sd        se        ci	
1   colgate    ecoli 15  5.666667 1.877181 0.4846861 1.0395483	
2   colgate    staph 15  6.933333 2.282438 0.5893230 1.2639721	
3 listerine    ecoli 15  6.333333 1.496026 0.3862724 0.8284719	
4 listerine    staph 15 10.933333 2.218966 0.5729345 1.2288223	
	
Listerine has produced a greater average diameter	
Listerine produced a greater diameter in S.aureus	
Colgate produced a greater diameter in E.coli	
	
	
	
	
	
                     STASTICAL ANALYSIS                           	
 The assumptions of the two-way ANOVA test are 1. the residuals are	
 normally distributed and 2. the variances is homogenous.	
	
mod <- aov(data = dataAM,diameter ~ bacteria*mouthwash)	
summary(mod)	
 Df Sum Sq Mean Sq F value   Pr(>F)    	
bacteria            1 129.07  129.07   32.48 4.67e-07 ***	
 mouthwash           1  81.67   81.67   20.55 3.10e-05 ***	
bacteria:mouthwash  1  41.67   41.67   10.48  0.00202 ** 	
Residuals          56 222.53    3.97 	
	
	
 Data shows that there are significant differences between bacteria,  	
This means that diameters around bacteria are significantly different	
S.aureus had a larger diameter,(F = 32.48, d.f = 1, p- vlaue = 4.67e-07 )	
Data also indicates differences in mouthwash are significant	
Using different mouthwashes results in different diameters as they have	
different efficacies,Listerine is more effective	
(F = 20.55, d.f = 1, p-value = 3.10e-05 )	
There are interactions between mouthwash and bacteria	
Therefore the diameter  depends on which mouthwash 	
is being used with which bacteria(F = 10.48 ,d.f = 1 , p-value = 0.00202 )	
	
	
interaction.plot(dataAM$bacteria, dataAM$mouthwash, dataAM$diameter)	
The plot shows that e.coli show smaller zone of inhbition diameter	
,so are harder to kill	
With regards to mouthwashes ,Listerine is more effective	
 Difference  in zone of inhinbition diameters of 	
different bacteria is greater in Listerine	
 post-analysis tests	
	
	
	
                             FIGURES	
	
	
     FIGURE 1 	
dataAMsum <- aggregate(dataAM$diameter,	
                       by = list( bacteria = dataAM$bacteria, mouthwash = dataAM$mouthwash),	
                       FUN = function(x) c(mean = mean(x), sd = sd(x),	
                                           n = length(x))) 	
	
dataAMsum <- do.call(data.frame, dataAMsum)	
	
dataAMsum$se <- 2.837202 / 21.18962	
	
colnames(dataAMsum) <- c("bacteria", "mouthwash", "mean", "sd", "n", "se")	
	
dataAMsum$names <- c(paste(dataAMsum$bacteria, "bacteria /",	
                           dataAMsum$mouthwash, " mouthwash"))	
	
plotTop <- max(dataAMsum$mean) +	
  dataAMsum[dataAMsum$mean == max(dataAMsum$mean), 6] * 3	
	
barCenters <- barplot(height = dataAMsum$mean,	
                      names.arg = dataAMsum$names,	
                      beside = TRUE, las = 2,	
                      ylim = c(0, plotTop),	
                      cex.names = 0.5, xaxt = "n",	
                      main = "Data",	
                      ylab = "diameter", xlab = "bacteria"	
                      ,border = "black", axes = TRUE)	
	
 Specify the groupings. We use srt = 45 for a	
 45 degree string rotation	
text(x = barCenters, y = par("usr")[3] - 1, srt = 45,	
     adj = 1, labels = dataAMsum$names, xpd = TRUE)	
	
segments(barCenters, dataAMsum$mean - dataAMsum$se * 2, barCenters,	
         dataAMsum$mean + dataAMsum$se * 2, lwd = 1.5)	
	
arrows(barCenters, dataAMsum$mean - dataAMsum$se * 2, barCenters,	
       dataAMsum$mean + dataAMsum$se * 2, lwd = 1.5, angle = 90,	
       code = 3, length = 0.05)	
	
	
tapply(dataAMsum$mean, list(dataAMsum$bacteria, dataAMsum$mouthwash),	
       function(x) c(x = x))	
	
	
	
tabbedMeans <- tapply(dataAMsum$mean, list(dataAMsum$mouthwash,	
                                           dataAMsum$bacteria),	
                      function(x) c(x = x))	
	
tabbedSE <- tapply(dataAMsum$se, list(dataAMsum$bacteria,	
                                      dataAMsum$mouthwash),	
                   function(x) c(x = x))	
	
barCenters <- barplot(height = tabbedMeans,	
                      beside = TRUE, las = 1,	
                      ylim = c(0, plotTop),	
                      cex.names = 0.75,	
                      ylab = "diameter(mm)",	
                      xlab = "bacteria",	
                      border = "black", axes = TRUE,	
                      legend.text = TRUE,	
                      args.legend = list(title = "mouthwash", 	
                                         x = "top",	
                                         cex = .7))	
	
segments(barCenters, tabbedMeans - tabbedSE * 2, barCenters,	
         tabbedMeans + tabbedSE * 2, lwd = 2)	
	
arrows(barCenters, tabbedMeans - tabbedSE * 2, barCenters,	
       tabbedMeans + tabbedSE * 2, lwd = 1.5, angle = 90,	
       code = 3, length = 0.05)	
 Figure 1 The effect of mouthwash and bacteria species on diameter 	
	
	
        FIGURE 2 	
ggplot(data = dataAMsummary , aes(x = mouthwash, y = diameter, shape = bacteria))	
geom_point(data = dataAM, position = position_jitterdodge()) +	
  geom_errorbar(aes(ymin = diameter,	
                    ymax = diameter),	
                width = .5,	
                colour = "red",	
                size = 1,	
                position = position_dodge(0.9)) +	
  geom_err(aes(ymin = diameter - se,	
               ymax = diameter +se),	
           width = .7,	
           colour = "red",	
           position = position_dodge(0.9))	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	

	
	
	
	
![image](https://github.com/rbanl/Antimicrobial-analysis-in-R-Studio/assets/162132997/0e333381-e2fe-430f-877a-3bf89dac603f)



