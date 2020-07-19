## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}
######################
library(ggplot2)
bandData <- read.csv("data2.csv", header=TRUE, sep=",");
head(bandData)
bandSummary<- summarySE(bandData, measurevar="value", groupvars=c("type", "band"))

# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(0.1) # move them .05 to the left and right

ggplot(bandSummary, aes(x=band, y=value,  color=type, group=type)) + 
  geom_line(aes(color=type), position=pd, size=1.5) +
  scale_color_manual(values=c("#FFB75D", "#D63000","#0B4A8B", "#2FB045")) +
  geom_point(position=pd, size=3, shape=21, fill="white") + # 21 is filled circle
  geom_errorbar(aes(ymin=value-ci, ymax=value+ci),  
                #color = "black",#
                width=.5, position=pd) +
  xlab("Band") +
  ylab("Surface Reflectance x 10^4") +
  ggtitle("Surface reflectance of four surface types with confidence intervals") +
  expand_limits(y=0) +                        # Expand y range
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.justification=c(1,0),
        legend.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.position=c(0.25,0.7))               # Position legend in bottom right


