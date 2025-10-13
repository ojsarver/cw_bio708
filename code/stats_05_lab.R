
# exercise 1 --------------------------------------------------------------
library(pacman)
pacman::p_load(tidyverse,
               patchwork,
               here,
               pwr)

#use Plant growth data set to create a figure
PlantGrowth%>%
  ggplot(aes(x=group,
             y=weight))+
  geom_violin(draw_quantiles=.5,
              alpha=.2)+
  geom_jitter(width=.1)+
  theme_bw()

#Conduct an ANOVA to compare weight amongst the different groups

pg_aov<-aov(weight~group,
       data=PlantGrowth)
summary(pg_aov)


# exercise 2 --------------------------------------------------------------

#large effect size (Cohen’s f = 0.5), 80% power, 0.05 significance level
#use pwr::pwr.anova.test() function in R

pwr::pwr.anova.test(k=3,f=.5,sig.level=.05,power=.8)

