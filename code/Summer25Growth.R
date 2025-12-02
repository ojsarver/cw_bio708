
library(pacman)
pacman::p_load(tidyverse,
               patchwork,
               here,
               ggplot2)

df_growth <- read_csv(here("data_raw/summergrowth_noV_csv.csv"))

df_growth<-na.omit(df_growth)

df_growth$GrowthStage <- factor(df_growth$GrowthStage, levels=c("VE","VC","V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","V11","V12","V13","V14","V15"))

df_c<-df_growth%>%
  filter(Treatment=="C")

df_mp<-df_growth%>%
  filter(Treatment=="MP")

#elise code

df_growth %>%
  
  ggplot(aes(x=as.numeric(as.factor(GrowthStage)), y=Weeks_to, color = Treatment))+
  
  geom_point()+
  
  geom_smooth(method =lm, formula = y~x)+
  labs(x="Growth Stage",
       y="Time to reach designated growth stage")
  
  
ggplot(data=df_growth,aes(x=GrowthStage,
             y=Weeks_to))+
  geom_point(data=df_c,aes(x=GrowthStage,
                           y=Weeks_to),color="pink")+
  geom_point(data=df_mp,aes(x=GrowthStage,
                          y=Weeks_to),color="lightblue")

#if we add geom_line()

ggplot(data=df_growth,aes(x=GrowthStage,
                          y=Weeks_to))+
  geom_point(data=df_c,aes(x=GrowthStage,
                           y=Weeks_to),color="pink")+
  geom_line(data=df_c,aes(x=GrowthStage,
                           y=Weeks_to),color="pink4")+
  geom_point(data=df_mp,aes(x=GrowthStage,
                            y=Weeks_to),color="lightblue")+
  geom_line(data=df_mp,aes(x=GrowthStage,
                            y=Weeks_to),color="lightblue4")
