
library(pacman)
pacman::p_load(tidyverse,
               patchwork,
               here,
               ggplot2)

df_growth <- read_csv(here("data_raw/summergrowth.csv"))

df_growth<-na.omit(df_growth)

df_growth$GrowthStage <- factor(df_growth$GrowthStage, levels=c("VE","VC","V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","V11","V12","V13","V14","V15"))

df_c<-df_growth%>%
  filter(Treatment=="C")

df_mp<-df_growth%>%
  filter(Treatment=="MP")
  
  
ggplot(data=df_growth,aes(x=GrowthStage,
             y=Weeks_to))+
  geom_point(data=df_c,aes(x=GrowthStage,
                           y=Weeks_to),color="pink")+
  geom_point(data=df_mp,aes(x=GrowthStage,
                          y=Weeks_to),color="lightblue")
