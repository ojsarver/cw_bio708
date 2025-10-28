pacman::p_load(tidyverse,
               patchwork,
               here)


# ttest vs lm -------------------------------------------------------------

df_fl<-read_csv(here("data_raw/data_fish_length.csv"))

m<-lm(length~lake,
    data=df_fl)

#group means for lake
v_mu<-df_fl%>%
  group_by(lake)%>%
  summarize(mu_l=mean(length))%>%
  pull(mu_l)

#mean lake b - mean lake a
v_mu[2]-v_mu[1]

#mean length lake b
sum(coef(m))

#look into lm result
summary(m)

#ttest
a<-df_fl%>%
  filter(lake=="a")%>%
  pull(length)

b<-df_fl%>%
  filter(lake=="b")%>%
  pull(length)

t.test(x=a,
       y=b,
       var.equal=TRUE) 

#ANOVA
df_anova<-read_csv(here("data_raw/data_fish_length_anova.csv"))

m1<-lm(length~lake,
       data=df_anova)

v_mu_anova<-df_anova%>%
  group_by(lake)%>%
  summarize(mu_l=mean(length))%>%
  pull(mu_l)

#intercept
v_mu_anova[1]

#lake b effect
v_mu_anova[2]-v_mu_anova[1]

#lake c effect
v_mu_anova[3]-v_mu_anova[1]

#aov comparison
aov(length~lake,
    data=df_anova)


# ancova ------------------------------------------------------------------


m2<-lm(Sepal.Length ~ Sepal.Width+Species,
   data=iris)

summary(m2)

#visualization
m_iris<-lm(Petal.Length~Petal.Width+Species,
       data=iris)

df_pred<-tibble(Petal.Width=rep(seq(min(iris$Petal.Width),
                                    max(iris$Petal.Width),
                                    length=100),
                                times=3),
                Species=rep(unique(iris$Species),
                            each=100))

y_pred<-predict(m_iris,
        newdata=df_pred)

df_pred<-df_pred%>%
  mutate(y_pred=y_pred)

iris %>% 
  ggplot(aes(x = Petal.Width,
             y = Petal.Length,
             color = Species)) +
  geom_point(alpha = 0.5) +
  geom_line(data = df_pred,
            aes(y = y_pred))


# interaction -------------------------------------------------------------


m_int<-lm(Petal.Length~Petal.Width*Species,
   data=iris)
#OR
#lm(Petal.Length~Petal.Width+Species+Petal.Width:Species,
   #data=iris)

summary(m_int)
