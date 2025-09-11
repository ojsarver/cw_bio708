library(tidyverse)



# exercise 1 --------------------------------------------------------------


z<-exp(rnorm(n = 1000, mean = 0, sd = 0.1))

median(z)
mean(z)
prod(z)^ (1/length(z))

df_z<-tibble(z)

z_hist<-df_z %>%
ggplot(aes(x=z))+
  geom_histogram()

z_hist+geom_vline(xintercept=median(z),color="magenta")+
  geom_vline(xintercept=mean(z),color="pink")+
  geom_vline(xintercept=prod(z)^ (1/length(z)),color="purple")

z_rev<-z + max(z) + 0.1

median(z_rev)
mean(z_rev)
prod(z_rev)^ (1/length(z_rev))

df_zrev<-tibble(z_rev)

df_zrev

zrev_hist<-df_zrev %>%
  ggplot(aes(x=z_rev))+
  geom_histogram()

zrev_hist+geom_vline(xintercept=median(z_rev),color="lightgreen")+
  geom_vline(xintercept=mean(z_rev),color="lightblue")+
  geom_vline(xintercept=prod(z_rev)^ (1/length(z_rev)),color="darkblue")


# exercise 2 --------------------------------------------------------------

w <- rnorm(100, mean = 10, sd = 1)
head(w) # show first 10 elements in w

m<-1000*w

s2_w<-sum((w-mean(w))^2)/length(w)
s_w<-sqrt(s2_w)

s2_m<-sum((m-mean(m))^2)/length(m)
s_m<-sqrt(s2_m)

med_w<-median(abs(w-median(w)))
med_m<-median(abs(m-median(m)))

cv_w<-s_w/mean(w)
cv_m<-s_m/mean(m)

madr_w<-med_w/median(w)
madr_m<-med_m/median(m)
