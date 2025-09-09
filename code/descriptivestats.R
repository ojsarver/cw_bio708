library(tidyverse)


# central tendency --------------------------------------------------------

# arithmetic mean
v_x<-rnorm(10)

(sum(v_x))/(length(v_x))
mu_x<-(sum(v_x))/(length(v_x))

#geometric mean

v_y<-runif(10,min=10,max=20)
prod(v_y)^ (1/length(v_y))

exp(mean(log(v_y)))

#median
v_z<-runif(9,min=10,max=20)
v_z<-sort(v_z)

index<-(length(v_z)+1)/2
v_z[index]

median(v_z)



# variance measures -------------------------------------------------------

#variance
v_a<-rnorm(100)

(sum((v_a-mean(v_a))^2))/length(v_a)
s2<-(sum((v_a-mean(v_a))^2))/length(v_a)

#standard deviation
s<-sqrt(s2)

#interquartile range
a_1<-quantile(v_a,probs=.25)
a_h<-quantile(v_a,probs=.75)
(iqr<-abs(a_h-a_1))

#median absolute deviation MAD

median(abs(v_a-median(v_a)))

#coefficient of variation (CV)

v_b<-runif(100,min=10,max=20)
s2<-(sum((v_b-mean(v_b))^2))/length(v_b)
s<-sqrt(s2)

cv<-s/(mean(v_b))

#MAD/median

MAD<-(median(abs(v_b-median(v_b))))
med<-(median(v_b))
mad2med<-MAD/med
