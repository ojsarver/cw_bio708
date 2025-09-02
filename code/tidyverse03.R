library(tidyverse)

set.seed(123)

iris_sub <- as_tibble(iris) %>% 
  group_by(Species) %>% 
  sample_n(3) %>% 
  ungroup()

print(iris_sub)


# refresher ---------------------------------------------------------------

#exercise 1
##filter iris_sub to those with Sepal.Length greater than 5, assign to df_g5

df_g5<-filter(iris_sub, Sepal.Length > 5)
df_g5

#exercise 2 
#select columns of Sepal.Length and Petal.width from iris_sub assign to df_sp

df_sp<-select(iris_sub, c(Sepal.Length, Petal.Width))
df_sp

#exercise 3
##arrange rows by Petal.width in iris_sub assign to df_arrange

df_arrange<-arrange(iris_sub,Petal.Width)
df_arrange

#exercise 4
#Do exercises 1-3 at once with pipes, assign to df_master

df_master <- iris_sub %>%
  filter(Sepal.Length > 5) %>%
  select(c(Sepal.Length, Petal.Width)) %>%
  arrange(Petal.Width)

#extra
#calculate mean petal width for each species separately using 
#group by() and summarize ()

df_means<-iris_sub %>% 
  group_by(Species) %>% 
  summarize(mu_pw = mean(Petal.Width))
