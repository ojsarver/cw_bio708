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


# ggplot ------------------------------------------------------------------
#basic syntax
##w/o pipe
g_example<-ggplot(data=iris,
       mapping=aes(x=Sepal.Length,
                   y=Sepal.Width)) + geom_point()
##W/ pipe
g_example<-iris %>%
  ggplot(mapping=aes(x=Sepal.Length,
                     y=Sepal.Width)) + 
           geom_point()

#color

iris %>%
  ggplot(mapping=aes(x=Sepal.Length,
                     y=Sepal.Width, color=Species)) + 
  geom_point()

#color by category, aka species, has to be in the aes parentheses otherwise wont work

iris %>%
  ggplot(mapping=aes(x=Sepal.Length,
                     y=Sepal.Width), color=Species) + 
  geom_point()

#to give uniform color put exact color in geom function
g_scol<-iris %>%
  ggplot(mapping=aes(x=Sepal.Length,
                     y=Sepal.Width)) + 
  geom_point(color="salmon")

##line plot
# sample data
df0 <- tibble(x = rep(1:50, 3),
              y = x * 2)

# basic plot
df0 %>% 
  ggplot(aes(x = x,
             y = y)) +
  geom_line()

##histogram
# basic plot; bins = 30 by default
iris %>% 
  ggplot(aes(x = Sepal.Length)) +
  geom_histogram()

#exerise 5
#create histogram colored by Species
iris %>%
  ggplot(mapping=aes(x=Sepal.Length,
                     color=Species)) + 
  geom_histogram()
#changes color of the borders, to fill borders use fill instead of color
iris %>%
  ggplot(mapping=aes(x=Sepal.Length,
                     fill=Species)) + 
  geom_histogram()

##boxplot
# basic plot
iris %>% 
  ggplot(aes(x = Species,
             y = Sepal.Length)) +
  geom_boxplot()

#exercise 6 
#create boxplot filled by Species

iris %>% 
  ggplot(aes(x = Species,
             y = Sepal.Length,
             fill=Species)) +
  geom_boxplot()

#use multiple layers

iris %>% 
  ggplot(aes(x = Species,
             y = Sepal.Length,
             fill=Species)) +
  geom_boxplot() +
  geom_point()

iris %>% 
  ggplot(aes(x = Species,
             y = Sepal.Length,
             fill=Species)) +
  geom_boxplot() +
  geom_jitter(alpha=.2)
#alpha controls transparency of points 1=full color 0=invisible