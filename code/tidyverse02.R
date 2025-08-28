library(tidyverse)

set.seed(123)

iris_sub <- as_tibble(iris) %>% 
  group_by(Species) %>% 
  sample_n(3) %>% 
  ungroup()

print(iris_sub)

#group operations
iris_sub %>% 
  group_by(Species)
#now says species 3 when u print iris because it grouped by the 3 distinct species

#useful because now you can calculate new values for each group
iris_sub %>% 
  group_by(Species) %>% 
  summarize(mu_sl = mean(Sepal.Length))
#gives the mean sepal length for each group, in this case species
iris_sub %>% 
  group_by(Species) %>% 
  summarize(mu_sl = mean(Sepal.Length),
            sum_sl = sum(Sepal.Length))
#can calculate more summary stats in one code, the one above 
#calculated mean and added sepal length

#summarize() function returns a summary table w/ each group represented by a
#single row. Mutate() retains the original table and adds summary columns
# grouping by "Species", then take means "Speal.Length" for each species

iris_sub %>% 
  group_by(Species) %>% 
  mutate(mu_sl = mean(Sepal.Length)) %>% 
  ungroup()

#ungroup () function afterwards to prevent errors/miscalculations

iris_w <- iris %>% 
  mutate(id = rep(1:50, 3)) %>% # add an ID column
  select(id, Sepal.Length, Species) %>% 
  pivot_wider(id_cols = "id", # unique row ID based on
              values_from = "Sepal.Length", # values in each cell from
              names_from = "Species") # new column names from

print(iris_w)

#pivot wider () reshape a data frame to a wide format

iris_W<-iris_sub %>% 
  mutate(id = rep(1:3, 3)) %>% # add an ID column
  select(id, Sepal.Length, Species) %>%
  pivot_wider(id_cols = "id", # unique row ID based on
              values_from = "Sepal.Length", # values in each cell from
              names_from = "Species") # new column names from

rm(iris_w)

#pivot_longer () to reshape to long format
iris_l<-iris_W %>%
  pivot_longer(cols=c("setosa", "versicolor","virginica"),
               names_to="Species",
               values_to="Sepal.Length")
