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

### join() function

# matching by a single column
## left join by "Species": one to one
df1 <- tibble(Species = c("A", "B", "C"),
              x = c(1, 2, 3))

df2 <- tibble(Species = c("A", "B", "C"),
              y = c(4, 5, 6))
###left_join() merge data frames based on column(s)
df12<-left_join(x = df1,
          y = df2,
          by = "Species")

#what happens if df2 doesn't contain species B?

df_minus_B<-tibble("Species"=c("A","C"),
                   y=c(4,6))
            
left_join(x = df1,
          y = df_minus_B,
          by = "Species") 

left_join(x = df_minus_B,
          y = df1,
          by = "Species") 

#additional resources

