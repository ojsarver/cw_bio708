#sampling lab

library(tidyverse)
library(patchwork)

#Obtain 100 sub-datasets with 50 and 100 measures each, 
#and draw histograms of sample means and unbiased variances (use var())

df_h0<-read_csv(here::here("data_raw/data_plant_height.csv"))

mu<-mean(df_h0$height)
sigma2<-sum((df_h0$height-mu)^2)/nrow(df_h0)

#subdata set 1?

mu50_i <- var50_ub_i <- NULL # create empty objects

# repeat the work in {} from i = 1 to i = 100, i designates the dataset you are creating
for (i in 1:100) {
  
  df_i <- df_h0 %>% 
    sample_n(size = 50) # random samples of 50 individuals
  
  # save mean for sample set i
  mu50_i[i] <- mean(df_i$height)
  
  # save variance for sample set i
  var50_ub_i[i] <- var(df_i$height) 
}

#subdata set 2 100

mu100_i <- var100_ub_i <- NULL # create empty objects

# repeat the work in {} from i = 1 to i = 100
for (i in 1:100) {
  
  df_i <- df_h0 %>% 
    sample_n(size = 100) # random samples of 100 individuals
  
  # save mean for sample set i
  mu100_i[i] <- mean(df_i$height)
  
  # save variance for sample set i
  var100_ub_i[i] <- var(df_i$height) 
}

#histograms

df_sample <- tibble(mu50 = mu50_i,
                    var50= var50_ub_i,
                    mu100=mu100_i,
                    var100=var100_ub_i)
#sample 50
g50_mu<-df_sample %>% 
  ggplot(aes(x = mu50)) +
  geom_histogram() +
  geom_vline(xintercept = mu)+
  scale_x_continuous(limits = c(18,22)) #sets x axis length of histogram

g50_var_ub <- df_sample %>% 
  ggplot(aes(x = var50)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2) +
  scale_x_continuous(limits= c(18,40))

#sample 100
g100_mu<-df_sample %>% 
  ggplot(aes(x = mu100)) +
  geom_histogram() +
  geom_vline(xintercept = mu) +
  scale_x_continuous(limits = c(18,22)) #sets x axis length of histogram

g100_var_ub <- df_sample %>% 
  ggplot(aes(x = var100)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2) +
  scale_x_continuous(limits= c(18,40))

g100_mu/g50_mu
g100_var_ub/g50_var_ub


# part 2 ------------------------------------------------------------------

df_h10 <- df_h0 %>% 
  filter(height >= 10)

mu_10<-mean(df_h10$height)
sigma2_10<-sum((df_h10$height-mu)^2)/nrow(df_h0)

#subdata set 1 50

h10_mu50_i <- h10_var50_ub_i <- NULL # create empty objects

# repeat the work in {} from i = 1 to i = 100, i designates the dataset you are creating
for (i in 1:100) {
  
  df_10_i <- df_h10 %>% 
    sample_n(size = 50) # random samples of 50 individuals
  
  # save mean for sample set i
  h10_mu50_i[i] <- mean(df_10_i$height)
  
  # save variance for sample set i
  h10_var50_ub_i[i] <- var(df_10_i$height) 
}

#subdata set 2 100

h10_mu100_i <- h10_var100_ub_i <- NULL # create empty objects

# repeat the work in {} from i = 1 to i = 100, i designates the dataset you are creating
for (i in 1:100) {
  
  df_10_i <- df_h10 %>% 
    sample_n(size = 100) # random samples of 50 individuals
  
  # save mean for sample set i
  h10_mu100_i[i] <- mean(df_10_i$height)
  
  # save variance for sample set i
  h10_var100_ub_i[i] <- var(df_10_i$height) 
}

#histograms

df_sample10 <- tibble(h10mu50 = h10_mu50_i,
                    h10var50= h10_var50_ub_i,
                    h10mu100=h10_mu100_i,
                    h10var100=h10_var100_ub_i)
#sample 50
h10g50_mu<-df_sample10 %>% 
  ggplot(aes(x = h10mu50)) +
  geom_histogram() +
  geom_vline(xintercept = mu_10) +
  scale_x_continuous(limits = c(18,22)) #sets x axis length of histogram

h10g50_var_ub <- df_sample10 %>% 
  ggplot(aes(x = h10var50)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2_10) +
  scale_x_continuous(limits= c(18,40))

#sample 100
h10g100_mu<-df_sample10 %>% 
  ggplot(aes(x = h10mu100)) +
  geom_histogram() +
  geom_vline(xintercept = mu_10) +
  scale_x_continuous(limits = c(18,22)) #sets x axis length of histogram

h10g100_var_ub <- df_sample10 %>% 
  ggplot(aes(x = h10var100)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2_10) +
  scale_x_continuous(limits= c(18,40))

g100_mu/g50_mu/h10g100_mu/h10g50_mu
g100_var_ub/g50_var_ub/h10g100_var_ub/h10g50_var_ub

