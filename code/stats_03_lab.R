
# exercise 1 - Normal Distribution ----------------------------------------

#The function rnorm() produces a random variable that follows a Normal 
#distribution with a specified mean and SD. Using this function,
#Generate a variable with 50 observations.

library(tidyverse)
rnorm<-rnorm(50)
df0<-tibble(rnorm)

df0 %>% 
  ggplot(aes(x = rnorm)) + 
  geom_histogram(binwidth = 1,
                 center = 0.5) + 
  geom_vline(aes(xintercept = mean(rnorm)))

x1 <- seq(min(df0$rnorm), max(df0$rnorm), length = 50)

mu1 <- mean(df0$rnorm)
sigma1 <- sd(df0$rnorm)
pd1 <- dnorm(x1, mean = mu1, sd = sigma1)


tibble(y = pd1, x = x1) %>% #
  ggplot(aes(x = x, y = y)) +
  geom_line() + 
  labs(y = "Probability density")

x1_min <- floor(min(df0$rnorm))
x1_max <- ceiling(max(df0$rnorm))
bin1 <- seq(x1_min, x1_max, by = 1)

p1 <- NULL
for (i in 1:(length(bin1) - 1)) {
  p1[i] <- pnorm(bin1[i+1], mean = mu1, sd = sigma1) - pnorm(bin1[i], 
                                                            mean = mu1, sd = sigma1)
}

df_prob1 <- tibble(p1, bin1 = bin1[-length(bin1)] + 0.5) %>% 
  mutate(freq = p1 * nrow(df0))

df0 %>% 
  ggplot(aes(x = rnorm)) + 
  geom_histogram(binwidth = 1, 
                 center = 0.5) + 
  geom_point(data = df_prob1,
             aes(y = freq,
                 x = bin1),
             color = "hotpink") +
  geom_line(data = df_prob1,
            aes(y = freq,
                x = bin1),
            color = "hotpink")


# exercise 2 --------------------------------------------------------------


#The function rpois() produces a random variable that follows a Poisson 
#distribution with a specified mean. Generate a variable with 1000 observations.

rpois<-rpois(1000, 10)
df1<-tibble(rpois)

x2 <- seq(0, 25, by = 1)

lambda_hat <- mean(df1$rpois)
pm <- dpois(x2, lambda = lambda_hat)

df_prob2 <- tibble(x = x2, y = pm) %>% 
  mutate(freq = y * nrow(df1))

df1 %>% 
  ggplot(aes(x = rpois)) +
  geom_histogram(binwidth = 0.5,
                 center = 0) +
  geom_line(data = df_prob2,
            aes(x = x,
                y = freq),
            linetype = "dashed") +
  geom_point(data = df_prob2,
             aes(x = x,
                 y = freq))
