#probability distribution

# load csv data
library(tidyverse)
df_h0 <- read_csv("data_raw/data_plant_height.csv")

df_h0 %>% 
  ggplot(aes(x = height)) + 
  geom_histogram(binwidth = 1, # specify bin width
                 center = 0.5) + # bin's center specification
  geom_vline(aes(xintercept = mean(height))) # draw vertical line at the mean

# vector of x values
# seq() generate min to max values with specified numbers of elements or interval
# the following produces 100 elements
x <- seq(min(df_h0$height), max(df_h0$height), length = 100)

# calculate probability density using mean and standard deviation
mu <- mean(df_h0$height)
sigma <- sd(df_h0$height)

#dnorm creates normal distribution
pd <- dnorm(x, mean = mu, sd = sigma)

#visualizing the distribution
tibble(y = pd, x = x) %>%
  ggplot(aes(x = x, y = y)) +
  geom_line() + 
  labs(y = "Probability density", x="Plant height")

# probability that x < 10
p10 <- pnorm(10, mean = mu, sd = sigma)

# probability that x < 20
p20 <- pnorm(20, mean = mu, sd = sigma)

# probability that x is between 10 and 20: 10 < x < 20
p20_10 <- p20 - p10

#above we compared 2 bins, 10 and 20, here's how to get the probability of
#getting a value that belongs in each bin
x_min <- floor(min(df_h0$height)) # floor takes the integer part of the value
x_max <- ceiling(max(df_h0$height)) # ceiling takes the next closest integer
bin <- seq(x_min, x_max, by = 1) # each bin has 1cm

p <- NULL # empty object for probability
for (i in 1:(length(bin) - 1)) {
  p[i] <- pnorm(bin[i+1], mean = mu, sd = sigma) -
    pnorm(bin[i], mean = mu, sd = sigma)
}

# data frame for probability
# bin: last element [-length(bin)] was removed to match length
# expected frequency in each bin is "prob times sample size"
# "+ 0.5" was added to represent a midpoint in each bin
df_prob <- tibble(p, bin = bin[-length(bin)] + 0.5) %>% 
  mutate(freq = p * nrow(df_h0))

df_h0 %>% 
  ggplot(aes(x = height)) + 
  geom_histogram(binwidth = 1, # specify bin width; must match the bin width used for probability
                 center = 0.5) + # bin's center position
  geom_point(data = df_prob,
             mapping=aes(y = freq,
                 x = bin),
             color = "hotpink") +
  geom_line(data = df_prob,
            mapping=aes(y = freq,
                x = bin),
            color = "hotpink")


# 9.2 poisson distribution ------------------------------------------------

df_count <- read_csv("data_raw/data_garden_count.csv")
print(df_count)

df_count %>% 
  ggplot(aes(x = count)) +
  geom_histogram(binwidth = 0.5, # define binwidth
                 center = 0) # relative position of each bin

# vector of x values
# create a vector of 0 to 10 with an interval one
# must be integer of > 0
x <- seq(0, 10, by = 1)

# calculate probability mass, dpois is poisson distribution function
lambda_hat <- mean(df_count$count)
pm <- dpois(x, lambda = lambda_hat)

# figure
tibble(y = pm, x = x) %>% # data frame
  ggplot(aes(x = x, y = y)) +
  geom_line(linetype = "dashed") + # draw dashed lines
  geom_point() + # draw points
  labs(y = "Probability",
       x = "Count") # re-label

#combo expected freq with original 9.2 histogram
df_prob2 <- tibble(x = x, y = pm) %>% 
  mutate(freq = y * nrow(df_count)) #added freq column

df_count %>% 
  ggplot(aes(x = count)) +
  geom_histogram(binwidth = 0.5, # must be divisible number of one; e.g., 0.1, 0.25, 0.5...
                 center = 0) +
  geom_line(data = df_prob2,
            aes(x = x,
                y = freq),
            linetype = "dashed") +
  geom_point(data = df_prob2,
             aes(x = x,
                 y = freq))