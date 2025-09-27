
# exercise 1 - Normal Distribution ----------------------------------------

#The function rnorm() produces a random variable that follows a Normal 
#distribution with a specified mean and SD. Using this function,
#Generate a variable with 50 observations.

NDdata<-rnorm(50)
tibnd<-tibble(NDdata)
mu1<-mean(NDdata)
sigma1<-sd(NDdata)

NDdata

tibnd %>% 
  ggplot(aes(x = NDdata)) + 
  geom_histogram(binwidth = 1,
                 center = 0.5) +
  geom_vline(aes(xintercept = mean(NDdata)))

xnd <- seq(min(tibnd), max(tibnd), length = 100)

pnd <- dnorm(xnd, mean = mu1, sd = sigma1)

tibble(y = pnd, x = xnd) %>%
  ggplot(aes(x = x, y = y)) +
  geom_line()

xnd_min <- floor(min(tibnd))
xnd_max <- ceiling(max(tibnd))
bin <- seq(xnd_min, xnd_max, by = 1) 

p1 <- NULL # empty object for probability
for (i in 1:(length(bin) - 1)) {
  p1[i] <- pnorm(bin[i+1], mean = mu1, sd = sigma1) -
    pnorm(bin[i], mean = mu1, sd = sigma1)
}

df_prob1 <- tibble(p1, bin = bin[-length(bin)] + 0.5) %>% 
  mutate(freq = p1 * nrow(tibnd))

tibnd %>% 
  ggplot(aes(x = p1)) + 
  geom_histogram(binwidth = 1, # specify bin width; must match the bin width used for probability
                 center = 0.5)
  geom_point(data = df_prob1,
             mapping=aes(y = freq,
                         x = bin),
             color = "hotpink") +
  geom_line(data = df_prob1,
            mapping=aes(y = freq,
                        x = bin),
            color = "hotpink")
