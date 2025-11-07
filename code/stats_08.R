pacman::p_load(tidyverse,
               patchwork,
               here)

#Count data
df_count <- read_csv(here("data_raw/data_garden_count.csv"))

#linear model assuming normal distribution
m_normal <- lm(count ~ nitrate,
               df_count)

summary(m_normal)

# extract estimates
alpha <- coef(m_normal)[1] # intercept
beta <- coef(m_normal)[2] # slope

#visualize

df_count %>% 
  ggplot(aes(x = nitrate,
             y = count)) +
  geom_point() +
  geom_abline(intercept = alpha,
              slope = beta)

#poisson distribution

m_pois <- glm(count ~ nitrate,
              data = df_count,
              family = "poisson")
summary(m_pois)

#lm reports t statistic, glm reports z statistic

# parameter estimates and their SEs
theta <- coef(m_pois)
se <- sqrt(diag(vcov(m_pois)))
z_value <- theta / se

print(z_value)

# estimate Pr(>|z|) (p-value) using a standardized normal distribution
p_value <- (1 - pnorm(abs(z_value), mean = 0, sd = 1)) * 2
print(p_value)

# make predictions
df_pred <- tibble(nitrate = seq(min(df_count$nitrate),
                                max(df_count$nitrate),
                                length = 100))

# y_pois is exponentiated because predict() returns values in log-scale
y_normal <- predict(m_normal, newdata = df_pred)
y_pois <- predict(m_pois, newdata = df_pred) %>% exp()

df_pred <- df_pred %>% 
  mutate(y_normal,
         y_pois)

# visualization
df_count %>% 
  ggplot(aes(x = nitrate,
             y = count)) +
  geom_point() +
  geom_line(data = df_pred,
            aes(y = y_normal),
            linetype = "dashed") +
  geom_line(data = df_pred,
            aes(y = y_pois),
            color = "pink")


# proportional data -------------------------------------------------------


df_mussel <- read_csv(here("data_raw/data_mussel.csv"))

# calculate the proportion of fertilized eggs
df_mussel <- df_mussel %>% 
  mutate(prop_fert = n_fertilized / n_examined)

#visualization
df_mussel %>% 
  ggplot(aes(x = density,
             y = prop_fert)) +
  geom_point() +
  labs(y = "Proportion of eggs fertilized",
       x = "Mussel density")

#data has upper limit and discrete variables rather than continuous
#Use binomial model: characterized by two parameters: the number of trials (N) 
#and the probability of success (p)

# x: produce 100 numbers from -100 to 100 (assume logit scale)
# y: convert with inverse-logit transformation (ordinary scale)
df_test <- tibble(logit_x = seq(-10, 10, length = 100),
                  x = exp(logit_x) / (1 + exp(logit_x)))

df_test %>% 
  ggplot(aes(x = logit_x,
             y = x)) +
  geom_point() +
  geom_line() +
  labs(y = "x",
       x = "logit(x)")

#GLM, need to use cbind()

m_binom <- glm(cbind(n_fertilized, n_examined - n_fertilized) ~ density,
               data = df_mussel,
               family = "binomial")

summary(m_binom)

# make prediction
df_pred <- tibble(density = seq(min(df_mussel$density),
                                max(df_mussel$density),
                                length = 100))

# y_binom is inv.logit-transformed because predict() returns values in logit-scale
y_binom <- predict(m_binom, newdata = df_pred) %>% boot::inv.logit()

df_pred <- df_pred %>% 
  mutate(y_binom)

#visualize
df_mussel %>% 
  ggplot(aes(x = density,
             y = prop_fert)) +
  geom_point() +
  geom_line(data = df_pred,
            aes(y = y_binom)) +
  labs(y = "Proportion of eggs fertilized",
       x = "Mussel density")

#To assist in selecting an appropriate probability distribution for modeling
#there is a concise decision tree graph in the GLM chapter of textbook