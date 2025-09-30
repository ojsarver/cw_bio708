#two group statistical test

library(tidyverse)
library(patchwork)
#install.packages("pacman")
library(pacman)
#allows you to load multiple packages in one code
pacman::p_load(tidyverse,
               patchwork,
               here)

df_fl <- read_csv(here("data_raw/data_fish_length.csv"))

unique(df_fl$lake) #lets you know how many unique elements are in a data set/vector
#distinct function will do something similar to unique

# visualization
df_fl_mu <- df_fl %>% 
  group_by(lake) %>% # group operation
  summarize(mu_l = mean(length), # summarize by mean()
            sd_l = sd(length)) # summarize with sd()

df_fl %>%
  ggplot(aes(x=lake,
             y=length))+
  geom_jitter(width=.1,
              alpha=.25) +
  geom_segment(data=df_fl_mu,
               aes(x=lake,
                   xend=lake,
                   y=mu_l-sd_l,
                   yend=mu_l+sd_l))+
  geom_point(data = df_fl_mu,
             aes(x = lake,
                 y = mu_l),
             size = 3)+
  labs(x = "Lake",
       y = "Fish body length")
#width describes how much space/scatter there is between points
#alpha describes point transparency
#geom segment/point is changing the data from df_fl to df_fl_mu

x <- df_fl %>%
  filter(lake == "a") %>%  # subset lake a
  pull(length) #pulls length as a vector so it can be analyzed by ttest, otherwise
#it stays as a data frame which the ttest does not like

y <- df_fl %>%
  filter(lake == "b") %>%
  pull(length)

t.test(x, y, var.equal = TRUE)
#t.test(vector 1 name, vector 2 name, var.equal = is variance equal? usually FALSE)

# details of ttest --------------------------------------------------------

mu_x<-mean(x)
mu_y<-mean(y)
mu_x-mu_y


df_t <- df_fl %>% 
  group_by(lake) %>% # group operation
  summarize(mu_l = mean(length), 
            var_l = var(length),
            n = n()) 


v_mu <- pull(df_t, mu_l)
v_var <- pull(df_t, var_l)
v_n <- pull(df_t, n)

var_a<-((v_n[1] - 1)/(sum(v_n) - 2)) * v_var[1]
var_b<-((v_n[2] - 1)/(sum(v_n) - 2)) * v_var[2]

var_p<-var_a+var_b

#calculate t statistic
t_value<-(v_mu[1]-v_mu[2])/sqrt(var_p*(1/v_n[1]+1/v_n[2]))

#null distribution
x2 <- seq(-5, 5, length = 500)

y2 <- dt(x2, df = sum(v_n) - 2)
#dt calculates probability density @ value x2

tibble(x2, y2) %>% 
  ggplot(aes(x = x2,
             y = y2)) +
  geom_line() +
  geom_vline(xintercept = t_value,
             color = "pink") +
  geom_vline(xintercept = abs(t_value),
             color = "pink")+
  labs(y = "Probability density",
       x = "t-statistic")

# calculate area under the curve from -infinity to t_value
p_lower <- pt(q = t_value, df = 98)

# calculate area under the curve from abs(t_value) to infinity
p_higher <- 1 - pt(q = abs(t_value), df = 98)

p_value <- p_lower + p_higher
p_value

#var.equal=FALSE is the other variant of the ttest, should be the default bc
#it covers variance being unequal AND variance being equal
t.test(x, y, var.equal = FALSE)
