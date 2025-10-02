library(pacman)
pacman::p_load(tidyverse,
               patchwork,
               here)

# exercise 1 --------------------------------------------------------------

xs<-rnorm(n=10,mean=10,sd=5)
ys<-rnorm(n=10,mean=12,sd=5)
x1<-rnorm(n=100,mean=10,sd=5)
y1<-rnorm(n=100,mean=12,sd=5)

#xs vs ys

t.test(xs, ys, var.equal = TRUE)

#x1 vs y1

t.test(x1, y1, var.equal = TRUE)


# exercise 2 --------------------------------------------------------------

a1 <- c(13.9, 14.9 ,13.4, 14.3, 11.8, 13.9, 14.5, 15.1, 13.3, 13.9)
a2 <- c(17.4, 17.3, 20.1, 17.2, 18.4, 19.6, 16.8, 18.7, 17.8, 18.9)

b1 <- c(10.9, 20.3, 9.6, 8.3, 14.5, 12.3, 14.5, 16.7, 9.3, 22.0)
b2 <- c(26.9, 12.9, 11.1, 16.7, 20.0, 20.9, 16.6, 15.4, 16.2, 16.2)


df_ex<-tibble(value=c(a1,a2,b1,b2),
                group=c(rep("a1",length(a1)),
                rep("a2",length(a2)),
                rep("b1",length(b1)),
                rep("b2",length(b2))
                )
)


df_ex_mu <- df_ex %>% 
  group_by(group) %>%
  summarize(mu_l = mean(value),
            sd_l = sd(value))

df_ex %>%
  filter(group %in% c("a1","a2")) %>%
  ggplot(aes(x=group,
             y=value))+
  geom_jitter(width=.1,
              alpha=.25) +
  geom_segment(data=df_ex_mu %>%
                 filter(group %in% c("a1","a2")),
               aes(x=group,
                   xend=group,
                   y=mu_l-sd_l,
                   yend=mu_l+sd_l))+
  geom_point(data = df_ex_mu %>%
               filter(group %in% c("a1","a2")),
             aes(x = group,
                 y = mu_l),
             size = 3)+
  labs(x = "Group",
       y = "Value")

#Welch's Test

t.test(a1,a2,var.equal=FALSE)
t.test(b1,b2,var.equal=FALSE)
