pacman::p_load(tidyverse,
               patchwork,
               here)
# exercise 1 --------------------------------------------------------------

#Develop a linear model examining the effect of supplement type (supp), 
#dose (dose), and their interaction on tooth length (len). Assign to m_tooth

df_tooth<-tibble(ToothGrowth)

m_tooth<-lm(len~supp*dose,
     data=df_tooth)

summary(m_tooth)

#test normality
eps<-resid(m_tooth)
shapiro.test(eps) #want to get higher than 0.05 because it means ur model
#is a good fit


# exercise 2 --------------------------------------------------------------

#predicted values
df_pred <- ToothGrowth %>%
  group_by(supp) %>%
  reframe(dose = seq(min(dose),
                     max(dose),
                     length = 100))

y_pred <- predict(m_tooth,
                  newdata = df_pred)

df_pred <- df_pred %>% 
  mutate(y_pred = y_pred)

#visualization

df_tooth%>% 
  ggplot(aes(x = dose,
             y = len,
             color = supp)) +
  geom_point(alpha = 0.5) +
  geom_line(data = df_pred,
            aes(y = y_pred))


# exercise 3 --------------------------------------------------------------


## variance-covariance matrix
mv <- rbind(c(1, 0.9),
            c(0.9, 1))

## true regression coefficients
b <- c(0.05, 1.00)

## produce simulated data
set.seed(523)
X <- MASS::mvrnorm(100,
                   mu = c(0, 0),
                   Sigma = mv)

df_y <- tibble(x1 = X[,1],
               x2 = X[,2]) %>% 
  mutate(y = rnorm(nrow(.),
                   mean = 1 + b[1] * x1 + b[2] * x2))

#Using df_y, create the following figures:
#A scatter plot of y (y-axis) versus x1 (x-axis)
#A scatter plot of y (y-axis) versus x2 (x-axis)

df_y%>%
  ggplot(aes(x=x1,
             y=y))+
  geom_point()

df_y%>%
  ggplot(aes(x=x2,
             y=y))+
  geom_point()

#Examine the statistical effects of x1 and x2 on y using + operator

m_y<-lm(y~x1+x2,
   data=df_y)
summary(m_y)

#Using df_y, Create a scatter plot of x1 and x2. Examine the Pearson’s 
#correlation between x1 and x2 using a function cor(). (?cor() for its usage).

df_y%>%
  ggplot(aes(x=x2,
             y=x1))+
  geom_point()

with(df_y,
    cor(x1,x2)) #0.88
