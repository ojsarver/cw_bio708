pacman::p_load(tidyverse,
               patchwork,
               here)


# exercise 1 --------------------------------------------------------------

#fish data y=n_sp x=distance, cat_area, hull_area

url <- "https://raw.githubusercontent.com/aterui/public-proj_fish-richness-shubuto/master/data/data_vpart.csv"
df_fish <- read_csv(url)

m_fish<-glm(n_sp~distance+cat_area+hull_area,
            data=df_fish,
            family="poisson")

summary(m_fish)

#mtcars y=am, x= mpg, hp, wt
m_am<-glm(cbind(am,1-am)~mpg+hp+wt,
          data=mtcars,
          family="binomial")
summary(m_am)

plot(am~wt,
     mtcars)

m_am_gau<-glm(am~mpg+hp+wt,
          data=mtcars,
          family="gaussian")%>%
  summary()


# exercise 2 --------------------------------------------------------------


#The function scale() will perform standardization. Create columns of 
#standardized distance (std_dist), cat_area (std_cat), and hull_area (std_hull)

df_fish<-df_fish%>%
  mutate(std_dist=scale(distance),
         std_cat=scale(cat_area),
         std_hull=scale(hull_area))

#Perform a GLM analysis of fish species richness with the standardized 
#variables, and identify the most influential variable based on effect size
m_fish_std<-glm(n_sp~std_dist+std_cat+std_hull,
    data=df_fish,
    family="poisson")

#compare coefs
coef(m_fish)
coef(m_fish_std)

# exercise 3 --------------------------------------------------------------

url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_raw/data_offset.csv"
df_offset <- read_csv(url)

#Plot the relationship between count and nitrate.
df_offset%>%
  ggplot(aes(x=nitrate,
             y=count))+
  geom_point()

#Plot the relationship between count and area.
df_offset%>%
  ggplot(aes(x=area,
             y=count))+
  geom_point()

#Plot the relationship between count/area and nitrate.
df_offset%>%
  mutate(density=count/area)%>%
  ggplot(aes(x=nitrate,
             y=density))+
  geom_point()

df_offset <- df_offset %>% 
  mutate(density = count / area)

glm(density ~ nitrate,
    data = df_offset,
    family = "poisson")

#Develop a GLM with the response variable count and the predictor nitrate

m_cn<-glm(count ~ nitrate,
    data = df_offset,
    family = "poisson")

#Develop a GLM with the response variable count, the predictor nitrate, 
#and the offset term offset(log(area)) (~ nitrate + offset(log(area)) will work

m_offsetcn<-glm(count ~ nitrate+offset(log(area)),
    data = df_offset,
    family = "poisson")

#Compare the results of the two models.

summary(m_cn)
summary(m_offsetcn)
# exercise 4 --------------------------------------------------------------


url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_raw/data_tadpole.csv"
df_tadpole <- read_csv(url)

#Plot the relationships between tadpole and aqveg and tadpole and permanence

df_tadpole%>%
  ggplot(aes(x=aqveg,
             y=tadpole))+
  geom_point()

df_tadpole%>%
  ggplot(aes(x=permanence,
             y=tadpole))+
  geom_point()

#Develop a GLM (with appropriate distribution) explaining tadpole with aqveg 
#and permanence

m_tadpole<-glm(tadpole~aqveg+permanence,
    data=df_tadpole,
    family="poisson")

summary(m_tadpole)

#breaks assumptions of poisson, mean and var are supposed to be similar

mean(df_tadpole$tadpole)
var(df_tadpole$tadpole)

#negative binomial is better choice

m_nb<-MASS::glm.nb(tadpole~aqveg+permanence,
             data=df_tadpole)

summary(m_nb)
