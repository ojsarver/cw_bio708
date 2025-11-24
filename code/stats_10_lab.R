pacman::p_load(tidyverse,
               patchwork,
               janitor,
               palmerpenguins,
               here)

# data manipulation -------------------------------------------------------

colnames(penguins_raw)

#clean column names w/clean_names()
?clean_names()

penguins_clean<-penguins_raw%>%
  clean_names()

colnames(penguins_raw)

#change input in clutch_completion using ifelse() and mutate()
unique(penguins_clean$clutch_completion)

penguins_clean<-penguins_clean%>%
  mutate(clutch_completion=ifelse(clutch_completion=="Yes",
                                  yes=1,
                                  no=0))

#changes values of "yes" to 1 and "no" to 0, use ifelse because only 2 inputs

#change species name input w/case_when() and mutate()
unique(penguins_clean$species)

penguins_clean<-penguins_clean%>%
  mutate(species=case_when(species=="Adelie Penguin (Pygoscelis adeliae)"~"adelie",
                           species=="Gentoo penguin (Pygoscelis papua)"~"gentoo",
                           species=="Chinstrap penguin (Pygoscelis antarctica)"~"chinstrap"))


#remove NAs

penguins_clean<-penguins_clean%>%
  drop_na(culmen_length_mm, 
          culmen_depth_mm, 
          flipper_length_mm, 
          body_mass_g, 
          sex)


# exercise 2 --------------------------------------------------------------


#Develop a statistical model that explains Clutch Completion using the 
#variables Species, Culmen Length (mm), Culmen Depth (mm), Flipper Length (mm),
#Body Mass (g), and Sex. Use an appropriate probability distribution for this model

model<-glm(clutch_completion~species+
      culmen_length_mm+
      culmen_depth_mm+
      flipper_length_mm+
      body_mass_g+
      sex,
    data=penguins_clean,
    family="binomial")

#install.packages("MuMIn")

library(MuMIn)

options(na.action = "na.fail")
m_set <- dredge(model, rank = "AIC")
subset(m_set, delta < 2)
