
library(pacman)
pacman::p_load(tidyverse,
               patchwork,
               here,
               ggplot2)

df_MGT <- read_csv(here("data_raw/fall25germinationdata.csv"))


# t.test  -----------------------------------------------------------------

x <- df_MGT %>%
  filter(Treatment == "C") %>%
  pull(DTG)

y <- df_MGT %>%
  filter(Treatment == "MP") %>%
  pull(DTG)

t.test(x, y, var.equal = FALSE)

# Column Chart ------------------------------------------------------------

df_MGT_mu <- df_MGT %>% 
  group_by(Treatment) %>% # group operation
  summarize(mu_l = mean(DTG),
            sd_l = sd(DTG))

Legend=c("Control","Microplastic")

df_MGT_mu %>%
  ggplot(aes(x=Treatment,
             y=mu_l, fill=Legend))+
  geom_bar(stat="identity")+
  scale_fill_manual("Legend", values = c("Control" = "gold", "Microplastic" = "deeppink3"))+
  labs(x = "Treatment",
       y = "Mean Germination Time in Days")
