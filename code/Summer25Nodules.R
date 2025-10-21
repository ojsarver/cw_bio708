
# t.test ------------------------------------------------------------------

library(pacman)
pacman::p_load(tidyverse,
               patchwork,
               here,
               ggplot2)

df_nod <- read_csv(here("data_raw/Summer25_Nodules.csv"))

x <- df_nod %>%
  filter(Treatment == "C") %>%
  pull(Nodules)

y <- df_nod %>%
  filter(Treatment == "MP") %>%
  pull(Nodules)

t.test(x, y, var.equal = FALSE)


# chart -------------------------------------------------------------------

df_nod_mu <- df_nod %>% 
  group_by(Treatment) %>% # group operation
  summarize(mu_n = mean(Nodules),
            sd_n = sd(Nodules),
            N_n = length(Nodules))

Legend=c("Control","Microplastic")

df_nod_mu %>%
  ggplot(aes(x=Treatment,
             y=mu_n, fill=Legend))+
  geom_bar(stat="identity")+
  scale_fill_manual("Legend", values = c("Control" = "lightblue2",
                                         "Microplastic" = "green4"))+
  geom_errorbar(aes(ymin=mu_n-(sd_n/sqrt(N_n)), 
                    ymax=mu_n+(sd_n/sqrt(N_n)), 
                    width=0.2))+
  labs(x = "Treatment",
       y = "Average Number of Nodules")
