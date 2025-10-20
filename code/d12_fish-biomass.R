# 1. Load packages ----

library(tidyverse)
library(digitize) # See https://lukemiller.org/index.php/2011/06/digitizing-data-from-old-plots-using-digitize/

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")

# 2. Extract data ----

if(FALSE){

## 2.1 Commercial fish ----

cal <- ReadAndCal("data/12_fish-data/hri_mar-report-card.png")

data_points <- DigitData(col = 'red')

data_commercial <- Calibrate(data_points, cal, 1, 4, 0, 1500) %>% 
  rename(condition = x, biomass = y) %>% 
  mutate(condition = c(rep("Fully protected", 3),
                         rep("Highly protected", 3),
                         rep("General use", 3),
                         rep("Outside MPA", 3)),
         type = rep(c("lower_ci", "mean", "higher_ci"), 4)) %>% 
  pivot_wider(names_from = type, values_from = biomass) %>% 
  mutate(name = "Commercial fish")

## 2.2 Herbivorous fish ----

cal <- ReadAndCal("data/12_fish-data/hri_mar-report-card.png")

data_points <- DigitData(col = 'red')

data_herbivorous <- Calibrate(data_points, cal, 1, 4, 0, 3000) %>% 
  rename(condition = x, biomass = y) %>% 
  mutate(condition = c(rep("Fully protected", 3),
                       rep("Highly protected", 3),
                       rep("General use", 3),
                       rep("Outside MPA", 3)),
         type = rep(c("lower_ci", "mean", "higher_ci"), 4)) %>% 
  pivot_wider(names_from = type, values_from = biomass) %>% 
  mutate(name = "Herbivorous fish")

## 2.3 Combine data ----

data_hri <- bind_rows(data_commercial, data_herbivorous) %>% 
  mutate(nb_sites = rep(c(68, 25, 132, 61), 2))

write.csv2(x = data_hri,
           file = "data/12_fish-data/hri_mar-report-card_extracted.csv",
           row.names = FALSE)

}

# 3. Redrawn the figure ----

data_hri <- read.csv2("data/12_fish-data/hri_mar-report-card_extracted.csv") %>% 
  mutate(condition = str_replace_all(condition, " ", "\n"),
         condition = factor(condition),
         condition = fct_relevel(condition,
                                 c("Fully\nprotected", "Highly\nprotected",
                                   "General\nuse", "Outside\nMPA")),
         across(c(lower_ci, mean, higher_ci), ~.x/1000))

ggplot(data = data_hri, aes(x = condition, y = mean, fill = condition)) + 
  geom_bar(stat = "identity", color = NA, position = position_dodge(),
           show.legend = FALSE, width = 0.7) +
  scale_fill_manual(values = c("#2C5D96", "#7393C9", "#B2BBCC", "#d35f5fff")) +
  facet_wrap(~name, scales = "free_y") +
  geom_errorbar(aes(ymin = lower_ci, ymax = higher_ci), width = 0.1,
                position = position_dodge()) +
  geom_text(aes(x = condition, y = 0, label = nb_sites), vjust = -1, color = "white") +
  labs(x = NULL, y = "Fish biomass (kg/100 m²)") +
  theme_graph() +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.1), limits = c(0, NA)) +
  theme(strip.background = element_blank(),
        panel.spacing = unit(2, "lines"),
        strip.text = element_text(face = "bold", size = 15))

ggsave("figs/01_part-1/fig-15.png", width = 10, height = 4.5)
