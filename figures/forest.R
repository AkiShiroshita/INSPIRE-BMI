packages = c("devtools",
             "usethis",
             "here",
             "readr",
             "readxl",
             "tidyverse",
             "tidylog",
             "lubridate",
             "ggplot2",
             "tidylog",
             "ggplotgui",
             "ggthemes",
             "arsenal",
             "margins",
             "mice",
             "VIM",
             "norm2",
             "patchwork",
             "haven",
             "ggbeeswarm",
             "RColorBrewer")
package.check <- lapply(packages, FUN = function(x){
  if (!require(x, character.only = TRUE)){
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

results <- read_csv("input/forest_tab.csv")

## RD

results_rd <- results %>% 
  filter(cat2 == "rd")

figure_rd <- ggplot(data = transform(results_rd,
                                     cat = factor(cat, levels =c("RSV infection during the first year",
                                                                 "Recurrent wheeze at 1 year",
                                                                 "5-year current asthma"))),
                    aes(x=name, y=est, ymin = lower, ymax = upper)) + 
  theme_bw() +
  geom_pointrange(aes(col = cat), size = 0.2) +
  xlab("Variables") + 
  ylab("Risk difference") +
  geom_errorbar(aes(ymin = lower, ymax = upper, col = cat), width = 0.5, cex = 0.5) +
  geom_hline(yintercept=0,linetype=2) +
  facet_wrap( ~ cat, strip.position = "top", nrow = 4, scales = "free_y") +
  theme(text = element_text(size = 14,
                            face = "bold"),
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(face="bold"),
        axis.title = element_text(size = 14,
                                  face = "bold"),
        strip.background = element_rect(fill = "light gray"),
        strip.text = element_text(size = 14, color = "black")) +
  coord_flip() +
  scale_colour_brewer(palette = "Dark2") +
  guides(color="none")  
figure_rd

# RR

results_rr <- results %>% 
  filter(cat2 == "rr")

figure_rr <- ggplot(data = transform(results_rr,
                                     cat = factor(cat, levels =c("RSV infection during the first year",
                                                                 "Recurrent wheeze at 1 year",
                                                                 "5-year current asthma"))),
                    aes(x=name, y=est, ymin = lower, ymax = upper)) + 
  theme_bw() +
  geom_pointrange(aes(col = cat), size = 0.2) +
  xlab("Variables") + 
  ylab("Risk ratio") +
  geom_errorbar(aes(ymin = lower, ymax = upper, col = cat), width = 0.5, cex = 0.5) +
  geom_hline(yintercept=1,linetype=2) +
  facet_wrap( ~ cat, strip.position = "top", nrow = 4, scales = "free_y") +
  theme(text = element_text(size = 14,
                            face = "bold"),
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(face="bold"),
        axis.title = element_text(size = 14,
                                  face = "bold"),
        strip.background = element_rect(fill = "light gray"),
        strip.text = element_text(size = 14, color = "black")) +
  coord_flip() +
  scale_colour_brewer(palette = "Dark2") +
  guides(color="none")  
figure_rr

# combine
figure_rd + figure_rr


## dist of BSS2
df <- read_dta("output/analysis_data_bmi_update.dta") %>% 
  drop_na(mat_bmi_prepreg_cat, bss2_dev)

graph1 <- ggplot(df, aes(x = bss2_dev)) +
  geom_histogram(color="#e9ecef", position = 'identity', alpha = 0.6, binwidth = 1) +
  scale_fill_manual(values=c("#404080")) +
  facet_grid( factor(mat_bmi_prepreg_cat, levels = c("0", "1", "2", "3"),
                     labels = c("Underweight", "Normal", "Overweight", "Obesity")) ~ . ) +
  xlab("Respiratory severity score") + 
  ylab("Number of children") +
  theme(text = element_text(size = 24,
                            face = "bold"),
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(face="bold"),
        axis.title = element_text(size = 24,
                                  face = "bold"),
        strip.background = element_rect(fill = "light gray"),
        strip.text = element_text(size = 24, color = "black")) +
  theme_bw()
graph1

df <- read_dta("output/analysis_data_bmi_update.dta") %>% 
  drop_na(gwg_cat, bss2_dev)

graph2 <- ggplot(df, aes(x = bss2_dev)) +
  geom_histogram(color="#e9ecef", position = 'identity', alpha = 0.6, binwidth = 1) +
  scale_fill_manual(values=c("#69b3a2")) + 
  facet_grid( factor(gwg_cat, levels = c("0", "1"),
                     labels = c("No", "Gestational weight gain")) ~ . ) +
  xlab("Respiratory severity score") + 
  ylab("Number of children") +
  theme(text = element_text(size = 24,
                            face = "bold"),
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(face="bold"),
        axis.title = element_text(size = 24,
                                  face = "bold"),
        strip.background = element_rect(fill = "light gray"),
        strip.text = element_text(size = 24, color = "black")) +
  theme_bw()
graph2

graph1 + graph2

# beeswarm plot
cols <- brewer.pal(4, "Dark2")
df <- read_dta("output/analysis_data_bmi_update.dta") %>% 
  drop_na(mat_bmi_prepreg_cat, bss2_dev) %>% 
  mutate(mat_bmi_prepreg_cat = factor(mat_bmi_prepreg_cat,
                                      levels = c("0", "1", "2", "3"),
                                      labels = c("Underweight", "Normal", "Overweight", "Obesity")),
         gwg_cat = factor(gwg_cat,
                          levels = c("0", "1"),
                          labels = c("No", "Gestational weight gain")))

ggplot(df, aes(x = mat_bmi_prepreg_cat, y = bss2_dev, fill = mat_bmi_prepreg_cat)) +
  geom_beeswarm(size = 2, shape = 21, cex = 0.6) +
  scale_fill_manual(values =cols) +
  theme_bw() +
  theme(axis.title = element_text(size = 24),
        axis.text = element_text(size = 20),
        legend.position = "none") +
  labs(x = "Maternal body mass index before pregnancy", y = "Respiratory severity score")

cols <- brewer.pal(6, "Dark2")

df <- read_dta("output/analysis_data_bmi_update.dta") %>% 
  drop_na(gwg_cat, bss2_dev) %>% 
  mutate(mat_bmi_prepreg_cat = factor(mat_bmi_prepreg_cat,
                                      levels = c("0", "1", "2", "3"),
                                      labels = c("Underweight", "Normal", "Overweight", "Obesity")),
         gwg_cat = factor(gwg_cat,
                          levels = c("0", "1"),
                          labels = c("No", "Gestational weight gain")))

ggplot(df, aes(x = gwg_cat, y = bss2_dev, fill = gwg_cat)) +
  geom_beeswarm(size = 2, shape = 21, cex = 0.6) +
  scale_fill_manual(values =cols[c(5,6)]) +
  theme_bw() +
  theme(axis.title = element_text(size = 24),
        axis.text = element_text(size = 20),
        legend.position = "none") +
  labs(x = "Gestatioanal weight gain", y = "Respiratory severity score")
