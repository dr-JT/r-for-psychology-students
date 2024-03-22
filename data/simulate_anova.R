# simulate example data for ANOVA
library(here)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

source(here("Lecture Slides/themes/ggplot2_theme.R"))

output_theme <- theme_linedraw() + 
  theme_spacious(font.size = 12) + 
  theme(panel.border = element_rect(color = "gray"),
        axis.line.x = element_line(color = "gray"),
        axis.line.y = element_line(color = "gray"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

theme_set(output_theme)

P     <- 2               # Xb1
R     <- 3               # Xw1
Njklm <- 45              # obs per cell
Njk   <- Njklm*P       # number of subjects
N     <- Njklm*P*R   # number of observations
id    <- gl(Njk,         R, N, labels = c(1:Njk))
Xb1   <- gl(P,   Njklm*R, N, labels = c("Rote Repetition", "Visual Imagery"))
Xw1   <- gl(R, 1, N, labels = c(1, 2, 4))

mu      <- 18
eB1     <- c(-3, 5)
eW1     <- c(-2, -4, 4)
eB1W1   <- c(-4, -3, -4, 0, -4, 4)
# no 3rd-order interaction B1xB2xW1xW2

names(eB1)     <- levels(Xb1)
names(eW1)     <- levels(Xw1)
names(eB1W1)   <- levels(interaction(Xb1, Xw1))

muJKLM <- mu +
  eB1[Xb1] + eW1[Xw1] +
  eB1W1[interaction(Xb1, Xw1)]

set.seed(42069)

muId  <- rep(rnorm(Njk, 0, 3), each = R)
mus   <- muJKLM + muId
sigma <- 5

Y  <- round(rnorm(N, mus, sigma), 1)
d2 <- data.frame(id, Xb1, Xw1, Y)

d2 <- d2 %>%
  rename(Subject = id, Memory_Strategy = Xb1, Presentation_Rate = Xw1, Recall_Performance = Y) %>%
  mutate(Recall_Performance = ifelse(Recall_Performance < 0, 1, Recall_Performance))

d2_wide <- d2 %>%
  pivot_wider(id_cols = c("Subject", "Memory_Strategy"),
              names_from = "Presentation_Rate",
              names_prefix = "Rate_",
              values_from = "Recall_Performance")

write_csv(d2, here("Lecture Slides/data", "recall_data.csv"))
write_csv(d2_wide, here("Lecture Slides/data", "recall_data_wide.csv"))

set.seed(42069)
ggplot(d2, aes(x = Presentation_Rate, y = Recall_Performance)) +
  geom_hline(aes(yintercept = mean(Recall_Performance)), linetype = "dashed") +
  geom_point(position = position_jitter(width = .05, seed = 1), color = "gray") +
  stat_summary(fun = mean, geom = "point", size = 5, color = "#3B5689") +
  coord_cartesian(ylim = c(0, 40)) +
  guides(color = FALSE) +
  labs(x = "Presentation Rate", y = "Recall Performance")

ggsave(here("Lecture Slides/images", "error_variance_plot1.png"), 
       width = 6, height = 4, units = "in", dpi = 300)

set.seed(42069)
ggplot(d2, aes(x = Presentation_Rate, y = Recall_Performance)) +
  geom_hline(aes(yintercept = mean(Recall_Performance)), linetype = "dashed") +
  geom_point(position = position_jitter(width = .05, seed = 1), 
             aes(color = (Subject == 76 & Presentation_Rate == 1) |
                   (Subject == 18 & Presentation_Rate == 1),
                 size = (Subject == 76 & Presentation_Rate == 1) |
                   (Subject == 18 & Presentation_Rate == 1))) +
  stat_summary(fun = mean, geom = "point", size = 5, color = "#3B5689") +
  coord_cartesian(ylim = c(0, 40)) +
  scale_size_manual(values = c(1.5, 3)) +
  scale_color_manual(values = c("gray", "red")) +
  guides(color = FALSE, size = FALSE) +
  labs(x = "Presentation Rate", y = "Recall Performance")

ggsave(here("Lecture Slides/images", "error_variance_plot1.png"), 
       width = 6, height = 4, units = "in", dpi = 300)

set.seed(42069)
ggplot(d2, aes(x = Presentation_Rate, y = Recall_Performance,
                         color = Presentation_Rate)) +
  geom_hline(aes(yintercept = mean(Recall_Performance)), linetype = "dashed") +
  geom_point(position = position_jitter(width = .05, seed = 1)) +
  stat_summary(fun = mean, geom = "point", size = 5, color = "#3B5689") +
  coord_cartesian(ylim = c(0, 40)) +
  scale_color_manual(values = c("#F8766D", "#00BA38", "#619CFF"),
                     guide = "none") +
  guides(color = FALSE, size = FALSE) +
  labs(x = "Presentation Rate", y = "Recall Performance")

ggsave(here("Lecture Slides/images", "error_variance_plot_fix1.png"), 
       width = 6, height = 4, units = "in", dpi = 300)

d2_highlight <- mutate(d2, 
                       select_point = 
                         case_when(Subject == 76 & Presentation_Rate == 1 ~ 1,
                                   Subject == 18 & Presentation_Rate == 1 ~ 1,
                                   Subject == 28 & Presentation_Rate == 2 ~ 2,
                                   Subject == 65 & Presentation_Rate == 2 ~ 2,
                                   Subject == 24 & Presentation_Rate == 4 ~ 3,
                                   Subject == 54 & Presentation_Rate == 4 ~ 3,
                                   TRUE ~ 4),
                       select_point = factor(select_point, levels = c(1,2,3,4)))
set.seed(42069)
ggplot(d2, aes(x = Presentation_Rate, y = Recall_Performance)) +
  geom_hline(aes(yintercept = mean(Recall_Performance)), linetype = "dashed") +
  geom_line(aes(group = Subject, 
                color = (Subject == 76 | Subject == 18 | Subject == 24),
                linewidth = (Subject == 76 | Subject == 18 | Subject == 24)),
            position = position_jitter(width = .05, seed = 1),
            linetype = "dashed") +
  geom_point(position = position_jitter(width = .05, seed = 1),
             aes(color = Presentation_Rate,
                 size = (Subject == 76 | Subject == 18 | Subject == 24))) +
  stat_summary(fun = mean, geom = "line", group = 1,
               color = "#3B5689", size = 1) +
  stat_summary(fun = mean, geom = "point", size = 5, color = "#3B5689") +
  coord_cartesian(ylim = c(0, 40)) +
  scale_color_manual(values = c("#F8766D", "#00BA38", "#619CFF", "gray", "black")) +
  scale_linewidth_manual(values = c(0, 1)) +
  scale_size_manual(values = c(1.5,5)) +
  guides(color = FALSE, linewidth = FALSE, size = FALSE) +
  labs(x = "Presentation Rate", y = "Recall Performance")

ggsave(here("Lecture Slides/images", "error_variance_plot_fix2.png"), 
       width = 6, height = 4, units = "in", dpi = 300)

set.seed(42069)
ggplot(d2, aes(x = Presentation_Rate, y = Recall_Performance)) +
  geom_hline(aes(yintercept = mean(Recall_Performance)), linetype = "dashed") +
  geom_line(aes(group = Subject), color = "gray",
            position = position_jitter(width = .05, seed = 1)) +
  geom_point(position = position_jitter(width = .05, seed = 1), color = "gray") +
  stat_summary(fun = mean, geom = "line", group = 1,
               color = "#3B5689", size = 1) +
  stat_summary(fun = mean, geom = "point", size = 5, color = "#3B5689") +
  coord_cartesian(ylim = c(0, 40)) +
  guides(color = FALSE) +
  labs(x = "Presentation Rate", y = "Recall Performance")

ggsave(here("Lecture Slides/images", "error_variance_plot2.png"), 
       width = 6, height = 4, units = "in", dpi = 300)

set.seed(42069)
ggplot(d2, aes(x = Presentation_Rate, y = Recall_Performance)) +
  stat_summary(fun = mean, geom = "bar") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = .1) +
  coord_cartesian(ylim = c(0, 40)) +
  labs(x = "Presentation Rate", y = "Recall Performance")

ggsave(here("Lecture Slides/images", "bar_plot.png"), 
       width = 6, height = 4, units = "in", dpi = 300)

set.seed(42069)
ggplot(d2, aes(x = Presentation_Rate, y = Recall_Performance)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = .1) +
  stat_summary(fun = mean, geom = "point", size = 5, color = "#3B5689") +
  coord_cartesian(ylim = c(0, 40)) +
  labs(x = "Presentation Rate", y = "Recall Performance")

ggsave(here("Lecture Slides/images", "point_plot.png"), 
       width = 6, height = 4, units = "in", dpi = 300)

set.seed(42069)
ggplot(d2, aes(x = Presentation_Rate, y = Recall_Performance)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = .1) +
  stat_summary(fun = mean, geom = "line", group = 1,
               size = 1, color = "#3B5689") +
  stat_summary(fun = mean, geom = "point", size = 5, color = "#3B5689") +
  coord_cartesian(ylim = c(0, 40)) +
  labs(x = "Presentation Rate", y = "Recall Performance")

ggsave(here("Lecture Slides/images", "line_plot.png"), 
       width = 6, height = 4, units = "in", dpi = 300)

plot_1v2 <- d2 |>
  filter(Presentation_Rate %in% c(1,2))

plot_2v4 <- d2 |>
  filter(Presentation_Rate %in% c(2,4))

set.seed(42069)
ggplot(d2, aes(x = Presentation_Rate, y = Recall_Performance)) +
  geom_point(position = position_jitter(width = .05)) +
  geom_hline(aes(yintercept = mean(Recall_Performance)), color = "firebrick") +
  geom_vline(aes(xintercept = 2), color = "firebrick") +
  coord_cartesian(ylim = c(0, 40))

set.seed(42069)
ggplot(plot_1v2, aes(x = Presentation_Rate, y = Recall_Performance)) +
  geom_point(position = position_jitter(width = .05)) +
  geom_hline(aes(yintercept = mean(Recall_Performance)), color = "firebrick") +
  geom_vline(aes(xintercept = 1.5), color = "firebrick") +
  coord_cartesian(ylim = c(0, 40))

set.seed(42069)
ggplot(plot_1v2, aes(x = Presentation_Rate, y = Recall_Performance)) +
  geom_point(position = position_jitter(width = .05), color = "darkgray") +
  geom_hline(aes(yintercept = mean(Recall_Performance)), color = "firebrick") +
  geom_vline(aes(xintercept = 1.5), color = "firebrick") +
  coord_cartesian(ylim = c(0, 40))

set.seed(42069)
ggplot(plot_2v4, aes(x = Presentation_Rate, y = Recall_Performance)) +
  geom_point(position = position_jitter(width = .05)) +
  geom_hline(aes(yintercept = mean(Recall_Performance)), color = "firebrick") +
  geom_vline(aes(xintercept = 1.5), color = "firebrick") +
  coord_cartesian(ylim = c(0, 40))

set.seed(42069)
ggplot(plot_2v4, aes(x = Presentation_Rate, y = Recall_Performance)) +
  geom_point(aes(color = 
                   (Presentation_Rate == 4 & 
                      Recall_Performance > mean(Recall_Performance)) |
                   (Presentation_Rate == 2 & 
                      Recall_Performance <= mean(Recall_Performance))),
  position = position_jitter(width = .05)) +
  geom_hline(aes(yintercept = mean(Recall_Performance)), color = "firebrick") +
  geom_vline(aes(xintercept = 1.5), color = "firebrick") +
  coord_cartesian(ylim = c(0, 40)) +
  scale_color_manual(values = c("darkgray", "#3B5689")) +
  guides(color = FALSE)

set.seed(42069)
ggplot(d2, aes(x = Presentation_Rate, y = Recall_Performance)) +
  geom_point(aes(color = (Presentation_Rate == 4 & 
                            Recall_Performance > mean(Recall_Performance)) |
                   (Presentation_Rate %in% c(1,2) & 
                      Recall_Performance <= mean(Recall_Performance))),
             position = position_jitter(width = .05)) +
  geom_hline(aes(yintercept = mean(Recall_Performance)), color = "firebrick") +
  geom_vline(aes(xintercept = 2), color = "firebrick") +
  coord_cartesian(ylim = c(0, 40)) +
  scale_color_manual(values = c("darkgray", "#3B5689")) +
  guides(color = FALSE)


plot_reduced <- d2 |>
  mutate(keep = case_when(Presentation_Rate == 1 & 
                            Subject %in% c(67, 11, 33, 85) ~ 1,
                          Presentation_Rate == 2 &
                            Subject %in% c(43, 74) ~ 1,
                          Presentation_Rate == 4 &
                            Subject %in% c(85, 9) ~ 1,
                          TRUE ~ 0),
         y_mean = mean(Recall_Performance)) |>
  mutate(.by = Presentation_Rate,
         y_hat = mean(Recall_Performance)) |>
  filter(keep == 1)

plot_reduced_2v4 <- plot_reduced |>
  filter(Presentation_Rate %in% c(2,4))

set.seed(42069)
ggplot(plot_reduced_2v4, aes(x = Presentation_Rate, y = Recall_Performance)) +
  geom_segment(aes(xend = Presentation_Rate, yend = y_mean),
               position = position_jitter(width = .05, seed = 6969),
               color = "gray", size = 1.25) +
  geom_point(position = position_jitter(width = .05, seed = 6969)) +
  geom_hline(aes(yintercept = y_mean), color = "firebrick") +
  coord_cartesian(ylim = c(0, 40))

set.seed(42069)
ggplot(plot_reduced_2v4, aes(x = Presentation_Rate, y = Recall_Performance)) +
  geom_segment(aes(xend = Presentation_Rate, yend = y_hat),
               position = position_jitter(width = .05, seed = 6969),
               color = "gray", size = 1.25, alpha = .55) +
  geom_point(position = position_jitter(width = .05, seed = 6969)) +
  geom_point(aes(y = y_hat), size = 5, color = "#3B5689") +
  geom_hline(aes(yintercept = y_mean), color = "firebrick") +
  coord_cartesian(ylim = c(0, 40))

set.seed(42069)
ggplot(plot_reduced_2v4, aes(x = Presentation_Rate, y = Recall_Performance)) +
  geom_segment(aes(y = y_hat, x = Presentation_Rate,
                   xend = Presentation_Rate, yend = y_mean),
               color = "#3B5689", size = 1.25, alpha = .55) +
  geom_point(position = position_jitter(width = .05, seed = 6969)) +
  geom_point(aes(y = y_hat), size = 5, color = "#3B5689") +
  geom_hline(aes(yintercept = y_mean), color = "firebrick") +
  coord_cartesian(ylim = c(0, 40))

library(afex)

fit <- aov_car(Recall_Performance ~ 
                 Presentation_Rate + 
                 Error(Subject/Presentation_Rate),
               data = d2)

parameters::model_parameters(fit)

modelbased::estimate_contrasts(fit, p_adjust = "tukey")


rm(list = ls())
  



  labs(x = "Presentation Rate", y = "Recall Performance") +
  scale_color_brewer(palette = "Set1", name = "Memory Strategy") +
  scale_fill_brewer(palette = "Set1") +
  guides(fill = "none")

