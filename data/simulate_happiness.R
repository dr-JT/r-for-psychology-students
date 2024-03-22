library(sjPlot)
library(ggplot2)
library(dplyr)
library(lm.beta)
library(faux)

seed <- 1973
set.seed(seed)

# the effect of the moderator on y

P     <- 4               # Xb1
Njklm <- 38              # obs per cell
Njk   <- Njklm*P       # number of subjects
N     <- Njklm*P   # number of observations
id    <- gl(Njk,         1, N, labels = c(1:Njk))
Xb1   <- gl(P,   Njklm*1, N, labels = c("White", "Milk", "Dark", "Alcohol"))

mu      <- 60
eB1     <- c(-10, -5, 5, 10)

names(eB1)     <- levels(Xb1)

muJKLM <- mu + eB1[Xb1]
muId  <- rep(rnorm(Njk, 0, 3), each = 1)
mus   <- muJKLM + muId
sigma <- 30

y  <- round(rnorm(N, mus, sigma), 1)





x1_mean <- 68.5
x1_sd <- 143.59

x2_mean <- 49.6
x2_sd <- 19.89

x3_mean <- 56.3
x3_sd <- 16.48

x4_mean <- 6
x4_sd <- 4

y_mean <- mean(y)
y_sd <- sd(y)

cor_matrix_list <- list(White = c(1.00, .326, .243, .107, .284,
                                  .326, 1.00, .632, .302, .501,
                                  .243, .632, 1.00, .201, .443,
                                  .107, .302, .201, 1.00, .052,
                                  .284, .501, .443, .052, 1.00),
                        
                        Milk = c(1.00, .226, .253, .097, .232,
                                 .226, 1.00, .682, .252, .441,
                                 .243, .682, 1.00, .211, .403,
                                 .097, .252, .211, 1.00, .212,
                                 .232, .441, .403, .212, 1.00),
                        
                        Dark = c(1.00, .226, .233, .117, .184,
                                 .226, 1.00, .582, .312, .561,
                                 .233, .582, 1.00, .191, .483,
                                 .117, .312, .191, 1.00, .502,
                                 .184, .561, .483, .502, 1.00),
                        
                        Alcohol = c(1.00, .224, .253, .100, .194,
                                    .224, 1.00, .622, .352, .491,
                                    .253, .622, 1.00, .205, .453,
                                    .100, .352, .205, 1.00, .632,
                                    .194, .491, .453, .632, 1.00))

data <- list()
for (chocolate_type in unique(Xb1)) {
  print(chocolate_type)
  
  if (chocolate_type == "White") y_mean_choc <- y_mean - 10
  if (chocolate_type == "Milk") y_mean_choc <- y_mean - 5
  if (chocolate_type == "Dark") y_mean_choc <- y_mean + 5
  if (chocolate_type == "Alcohol") y_mean_choc <- y_mean + 10
  
  data[[chocolate_type]] <- rnorm_multi(n = Njklm,
                                        mu = c(x1_mean, x2_mean, x3_mean, 
                                               x4_mean, y_mean_choc),
                                        sd = c(x1_sd, x2_sd, x3_sd, 
                                               x4_sd, y_sd),
                                        r = cor_matrix_list[[chocolate_type]],
                                        varnames = c("x1", "x2", "x3", 
                                                     "x4", "y"),
                                        empirical = TRUE)
}
data <- bind_rows(data)
data$x5 <- Xb1

data <- data %>%
  mutate(x1 = (x1 - min(x1)) + 10,
         x2 = (x2 - min(x2)) + 5,
         x3 = (x3 - min(x3)) + 7,
         x4 = (x4 - min(x4)) + 0,
         y = (y - min(y)) + 10,
         id = row_number()) %>%
  select(Subject = id, Happiness = y, Financial_Wealth = x1, 
         Emotion_Regulation = x2, Social_Support = x3, 
         Chocolate_Consumption = x4, Type_of_Chocolate = x5)

readr::write_csv(data, here::here("Lecture Slides/data", "happiness_data.csv"))

rm(list = ls())

