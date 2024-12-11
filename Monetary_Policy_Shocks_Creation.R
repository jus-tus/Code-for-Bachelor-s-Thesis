# ---
# title: "Monetary Policy Shocks Creation"
# author: "Justus Felkel"
# date: "12.12.2024"
# ---

# load packages 
library(dplyr)
library(ggplot2)
library(tidyverse)
library(readr)
library(lubridate)
library(cowplot)


#setting the working directory
setwd("C:/Users/Justus/BachelorsThesisJustus/Inflation-adjusted transactions/Bachelor's Thesis JF Code")


######################################################################################################################
#Creating monetary policy shocks using SARON futures prices
################################################################################################################################

#loading relevant datasets
mp_shocks <- read_delim("Saron Future Shocks.csv", delim = ";")

#make date into the right format
mp_shocks$date <- as.Date(mp_shocks$date, format = "%d.%m.%Y")

mp_shocks2 <- mp_shocks %>%
  mutate(month = month(date), year = year(date)) 

#start with june 2022
mp_shocks3 <- mp_shocks2[-(1:9), ]

#plotting the MP shocks, calculated as Ft - Ft-1
ggplot(mp_shocks3, aes(x = date, y = policy_shock)) +
  geom_line(color = "blue") +
  labs(title = "Surprise Element of Monetary Policy Changes", x = "Date", y = "Policy Shock in % points") +
  theme_minimal()


#start with 2020
shocks_ratechange_2020start1 <- read_delim("futureshocks_ratechange_start2020.csv", delim = ";")

shocks_ratechange_2020start1$date <- as.Date(shocks_ratechange_2020start1$date, format="%d.%m.%Y")

shocks_ratechange_2020start2 <- shocks_ratechange_2020start1 %>%
  arrange(date)


plot_shock_ratechange_2020start1 <- ggplot(shocks_ratechange_2020start2, aes(x = date)) +
  geom_point(aes(y = rate_change, color = "Change in Actual Policy Rate"), size = 1.5, shape = 1) + 
  geom_line(aes(y = policy_shock, color = "Monetary Policy Shocks"), size = 0.5) + 
  scale_y_continuous(
    name = "Interest Rate Level [%]",
    breaks = seq(-0.3, max(shocks_ratechange_2020start2$rate_change), by = 0.1),  
    labels = function(x) formatC(x, format = "f", digits = 2)
  ) + 
  scale_x_date(
    breaks = shocks_ratechange_2020start2$date,    # set breaks to be dates from dataset
    date_labels = "%d.%m.%Y"              # show full date
  ) +
  scale_color_manual(
    values = c("Change in Actual Policy Rate" = "red", "Monetary Policy Shocks" = "black")  # Define colors for the two legends
  ) +
  theme_bw() +
  theme(
    axis.title.y = element_text(color = "black", size = 12),  # Make main y-axis title larger
    axis.title.x = element_text(color = "black", size = 12),  # Make x-axis title larger
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Rotate x-axis labels
    axis.text.y = element_text(size = 10),  # Increase y-axis text size
    legend.position = "bottom",  # Place legend at the bottom
    legend.text = element_text(size = 12),  # Increase legend text size
    legend.title = element_blank(),  # Remove the legend title
    panel.grid.minor.x = element_blank(),  # Remove minor x-axis grid lines
    axis.line.y.right = element_blank(),  # Remove the right y-axis
    axis.ticks.y.right = element_blank()  # Remove the ticks on the right y-axis
  ) +
  labs(x = "Date")


#start with june 2022 instead
shocks_ratechange_2022start1 <- shocks_ratechange_2020start2[-(1:9), ]
  
shocks_ratechange_2022start1$date <- as.Date(shocks_ratechange_2022start1$date, format="%d.%m.%Y")

shocks_ratechange_2022start2 <- shocks_ratechange_2022start1 %>%
  arrange(date)
  

plot_shock_ratechange1 <- ggplot(shocks_ratechange_2022start2, aes(x = date)) +
  geom_point(aes(y = rate_change, color = "Change in Actual Policy Rate"), size = 1.5, shape = 1) + 
  geom_line(aes(y = policy_shock, color = "Monetary Policy Shocks"), size = 0.5) + 
  scale_y_continuous(
    name = "Interest Rate Level [%]",
    breaks = seq(-0.3, max(shocks_ratechange1$rate_change), by = 0.1),  
    labels = function(x) formatC(x, format = "f", digits = 2)
  ) + 
  scale_x_date(
    breaks = shocks_ratechange1$date,    # set breaks to be dates from dataset
    date_labels = "%d.%m.%Y"              # show full date
  ) +
  scale_color_manual(
    values = c("Change in Actual Policy Rate" = "red", "Monetary Policy Shocks" = "black")  # Define colors for the two legends
  ) +
  theme_bw() +
  theme(
    axis.title.y = element_text(color = "black", size = 12),  # Make main y-axis title larger
    axis.title.x = element_text(color = "black", size = 12),  # Make x-axis title larger
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Rotate x-axis labels
    axis.text.y = element_text(size = 10),  # Increase y-axis text size
    legend.position = "bottom",  # Place legend at the bottom
    legend.text = element_text(size = 12),  # Increase legend text size
    legend.title = element_blank(),  # Remove the legend title
    panel.grid.minor.x = element_blank(),  # Remove minor x-axis grid lines
    axis.line.y.right = element_blank(),  # Remove the right y-axis
    axis.ticks.y.right = element_blank()  # Remove the ticks on the right y-axis
  ) +
  labs(x = "Date")

#ggsave("graph.png", plot = plot_shock_ratechange2, width = 6, height = 4, dpi = 300)