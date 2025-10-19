# Packages/Libraries =====
library(googlesheets4)
library(ggplot2)
library(tidyverse)
install.packages("corrplot")
library(corrplot)

# Data import ====
data <- read_sheet("https://docs.google.com/spreadsheets/d/15IVe2_WEcwP6BUVWD9CAtAhV_jD6cXkmSl4ZSLyNklc/edit?gid=0#gid=0",
                   sheet = "data")
write_csv(data,file="data/dataOCT19.csv")

# Public vs Private - Comparing GD and Golf ====
sum(is.na(data$GD2025Rank))
data %>% filter(!is.na(GD2025Rank)) %>% count(Status)

data %>% filter(GD2025Rank >= 1 & GD2025Rank <= 100) %>% count(Status)
data %>% filter(!is.na(GOLF2024Rank)) %>% count(Status)

# Calculate the correlation matrix
GD2025categories <-  data %>% filter(!is.na(GD2025Rank)) %>%
  select(GD2025ShotOptions:GD2025Conditioning)
# Create a basic correlation plot
cor_matrixGD2025 <- cor(GD2025categories)
corrplot(cor_matrixGD2025, method = "number", diag = FALSE, type = 'upper')

# post mm/dd/yy ====
