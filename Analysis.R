# Packages/Libraries =====
library(googlesheets4)
library(ggplot2)
library(tidyverse)
install.packages("corrplot")
library(corrplot)
install.packages("gganimate")
library(gganimate)
install.packages("ggridges")
library(ggridges)
install.packages("tidygeocoder")
library(tidygeocoder)
install.packages("sf")
library(sf)
install.packages(c( "sfhotspot", "rnaturalearth", "rnaturalearthdata"))
library(sfhotspot)
library(rnaturalearth)
library(rnaturalearthdata)
install.packages("tidyr")
library(tidyr)


# Data import ====
data <- read_sheet("https://docs.google.com/spreadsheets/d/15IVe2_WEcwP6BUVWD9CAtAhV_jD6cXkmSl4ZSLyNklc/edit?gid=0#gid=0",
                   sheet = "data")
write_csv(data,file="data/dataOCT19.csv")


# Datasets =====
#only the 7 categories for 2025 N=200
GD2025categories <-  data %>% filter(!is.na(GD2025Rank)) %>% select(GD2025ShotOptions:GD2025Conditioning)

#2025 GD courses with all columns N=200
GD2025data <- data %>% filter(!is.na(GD2025Rank))

GD2025long <- GD2025categories  %>% 
  pivot_longer(cols = everything(), names_to = "Category", values_to = "Score")

GD2025cats_rank <- data %>% filter(!is.na(GD2025Rank)) %>% select(GD2025ShotOptions:GD2025Conditioning, GD2025Rank)
GD2025cats_ranklong <- GD2025cats_rank %>%  rename(Rank2025GD = GD2025Rank) %>%
  pivot_longer(
    cols = starts_with("GD2025"), # Select columns to pivot
    names_to = "Category",        # New column for original column names
    values_to = "Score")   %>%           # New column for the values
   mutate(Year = "2025") %>% rename(Rank = Rank2025GD)

GD2023cats_rank <- data %>% filter(!is.na(GD2023Rank)) %>% select(GD2023ShotOptions:GD2023Conditioning, GD2023Rank)
GD2023cats_ranklong <- GD2023cats_rank %>%  rename(Rank2023GD = GD2023Rank) %>%
  pivot_longer(
    cols = starts_with("GD2023"), # Select columns to pivot
    names_to = "Category",        # New column for original column names
    values_to = "Score")   %>%           # New column for the values
  mutate(Year = "2023") %>% rename(Rank = Rank2023GD)

GD2021cats_rank <- data %>% filter(!is.na(GD2021Rank)) %>% select(GD2021ShotOptions:GD2021Character, GD2021Rank)
GD2021cats_ranklong <- GD2021cats_rank %>%  rename(Rank2021GD = GD2021Rank) %>%
  pivot_longer(
    cols = starts_with("GD2021"), # Select columns to pivot
    names_to = "Category",        # New column for original column names
    values_to = "Score")   %>%           # New column for the values
  mutate(Year = "2021") %>% rename(Rank = Rank2021GD)

combined_yearstop200GD <- bind_rows(GD2021cats_ranklong, GD2023cats_ranklong, GD2025cats_ranklong)
combined_yearstop200GD$Category <- str_remove(combined_yearstop200GD$Category, "GD2021")
combined_yearstop200GD$Category <- str_remove(combined_yearstop200GD$Category, "GD2023")
combined_yearstop200GD$Category <- str_remove(combined_yearstop200GD$Category, "GD2025")

ggplot(combined_yearstop200GD) + geom_density_ridges(mapping = aes(x = Score, y = Year, fill = Year), color = "white") + 
  theme_classic() + facet_wrap(~ Category) + scale_x_continuous(breaks = seq(7, 9, by = .5)) +
  geom_vline(xintercept = c(7.5, 8.0, 8.5), linetype = "dashed", color = "gray", linewidth = .2)

ggplot(GD2025cats_ranklong, aes(x = Rank2025GD, y = Score, color = Category, group = Category)) +
  geom_smooth(se = FALSE) +
  labs(title = "Category Score (scale: 1-10)", x = "Rank", y = "Score") +
  theme_classic()  +
  scale_y_continuous(limits = c(7.00, 9.00), breaks = seq(7.00, 9.00, by = .25)) +
  scale_color_discrete(labels = c(
    "GD2025Aesthetics" = "Aesthetics",
    "GD2025Challenge" = "Challenge",
    "GD2025Character" = "Character",
    "GD2025Conditioning" = "Conditioning",
    "GD2025Fun" = "Fun",
    "GD2025LayoutVariety" = "Layout Variety",
    "GD2025ShotOptions" = "Shot Options"))

GD2025cats_ranklong %>% filter(Category == "GD2025ShotOptions" | Category == "GD2025LayoutVariety") %>%
  ggplot(aes(x = Rank2025GD, y = Score, color = Category, group = Category)) +
  geom_smooth(se = FALSE) +
  labs(title = "Category Score (scale: 1-10)", x = "Rank", y = "Score") +
  theme_classic()  +
  scale_y_continuous(limits = c(7.00, 9.00), breaks = seq(7.00, 9.00, by = .25)) +
  scale_color_discrete(labels = c(
    "GD2025LayoutVariety" = "Layout Variety",
    "GD2025ShotOptions" = "Shot Options"))

data %>% filter(!is.na(GD2025Rank))

# Public vs Private - Comparing GD and Golf ====
sum(is.na(data$GD2025Rank))
data %>% filter(!is.na(GD2025Rank)) %>% count(Status)

data %>% filter(GD2025Rank >= 1 & GD2025Rank <= 100) %>% count(Status)
data %>% filter(!is.na(GOLF2024Rank)) %>% count(Status)


# Percent of Courses covered ====
#approx 16,000 courses in the US
#approx 12,000 are public, approx 4,000 are private
(200/16000)*100
(33/12000)*100
(167/4000)*100


# Calculate the correlation matrix
GD2025categories <-  data %>% filter(!is.na(GD2025ShotOptions)) %>%
  select(GD2025ShotOptions:GD2025Conditioning)
# Create a basic correlation plot
cor_matrixGD2025 <- cor(GD2025categories)
new_names <- c("Shot Options", "Character", "Challenge", "Layout Variety",
               "Fun", "Aesthetics", "Conditioning")

# slope vs difficulty
SlopevDifficultymodel5 <- lm(GD2025Challenge ~ Slope, data = data)
summary(SlopevDifficultymodel5)
SlopevDifficultymodel3 <- lm(GD2023Challenge ~ Slope, data = data)
summary(SlopevDifficultymodel3)
SlopevDifficultymodel1 <- lm(GD2021Challenge ~ Slope, data = data)
summary(SlopevDifficultymodel1)
ggplot(data = data, aes(x = GD2021Challenge,  y = Slope)) + 
  geom_point(size=1) + geom_smooth(method = "lm") + theme_classic()

ggplot(data = data, aes(x = GD2025Rank,  y = Slope, color=Status)) + 
  geom_point(size=1) + geom_smooth(method = "lm", se = FALSE) + theme_classic()
ggplot(data = data, aes(x = GD2025Rank,  y = GD2025Challenge, color=Status)) + 
  geom_point(size=1) + geom_smooth(method = "lm", se = FALSE) + theme_classic()

ggplot(data = data, aes(x = GD2025Rank,  y = GD2025Conditioning, group=Status, color=Status)) + 
   geom_point(aes(color = Status)) + theme_classic() + geom_smooth(se = FALSE)
  scale_y_continuous(limits = c(6.75, 9.25), breaks = seq(6.75, 9.25, by = .25))

  ggplot(data = data, aes(x = GD2025Rank,  y = GD2025Challenge, group=Status, color=Status)) + 
    geom_point(aes(color = Status)) + theme_classic() + geom_smooth(se = FALSE)
  scale_y_continuous(limits = c(6.75, 9.25), breaks = seq(6.75, 9.25, by = .25))

  ggplot(data = data, aes(x = GD2025Rank,  y = GD2025ShotOptions, group=Status, color=Status)) + 
    geom_point(aes(color = Status)) + theme_classic() + geom_smooth(se = FALSE)
  scale_y_continuous(limits = c(6.75, 9.25), breaks = seq(6.75, 9.25, by = .25))
  
ggplot(data = data, aes(x = GD2025Rank,  y = GD2025LayoutVariety, group=Status, color=Status)) + 
    geom_point(aes(color = Status)) + theme_classic() + geom_smooth(se = FALSE)
  scale_y_continuous(limits = c(6.75, 9.25), breaks = seq(6.75, 9.25, by = .25))
  
  ggplot(data = data, aes(x = GD2025Rank,  y = GD2025Fun, group=Status, color=Status)) + 
    geom_point(aes(color = Status)) + theme_classic() + geom_smooth(se = FALSE)
  scale_y_continuous(limits = c(6.75, 9.25), breaks = seq(6.75, 9.25, by = .25))
 
  ggplot(data = data, aes(x = GD2025Rank,  y = GD2025Aesthetics, group=Status, color=Status)) + 
    geom_point(aes(color = Status)) + theme_classic() + geom_smooth(se = FALSE)
  scale_y_continuous(limits = c(6.75, 9.25), breaks = seq(6.75, 9.25, by = .25))
  
  ggplot(data = data, aes(x = GD2025Rank,  y = GD2025Character, group=Status, color=Status)) + 
    geom_point(aes(color = Status)) + theme_classic() + geom_smooth(se = FALSE)
  scale_y_continuous(limits = c(6.75, 9.25), breaks = seq(6.75, 9.25, by = .25))
  
rownames(cor_matrixGD2025) <- new_names
colnames(cor_matrixGD2025) <- new_names
corrplot(cor_matrixGD2025, method = "number", diag = FALSE, type = 'upper', tl.srt = 45,
         tl.col = 'black', tl.cex = 0.8)

ggplot(data = data, aes(x = GD2025ShotOptions,  y = GD2025LayoutVariety)) + 
  geom_point(size=1) + geom_smooth(method = "lm") + theme_classic() + 
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dotted", size = 1) +
  scale_x_continuous(limits = c(6.75, 9.25), breaks = seq(6.75, 9.25, by = .25)) +
  scale_y_continuous(limits = c(6.75, 9.25), breaks = seq(6.75, 9.25, by = .25))+
  labs(x = "Shot Options", y = "Layout Variety", title = "Golf Digest 2025 Scores", subtitle = "Layout Variety vs Shot Options")
  
ggplot(GD2025long) +
  geom_density_ridges(mapping = aes(x = Score, 
                                    y = Category, 
                                    fill = Category), 
                      color = "white") + theme_classic()

GDtheme_classic()GD2025categories$SO_LVdiff <- GD2025categories$GD2025ShotOptions - GD2025categories$GD2025LayoutVariety
ggplot(data = GD2025categories, aes(x = GD2025ShotOptions, y = SO_LVdiff)) + geom_point() +
   theme_classic() + scale_y_continuous(limits = c(-0.35, 0.35))




# post mm/dd/yy ====

data <- data %>%
  mutate(differenceG2024_GD2025 = GOLF2024Rank -GD2025Rank)
ggplot(data=data, aes(x=GD2025Rank, y=differenceG2024_GD2025)) + geom_point()


GD2025data <- GD2025data %>% mutate(GD2025ShotOptionsChange = GD2025ShotOptions - lead(GD2025ShotOptions)) %>%
  mutate(GD2025CharacterChange = GD2025Character - lead(GD2025Character)) %>%
  mutate(GD2025LayoutVarietyChange = GD2025LayoutVariety - lead(GD2025LayoutVariety)) %>%
  mutate(GD2025FunChange = GD2025Fun - lead(GD2025Fun)) %>%
  mutate(GD2025ChallengeChange = GD2025Challenge - lead(GD2025Challenge))  %>%
  mutate(GD2025AesteticsChange = GD2025Aesthetics - lead(GD2025Aesthetics)) %>%
  mutate(GD2025ConditioningChange = GD2025Conditioning - lead(GD2025Conditioning)) 

data <- data %>% mutate(GDShotOptionsChange = GD2025ShotOptions - lead(GD2023ShotOptions)) %>%
  mutate(GDCharacterChange = GD2025Character - lead(GD2023Character)) %>%
  mutate(GDLayoutVarietyChange = GD2025LayoutVariety - lead(GD2023LayoutVariety)) %>%
  mutate(GDFunChange = GD2025Fun - lead(GD2023Fun)) %>%
  mutate(GDChallengeChange = GD2025Challenge - lead(GD2023Challenge))  %>%
  mutate(GDAesteticsChange = GD2025Aesthetics - lead(GD2023Aesthetics)) %>%
  mutate(GDConditioningChange = GD2025Conditioning - lead(GD2023Conditioning))

data <- data %>% mutate(GD2025Score = GD2025ShotOptions+GD2025ShotOptions+GD2025Character+GD2025Challenge+
                          GD2025LayoutVariety+GD2025LayoutVariety+GD2025Fun+GD2025Aesthetics+GD2025Conditioning)


ggplot(GD2025long, aes(x = Score, fill = Category)) +
  geom_density(alpha = 0.3) + # alpha controls transparency
  labs(title = "Overlayed Density Plots", x = "Score", y = "Density") +
  theme_minimal()


data <- data %>%
  geocode(city = City, state = State, method = "osm")
us_states <- map_data("state")
ggplot() +
  geom_polygon(data = us_states, aes(x = long, y = lat, group = group), fill = "white", color = "darkslategray") +
  geom_point(data = data, aes(x = long, y = lat, color = Status), size = 1) +
  #scale_color_viridis_c() + # Example for coloring points based on a 'value'
  labs(title = "Points on US Map by City and State") +
  theme_void() +
  coord_map() # Ensures correct aspect ratio for the map

data %>% filter(!is.na(GD2025PublicRank)) %>% ggplot() + 
  geom_polygon(data = us_states, aes(x = long, y = lat, group = group), fill = "white", color = "darkslategray") +
  geom_point(aes(x = long, y = lat), size = 2) +
  #scale_color_viridis_c() + # Example for coloring points based on a 'value'
  labs(title = "Points on US Map by City and State") +
  theme_void() +
  coord_map() # Ensures correct aspect ratio for the map

data <- data %>% arrange(GD2021Rank) %>%
  mutate(
    next_Value2021score = lead(GD2021Score), # Get the value from the row below
    Percent_Greater_Than_next2021score = ((GD2021Score - next_Value2021score) / next_Value2021score) * 100
  )
ggplot(data = data, aes(x = GD2021Rank,  y = Percent_Greater_Than_next2021score)) + 
  geom_point(size=1) + geom_smooth(method = "lm") + theme_classic() + 
  geom_text(aes(label = GD2021Rank), nudge_y = 0.2)

data %>% filter(GD2025Rank >= 1 & GD2025Rank <= 20) %>% 
  select(Course, GD2025Conditioning, GD2023Conditioning, GD2021Conditioning) %>% 
  pivot_longer(
  cols = c(GD2025Conditioning, GD2023Conditioning, GD2021Conditioning), 
  names_to = "Year",        # New column for original column names
  values_to = "CondScore",         # New column for original column values
) %>% ggplot(aes(x = Year, y = CondScore)) + geom_point() + facet_wrap(~Course)
  